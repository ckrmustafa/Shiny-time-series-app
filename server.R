suppressPackageStartupMessages({
  library(Boruta)
  library(dplyr)
  library(ggplot2)
  library(tibble)
  library(iml)
  library(shapviz)
})

server <- function(input, output, session) {
  # ==================================================
  # Reactive Value Initializations
  # ==================================================
  processed_data_reactive <- reactiveVal(NULL)
  ml_data_initial_features_reactive <- reactiveVal(NULL)
  selected_features_for_training_reactive <- reactiveVal(NULL)
  full_analysis_results_reactive <- reactiveVal(NULL)
  test_data_reactive <- reactiveVal(NULL)
  ml_data_for_training_reactive <- reactiveVal(NULL)
  
  lime_plot_rf_object <- reactiveVal(NULL)
  lime_plot_xgb_object <- reactiveVal(NULL)
  
  feature_selection_method_reactive <- reactiveVal("Manual")
  
  # ==================================================
  # 1. File Upload and Raw Data Reading
  # ==================================================
  raw_data <- reactive({
    req(input$file1)
    inFile <- input$file1
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    return(df)
  })
  
  output$uploaded_data_head <- renderDT({
    req(raw_data())
    datatable(head(raw_data(), 10), options = list(pageLength = 5, dom = 'tip'))
  })
  
  output$download_uploaded_data <- downloadHandler(
    filename = function() { "uploaded_data_preview.xlsx" },
    content = function(file) {
      req(raw_data())
      writexl::write_xlsx(head(raw_data(), 10), path = file)
    }
  )
  
  output$value_col_ui <- renderUI({
    req(raw_data())
    available_cols <- colnames(raw_data())
    exclude_cols <- c("Year", "Month", "Day", "year", "month", "day")
    choices_for_value <- setdiff(available_cols, exclude_cols)
    selectInput("value_col", "Select Dependent Variable Column", choices = choices_for_value)
  })
  
  # ==================================================
  # 2. Data Processing and Time Series Creation
  # ==================================================
  observeEvent(input$process_data, {
    req(raw_data(), input$value_col, input$num_lags)
    showNotification("Processing data...", type = "message", duration = NULL, id = "data_process_status")
    feature_selection_method_reactive("Manual")
    df <- raw_data()
    value_col_name <- input$value_col
    if (!("Year" %in% colnames(df) && "Month" %in% colnames(df) && "Day" %in% colnames(df))) {
      showNotification("Error: 'Year', 'Month', and 'Day' columns not found.", type = "error", duration = 5)
      return()
    }
    df$Date <- as.Date(paste(df$Year, df$Month, df$Day, sep = "-"), format = "%Y-%m-%d")
    if (any(is.na(df$Date))) {
      showNotification("Error: Date column contains invalid values.", type = "error", duration = 5)
      return()
    }
    df[[value_col_name]] <- as.numeric(df[[value_col_name]])
    if (any(is.na(df[[value_col_name]]))) {
      showNotification(paste0("Error: Column '", value_col_name, "' contains non-numeric values."), type = "error", duration = 5)
      return()
    }
    start_date <- min(df$Date, na.rm = TRUE)
    start_year <- as.numeric(format(start_date, "%Y"))
    start_day <- as.numeric(format(start_date, "%j"))
    ts_data_raw <- ts(df[[value_col_name]], frequency = 365, start = c(start_year, start_day))
    lambda <- BoxCox.lambda(ts_data_raw)
    ts_data_bc <- BoxCox(ts_data_raw, lambda)
    ml_data_full_gen <- tryCatch({
      lagged_features(data = df, lags = input$num_lags, target_col_name = value_col_name)
    }, error = function(e) {
      showNotification(paste0("Error in feature lagging: ", e$message), type = "error", duration = 10)
      return(NULL)
    })
    if (is.null(ml_data_full_gen)) {
      removeNotification(id = "data_process_status")
      return()
    }
    processed_data_reactive(list(original_df = df, ts_data_raw = ts_data_raw, ts_data_bc = ts_data_bc, lambda = lambda, value_col_name = value_col_name))
    ml_data_initial_features_reactive(ml_data_full_gen)
    initial_features <- setdiff(colnames(ml_data_full_gen), "Target")
    selected_features_for_training_reactive(initial_features)
    output$selected_features_status <- renderText(paste("Initial features for modeling:", paste(initial_features, collapse = ", ")))
    removeNotification(id = "data_process_status")
    output$data_processing_status <- renderText("Data processing complete.")
    showNotification("Data successfully processed!", type = "message", duration = 3)
  })
  
  output$data_processing_status <- renderText("Please upload a file and click 'Process Data'.")
  
  # ==================================================
  # Reactive Plot Themes
  # ==================================================
  classic_plot_theme <- reactive({
    ggplot2::theme_minimal(base_size = input$plot_font_size, base_family = input$plot_font_family) +
      ggplot2::theme(legend.position = input$legend_position_classic, axis.text = element_text(size = input$plot_font_size * 0.9),
                     plot.title = element_text(size = input$plot_font_size * 1.2, face = "bold"),
                     axis.title = element_text(size = input$plot_font_size))
  })
  
  ml_plot_theme <- reactive({
    ggplot2::theme_minimal(base_size = input$plot_font_size_ml, base_family = input$plot_font_family_ml) +
      ggplot2::theme(legend.position = input$legend_position_ml, axis.text = element_text(size = input$plot_font_size_ml * 0.9),
                     plot.title = element_text(size = input$plot_font_size_ml * 1.2, face = "bold"),
                     axis.title = element_text(size = input$plot_font_size_ml))
  })
  
  # ==================================================
  # 3. EDA Outputs & Download Handlers
  # ==================================================
  ts_plot_raw_object <- reactive({
    req(processed_data_reactive())
    data_processed <- processed_data_reactive()
    autoplot(data_processed$ts_data_raw) +
      labs(title = "Raw Time Series Plot", x = "Time", y = data_processed$value_col_name) +
      classic_plot_theme()
  })
  output$ts_plot_raw <- renderPlot({ ts_plot_raw_object() })
  output$download_ts_plot_raw <- downloadHandler(
    filename = function() { "time_series_raw_plot.png" },
    content = function(file) { ggsave(file, plot = ts_plot_raw_object(), device = "png", width = 10, height = 6, units = "in", dpi = 300) }
  )
  
  output$summary_stats <- renderPrint({
    req(processed_data_reactive())
    summary(processed_data_reactive()$original_df[[processed_data_reactive()$value_col_name]])
  })
  output$summary_stats_bc <- renderPrint({
    req(processed_data_reactive())
    summary(as.numeric(processed_data_reactive()$ts_data_bc))
  })
  
  output$acf_pacf_plots <- renderPlot({
    req(processed_data_reactive())
    data_processed <- processed_data_reactive()
    par(mfrow = c(1, 2))
    acf(data_processed$ts_data_bc, main = "ACF (Transformed Data)")
    pacf(data_processed$ts_data_bc, main = "PACF (Transformed Data)")
    par(mfrow = c(1, 1))
  })
  output$download_acf_pacf_plots <- downloadHandler(
    filename = function() { "acf_pacf_plots.png" },
    content = function(file) {
      req(processed_data_reactive())
      data_processed <- processed_data_reactive()
      png(file, width = 10, height = 5, units = "in", res = 300)
      par(mfrow = c(1, 2)); acf(data_processed$ts_data_bc, main = "ACF"); pacf(data_processed$ts_data_bc, main = "PACF"); dev.off()
    }
  )
  
  output$hist_bc <- renderPlot({
    req(processed_data_reactive())
    hist(processed_data_reactive()$ts_data_bc, main = "Transformed Data Histogram", xlab = "Transformed Data", col = "skyblue", border = "white")
  })
  output$download_hist_bc <- downloadHandler(
    filename = function() { "histogram_transformed.png" },
    content = function(file) {
      req(processed_data_reactive())
      png(file, width = 7, height = 6, units = "in", res = 300); hist(processed_data_reactive()$ts_data_bc, main = "Histogram", xlab = "Transformed Data", col = "skyblue", border = "white"); dev.off()
    }
  )
  
  output$qq_bc <- renderPlot({
    req(processed_data_reactive())
    qqnorm(processed_data_reactive()$ts_data_bc); qqline(processed_data_reactive()$ts_data_bc, col = "red")
  })
  output$download_qq_bc <- downloadHandler(
    filename = function() { "qq_plot_transformed.png" },
    content = function(file) {
      req(processed_data_reactive())
      png(file, width = 7, height = 6, units = "in", res = 300); qqnorm(processed_data_reactive()$ts_data_bc); qqline(processed_data_reactive()$ts_data_bc, col = "red"); dev.off()
    }
  )
  
  output$hist_raw_transformed <- renderPlot({
    req(processed_data_reactive())
    data_processed <- processed_data_reactive()
    par(mfrow = c(1, 2))
    hist(data_processed$original_df[[data_processed$value_col_name]], main = "Raw Data", xlab = "Raw Data", col = "lightblue", border = "white")
    hist(data_processed$ts_data_bc, main = "Transformed Data", xlab = "Transformed Data", col = "lightgreen", border = "white")
    par(mfrow = c(1, 1))
  })
  output$download_hist_raw_transformed <- downloadHandler(
    filename = function() { "histograms_raw_vs_transformed.png" },
    content = function(file) {
      req(processed_data_reactive())
      data_processed <- processed_data_reactive()
      png(file, width = 12, height = 6, units = "in", res = 300)
      par(mfrow = c(1, 2)); hist(data_processed$original_df[[data_processed$value_col_name]], main = "Raw Data", xlab = "Raw Data", col = "lightblue", border = "white"); hist(data_processed$ts_data_bc, main = "Transformed Data", xlab = "Transformed Data", col = "lightgreen", border = "white"); dev.off()
    }
  )
  
  output$correlation_plot <- renderPlot({
    req(processed_data_reactive())
    df_num <- processed_data_reactive()$original_df %>% select_if(is.numeric) %>% select(-any_of(c("Year", "Month", "Day")))
    if(ncol(df_num) < 2) {
      plot(1, type="n", main="Not Enough Numeric Features", xlab="", ylab="", axes=FALSE); text(1, 1, "Need at least 2 numeric columns.", cex=1.2)
    } else {
      M <- cor(df_num, use = "pairwise.complete.obs"); corrplot(M, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)
    }
  }, res = 96)
  
  output$download_correlation_plot <- downloadHandler(
    filename = function() { "correlation_heatmap.png" },
    content = function(file) {
      req(processed_data_reactive())
      df_num <- processed_data_reactive()$original_df %>% select_if(is.numeric) %>% select(-any_of(c("Year", "Month", "Day")))
      if(ncol(df_num) >= 2) {
        png(file, width = 8, height = 8, units = "in", res = 300); M <- cor(df_num, use = "pairwise.complete.obs"); corrplot(M, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7); dev.off()
      }
    }
  )
  
  # ==================================================
  # 4. Feature Selection & Download Handlers
  # ==================================================
  output$feature_selection_ui <- renderUI({
    req(ml_data_initial_features_reactive())
    all_features <- setdiff(colnames(ml_data_initial_features_reactive()), "Target")
    selectizeInput("features_to_use", "Select Features for ML Models:", choices = all_features, selected = selected_features_for_training_reactive(), multiple = TRUE, options = list('plugins' = list('remove_button'), 'persist' = FALSE))
  })
  
  observeEvent(input$confirm_selected_features, {
    req(input$features_to_use)
    selected_features_for_training_reactive(input$features_to_use)
    feature_selection_method_reactive("Manual")
    output$selected_features_status <- renderText(paste("Features selected:", paste(input$features_to_use, collapse = ", ")))
    showNotification("Selected features confirmed.", type = "message", duration = 5)
  })
  
  observeEvent(input$apply_correlation_fs, { feature_selection_method_reactive("Corr") })
  
  selected_features_correlation <- eventReactive(input$apply_correlation_fs, {
    req(ml_data_initial_features_reactive())
    data <- ml_data_initial_features_reactive(); correlation_threshold <- input$correlation_threshold
    features <- data %>% select(-Target)
    if (ncol(features) < 2) { output$correlation_fs_output <- renderPrint("Not enough features."); return(NULL) }
    cor_matrix <- cor(features, use = "pairwise.complete.obs")
    high_corr <- findCorrelation(cor_matrix, cutoff = correlation_threshold, names = TRUE, verbose = FALSE)
    features_to_keep <- setdiff(colnames(features), high_corr)
    output$correlation_fs_output <- renderPrint({ if (length(high_corr) > 0) { cat("Removed features:\n", paste(high_corr, collapse = ", "), "\n") } else { cat("No features removed.\n") }
      cat("\nKept features:\n", paste(features_to_keep, collapse = ", "), "\n")
    })
    updateSelectizeInput(session, "features_to_use", selected = features_to_keep)
    return(features_to_keep)
  })
  
  boruta_results <- eventReactive(input$run_boruta_fs, {
    shinyjs::disable("use_boruta_features")
    req(ml_data_initial_features_reactive())
    data <- ml_data_initial_features_reactive()
    res <- withProgress(message = "Running Boruta...", value = 0, {
      set.seed(input$seed_value)
      data_clean <- na.omit(data)
      req(nrow(data_clean) > 30, "Target" %in% colnames(data_clean))
      features_only <- dplyr::select(data_clean, -Target)
      req(ncol(features_only) > 0)
      res_internal <- tryCatch(Boruta(x = features_only, y = data_clean$Target, doTrace = 0, maxRuns = 100),
                               error = function(e) { showNotification(paste0("Boruta error: ", e$message), type = "error", duration = 10); NULL })
      if (!is.null(res_internal)) { showNotification("Boruta completed.", type = "message", duration = 3); shinyjs::enable("use_boruta_features") }
      res_internal
    })
    return(res)
  })
  
  output$boruta_fs_output <- renderPrint({ res <- boruta_results(); req(res); print(res); cat("\nConfirmed Features:\n"); print(getSelectedAttributes(res, withTentative = FALSE)) })
  
  boruta_plot_object <- reactive({
    res <- boruta_results(); req(res)
    top_n <- if (!is.null(input$boruta_top_n)) input$boruta_top_n else 20
    stats <- Boruta::attStats(res)
    df <- tibble::as_tibble(stats, rownames = "Attribute") %>% filter(!is.na(medianImp)) %>% arrange(desc(medianImp))
    keep <- utils::head(df, top_n)
    ggplot(keep, aes(x = stats::reorder(Attribute, medianImp), y = medianImp, fill = decision)) + geom_col() + coord_flip() + labs(x = NULL, y = "Median importance", title = paste0("Top-", nrow(keep), " Boruta Features")) + theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  })
  output$boruta_plot <- renderPlot({ boruta_plot_object() })
  output$download_boruta_plot <- downloadHandler(filename = function() { "boruta_importance.png" }, content = function(file) { ggsave(file, plot = boruta_plot_object(), device = "png", width = 10, height = 8, units = "in", dpi = 300) })
  
  observeEvent(input$use_boruta_features, {
    res <- boruta_results(); req(res)
    feature_selection_method_reactive("Boruta")
    confirmed_features <- getSelectedAttributes(res, withTentative = FALSE)
    if (length(confirmed_features) > 0) {
      updateSelectizeInput(session, inputId = "features_to_use", selected = confirmed_features)
      showNotification("Boruta's features selected for modeling.", type = "message", duration = 5)
    } else { showNotification("Boruta did not confirm any features.", type = "warning", duration = 5) }
  })
  
  # ==================================================
  # 5. Settings & Tuning
  # ==================================================
  observe({
    req(input$seed_value, input$train_test_split, input$cv_folds, feature_selection_method_reactive())
    seed <- input$seed_value; split_str <- gsub("\\.", "", as.character(input$train_test_split)); folds <- input$cv_folds; fs_method <- feature_selection_method_reactive()
    filename <- paste0("results_Seed", seed, "_Split", split_str, "_CV", folds, "_FS-", fs_method, ".RData")
    updateTextInput(session, "rdata_filename", value = filename)
  })
  
  output$tuning_params_ui <- renderUI({
    req(input$model_to_tune)
    if (input$model_to_tune == "rf") {
      tagList(
        h5("Random Forest Tuning"),
        sliderInput("rf_mtry", "Number of Variables to Sample (mtry):", min = 1, max = 20, value = c(2, 5), step = 1),
        helpText("Defines the range for 'mtry' to be tested.")
      )
    } else if (input$model_to_tune == "gbm") {
      tagList(
        h5("GBM Tuning"),
        sliderInput("gbm_ntrees", "Number of Trees (n.trees):", min = 50, max = 500, value = c(100, 200), step = 50),
        sliderInput("gbm_depth", "Interaction Depth:", min = 1, max = 7, value = c(1, 3), step = 1),
        sliderInput("gbm_shrinkage", "Shrinkage (Learning Rate):", min = 0.001, max = 0.1, value = c(0.01, 0.1))
      )
    } else if (input$model_to_tune == "xgbTree") {
      tagList(
        h5("XGBoost Tuning"),
        sliderInput("xgb_nrounds", "Number of Rounds (nrounds):", min = 50, max = 500, value = c(100, 200), step = 50),
        sliderInput("xgb_max_depth", "Max Depth:", min = 1, max = 10, value = c(3, 6), step = 1),
        sliderInput("xgb_eta", "Learning Rate (eta):", min = 0.01, max = 0.3, value = c(0.05, 0.15), step = 0.01)
      )
    } else {
      p("No model selected for tuning. Default parameters will be used.")
    }
  })
  
  # ==================================================
  # 6. Model Training and Prediction
  # ==================================================
  observeEvent(input$run_analysis, {
    req(ml_data_initial_features_reactive(), selected_features_for_training_reactive(), input$models_to_run)
    if (length(selected_features_for_training_reactive()) == 0) { showNotification("Error: No features selected.", type = "error", duration = 8); return() }
    if (length(input$models_to_run) == 0) { showNotification("Error: No ML models selected.", type = "error", duration = 8); return() }
    run_full_analysis()
  })
  
  run_full_analysis <- function() {
    selected_ml_models <- input$models_to_run
    num_main_steps <- 2 + length(selected_ml_models) + 1
    
    withProgress(message = 'Running Full Analysis', value = 0, {
      steps_completed <- 0
      incProgress(0, message = "Initializing...")
      ts_data_bc_val <- processed_data_reactive()$ts_data_bc; lambda_val <- processed_data_reactive()$lambda; ts_data_raw_val <- processed_data_reactive()$ts_data_raw; original_value_col_name <- processed_data_reactive()$value_col_name
      selected_cols_for_training <- c(selected_features_for_training_reactive(), "Target")
      ml_data_for_training_val <- ml_data_initial_features_reactive() %>% select(all_of(selected_cols_for_training)) %>% na.omit()
      if (nrow(ml_data_for_training_val) == 0 || ncol(ml_data_for_training_val) < 2) { showNotification("Error: No valid data for training.", type = "error", duration = 10); return(NULL) }
      train_index <- createDataPartition(ml_data_for_training_val$Target, p = input$train_test_split, list = FALSE)
      train_data <- ml_data_for_training_val[train_index, ]; test_data <- ml_data_for_training_val[-train_index, ]
      test_data_reactive(test_data)
      ctrl <- trainControl(method = "cv", number = input$cv_folds, savePredictions = "final")
      
      train_and_evaluate <- function(method, tuneGrid = NULL, ...) {
        model <- train(Target ~ ., data = train_data, method = method, trControl = ctrl, tuneGrid = tuneGrid, na.action = na.omit, ...)
        predictions <- predict(model, test_data)
        steps_completed <<- steps_completed + 1
        incProgress(1/num_main_steps, message = paste0("Training ", method, "..."))
        list(model = model, cv_metrics = c(RMSE = min(model$results$RMSE), R2 = max(model$results$Rsquared)),
             test_metrics = c(RMSE = sqrt(mean((test_data$Target - predictions)^2)), R2 = calc_r2(test_data$Target, predictions)),
             predictions = predictions, actual = test_data$Target)
      }
      
      tuneGrid_rf <- if(input$model_to_tune == "rf") expand.grid(mtry = seq(input$rf_mtry[1], input$rf_mtry[2], by = 1)) else NULL
      tuneGrid_gbm <- if(input$model_to_tune == "gbm") expand.grid(n.trees = seq(input$gbm_ntrees[1], input$gbm_ntrees[2], by=50), interaction.depth = seq(input$gbm_depth[1], input$gbm_depth[2], by=1), shrinkage = seq(input$gbm_shrinkage[1], input$gbm_shrinkage[2], length.out=2), n.minobsinnode = 10) else expand.grid(interaction.depth = 3, n.trees = 100, shrinkage = 0.1, n.minobsinnode = 10)
      tuneGrid_xgb <- if(input$model_to_tune == "xgbTree") expand.grid(nrounds = seq(input$xgb_nrounds[1], input$xgb_nrounds[2], by=50), max_depth = seq(input$xgb_max_depth[1], input$xgb_max_depth[2], by=1), eta = seq(input$xgb_eta[1], input$xgb_eta[2], length.out=2), gamma = 0, colsample_bytree = 0.8, min_child_weight = 1, subsample = 0.8) else expand.grid(nrounds = 100, max_depth = 6, eta = 0.1, gamma = 0, colsample_bytree = 0.8, min_child_weight = 1, subsample = 0.8)
      
      results <- list()
      if ("rf" %in% selected_ml_models) { if(!is.null(tuneGrid_rf)) showNotification("Tuning RF...", duration = 4); results$RF <- train_and_evaluate("rf", tuneGrid = tuneGrid_rf) }
      if ("gbm" %in% selected_ml_models) { if(input$model_to_tune == "gbm") showNotification("Tuning GBM...", duration = 4); results$GBM <- train_and_evaluate("gbm", tuneGrid = tuneGrid_gbm, verbose = FALSE) }
      if ("svmRadial" %in% selected_ml_models) { results$SVM <- train_and_evaluate("svmRadial") }
      if ("lm" %in% selected_ml_models) { results$LM <- train_and_evaluate("lm") }
      if ("rpart" %in% selected_ml_models) { results$RPART <- train_and_evaluate("rpart") }
      if ("nnet" %in% selected_ml_models) { results$NNET <- train_and_evaluate("nnet", linout = TRUE, trace = FALSE, skip = TRUE, MaxNWts = 10000, maxit = 500) }
      if ("xgbTree" %in% selected_ml_models) { if(input$model_to_tune == "xgbTree") showNotification("Tuning XGBoost...", duration = 4); results$XGB <- train_and_evaluate("xgbTree", tuneGrid = tuneGrid_xgb) }
      
      performance_df_ml <- if (length(results) > 0) sapply(results, `[[`, "test_metrics") %>% t() %>% as.data.frame() else data.frame(Test_RMSE=numeric(), Test_R2=numeric())
      if(nrow(performance_df_ml) > 0) colnames(performance_df_ml) <- c("Test_RMSE", "Test_R2")
      
      equation <- if ("lm" %in% input$models_to_run && "LM" %in% names(results)) {
        coefs <- summary(results$LM$model)$coefficients; eq <- paste0(original_value_col_name, " = ", sprintf("%.4f", coefs[1, 1]));
        for (i in 2:nrow(coefs)) { eq <- paste0(eq, sprintf(" + %.4f*%s", coefs[i, 1], rownames(coefs)[i])) }; eq
      } else { "Linear Regression model was not run." }
      
      fit_arima <- auto.arima(ts_data_bc_val, seasonal = TRUE); forecast_arima <- forecast(fit_arima, h = 7 * 365); steps_completed <<- steps_completed + 1; incProgress(1/num_main_steps, message = "Training ARIMA...")
      fit_tbats <- tbats(ts_data_bc_val); forecast_tbats <- forecast(fit_tbats, h = 7 * 365); steps_completed <<- steps_completed + 1; incProgress(1/num_main_steps, message = "Training TBATS...")
      
      fitted_arima_orig <- InvBoxCox(fitted(fit_arima), lambda_val); actuals_arima <- tail(as.numeric(ts_data_raw_val), length(fitted_arima_orig))
      rmse_arima_comp <- sqrt(mean((actuals_arima - fitted_arima_orig)^2, na.rm = TRUE)); r2_arima_comp <- calc_r2(actuals_arima, fitted_arima_orig)
      
      fitted_tbats_orig <- InvBoxCox(fitted(fit_tbats), lambda_val); actuals_tbats <- tail(as.numeric(ts_data_raw_val), length(fitted_tbats_orig))
      rmse_tbats_comp <- sqrt(mean((actuals_tbats - fitted_tbats_orig)^2, na.rm = TRUE)); r2_tbats_comp <- calc_r2(actuals_tbats, fitted_tbats_orig)
      
      performance_df_ts <- data.frame(Test_RMSE = c(rmse_arima_comp, rmse_tbats_comp), Test_R2 = c(r2_arima_comp, r2_tbats_comp), row.names = c("ARIMA", "TBATS"))
      
      performance_df_full <- rbind(performance_df_ml, performance_df_ts) %>% round(6)
      performance_df_full$ModelType <- c(rep("ML", nrow(performance_df_ml)), rep("Time Series", 2))
      
      incProgress(1/num_main_steps, message = "Calculating XAI Explainers...")
      xai_data <- list()
      
      # --- FIX: Avoid shapviz.default error. Compute mean |SHAP| using iml::Shapley results and DO NOT pass Shapley directly to shapviz(). ---
      if("RF" %in% names(results)) {
        predictor_rf <- Predictor$new(model = results$RF$model, data = train_data %>% select(-Target), y = train_data$Target, type = "raw")
        shap_values_rf <- Shapley$new(predictor_rf, x.interest = head(test_data %>% select(-Target)))
        sv_rf <- shap_values_rf$results
        imp_rf <- sv_rf %>% group_by(feature) %>% summarise(mean_abs_shap = mean(abs(phi), na.rm = TRUE), .groups = 'drop') %>% arrange(desc(mean_abs_shap))
        xai_data$shap_rf_importance <- imp_rf
        pdp_feature_rf <- imp_rf$feature[1]
        xai_data$pdp_rf <- FeatureEffect$new(predictor_rf, feature = pdp_feature_rf, method = "pdp+ice")
        xai_data$pdp_feature_rf <- pdp_feature_rf
      }
      
      if("XGB" %in% names(results)) {
        predictor_xgb <- Predictor$new(model = results$XGB$model, data = train_data %>% select(-Target), y = train_data$Target, type = "raw")
        shap_values_xgb <- Shapley$new(predictor_xgb, x.interest = head(test_data %>% select(-Target)))
        sv_xgb <- shap_values_xgb$results
        imp_xgb <- sv_xgb %>% group_by(feature) %>% summarise(mean_abs_shap = mean(abs(phi), na.rm = TRUE), .groups = 'drop') %>% arrange(desc(mean_abs_shap))
        xai_data$shap_xgb_importance <- imp_xgb
        pdp_feature_xgb <- imp_xgb$feature[1]
        xai_data$pdp_xgb <- FeatureEffect$new(predictor_xgb, feature = pdp_feature_xgb, method = "pdp+ice")
        xai_data$pdp_feature_xgb <- pdp_feature_xgb
      }
      
      showNotification("Analysis completed!", type = "message", duration = 3)
      
      full_analysis_results_reactive(list(results = results, performance_df = performance_df_full, lm_equation = equation,
                                          arima_metrics=list(rmse=rmse_arima_comp, r2=r2_arima_comp), tbats_metrics=list(rmse=rmse_tbats_comp, r2=r2_tbats_comp),
                                          arima_forecast_df = na.omit(data.frame(Time=as.numeric(time(forecast_arima$mean)), Forecast=as.numeric(InvBoxCox(forecast_arima$mean, lambda_val)))),
                                          tbats_forecast_df = na.omit(data.frame(Time=as.numeric(time(forecast_tbats$mean)), Forecast=as.numeric(InvBoxCox(forecast_tbats$mean, lambda_val)))),
                                          errors_tbats = as.numeric(residuals(fit_tbats)), errors_arima = as.numeric(residuals(fit_arima)),
                                          common_ylim = range(c(as.numeric(residuals(fit_tbats)), as.numeric(residuals(fit_arima))), na.rm = TRUE),
                                          ts_data_raw=ts_data_raw_val, value_col_name=original_value_col_name,
                                          actual_df_original_scale = data.frame(Time=as.numeric(time(ts_data_raw_val)), Actual=as.numeric(ts_data_raw_val)),
                                          train_data=train_data, test_data=test_data, xai_data = xai_data))
    })
  }
  
  # ==================================================
  # 7. Save/Load Analysis Results & Report
  # ==================================================
  observeEvent(input$save_results, {
    req(full_analysis_results_reactive())
    results_to_save <- full_analysis_results_reactive()
    file_path <- file.path(getwd(), input$rdata_filename)
    tryCatch({
      saveRDS(results_to_save, file = file_path)
      showNotification(paste0("Analysis results saved to '", input$rdata_filename, "'"), type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste0("Error saving results: ", e$message), type = "error", duration = 10)
    })
  })
  
  observeEvent(input$load_results, {
    file_path <- file.path(getwd(), input$rdata_filename)
    if (!file.exists(file_path)) {
      showNotification(paste0("Error: File '", input$rdata_filename, "' not found."), type = "error", duration = 5)
      return()
    }
    tryCatch({
      loaded_results <- readRDS(file_path)
      processed_data_reactive(list(ts_data_raw = loaded_results$ts_data_raw, ts_data_bc = loaded_results$ts_data_bc, lambda = loaded_results$lambda, value_col_name = loaded_results$value_col_name))
      test_data_reactive(loaded_results$test_data)
      reconstructed_ml_data_full <- rbind(loaded_results$train_data, loaded_results$test_data)
      ml_data_initial_features_reactive(reconstructed_ml_data_full)
      loaded_features <- setdiff(colnames(loaded_results$train_data), "Target")
      selected_features_for_training_reactive(loaded_features)
      updateSelectizeInput(session, "features_to_use", selected = loaded_features)
      output$selected_features_status <- renderText(paste("Features loaded:", paste(loaded_features, collapse = ", ")))
      full_analysis_results_reactive(loaded_results)
      showNotification(paste0("Results loaded from '", input$rdata_filename, "'."), type = "message", duration = 5)
      output$analysis_status <- renderText("Results loaded from file.")
    }, error = function(e) {
      showNotification(paste0("Error loading results: ", e$message), type = "error", duration = 10)
    })
  })
  
  output$report <- downloadHandler(
    filename = function() { paste0("analysis-report-", Sys.Date(), ".html") },
    content = function(file) {
      req(full_analysis_results_reactive())
      withProgress(message = 'Generating Detailed Report...', value = 0, {
        temp_report_path <- file.path(tempdir(), "report_template.Rmd")
        file.copy("report_template.Rmd", temp_report_path, overwrite = TRUE)
        pdp_feature_to_show <- if(!is.null(full_analysis_results_reactive()$xai_data$pdp_feature_xgb)) {
          full_analysis_results_reactive()$xai_data$pdp_feature_xgb
        } else if(!is.null(full_analysis_results_reactive()$xai_data$pdp_feature_rf)) {
          full_analysis_results_reactive()$xai_data$pdp_feature_rf
        } else { "N/A" }
        params <- list(analysis_results = full_analysis_results_reactive(),
                       settings = list(seed = input$seed_value, split = input$train_test_split, folds = input$cv_folds,
                                       lags = input$num_lags, fs_method = feature_selection_method_reactive(),
                                       xai_row_index = 1,
                                       pdp_feature = pdp_feature_to_show))
        rmarkdown::render(input = temp_report_path, output_file = file, params = params, envir = new.env(parent = globalenv()))
      })
    }
  )
  
  # ==================================================
  # 8. Outputs for Analysis Results & Download Handlers
  # ==================================================
  arima_plot_object <- reactive({
    res <- full_analysis_results_reactive(); req(res)
    ggplot() +
      geom_line(data = res$actual_df_original_scale, aes(x = Time, y = Actual, color = "Actual"), size = 1.2) +
      geom_line(data = res$arima_forecast_df, aes(x = Time, y = Forecast, color = "Forecast"), linetype = "dashed") +
      geom_smooth(data = res$arima_forecast_df, aes(x = Time, y = Forecast), method = "lm", se = FALSE, color = "red") +
      labs(title = paste("ARIMA", input$arima_plot_title_suffix, "\nRMSE:", round(res$arima_metrics$rmse, 5), " R²:", round(res$arima_metrics$r2, 2)),
           x = "Year", y = res$value_col_name) +
      scale_x_continuous(limits = c(min(res$actual_df_original_scale$Time), max(res$arima_forecast_df$Time)), breaks = seq(floor(min(res$actual_df_original_scale$Time)), ceiling(max(res$arima_forecast_df$Time)), by = 1)) +
      scale_color_manual(name = "Data", values = c("Actual" = "black", "Forecast" = "blue")) +
      classic_plot_theme()
  })
  output$arima_plot <- renderPlot({ arima_plot_object() })
  output$download_arima_plot <- downloadHandler(
    filename = function() { "arima_forecast.png" },
    content = function(file) { ggsave(file, plot = arima_plot_object(), device = "png", width = 12, height = 7, units = "in", dpi = 300) }
  )
  
  tbats_plot_object <- reactive({
    res <- full_analysis_results_reactive(); req(res)
    ggplot() +
      geom_line(data = res$actual_df_original_scale, aes(x = Time, y = Actual, color = "Actual"), size = 1.2) +
      geom_line(data = res$tbats_forecast_df, aes(x = Time, y = Forecast, color = "Forecast"), linetype = "dashed") +
      geom_smooth(data = res$tbats_forecast_df, aes(x = Time, y = Forecast), method = "lm", se = FALSE, color = "red") +
      labs(title = paste("TBATS", input$tbats_plot_title_suffix, "\nRMSE:", round(res$tbats_metrics$rmse, 5), " R²:", round(res$tbats_metrics$r2, 2)),
           x = "Year", y = res$value_col_name) +
      scale_x_continuous(limits = c(min(res$actual_df_original_scale$Time), max(res$tbats_forecast_df$Time)), breaks = seq(floor(min(res$actual_df_original_scale$Time)), ceiling(max(res$tbats_forecast_df$Time)), by = 1)) +
      scale_color_manual(name = "Data", values = c("Actual" = "black", "Forecast" = "purple")) +
      classic_plot_theme()
  })
  output$tbats_plot <- renderPlot({ tbats_plot_object() })
  output$download_tbats_plot <- downloadHandler(
    filename = function() { "tbats_forecast.png" },
    content = function(file) { ggsave(file, plot = tbats_plot_object(), device = "png", width = 12, height = 7, units = "in", dpi = 300) }
  )
  
  output$tbats_metrics_output <- renderPrint({
    res <- full_analysis_results_reactive(); req(res)
    cat("TBATS Model Performance Metrics (Original Scale):\n"); cat("RMSE:", res$tbats_metrics$rmse, "\n"); cat("R²:", res$tbats_metrics$r2, "\n")
  })
  output$arima_metrics_output <- renderPrint({
    res <- full_analysis_results_reactive(); req(res)
    cat("ARIMA Model Performance Metrics (Original Scale):\n"); cat("RMSE:", res$arima_metrics$rmse, "\n"); cat("R²:", res$arima_metrics$r2, "\n")
  })
  
  plot_tbats_error_object <- reactive({
    res <- full_analysis_results_reactive(); req(res)
    ggplot(data.frame(Time = 1:length(res$errors_tbats), Error = res$errors_tbats), aes(x = Time, y = Error)) +
      geom_line(color = "blue", size = 1) + labs(title = "TBATS Model Errors", x = "Time", y = "Error") + ylim(res$common_ylim) + classic_plot_theme()
  })
  output$plot_tbats_error <- renderPlot({ plot_tbats_error_object() })
  output$download_plot_tbats_error <- downloadHandler(
    filename = function() { "tbats_errors.png" },
    content = function(file) { ggsave(file, plot = plot_tbats_error_object(), device = "png", width = 10, height = 6, units = "in", dpi = 300) }
  )
  
  plot_arima_error_object <- reactive({
    res <- full_analysis_results_reactive(); req(res)
    ggplot(data.frame(Time = 1:length(res$errors_arima), Error = res$errors_arima), aes(x = Time, y = Error)) +
      geom_line(color = "red", size = 1) + labs(title = "ARIMA Model Errors", x = "Time", y = "Error") + ylim(res$common_ylim) + classic_plot_theme()
  })
  output$plot_arima_error <- renderPlot({ plot_arima_error_object() })
  output$download_plot_arima_error <- downloadHandler(
    filename = function() { "arima_errors.png" },
    content = function(file) { ggsave(file, plot = plot_arima_error_object(), device = "png", width = 10, height = 6, units = "in", dpi = 300) }
  )
  
  hist_tbats_object <- reactive({
    res <- full_analysis_results_reactive(); req(res)
    ggplot(data.frame(Error = res$errors_tbats), aes(x = Error)) +
      geom_histogram(bins = 30, fill = "lightblue", color = "black") + labs(title = "TBATS Error Distribution", x = "Error", y = "Frequency") + classic_plot_theme()
  })
  output$hist_tbats <- renderPlot({ hist_tbats_object() })
  output$download_hist_tbats <- downloadHandler(
    filename = function() { "tbats_error_histogram.png" },
    content = function(file) { ggsave(file, plot = hist_tbats_object(), device = "png", width = 8, height = 6, units = "in", dpi = 300) }
  )
  
  hist_arima_object <- reactive({
    res <- full_analysis_results_reactive(); req(res)
    ggplot(data.frame(Error = res$errors_arima), aes(x = Error)) +
      geom_histogram(bins = 30, fill = "lightcoral", color = "black") + labs(title = "ARIMA Error Distribution", x = "Error", y = "Frequency") + classic_plot_theme()
  })
  output$hist_arima <- renderPlot({ hist_arima_object() })
  output$download_hist_arima <- downloadHandler(
    filename = function() { "arima_error_histogram.png" },
    content = function(file) { ggsave(file, plot = hist_arima_object(), device = "png", width = 8, height = 6, units = "in", dpi = 300) }
  )
  
  output$ml_plots_ui <- renderUI({
    res <- full_analysis_results_reactive(); req(res)
    trained_models <- names(res$results)
    if (length(trained_models) == 0) return(p("No ML models were selected to run."))
    
    model_details <- list(RF = "Random Forest", GBM = "GBM", SVM = "SVM", LM = "Linear Regression",
                          RPART = "Decision Tree", NNET = "Neural Network", XGB = "XGBoost")
    
    plot_boxes <- lapply(trained_models, function(model_name) {
      fluidRow(box(title = paste(model_details[[model_name]], "Predictions"), status = "primary", solidHeader = TRUE, width = 12,
                   plotOutput(paste0(tolower(model_name), "_plot")),
                   div(class = "download-btn-wrapper", downloadButton(paste0("download_", tolower(model_name), "_plot"), "Save Plot", class = "btn-sm"))))
    })
    do.call(tagList, plot_boxes)
  })
  
  generate_plot_ml <- function(result_list, method_name, color, value_col_name, plot_title_suffix, current_theme) {
    df <- data.frame(Index = seq_along(result_list$actual), Actual = result_list$actual, Prediction = result_list$predictions)
    ggplot(df, aes(x = Index)) +
      geom_line(aes(y = Actual, color = "Actual"), size = 1.2) +
      geom_line(aes(y = Prediction, color = "Prediction"), linetype = "dashed") +
      labs(title = sprintf("%s %s\nCV RMSE: %.5f | Test RMSE: %.5f", method_name, plot_title_suffix, result_list$cv_metrics["RMSE"], result_list$test_metrics["RMSE"]),
           x = "Index", y = value_col_name) +
      scale_color_manual(name = "Data", values = c("Actual" = "black", "Prediction" = color)) +
      current_theme
  }
  
  model_map_full <- list(
    rf = list(short_name = "RF", long_name = "Random Forest", color = "blue"),
    gbm = list(short_name = "GBM", long_name = "GBM", color = "purple"),
    svmRadial = list(short_name = "SVM", long_name = "SVM", color = "red"),
    lm = list(short_name = "LM", long_name = "Linear Regression", color = "green"),
    rpart = list(short_name = "RPART", long_name = "Decision Tree", color = "orange"),
    nnet = list(short_name = "NNET", long_name = "Neural Network", color = "brown"),
    xgbTree = list(short_name = "XGB", long_name = "XGBoost", color = "deeppink")
  )
  
  observe({
    res <- full_analysis_results_reactive(); req(res, res$results)
    trained_short_names <- names(res$results)
    lapply(trained_short_names, function(short_name) {
      local({
        my_short_name <- short_name
        model_details <- Filter(function(x) x$short_name == my_short_name, model_map_full)[[1]]
        plot_obj <- reactive({
          res_local <- full_analysis_results_reactive(); req(res_local, my_short_name %in% names(res_local$results))
          generate_plot_ml(res_local$results[[my_short_name]], model_details$long_name, model_details$color, res_local$value_col_name, input$ml_plot_title_suffix, ml_plot_theme())
        })
        output[[paste0(tolower(my_short_name), "_plot")]] <- renderPlot({ plot_obj() })
        output[[paste0("download_", tolower(my_short_name), "_plot")]] <- downloadHandler(
          filename = function() { paste0(tolower(my_short_name), "_predictions.png") },
          content = function(file) { ggsave(file, plot = plot_obj(), device = "png", width = 12, height = 7, units = "in", dpi = 300) }
        )
      })
    })
  })
  
  combined_performance_plot_object <- reactive({
    res <- full_analysis_results_reactive(); req(res)
    perf_data <- res$performance_df %>% rownames_to_column(var = "Model")
    metric <- input$perf_metric_choice
    if (metric == "Test_RMSE") {
      perf_data$Model <- factor(perf_data$Model, levels = perf_data$Model[order(perf_data[[metric]], decreasing = FALSE)])
      y_label <- "Root Mean Square Error (Lower is Better)"
    } else {
      perf_data$Model <- factor(perf_data$Model, levels = perf_data$Model[order(perf_data[[metric]], decreasing = TRUE)])
      y_label <- "R-squared (Higher is Better)"
    }
    ggplot(perf_data, aes(x = Model, y = .data[[metric]], fill = ModelType)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = round(.data[[metric]], 4)), vjust = -0.5, size = 4) +
      labs(title = paste("Comparison of All Models by", metric), x = "Model", y = y_label, fill = "Model Type") +
      scale_fill_manual(values = c("ML" = "#3c8dbc", "Time Series" = "#f39c12")) +
      ml_plot_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
  })
  output$combined_performance_plot <- renderPlot({ combined_performance_plot_object() })
  output$download_combined_performance_plot <- downloadHandler(
    filename = function() { paste0("model_comparison_", input$perf_metric_choice, ".png") },
    content = function(file) { ggsave(file, plot = combined_performance_plot_object(), device = "png", width = 12, height = 8, units = "in", dpi = 300) }
  )
  
  output$performance_table <- renderDT({
    res <- full_analysis_results_reactive(); req(res)
    datatable(res$performance_df, options = list(pageLength = 10, dom = 'tip'), rownames = TRUE)
  })
  output$download_performance_table <- downloadHandler(
    filename = function() { "model_performance_metrics.xlsx" },
    content = function(file) {
      req(full_analysis_results_reactive())
      data_with_rownames <- full_analysis_results_reactive()$performance_df %>%
        rownames_to_column(var = "Model")
      writexl::write_xlsx(data_with_rownames, path = file)
    }
  )
  
  output$lm_equation <- renderPrint({
    res <- full_analysis_results_reactive(); req(res, "LM" %in% names(res$results))
    cat(res$lm_equation)
  })
  
  output$rpart_tree_plot <- renderPlot({
    res <- full_analysis_results_reactive(); req(res, "RPART" %in% names(res$results))
    prp(res$results$RPART$model$finalModel, type = 4, extra = 101, fallen.leaves = FALSE, roundint = FALSE, branch = 0.5, shadow.col = "gray", box.palette = "BuGn", main = "Decision Tree Structure")
  })
  output$download_rpart_tree_plot <- downloadHandler(
    filename = function() { "decision_tree.png" },
    content = function(file) {
      res <- full_analysis_results_reactive(); req(res, "RPART" %in% names(res$results))
      png(file, width = 12, height = 8, units = "in", res = 300)
      prp(res$results$RPART$model$finalModel, type = 4, extra = 101, fallen.leaves = FALSE, roundint = FALSE, branch = 0.5, shadow.col = "gray", box.palette = "BuGn", main = "Decision Tree Structure")
      dev.off()
    }
  )
  
  output$rpart_rules <- renderPrint({
    res <- full_analysis_results_reactive(); req(res, "RPART" %in% names(res$results))
    rpart.rules(res$results$RPART$model$finalModel, style = "wide", cover = TRUE, roundint = FALSE)
  })
  
  # ==================================================
  # 9. XAI - Explainable AI & Download Handlers
  # ==================================================
  observeEvent(input$run_lime_rf, {
    res <- full_analysis_results_reactive(); req(res, "RF" %in% names(res$results))
    req(test_data_reactive())
    test_data_xai <- test_data_reactive(); req(input$lime_row_rf <= nrow(test_data_xai))
    explainer <- lime::lime(res$train_data %>% select(-Target), model = res$results$RF$model)
    explanation <- lime::explain(test_data_xai[input$lime_row_rf, ] %>% select(-Target), explainer, n_features = 5)
    p <- plot_features(explanation) + labs(title = "LIME Explanation for Random Forest")
    lime_plot_rf_object(p); showNotification("LIME for Random Forest generated!", type = "message", duration = 3)
  })
  output$lime_plot_rf <- renderPlot({ req(lime_plot_rf_object()); lime_plot_rf_object() })
  output$download_lime_plot_rf <- downloadHandler(
    filename = function() { paste0("lime_rf_row_", input$lime_row_rf, ".png") },
    content = function(file) { req(lime_plot_rf_object()); ggsave(file, plot = lime_plot_rf_object(), device = "png", width = 8, height = 6, units = "in", dpi = 300) }
  )
  
  observeEvent(input$run_lime_xgb, {
    res <- full_analysis_results_reactive(); req(res, "XGB" %in% names(res$results))
    req(test_data_reactive())
    test_data_xai <- test_data_reactive(); req(input$lime_row_xgb <= nrow(test_data_xai))
    explainer <- lime::lime(res$train_data %>% select(-Target), model = res$results$XGB$model)
    explanation <- lime::explain(test_data_xai[input$lime_row_xgb, ] %>% select(-Target), explainer, n_features = 5)
    p <- plot_features(explanation) + labs(title = "LIME Explanation for XGBoost")
    lime_plot_xgb_object(p); showNotification("LIME for XGBoost generated!", type = "message", duration = 3)
  })
  output$lime_plot_xgb <- renderPlot({ req(lime_plot_xgb_object()); lime_plot_xgb_object() })
  output$download_lime_plot_xgb <- downloadHandler(
    filename = function() { paste0("lime_xgb_row_", input$lime_row_xgb, ".png") },
    content = function(file) { req(lime_plot_xgb_object()); ggsave(file, plot = lime_plot_xgb_object(), device = "png", width = 8, height = 6, units = "in", dpi = 300) }
  )
  
  shap_results_data <- eventReactive(input$run_shap_importance, {
    res <- full_analysis_results_reactive(); req(res, "RF" %in% names(res$results), "XGB" %in% names(res$results))
    withProgress(message = 'Calculating SHAP.', value = 0, {
      train_features_shap <- res$train_data %>% select(-Target); train_target_shap <- res$train_data$Target
      explainer_rf_shap <- DALEX::explain(model = res$results$RF$model, data = train_features_shap, y = train_target_shap, label = "Random Forest", type = "regression", verbose = FALSE)
      explainer_xgb_shap <- DALEX::explain(model = res$results$XGB$model, data = train_features_shap, y = train_target_shap, label = "XGBoost", type = "regression", verbose = FALSE)
      mp_rf <- model_parts(explainer_rf_shap, loss_function = loss_root_mean_square, B = 25)
      mp_xgb <- model_parts(explainer_xgb_shap, loss_function = loss_root_mean_square, B = 25)
      shap_data_rf <- as.data.frame(mp_rf) %>% mutate(Model = "Random Forest")
      shap_data_xgb <- as.data.frame(mp_xgb) %>% mutate(Model = "XGBoost")
      combined_shap_importance <- bind_rows(shap_data_rf, shap_data_xgb) %>%
        filter(variable != "_full_model_", variable != "_baseline_") %>%
        group_by(Model, variable) %>%
        summarise(MeanImportance = mean(dropout_loss), .groups = 'drop') %>%
        arrange(Model, desc(MeanImportance))
      showNotification("SHAP importance calculated!", type = "message", duration = 3)
      return(list(plot_data = plot(mp_rf, mp_xgb), table_data = combined_shap_importance))
    })
  })
  
  shap_plot_display_object <- reactive({
    req(shap_results_data())
    shap_results_data()$plot_data + 
      labs(title = "SHAP Feature Importance (Mean Dropout Loss)") + ml_plot_theme() + theme(axis.text.y = element_text(size = 10))
  })
  output$shap_importance_plot <- renderPlot({ shap_plot_display_object() })
  output$download_shap_importance_plot <- downloadHandler(
    filename = function() { "shap_importance.png" },
    content = function(file) { ggsave(file, plot = shap_plot_display_object(), device = "png", width = 10, height = 8, units = "in", dpi = 300) }
  )
  output$shap_importance_table <- renderDT({ req(shap_results_data()); datatable(shap_results_data()$table_data, rownames = FALSE, options = list(pageLength = 10, dom = 'tip')) })
  output$download_shap_table <- downloadHandler(
    filename = function() { "shap_importance.xlsx" },
    content = function(file) { req(shap_results_data()); writexl::write_xlsx(shap_results_data()$table_data, path = file) }
  )
  
  output$analysis_status <- renderText("Please upload a file from 'File Upload' tab and click 'Process Data'.")
}
