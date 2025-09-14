# ui.R

dashboardPage(
  dashboardHeader(title = "Time Series & ML Models Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("File Upload", tabName = "file_upload", icon = icon("upload")),
      menuItem("EDA & Preprocessing", tabName = "eda", icon = icon("chart-line")),
      menuItem("Feature Selection", tabName = "feature_selection", icon = icon("filter")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs")),
      menuItem("ARIMA & TBATS Models", tabName = "classic_models", icon = icon("line-chart")),
      menuItem("ML Models", tabName = "ml_models", icon = icon("brain")),
      menuItem("XAI - Explainable AI", tabName = "xai", icon = icon("question-circle")),
      menuItem("Model Performance", tabName = "performance", icon = icon("table")),
      menuItem("Linear Regression Formula", tabName = "lm_formula", icon = icon("calculator")),
      menuItem("Decision Tree Plot", tabName = "dt_plot", icon = icon("tree"))
    )
  ),
  dashboardBody(
    useShinyjs(), 
    tags$head(tags$style(HTML(".download-btn-wrapper { margin-top: 10px; margin-bottom: 5px; text-align: right; }"))),
    
    tabItems(
      tabItem(tabName = "welcome",
              h2("Welcome to the Time Series and Machine Learning Models Application!"),
              fluidRow(
                box(title = "Application Information", status = "primary", solidHeader = TRUE, width = 12,
                    p("This application allows you to perform forecasting and analysis on time series data using classical time series (ARIMA, TBATS) and various Machine Learning (ML) models."),
                    p("To start, go to the 'File Upload' section in the left menu and upload your own data."),
                    p("Then, you can explore your data's structure in the 'EDA & Preprocessing' tab, set model parameters in the 'Settings' tab, and initiate the analysis.")
                )
              )
      ),
      tabItem(tabName = "file_upload",
              h2("Upload Data File"),
              fluidRow(
                box(title = "Select CSV File", status = "primary", solidHeader = TRUE, width = 6,
                    fileInput("file1", "Please select your CSV file:",
                              multiple = FALSE,
                              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                    checkboxInput("header", "Header?", TRUE),
                    radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
                    radioButtons("quote", "Quote Character", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
                ),
                box(title = "First Look at Uploaded Data", status = "info", solidHeader = TRUE, width = 6,
                    DTOutput("uploaded_data_head"),
                    div(class = "download-btn-wrapper", downloadButton("download_uploaded_data", "Save Table (.xlsx)", class = "btn-sm"))
                )
              ),
              fluidRow(
                box(title = "Time Series Column & Lag Settings", status = "warning", solidHeader = TRUE, width = 12,
                    uiOutput("value_col_ui"),
                    sliderInput("num_lags", "Select Number of Lags for ML Features:", min = 1, max = 20, value = 3, step = 1),
                    actionButton("process_data", "Process Data and Create Time Series", icon = icon("cogs")),
                    verbatimTextOutput("data_processing_status")
                )
              )
      ),
      tabItem(tabName = "eda",
              h2("Exploratory Data Analysis (EDA) & Preprocessing Results"),
              fluidRow(
                box(title = "Time Series Plot", status = "primary", solidHeader = TRUE, width = 12, plotOutput("ts_plot_raw"), div(class = "download-btn-wrapper", downloadButton("download_ts_plot_raw", "Save Plot", class = "btn-sm"))),
                box(title = "Basic Statistics (Raw Data)", status = "info", solidHeader = TRUE, width = 6, verbatimTextOutput("summary_stats")),
                box(title = "Transformed Data Statistics (Box-Cox)", status = "info", solidHeader = TRUE, width = 6, verbatimTextOutput("summary_stats_bc"))
              ),
              fluidRow(
                box(title = "Autocorrelation (ACF) and Partial Autocorrelation (PACF) Plots", status = "warning", solidHeader = TRUE, width = 12, plotOutput("acf_pacf_plots"), div(class = "download-btn-wrapper", downloadButton("download_acf_pacf_plots", "Save Plot", class = "btn-sm"))),
                box(title = "Transformed Data Histogram", status = "primary", solidHeader = TRUE, width = 6, plotOutput("hist_bc"), div(class = "download-btn-wrapper", downloadButton("download_hist_bc", "Save Plot", class = "btn-sm"))),
                box(title = "Transformed Data Q-Q Plot", status = "primary", solidHeader = TRUE, width = 6, plotOutput("qq_bc"), div(class = "download-btn-wrapper", downloadButton("download_qq_bc", "Save Plot", class = "btn-sm")))
              ),
              fluidRow(box(title = "Raw and Transformed Data Histograms (Side-by-Side)", status = "info", solidHeader = TRUE, width = 12, plotOutput("hist_raw_transformed"), div(class = "download-btn-wrapper", downloadButton("download_hist_raw_transformed", "Save Plot", class = "btn-sm")))),
              fluidRow(box(title = "Feature Correlation Heatmap", status = "success", solidHeader = TRUE, width = 12, plotOutput("correlation_plot", height = "500px"), div(class = "download-btn-wrapper", downloadButton("download_correlation_plot", "Save Plot", class = "btn-sm"))))
      ),
      tabItem(tabName = "feature_selection",
              h2("Feature Selection Algorithms & Feature Selection for Modeling"),
              fluidRow(
                box(title = "Correlation-based Feature Removal", status = "primary", solidHeader = TRUE, width = 6,
                    sliderInput("correlation_threshold", "Correlation Threshold for Removal", min = 0.5, max = 0.99, value = 0.9, step = 0.01),
                    actionButton("apply_correlation_fs", "Apply Correlation Feature Selection"),
                    verbatimTextOutput("correlation_fs_output")
                ),
                box(title = "Boruta Feature Selection (Wrapper Method)", status = "primary", solidHeader = TRUE, width = 6,
                    p("Boruta performs a random forest-based feature selection. This might take time for large datasets."),
                    actionButton("run_boruta_fs", "Run Boruta Feature Selection"),
                    shinyjs::disabled(actionButton("use_boruta_features", "Use Boruta's Confirmed Features", icon = icon("check-double"), style = "color: #fff; background-color: #28a745; border-color: #28a745")),
                    sliderInput("boruta_top_n", "Top N features", min = 5, max = 50, value = 20, step = 1),
                    verbatimTextOutput("boruta_fs_output"),
                    plotOutput("boruta_plot", height = "600px"),
                    div(class = "download-btn-wrapper", downloadButton("download_boruta_plot", "Save Plot", class = "btn-sm"))
                )
              ),
              fluidRow(
                box(title = "Select Features for ML Models", status = "success", solidHeader = TRUE, width = 12,
                    p("Choose which features to include in the Machine Learning models. The models will be trained only with the selected features."),
                    uiOutput("feature_selection_ui"),
                    actionButton("confirm_selected_features", "Confirm Selected Features for Modeling", icon = icon("check")),
                    verbatimTextOutput("selected_features_status")
                )
              )
      ),
      tabItem(tabName = "settings",
              h2("Model Settings"),
              fluidRow(
                box(title = "General Settings", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(width = 6,
                             numericInput("seed_value", "Random Seed (set.seed)", value = 1881, min = 1, step = 1),
                             sliderInput("train_test_split", "Train/Test Split Ratio", value = 0.8, min = 0.5, max = 0.9, step = 0.05),
                             sliderInput("cv_folds", "Cross-Validation Folds (K-Fold)", value = 10, min = 3, max = 20, step = 1)
                      ),
                      column(width = 6,
                             checkboxGroupInput("models_to_run", "Select Machine Learning Models to Run:",
                                                choices = c("Random Forest" = "rf", "GBM" = "gbm", "SVM" = "svmRadial",
                                                            "Linear Regression" = "lm", "Decision Tree" = "rpart",
                                                            "Neural Network" = "nnet", "XGBoost" = "xgbTree"),
                                                selected = c("rf", "gbm", "svmRadial", "lm", "rpart", "nnet", "xgbTree"),
                                                inline = FALSE)
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             actionButton("run_analysis", "Start/Re-Run Analysis", icon = icon("play"), width = "100%", class = "btn-success btn-lg"),
                             helpText("Click 'Start/Re-Run Analysis' after changing settings or loading new data.")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Hyperparameter Tuning (Optional)", status = "warning", solidHeader = TRUE, width = 6,
                    selectInput("model_to_tune", "Select a model to tune its parameters:",
                                choices = c("None", 
                                            "Random Forest" = "rf", 
                                            "GBM" = "gbm", 
                                            "XGBoost" = "xgbTree")),
                    uiOutput("tuning_params_ui")
                ),
                box(title = "Save/Load & Report", status = "info", solidHeader = TRUE, width = 6,
                    p("You can save the current analysis results to an .RData file to load them later."),
                    textInput("rdata_filename", "RData File Name (auto-generated):", value = "analysis_results.RData"),
                    actionButton("save_results", "Save Results", icon = icon("save")),
                    actionButton("load_results", "Load Results", icon = icon("folder-open")),
                    hr(),
                    p("Generate a complete HTML report of the current analysis."),
                    downloadButton("report", "Generate and Download HTML Report")
                )
              ),
              fluidRow(box(title = "Analysis Status", status = "info", solidHeader = TRUE, width = 12, verbatimTextOutput("analysis_status")))
      ),
      tabItem(tabName = "classic_models",
              h2("ARIMA and TBATS Model Analysis"),
              fluidRow(
                column(width = 3,
                       box(title = "Plot Settings", status = "warning", solidHeader = TRUE, width = 12,
                           selectizeInput("plot_font_family", "Font Family", choices = c("Arial", "Times New Roman", "Courier New", "serif", "sans-serif"), selected = "sans-serif"),
                           sliderInput("plot_font_size", "Base Font Size", min = 8, max = 20, value = 14, step = 1),
                           textInput("arima_plot_title_suffix", "ARIMA Plot Title Suffix", value = "Forecast"),
                           textInput("tbats_plot_title_suffix", "TBATS Plot Title Suffix", value = "Forecast"),
                           radioButtons("legend_position_classic", "Legend Position", choices = c("None" = "none", "Right" = "right", "Left" = "left", "Top" = "top", "Bottom" = "bottom"), selected = "right"),
                           helpText("These settings apply to ARIMA & TBATS plots.")
                       )
                ),
                column(width = 9,
                       box(title = "ARIMA Forecast", status = "primary", solidHeader = TRUE, width = 12, plotOutput("arima_plot"), div(class = "download-btn-wrapper", downloadButton("download_arima_plot", "Save Plot", class = "btn-sm"))),
                       box(title = "TBATS Forecast", status = "primary", solidHeader = TRUE, width = 12, plotOutput("tbats_plot"), div(class = "download-btn-wrapper", downloadButton("download_tbats_plot", "Save Plot", class = "btn-sm")))
                )
              ),
              fluidRow(
                box(title = "TBATS Model Performance", status = "info", solidHeader = TRUE, width = 6, verbatimTextOutput("tbats_metrics_output")),
                box(title = "ARIMA Model Performance", status = "info", solidHeader = TRUE, width = 6, verbatimTextOutput("arima_metrics_output"))
              ),
              fluidRow(
                box(title = "TBATS Error Plot", status = "warning", solidHeader = TRUE, width = 6, plotOutput("plot_tbats_error"), div(class = "download-btn-wrapper", downloadButton("download_plot_tbats_error", "Save Plot", class = "btn-sm"))),
                box(title = "ARIMA Error Plot", status = "warning", solidHeader = TRUE, width = 6, plotOutput("plot_arima_error"), div(class = "download-btn-wrapper", downloadButton("download_plot_arima_error", "Save Plot", class = "btn-sm")))
              ),
              fluidRow(
                box(title = "TBATS Error Distribution", status = "warning", solidHeader = TRUE, width = 6, plotOutput("hist_tbats"), div(class = "download-btn-wrapper", downloadButton("download_hist_tbats", "Save Plot", class = "btn-sm"))),
                box(title = "ARIMA Error Distribution", status = "warning", solidHeader = TRUE, width = 6, plotOutput("hist_arima"), div(class = "download-btn-wrapper", downloadButton("download_hist_arima", "Save Plot", class = "btn-sm")))
              )
      ),
      tabItem(tabName = "ml_models",
              h2("Machine Learning Model Predictions"),
              fluidRow(
                column(width = 3,
                       box(title = "Plot Settings", status = "warning", solidHeader = TRUE, width = 12,
                           selectizeInput("plot_font_family_ml", "Font Family", choices = c("Arial", "Times New Roman", "Courier New", "serif", "sans-serif"), selected = "sans-serif"),
                           sliderInput("plot_font_size_ml", "Base Font Size", min = 8, max = 20, value = 14, step = 1),
                           textInput("ml_plot_title_suffix", "ML Plot Title Suffix", value = "Predictions"),
                           radioButtons("legend_position_ml", "Legend Position", choices = c("None" = "none", "Right" = "right", "Left" = "left", "Top" = "top", "Bottom" = "bottom"), selected = "right"),
                           helpText("These settings apply to ML prediction plots.")
                       )
                ),
                column(width = 9, uiOutput("ml_plots_ui"))
              )
      ),
      tabItem(tabName = "xai",
              h2("XAI - Explainable AI (LIME / SHAP)"),
              fluidRow(
                box(title = "LIME Explanation for Random Forest", status = "primary", solidHeader = TRUE, width = 6,
                    numericInput("lime_row_rf", "Select Row for RF Explanation (1 to N_test)", value = 1, min = 1, step = 1),
                    actionButton("run_lime_rf", "Generate LIME Explanation for RF"),
                    plotOutput("lime_plot_rf", height = "400px"),
                    div(class = "download-btn-wrapper", downloadButton("download_lime_plot_rf", "Save Plot", class = "btn-sm"))
                ),
                box(title = "LIME Explanation for XGBoost", status = "primary", solidHeader = TRUE, width = 6,
                    numericInput("lime_row_xgb", "Select Row for XGBoost Explanation (1 to N_test)", value = 1, min = 1, step = 1),
                    actionButton("run_lime_xgb", "Generate LIME Explanation for XGBoost"),
                    plotOutput("lime_plot_xgb", height = "400px"),
                    div(class = "download-btn-wrapper", downloadButton("download_lime_plot_xgb", "Save Plot", class = "btn-sm"))
                )
              ),
              fluidRow(
                box(title = "SHAP Explanations (Feature Importance)", status = "warning", solidHeader = TRUE, width = 12,
                    p("SHAP overall feature importance for Random Forest and XGBoost models. This calculation can be time consuming."),
                    actionButton("run_shap_importance", "Calculate SHAP Importance"),
                    plotOutput("shap_importance_plot", height = "500px"),
                    div(class = "download-btn-wrapper", downloadButton("download_shap_importance_plot", "Save Plot", class = "btn-sm")),
                    DTOutput("shap_importance_table"),
                    div(class = "download-btn-wrapper", downloadButton("download_shap_table", "Save Table (.xlsx)", class = "btn-sm"))
                )
              )
      ),
      tabItem(tabName = "performance",
              h2("Overall Model Performance Comparison"),
              fluidRow(
                box(
                  title = "Performance Visualization", status = "primary", solidHeader = TRUE, width = 12,
                  p("This chart compares the performance of all trained models. Time Series model metrics (ARIMA, TBATS) have been recalculated on the original data scale to ensure a fair comparison with ML models."),
                  radioButtons("perf_metric_choice", "Select Metric to Display:",
                               choices = c("Test RMSE" = "Test_RMSE", "Test R²" = "Test_R2"),
                               selected = "Test_RMSE", inline = TRUE),
                  plotOutput("combined_performance_plot", height = "600px"),
                  div(class = "download-btn-wrapper", downloadButton("download_combined_performance_plot", "Save Plot", class = "btn-sm"))
                )
              ),
              fluidRow(
                box(title = "Performance Metrics Table", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("performance_table"),
                    div(class = "download-btn-wrapper", downloadButton("download_performance_table", "Save Table (.xlsx)", class = "btn-sm"))
                )
              )
      ),
      tabItem(tabName = "lm_formula",
              h2("Mathematical Formulation of Linear Regression Model"),
              fluidRow(box(title = "Equation", status = "info", solidHeader = TRUE, width = 12, verbatimTextOutput("lm_equation")))
      ),
      tabItem(tabName = "dt_plot",
              h2("Decision Tree Structure"),
              fluidRow(box(title = "Decision Tree Visualization", status = "info", solidHeader = TRUE, width = 12, plotOutput("rpart_tree_plot", height = "600px"), div(class = "download-btn-wrapper", downloadButton("download_rpart_tree_plot", "Save Plot", class = "btn-sm")))),
              fluidRow(box(title = "Decision Tree Rules", status = "info", solidHeader = TRUE, width = 12, verbatimTextOutput("rpart_rules")))
      )
    )
  )
)

