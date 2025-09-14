# global.R

# ==================================================
# Load Required Packages
# ==================================================
library(shiny)
library(shinydashboard)
library(shinyjs) 
library(writexl)
library(forecast)
library(ggplot2)
library(gridExtra)
library(zoo)
library(lmtest)
library(caret)
library(randomForest)
library(gbm)
library(e1071)
library(readr)
library(dplyr)
library(stats)
library(rpart)
library(nnet)
library(xgboost)
library(DT)
library(rpart.plot)
library(partykit)
library(rmarkdown)
library(tseries)
library(jsonlite)
library(corrplot)
library(Boruta)
library(DALEX)
library(lime)

# --- NEW PACKAGES FOR ADVANCED REPORTING ---
library(iml)      # For PDP/ICE plots
library(shapviz)  # For SHAP Waterfall plots
# ==================================================

# Helper Functions
shift_vector <- function(x, k = 1) {
  n <- length(x)
  if(k < 0) stop("k must be non-negative")
  if(k == 0) return(x)
  if(k >= n) return(rep(NA, n))
  c(rep(NA, k), x[1:(n - k)])
}

lagged_features <- function(data, lags = 3, target_col_name) {
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  
  n_rows <- nrow(data)
  if (n_rows <= lags) {
    stop("Not enough rows in data to create specified number of lags.")
  }
  
  numerical_cols <- sapply(data, is.numeric)
  feature_cols_names <- setdiff(names(data)[numerical_cols], c("Year", "Month", "Day", target_col_name))
  
  ml_data_list <- list()
  
  for (col_name in feature_cols_names) {
    for (i in 0:lags) {
      ml_data_list[[paste0(col_name, "_Lag_", i)]] <- data[[col_name]][(lags - i + 1):(n_rows - i)]
    }
  }
  
  for (i in 1:lags) {
    ml_data_list[[paste0(target_col_name, "_Lag_", i)]] <- data[[target_col_name]][(lags - i + 1):(n_rows - i)]
  }
  
  ml_data_list$Target <- data[[target_col_name]][(lags + 1):n_rows]
  
  final_ml_df <- as.data.frame(ml_data_list)
  
  final_ml_df <- na.omit(final_ml_df)
  
  return(final_ml_df)
}

calc_r2 <- function(actual, predicted) {
  sse <- sum((actual - predicted)^2, na.rm = TRUE)
  sst <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  1 - sse/sst
}

