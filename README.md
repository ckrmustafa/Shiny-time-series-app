# Shiny Time Series & Machine Learning App

This repository contains an interactive **Shiny dashboard** for time series analysis and machine learning–based forecasting.

## Structure
- **global.R** – Loads required packages and helper functions.
- **ui.R** – Defines the Shiny dashboard interface.
- **server.R** – Contains the server-side logic, model training, and report generation.
- **report_template.Rmd** – R Markdown template used to generate reports from the app.

## Installation
Clone or download the repository, then install required R packages:

```r
# Install required packages (only the first time)
pkgs <- c("shiny","shinydashboard","shinyjs","writexl","forecast","ggplot2","gridExtra",
          "zoo","lmtest","caret","randomForest","gbm","e1071","readr","dplyr","stats",
          "rpart","nnet","xgboost","DT","rpart.plot","partykit","rmarkdown",
          "tseries","jsonlite","corrplot","Boruta","DALEX","lime","iml","shapviz")
install.packages(setdiff(pkgs, rownames(installed.packages())))
