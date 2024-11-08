# global.R

# Load necessary libraries
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)

library(DT)
library(tidyverse)
library(lubridate)
library(slider)
library(purrr)
library(scales)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(readxl)
library(xts)

# Load the dataset
ret_data <- read_excel("data/Ret2.xlsx")
Returns <- xts(ret_data[, -1], order.by = as.Date(ret_data$Date))

# Define exact asset names based on dataset columns
asset_choices <- colnames(Returns)
