# global.R
# Load necessary libraries
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
# library(shinyjs)

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

# Define asset classes and currency groups
asset_classes <- c("Alternatives", "Equities", "Gov_bonds", "Corp_bonds")
currency_areas <- c("US", "UK", "CH", "EU", "JP")

# Define predefined groups for dynamic constraints
predefined_groups <- list(
  Alternatives_USD = "Alternatives",
  Equities_US = "Equities", Equities_UK = "Equities", Equities_CH = "Equities", Equities_EU = "Equities", Equities_JP = "Equities",
  Gov_bonds_US = "Gov_bonds", Gov_bonds_UK = "Gov_bonds", Gov_bonds_CH = "Gov_bonds", Gov_bonds_EU = "Gov_bonds", Gov_bonds_JP = "Gov_bonds",
  Corp_bonds_US = "Corp_bonds", Corp_bonds_UK = "Corp_bonds", Corp_bonds_CH = "Corp_bonds", Corp_bonds_EU = "Corp_bonds", Corp_bonds_JP = "Corp_bonds"
)

# Color scheme for styling
color_scheme <- list(
  democrat_color = "#337ab7",
  republican_color = "#d9534f",
  neutral_color = "#5bc0de"
)
