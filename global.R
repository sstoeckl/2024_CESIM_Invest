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
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

# Load the dataset
ret_data <- read_excel("data/Ret2.xlsx")
Returns <- xts(ret_data[, -1], order.by = as.Date(ret_data$Date))

# Define exact asset names based on dataset columns
asset_choices <- colnames(Returns)

#
input <- list()
input$asset_selection <- c("Alternatives","Equities_US","Equities_UK" )
input$min_box <- 0
input$max_box <- 1
input$objectives <- c("risk")
input$date_range <- c("2000-01-01","2015-12-31")
input$mean_adj_Alternatives <- 0.01
input$mean_adj_Equities_US <- 0.01
input$sd_adj_Alternatives <- 0.1
input$sd_adj_Equities_US <- 0.1
