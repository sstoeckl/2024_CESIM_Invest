# server.R
source("global.R")
library(DT)

server <- function(input, output, session) {
  
  # Reactive expression to filter Returns based on selected assets
  selected_returns <- reactive({
    selected_assets <- input$asset_selection
    
    # Check if selected assets match available columns in Returns
    valid_assets <- intersect(selected_assets, colnames(Returns))
    
    # Show notification if no valid assets are selected
    if (length(valid_assets) == 0) {
      showNotification("No valid assets selected. Please choose from available options.", type = "error")
      return(NULL)
    }
    
    Returns[, valid_assets, drop = FALSE]
  })
  
  # Calculate enhanced summary statistics (annualized) for selected assets
  output$summary_stats <- renderDT({
    data <- selected_returns()
    req(data)  # Ensure data is available
    
    # Calculate annualized mean and standard deviation
    mean_annual <- apply(data, 2, function(x) mean(x) * 252)  # Assuming 252 trading days per year
    sd_annual <- apply(data, 2, function(x) sd(x) * sqrt(252))
    
    # Calculate additional statistics
    sharpe_ratio <- apply(data, 2, function(x) SharpeRatio.annualized(x))
    skewness <- apply(data, 2, function(x) skewness(x))
    kurtosis <- apply(data, 2, function(x) kurtosis(x))
    downside_dev <- apply(data, 2, function(x) DownsideDeviation(x))
    sortino_ratio <- apply(data, 2, function(x) SortinoRatio(x, MAR = 0))  # MAR = Minimum Acceptable Return
    max_drawdown <- apply(data, 2, function(x) maxDrawdown(x))
    value_at_risk <- apply(data, 2, function(x) VaR(x, p = 0.05, method = "historical"))
    
    # Compile all statistics into a data frame
    stats <- data.frame(
      Asset = colnames(data),
      Mean_Annual = mean_annual,
      SD_Annual = sd_annual,
      Sharpe_Ratio = sharpe_ratio,
      Skewness = skewness,
      Kurtosis = kurtosis,
      Downside_Dev = downside_dev,
      Sortino_Ratio = sortino_ratio,
      Max_Drawdown = max_drawdown,
      VaR_5perc = value_at_risk
    )
    
    # Use DT to display the table with rounded formatting
    datatable(stats, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(columns = c("Mean_Annual", "SD_Annual", "Sharpe_Ratio", "Skewness", "Kurtosis", "Downside_Dev", "Sortino_Ratio", "Max_Drawdown", "VaR_5perc"), digits = 4)
  })
  
  # Calculate and render covariance/correlation matrix with 4 decimal places
  output$cov_corr_matrix <- renderTable({
    data <- selected_returns()
    req(data)  # Ensure data is available
    
    # Calculate covariances and correlations
    cov_matrix <- cov(data, use = "complete.obs")
    cor_matrix <- cor(data, use = "complete.obs")
    
    # Create a combined matrix with covariances in lower, variances in diagonal, correlations in upper
    combined_matrix <- cov_matrix
    combined_matrix[upper.tri(combined_matrix)] <- cor_matrix[upper.tri(cor_matrix)]
    
    # Format each entry to four decimal places
    formatted_matrix <- apply(combined_matrix, c(1, 2), function(x) formatC(x, format = "f", digits = 4))
    as.data.frame(formatted_matrix)
  }, rownames = TRUE)
}
