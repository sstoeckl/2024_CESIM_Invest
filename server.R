# server.R
source("global.R")
library(DT)

server <- function(input, output, session) {
  
  # Reactive expression to determine active groups based on asset selection
  active_groups <- reactive({
    selected_assets <- input$asset_selection
    
    # Define groups based on the selected assets
    active_groups <- list(
      US = which(grepl("_US$", selected_assets)),
      EUR = which(grepl("_EU$", selected_assets)),
      CHF = which(grepl("_CH$", selected_assets)),
      GBP = which(grepl("_UK$", selected_assets)),
      JPY = which(grepl("_JP$", selected_assets)),
      Alternatives = which(grepl("^Alternatives", selected_assets)),
      Equities = which(grepl("^Equities", selected_assets)),
      Gov_bonds = which(grepl("^Gov_bonds", selected_assets)),
      Corp_bonds = which(grepl("^Corp_bonds", selected_assets))
    )
    
    # Delete inactive groups with integer(0)
    for (group in names(active_groups)) {
      if (length(active_groups[[group]]) == 0) {
        active_groups[[group]] <- NULL
      }
    }
    names(active_groups)
    active_groups
  })
  
  # UI for group constraints
  output$group_constraints_ui <- renderUI({
    groups <- active_groups()
    
    # Generate input fields for each group constraint
    tagList(
      lapply(names(groups), function(group) {
        fluidRow(
          column(6, numericInput(paste0("min_", group), label = paste("Min weight for", group), value = 0, step = 0.1)),
          column(6, numericInput(paste0("max_", group), label = paste("Max weight for", group), value = 1, step = 0.1))
        )
      }),
      hr(),
      # Box constraint (valid for all assets)
      h4("Box Constraint for All Assets"),
      fluidRow(
        column(6, numericInput("min_box", "Min weight for all assets", value = 0, step = 0.1)),
        column(6, numericInput("max_box", "Max weight for all assets", value = 1, step = 0.1))
      ),
      hr(),
      # Box constraint (valid for all assets)
      h4("Sum Constraint"),
      fluidRow(
        column(6, numericInput("min_sum", "Min weight sum", value = 1, step = 0.1)),
        column(6, numericInput("max_sum", "Max weight sum", value = 1, step = 0.1))
      )
    )
  })
  
  # Dynamically generate adjustment inputs based on selected assets
  output$adjustments_ui <- renderUI({
    selected_assets <- input$asset_selection  # Retrieve selected assets
    
    # Generate input fields for each selected asset
    tagList(
      lapply(selected_assets, function(asset) {
        fluidRow(
          column(6, numericInput(paste0("mean_adj_", asset), 
                                 label = paste("Adjust Mean for", asset), 
                                 value = 0, step = 0.01)),
          column(6, numericInput(paste0("sd_adj_", asset), 
                                 label = paste("Adjust Std Dev for", asset), 
                                 value = 0, step = 0.01))
        )
      })
    )
  })
  
  # Observe and update constraints based on asset selection
  observe({
    selected_groups <- active_groups()
    updateSelectizeInput(session, "group_constraints_ui", choices = selected_groups)
  })
  
  # Reactive expression to filter Returns based on selected assets
  # selected_returns <- reactive({
  #   selected_assets <- input$asset_selection
  #   valid_assets <- intersect(selected_assets, colnames(Returns))
  #   
  #   if (length(valid_assets) == 0) {
  #     showNotification("No valid assets selected. Please choose from available options.", type = "error")
  #     return(NULL)
  #   }
  #   # Ensure output is a matrix with column names for compatibility
  #   data_matrix <- as.matrix(Returns[, valid_assets, drop = FALSE])
  #   
  #   # Print structure of selected_returns for debugging
  #   print("Structure of selected_returns:")
  #   print(str(data_matrix))
  #   print("Column names of selected_returns:")
  #   print(colnames(data_matrix))
  #   # dput the final matrix for debugging
  #   #dput(data_matrix)
  #   
  #   return(data_matrix)
  #   # selected_returns <- data_matrix
  # })
  # Reactive dataset for selected assets, filtered by date and adjusted by user inputs
  # Reactive dataset for selected assets, filtered by date and adjusted by user inputs
  selected_returns <- reactive({
    selected_assets <- input$asset_selection  # Get selected assets
    date_range <- input$date_range            # Get selected date range
    
    # Filter Returns by selected date range and selected assets
    filtered_data <- Returns[paste0(date_range[1], date_range[2], sep = "/"), selected_assets, drop = FALSE]
    # Define weekly conversion factors
    weeks_per_year <- 52
    sd_conversion_factor <- sqrt(weeks_per_year)
    
    # Apply mean and standard deviation adjustments
    adjusted_data <- filtered_data
    
    for (asset in selected_assets) {
      # Retrieve user-specified adjustments
      mean_adj_annual <- input[[paste0("mean_adj_", asset)]] %||% 0  # Default to 0 if not provided
      sd_adj_annual <- input[[paste0("sd_adj_", asset)]] %||% 0      # Default to 0 if not provided
      
      # Convert annual adjustments to weekly adjustments
      mean_adj_weekly <- mean_adj_annual / weeks_per_year
      sd_adj_weekly <- sd_adj_annual / sd_conversion_factor
      
      # Apply adjustments to the returns data
      adjusted_data[,asset] <- adjusted_data[,asset] + mean_adj_weekly
      adjusted_data[,asset] <- (adjusted_data[,asset]-mean(adjusted_data[,asset])) * (1 + sd_adj_weekly/sd(adjusted_data[,asset])) + mean(adjusted_data[,asset])
    }
    
    return(as.matrix(adjusted_data))
  })
  
  ###### Portfolio Stuff
  # Run the optimization and store results
  optimize_result <- reactiveVal()  # Stores the result for access in multiple outputs
  saved_optimizations <- reactiveVal(list())  # Stores all saved optimizations
  
  # Set up portfolio specification and objectives dynamically
  observeEvent(input$optimize, {
    req(selected_returns())
    
    # Define portfolio specification
    # port_spec <- portfolio.spec(assets = colnames(selected_returns))
    port_spec <- portfolio.spec(assets = colnames(selected_returns()))
    
    # definitely add full investment constraint
    # replace by weight-sum
    port_spec <- add.constraint(
      portfolio = port_spec, 
      type = 'weight_sum', 
      min_sum = input$min_sum, 
      max_sum = input$max_sum
    )
    
    # Debug: Print portfolio spec after creation
    # print("Initial portfolio spec:")
    # print(port_spec)
    
    # Apply combined group constraints if any groups are active
    if (!is.null(active_groups())) {
      group_constraints <- list()
      group_labels <- names(active_groups())
      group_min <- c()
      group_max <- c()
      
      # Gather all active group constraints into a single list
      for (group in names(active_groups())) {
        group_constraints[[group]] <- active_groups()[[group]]
        group_min <- c(group_min, input[[paste0("min_", group)]])
        group_max <- c(group_max, input[[paste0("max_", group)]])
      }
      
      # Add the combined group constraint to the portfolio specification
      port_spec <- add.constraint(
        portfolio = port_spec, 
        type = "group", 
        groups = group_constraints,
        group_labels = group_labels,
        group_min = group_min,
        group_max = group_max,
        enabled = TRUE,
        message = FALSE
      )
    }
    
    # Add box constraint for all assets
    port_spec <- add.constraint(portfolio = port_spec, type = "box", min = input$min_box, max = input$max_box)
    
    # Debug: Print portfolio spec after adding constraints
    print("Portfolio spec after adding constraints:")
    print(port_spec)
    
    # Add selected objectives to the portfolio specification
    # Add objectives based on user selection
    if ("quadratic_utility" %in% input$objectives) {
      port_spec <- add.objective(portfolio = port_spec, type = "quadratic_utility", risk_aversion = input$risk_aversion)
    } else if ("sharpe" %in% input$objectives){
      port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")
      port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")
    } else {
      # Add risk (variance) and return (mean) objectives if Sharpe or standard optimization is selected
      if ("return" %in% input$objectives) {
        port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")
      }
      if ("risk" %in% input$objectives) {
        port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "var")
      }
    }
    # Set maxSR to TRUE if Sharpe Ratio is selected and quadratic utility is not selected
    max_sharpe <- ("sharpe" %in% input$objectives)
    # Debugging output for max_sharpe
    print(paste("max_sharpe is set to:", max_sharpe))
    
    # Debug: Print portfolio spec after adding objectives
    print("Portfolio spec after adding objectives:")
    print(port_spec)
    
    # Run optimization with maxSR based on Sharpe Ratio selection
    opt_results <- tryCatch(
      {
        rets <- selected_returns()
        if (max_sharpe){
          result <- optimize.portfolio(R = rets, portfolio = port_spec, optimize_method = "ROI", trace=TRUE, maxSR = TRUE)
        } else{
          result <- optimize.portfolio(R = rets, portfolio = port_spec, optimize_method = "ROI", trace=TRUE)
        }
        # Check if the result contains NA values and skip saving if so
        if (any(is.na(result$weights))) {
          showNotification("Optimization did not converge or produced NA values. Please check your inputs.", type = "error")
          return(NULL)
        }
        print(result)
        result
      },
      error = function(e) {
        showNotification(paste("Optimization failed:", e$message), type = "error")
        print("Error during optimization:")
        print(e$message)
        NULL
      }
    )

    if (!is.null(opt_results)) {
      optimize_result(opt_results)  # Store the current result
      
      # Save the result to saved optimizations
      saved_list <- saved_optimizations()
      saved_list[[length(saved_list) + 1]] <- opt_results
      saved_optimizations(saved_list)
    }
  })
  # Export code
  # Export Code Generation
  output$export_code <- renderText({
    req(selected_returns())
    selected_assets <- input$asset_selection
    valid_assets <- intersect(selected_assets, colnames(Returns))
    
    # Generate the R code
    code <- paste0(
      "library(PortfolioAnalytics)\n",
      "library(xts)\n",
      "library(PerformanceAnalytics)\n\n",
      
      "# Filter Returns data for selected assets\n",
      "selected_assets <- c(", paste0("'", valid_assets, "'", collapse = ", "), ")\n",
      "Returns_selected <- Returns[, selected_assets]\n\n",
      
      "# Portfolio specification\n",
      "port_spec <- portfolio.spec(assets = colnames(Returns_selected))\n",
      "port_spec <- add.constraint(portfolio = port_spec, type = 'weight_sum', ","min_sum = ", input$min_sum, ", max_sum = ", input$max_sum, ")\n",
      "port_spec <- add.constraint(portfolio = port_spec, type = 'box', min = ", input$min_box, ", max = ", input$max_box, ")\n"
    )
    
    # Apply combined group constraints if any groups are active
    # Add combined group constraints to the exported code using active groups
    if (!is.null(active_groups())) {
      group_constraints <- list()
      group_labels <- names(active_groups())
      group_min <- c()
      group_max <- c()
      
      for (group in names(active_groups())) {
        group_constraints[[group]] <- active_groups()[[group]]
        group_min <- c(group_min, input[[paste0("min_", group)]])
        group_max <- c(group_max, input[[paste0("max_", group)]])
      }
      
      # Generate export code with combined group constraints
      code <- paste0(code, 
                     "port_spec <- add.constraint(portfolio = port_spec, type = 'group', ",
                     "groups = list(", 
                     paste(sapply(names(group_constraints), function(g) {
                       paste0(g, " = c(", paste(group_constraints[[g]], collapse = ", "), ")")
                     }), collapse = ", "), 
                     "), group_labels = c(", paste(shQuote(group_labels), collapse = ", "), "), ",
                     "group_min = c(", paste(group_min, collapse = ", "), "), ",
                     "group_max = c(", paste(group_max, collapse = ", "), "), ",
                     "enabled = TRUE, message = FALSE)\n")
    }
    
    # Add objectives based on user selection
    if ("quadratic_utility" %in% input$objectives) {
      # Add only quadratic utility as an objective if selected
      code <- paste0(code, "port_spec <- add.objective(portfolio = port_spec, type = 'quadratic_utility', risk_aversion = ", input$risk_aversion, ")\n")
    } else {
      # Otherwise, add mean and variance objectives
      code <- paste0(
        code, 
        "port_spec <- add.objective(portfolio = port_spec, type = 'risk', name = 'var')\n",
        "port_spec <- add.objective(portfolio = port_spec, type = 'return', name = 'mean')\n"
      )
    }
    
    # Add optimization code with maxSR if Sharpe Ratio is selected
    max_sharpe <- "sharpe" %in% input$objectives
    code <- paste0(
      code, 
      "\n# Run optimization\n",
      "opt_results <- optimize.portfolio(R = Returns_selected, portfolio = port_spec, optimize_method = 'ROI', trace = TRUE, maxSR = ", max_sharpe, ")\n"
    )
    
    # Add optimization and plotting
    code <- paste0(
      code,
      "# Display results\n",
      "print(opt_results)\n",
      "weights <- extractWeights(opt_results)\n",
      "print(weights)\n",
      
      "# Plot efficient frontier\n",
      "chart.EfficientFrontier(opt_results, match.col = 'StdDev', main = 'Efficient Frontier')\n",
      "chart.EF.Weights(opt_results, main = 'Efficient Frontier Asset Weights')\n",
      "chart.GroupWeights(opt_results, main = 'Efficient Frontier Group Weights')\n"
    )
    
    # Display generated code as output
    code
  })
  
  # Display optimized weights as a table
  output$optimized_weights <- renderDataTable({
    req(optimize_result())  # Ensure optimization result is available
    
    # Force reactivity by assigning optimize_result() to a local variable
    opt_res <- optimize_result()
    
    # Extract optimized weights
    weights <- extractWeights(opt_res)
    
    # Convert to data frame for display
    weights_df <- data.frame(
      Asset = names(weights),
      Weight = round(weights, 4)
    )
    
    print("Updating optimized weights table.")  # Debug to confirm this part triggers
    datatable(weights_df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Display objective values achieved in the optimization
  output$objective_values <- renderPrint({
    req(optimize_result())  # Ensure optimization result is available
    
    # Force reactivity by assigning optimize_result() to a local variable
    opt_res <- optimize_result()
    
    print("Updating objective values.")  # Debug to confirm this part triggers
    print(opt_res)
  })
  
  # Plot efficient frontier with optimized portfolio
  output$efficient_frontier <- renderPlot({
    req(optimize_result())  # Ensure optimization result is available
    if ("efficient_frontier" %in% input$plot_selection) {
      # Force reactivity by assigning optimize_result() to a local variable
      opt_res <- optimize_result()
      
      print("Updating efficient frontier plot.")  # Debug to confirm this part triggers
      chart.EfficientFrontier(opt_res, match.col = "StdDev", main = "Efficient Frontier", n.portfolios = 10)
    }
  })
  
  # Extract and display group weights as a table
  output$group_weights <- renderDataTable({
    req(optimize_result())
    opt_res <- optimize_result()
    
    # Extract group weights
    group_weights <- extractGroups(opt_res)
    
    # Convert to data frame for display
    group_weights_df <- data.frame(
      Group = names(group_weights$group_weights),
      Weight = round(group_weights$group_weights, 4)
    )
    
    datatable(group_weights_df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Plot asset weights across the efficient frontier
  output$chart_ef_weights <- renderPlot({
    req(optimize_result())
    if ("weights_plot" %in% input$plot_selection) {
      opt_res <- optimize_result()
      chart.EF.Weights(opt_res, main = "Efficient Frontier Asset Weights", n.portfolios = 10)
    }
  })
  
  # Plot group weights across the efficient frontier
  output$chart_ef_weights_group <- renderPlot({
    req(optimize_result())
    if ("weights_plot" %in% input$plot_selection) {
      opt_res <- optimize_result()
      chart.EF.Weights(opt_res, main = "Efficient Frontier Group Weights", plot.type="barplot", n.portfolios = 10, by.groups=TRUE)
    }
  })
  # Plot group weights across the efficient frontier
  output$chart_group_weights <- renderPlot({
    req(optimize_result())
    if ("group_weights_plot" %in% input$plot_selection) {
      opt_res <- optimize_result()
      chart.GroupWeights(opt_res, grouping="groups", main = "Group Weights", plot.type="barplot")
    }
  })
  
  # Display optimization results in Optimization Results tab
  # output$optimization_results_ui <- renderUI({
  #   verbatimTextOutput("optimization_results")
  # })
  
  ######## SAVED OPTIMNIZATION STUFF
  # Create a reactive function to get saved optimizations with the unconstrained portfolio added
  saved_optimizations_with_unconstrained <- reactive({
    rets <- selected_returns()
    # Calculate the unconstrained portfolio specification for the efficient frontier
    unconstrained_spec <- portfolio.spec(assets = colnames(rets))
    unconstrained_spec <- add.constraint(portfolio = unconstrained_spec, type = "box", min = 0, max = 1)
    unconstrained_spec <- add.objective(portfolio = unconstrained_spec, type = "risk", name = "StdDev")
    unconstrained_spec <- add.objective(portfolio = unconstrained_spec, type = "return", name = "mean")
    
    # Calculate the efficient frontier for the unconstrained portfolio
    # Calculate the efficient frontier for the unconstrained portfolio and check for convergence
    unconstrained_frontier <- tryCatch({
      result <- optimize.portfolio(R = Returns_selected, portfolio = unconstrained_spec, optimize_method = "ROI", trace = TRUE)
      # Ensure result has valid weights (no NA) before adding
      if (any(is.na(result$weights))) {
        showNotification("Unconstrained optimization did not converge or produced NA values.", type = "error")
        return(NULL)
      }
      result  # Return the valid result
    }, error = function(e) {
      showNotification("Unconstrained optimization failed.", type = "error")
      NULL  # Return NULL on failure
    })
    
    # Add the unconstrained optimization as the first entry in the saved list
    saved_list <- saved_optimizations()
    if (!is.null(unconstrained_frontier)) {
      saved_list <- c(list(unconstrained_frontier), saved_list)
    }
    return(saved_list)
  })
  # Display saved optimizations as a table
  output$saved_optimizations <- renderDataTable({
    saved_list <- saved_optimizations_with_unconstrained()
    
    if (length(saved_list) > 0) {
      saved_summary <- lapply(saved_list, function(opt) {
        # Initialize an empty list to store calculated objective measures
        objective_measures <- list()
        
        # Check if mean was calculated and add it if present
        if ("mean" %in% names(extractObjectiveMeasures(opt))) {
          objective_measures$Mean_Return <- round(extractObjectiveMeasures(opt)$mean, 4)
        }
        
        # Check if StdDev was calculated and add it if present
        if ("StdDev" %in% names(extractObjectiveMeasures(opt))) {
          objective_measures$Std_Dev <- round(extractObjectiveMeasures(opt)$StdDev, 4)
        }
        
        # Calculate Sharpe Ratio if both Mean and StdDev are available
        if (!is.null(objective_measures$Mean_Return) && !is.null(objective_measures$Std_Dev) && objective_measures$Std_Dev > 0) {
          objective_measures$Sharpe_Ratio <- round(objective_measures$Mean_Return / objective_measures$Std_Dev, 4)
        } else {
          objective_measures$Sharpe_Ratio <- NA  # Assign NA if calculation is not possible
        }
        
        # Convert to data frame
        as.data.frame(objective_measures, stringsAsFactors = FALSE)
      })
      
      # Combine into a single data frame for display
      saved_summary_df <- do.call(rbind, saved_summary)
      saved_summary_df$Optimization_ID <- seq_len(nrow(saved_summary_df))  # Add ID column for reference
      datatable(saved_summary_df, options = list(pageLength = 10, scrollX = TRUE))
    } else {
      # Display message if no saved optimizations are found
      datatable(data.frame(Message = "No saved optimizations found."), options = list(pageLength = 1, scrollX = TRUE))
    }
  })
  
  # Plot the efficient frontier for the unconstrained solution and overlay saved optimizations
  output$efficient_frontier_with_saved <- renderPlot({
    # Use the combined saved list with the unconstrained portfolio as the first entry
    saved_list <- saved_optimizations_with_unconstrained()
    
    print(saved_list[[1]])
    # Plot the efficient frontier for the unconstrained solution
    chart.EfficientFrontier(saved_list[[1]], match.col = "StdDev", main = "Efficient Frontier with Saved Portfolios", col = "blue",
                            n.portfolios=10)
    
    # Overlay saved optimizations with labels
    for (i in seq_along(saved_list)) {
      opt <- saved_list[[i]]
      weights <- extractWeights(opt)
      mean_return <- extractObjectiveMeasures(opt)$mean
      std_dev <- extractObjectiveMeasures(opt)$StdDev
      
      # Add each saved portfolio to the plot, label the unconstrained optimization as "Unconstrained Portfolio"
      if (i == 1) {
        points(std_dev, mean_return, pch = 19, col = "red")
        text(std_dev, mean_return, labels = "Unconstrained Portfolio", pos = 4, col = "red")
      } else {
        points(std_dev, mean_return, pch = 19, col = i + 1)
        text(std_dev, mean_return, labels = paste("Portfolio", i - 1), pos = 4, col = i + 1)
      }
    }
  })
  # Reactive matrix of individual asset weights for all saved optimizations
  output$asset_weights_matrix <- renderTable({
    saved_list <- saved_optimizations_with_unconstrained()  # Retrieve saved optimizations with the unconstrained solution included
    
    if (length(saved_list) > 0) {
      selected_assets <- colnames(selected_returns())  # Get asset names
      
      # Extract weights for each saved optimization using extractGroups
      asset_weights <- sapply(saved_list, function(opt) {
        weights <- extractGroups(opt)$weights  # Extract individual asset weights
        round(weights[selected_assets], 4)     # Round weights to 4 digits for selected assets
      })
      
      # Convert the result to a matrix format with assets as rows and optimizations as columns
      rownames(asset_weights) <- selected_assets
      colnames(asset_weights) <- paste("Optimization", seq_along(saved_list))
      return(rownames_to_column(as.data.frame(asset_weights)))
    } else {
      return(data.frame(Message = "No saved optimizations available."))
    }
  })
  
  # Reactive matrix of group weights for all saved optimizations
  output$group_weights_matrix <- renderTable({
    saved_list <- saved_optimizations_with_unconstrained()  # Retrieve saved optimizations with the unconstrained solution included
    
    if (length(saved_list) > 0) {
      # Extract group weights for each saved optimization
      group_weights <- sapply(saved_list[-1], function(opt) {
        groups_raw <- extractGroups(opt)
        groups <- groups_raw$group_weights  # Extract group weights
        if (!is.null(groups)) {
          groups <- round(groups, 4)  # Round group weights to 4 digits
        }
        groups
      })
      print("Group Weights:")
      print(group_weights)
      
      # Convert to matrix format with groups as rows and optimizations as columns
      #rownames(group_weights) <- names(extractGroups(saved_list[[1]])$group_weights)
      colnames(group_weights) <- paste("Optimization", seq_along(saved_list[-1])+1)
      return(rownames_to_column(as.data.frame(group_weights)))
    } else {
      return(data.frame(Message = "No saved optimizations available."))
    }
  })
  
  # Remove a specific saved optimization
  observeEvent(input$delete_optimization, {
    saved_list <- saved_optimizations()
    if (length(saved_list) > 0 && input$delete_id <= length(saved_list)) {
      saved_list <- saved_list[-input$delete_id]  # Remove the selected optimization
      saved_optimizations(saved_list)  # Update saved optimizations
    }
  })
  
  ###### Statistical Stuff
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
    #value_at_risk <- apply(data, 2, function(x) VaR(x, p = 0.05, method = "historical"))

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
      VaR_5perc = NA#value_at_risk
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
