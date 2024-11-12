# ui.R
source("global.R")

ui <- dashboardPage(
  dashboardHeader(title = "Portfolio Optimization App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Asset Selection and Statistics", tabName = "assets", icon = icon("chart-line")),
      menuItem("Portfolio Optimization", tabName = "portfolio_opt", icon = icon("chart-bar")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # First Page: Asset Selection and Statistics
      tabItem(
        tabName = "assets",
        fluidRow(
          tabBox(
            width = 12,
            title = "Asset Selection and Statistics",
            tabPanel("Select Assets", 
                     pickerInput(
                       inputId = "asset_selection",
                       label = "Select Asset Classes and Currency Areas:",
                       choices = asset_choices,
                       multiple = TRUE,
                       selected = asset_choices,
                       options = pickerOptions(actionsBox = TRUE)
                     ),
                     sliderInput("date_range", 
                                 "Select Date Range", 
                                 min = min(time(Returns)), 
                                 max = max(index(Returns)),
                                 value = c(min(index(Returns)), max(index(Returns))),
                                 timeFormat = "%Y-%m-%d")
            ),
            tabPanel("Statistics and Correlation",
                     h3("Summary Statistics"),
                     DTOutput("summary_stats")|> withSpinner(type=3, color.background="white"),  # Use DTOutput for displaying the table
                     br(),
                     h3("Covariance / Correlation Matrix"),
                     tableOutput("cov_corr_matrix")|> withSpinner(type=3, color.background="white")
            ),
            tabPanel("Adjust Parameters", 
                     uiOutput("adjustments_ui"),
                     actionButton("save_parameters", "Save Changes")
            )
          )
        )
      ),
      
      # Second Page: Portfolio Optimization
      tabItem(
        tabName = "portfolio_opt",
        fluidRow(
          tabBox(
            width = 12,
            title = "Portfolio Optimization",
            tabPanel("Group Constraints", 
                     h3("Set Group Constraints"),
                     uiOutput("group_constraints_ui")  # Dynamic group constraints UI with box constraint
            ),
            tabPanel("Portfolio Objectives",
                     h3("Select Portfolio Objectives"),
                     checkboxGroupInput("objectives", "Choose Objectives:",
                                        choices = list(
                                          "Risk" = "risk",
                                          "Return" = "return",
                                          "Sharpe Ratio" = "sharpe",
                                          "Quadratic Utility" = "quadratic_utility"
                                        ),
                                        selected = "sharpe"
                     ),
                     # Additional input for Quadratic Utility (only visible if selected)
                     conditionalPanel(
                       condition = "input.objectives.includes('quadratic_utility')",
                       numericInput("risk_aversion", "Risk Aversion (for Quadratic Utility)", value = 2.5, min = 0, step = 0.1)
                     ),
                     # Button to execute optimization
                     actionButton("optimize", "Run Optimization"),
                     br(),
                     h3("Export Code"),
                     actionButton("export_code_button", "Export Code"),
                     verbatimTextOutput("export_code")  # Display the generated code
            ),
            tabPanel("Optimization Results",
                     h3("Optimized Portfolio Weights"),
                     DTOutput("optimized_weights")|> withSpinner(type=3, color.background="white"),
                     br(),
                     h3("Group Weights"),
                     DTOutput("group_weights")|> withSpinner(type=3, color.background="white"),
                     br(),
                     h3("Objective Values Achieved"),
                     verbatimTextOutput("objective_values")|> withSpinner(type=3, color.background="white"),
                     br(),
                     h3("Select Plots"),
                     # Checkbox inputs for selecting plots on the Optimization Output tab
                     checkboxGroupInput("plot_selection", "Select Plots to Display", 
                                        choices = c("Efficient Frontier" = "efficient_frontier",
                                                    "Weights Plot" = "weights_plot",
                                                    "Performance Summary" = "performance_summary",
                                                    "Group_Weights" = "group_weights"),
                                        selected = c("")),
                     br(),
                     h3("Efficient Frontier"),
                     plotOutput("efficient_frontier")|> withSpinner(type=3, color.background="white"),
                     br(),
                     h3("Efficient Frontier Asset Weights"),
                     plotOutput("chart_ef_weights")|> withSpinner(type=3, color.background="white"),
                     br(),
                     h3("Efficient Frontier Group Weights"),
                     plotOutput("chart_ef_weights_group")|> withSpinner(type=3, color.background="white"),
                     br(),
                     h3("Efficient Group Weights"),
                     plotOutput("chart_group_weights") |> withSpinner(type=3, color.background="white")
            ),
            tabPanel("Saved Optimizations",
                     h3("Saved Optimizations Summary"),
                     DTOutput("saved_optimizations")|> withSpinner(type=3, color.background="white"),
                     br(),
                     h3("Efficient Frontier for Saved Optimizations"),
                     plotOutput("efficient_frontier_with_saved")|> withSpinner(type=3, color.background="white"),
                     br(),
                     h3("Optimal Weights"),
                     tableOutput("asset_weights_matrix"),
                     br(),
                     h3("Optimal Group weights"),
                     tableOutput("group_weights_matrix"),
                     br(),
                     h3("Delete Optimization"),
                     numericInput("delete_id", "Enter Optimization ID to Delete", value = 1, min = 1, step = 1),
                     actionButton("delete_optimization", "Delete Selected Optimization")
            )
          )
        )
      ),
      
      # Settings Page (for additional configurations if needed)
      tabItem(
        tabName = "settings",
        h2("Settings"),
        p("Configure additional settings here.")
      )
    )
  )
)
