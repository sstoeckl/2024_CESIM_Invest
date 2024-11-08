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
                     )
            ),
            tabPanel("Statistics and Correlation",
                     h3("Summary Statistics"),
                     DTOutput("summary_stats"),  # Use DTOutput for displaying the table
                     br(),
                     h3("Covariance / Correlation Matrix"),
                     tableOutput("cov_corr_matrix")
            ),
            tabPanel("Adjust Parameters", 
                     "Content for Parameter Adjustment...",
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
            tabPanel("Portfolio Objectives", "Content for Portfolio Objectives..."),
            tabPanel("Optimization Results", "Content for Optimization Results..."),
            tabPanel("Saved Optimizations", "Content for Saved Optimizations...")
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
