# server.R
source("global.R")

server <- function(input, output, session) {
  
  # Reactive expression to determine active groups based on asset selection
  active_groups <- reactive({
    selected_assets <- input$asset_selection
    
    # Define groups based on the selected assets
    active_groups <- list(
      US = any(grepl("_US$", selected_assets)),
      EUR = any(grepl("_EU$", selected_assets)),
      CHF = any(grepl("_CH$", selected_assets)),
      GBP = any(grepl("_UK$", selected_assets)),
      JPY = any(grepl("_JP$", selected_assets)),
      Alternatives = any(grepl("^Alternatives", selected_assets)),
      Equity = any(grepl("^Equities", selected_assets)),
      Gov_bonds = any(grepl("^Gov_bonds", selected_assets)),
      Corp_bonds = any(grepl("^Corp_bonds", selected_assets))
    )
    
    # Filter out inactive groups
    active_groups <- active_groups[unlist(active_groups)]
    names(active_groups)
  })
  
  # UI for group constraints
  output$group_constraints_ui <- renderUI({
    groups <- active_groups()
    
    # Generate input fields for each group constraint
    tagList(
      lapply(groups, function(group) {
        fluidRow(
          column(6, numericInput(paste0("min_", group), label = paste("Min weight for", group), value = 0)),
          column(6, numericInput(paste0("max_", group), label = paste("Max weight for", group), value = 1))
        )
      }),
      hr(),
      # Box constraint (valid for all assets)
      h4("Box Constraint for All Assets"),
      fluidRow(
        column(6, numericInput("min_box", "Min weight for all assets", value = 0)),
        column(6, numericInput("max_box", "Max weight for all assets", value = 1))
      )
    )
  })
  
  # Observe and update constraints based on asset selection
  observe({
    selected_groups <- active_groups()
    updateSelectizeInput(session, "group_constraints_ui", choices = selected_groups)
  })
  
}
