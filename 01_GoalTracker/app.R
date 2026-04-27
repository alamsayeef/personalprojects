library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)

# --- Initial Data Setup ---
start_date <- as.Date("2026-05-01")
month_seq <- seq(start_date, by = "month", length.out = 120)

init_df <- data.frame(
  Date = month_seq,
  Income = 2350,
  Extra_Income = 0,
  Rent = 1100,
  CouncilTax = 138.67,
  Gas_electricity = 110,
  Phone = 10,
  Broadband = 15,
  Food = 300,
  Emergency_Contribution = 100,
  Investment_Contribution = 55,
  Property_Contribution = 100,
  Travel_Contribution = 50
)

# Initial Expense Calculation
init_df$Expense <- rowSums(init_df[, c("Rent", "CouncilTax", "Gas_electricity", "Phone", "Broadband", "Food")])

targets <- c(Emergency = 7500, Investment = 50000, Property = 80000, Travel = 1800)

ui <- page_navbar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Goals Tracker",
  nav_panel(
    "Dashboard",
    layout_column_wrap(
      width = 1/4,
      value_box(title = "Emergency Pot", value = uiOutput("ef_status"), showcase = icon("shield-heart"), theme = "info"),
      value_box(title = "Investment Pot", value = uiOutput("if_status"), showcase = icon("chart-line"), theme = "primary"),
      value_box(title = "Property Pot", value = uiOutput("pf_status"), showcase = icon("house"), theme = "success"),
      value_box(title = "Travel Pot", value = uiOutput("tf_status"), showcase = icon("plane"), theme = "warning")
    ),
    layout_column_wrap(
      width = 1/2,
      card(card_header("Emergency Fund Timeline"), plotOutput("plot_ef_timeline")),
      card(card_header("Property Fund Timeline"), plotOutput("plot_pf_timeline")),
      card(card_header("Investment Fund Timeline"), plotOutput("plot_if_timeline")),
      card(card_header("Travel Fund Timeline"), plotOutput("plot_tf_timeline"))
    )
  ),
  nav_panel(
    "Inputs",
    card(
      card_header("Monthly Budget Projection"),
      markdown("Double-click a cell to edit. **Note:** All columns are recalculated automatically."),
      DTOutput("budget_table")
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveVal(init_df)
  
  observeEvent(input$budget_table_cell_edit, {
    info <- input$budget_table_cell_edit
    df <- values()
    df[info$row, info$col] <- info$value
    # Recalculate Expense column in case Rent/Food etc were changed
    df$Expense <- rowSums(df[, c("Rent", "CouncilTax", "Gas_electricity", "Phone", "Broadband", "Food")])
    values(df)
  })
  
  processed_data <- reactive({
    df <- values()
    
    # 1. Calculate Standard Cumulative Sums
    df$cumm_ef <- cumsum(df$Emergency_Contribution)
    df$cumm_pf <- cumsum(df$Property_Contribution)
    df$cumm_tf <- cumsum(df$Travel_Contribution)
    
    # 2. Calculate Compounding Investment (10% Annual = 0.797% Monthly)
    # Using a loop or Reduce for actual compounding
    inv_contributions <- df$Investment_Contribution
    compounded_if <- numeric(length(inv_contributions))
    balance <- 0
    multiplier <- 1.00797
    
    for(i in 1:length(inv_contributions)) {
      balance <- (balance + inv_contributions[i]) * multiplier
      compounded_if[i] <- balance
    }
    df$cumm_if <- compounded_if
    
    return(df)
  })
  
  # --- Value Boxes ---
  output$ef_status <- renderUI({
    curr <- last(processed_data()$cumm_ef)
    paste0("£", format(round(curr), big.mark = ","), " (", round((curr/targets['Emergency'])*100), "%)")
  })
  
  output$pf_status <- renderUI({
    curr <- last(processed_data()$cumm_pf)
    paste0("£", format(round(curr), big.mark = ","), " (", round((curr/targets['Property'])*100), "%)")
  })
  
  output$if_status <- renderUI({
    curr <- last(processed_data()$cumm_if)
    paste0("£", format(round(curr), big.mark = ","), " (", round((curr/targets['Investment'])*100), "%)")
  })
  
  output$tf_status <- renderUI({
    curr <- last(processed_data()$cumm_tf)
    paste0("£", format(round(curr), big.mark = ","), " (", round((curr/targets['Travel'])*100), "%)")
  })
  
  # --- Plots (Modified to avoid 'select' errors) ---
  output$plot_ef_timeline <- renderPlot({
    ggplot(processed_data(), aes(x = Date, y = cumm_ef)) +
      geom_line(color = "#17a2b8", size = 1.2) + theme_minimal() +
      geom_hline(yintercept = targets['Emergency'], linetype = "dashed", color = "red")
  })
  
  output$plot_pf_timeline <- renderPlot({
    ggplot(processed_data(), aes(x = Date, y = cumm_pf)) +
      geom_line(color = "#28a745", size = 1.2) + theme_minimal() +
      geom_hline(yintercept = targets['Property'], linetype = "dashed", color = "red")
  })
  
  output$plot_if_timeline <- renderPlot({
    ggplot(processed_data(), aes(x = Date, y = cumm_if)) +
      geom_line(color = "#007bff", size = 1.2) + theme_minimal() +
      geom_hline(yintercept = targets['Investment'], linetype = "dashed", color = "red")
  })
  
  output$plot_tf_timeline <- renderPlot({
    ggplot(processed_data(), aes(x = Date, y = cumm_tf)) +
      geom_line(color = "#ffc107", size = 1.2) + theme_minimal() +
      geom_hline(yintercept = targets['Travel'], linetype = "dashed", color = "red")
  })
  
  output$budget_table <- renderDT({
    datatable(values(), editable = TRUE, options = list(pageLength = 12, scrollX = TRUE))
  })
}

shinyApp(ui, server)