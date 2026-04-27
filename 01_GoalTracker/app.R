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
      card(card_header("Emergency Fund Progress"), plotOutput("plot_ef_timeline")),
      card(card_header("Property Fund Progress"), plotOutput("plot_pf_timeline")),
      card(card_header("Investment Fund Progress"), plotOutput("plot_if_timeline")),
      card(card_header("Travel Fund Progress"), plotOutput("plot_tf_timeline"))
    )
  ),
  nav_panel(
    "Inputs",
    card(
      card_header("Monthly Budget Projection"),
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
    df$Expense <- rowSums(df[, c("Rent", "CouncilTax", "Gas_electricity", "Phone", "Broadband", "Food")])
    values(df)
  })
  
  processed_data <- reactive({
    df <- values()
    df$cumm_ef <- cumsum(df$Emergency_Contribution)
    df$cumm_pf <- cumsum(df$Property_Contribution)
    df$cumm_tf <- cumsum(df$Travel_Contribution)
    
    # Compounding Investment
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
  
  # Helper Function for Plotting
  make_goal_plot <- function(data, y_var, target, title, color) {
    # Find achievement date
    achieve_date <- data$Date[which(data[[y_var]] >= target)[1]]
    
    p <- ggplot(data, aes(x = Date, y = .data[[y_var]])) +
      geom_line(color = color, size = 1.5) +
      geom_hline(yintercept = target, linetype = "dashed", color = "red", size = 0.8) +
      theme_minimal(base_size = 16) + # Increased text size
      labs(y = title, x = "Year") +
      scale_y_continuous(labels = scales::comma_format(prefix = "£"))
    
    if (!is.na(achieve_date)) {
      p <- p + geom_vline(xintercept = as.numeric(achieve_date), linetype = "dotted", color = "darkgrey", size = 1)
    }
    return(p)
  }
  
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
  
  # --- Plots ---
  output$plot_ef_timeline <- renderPlot({
    make_goal_plot(processed_data(), "cumm_ef", targets['Emergency'], "Emergency", "#17a2b8")
  })
  
  output$plot_pf_timeline <- renderPlot({
    make_goal_plot(processed_data(), "cumm_pf", targets['Property'], "Property", "#28a745")
  })
  
  output$plot_if_timeline <- renderPlot({
    make_goal_plot(processed_data(), "cumm_if", targets['Investment'], "Investment", "#007bff")
  })
  
  output$plot_tf_timeline <- renderPlot({
    make_goal_plot(processed_data(), "cumm_tf", targets['Travel'], "Travel", "#ffc107")
  })
  
  output$budget_table <- renderDT({
    datatable(values(), editable = TRUE, options = list(pageLength = 12, scrollX = TRUE))
  })
}

shinyApp(ui, server)