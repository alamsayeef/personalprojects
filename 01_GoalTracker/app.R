library(shiny)
library(shinydashboard)
library(ggplot2)

# --- UI Section ---
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Goals tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("chart-line")),
      menuItem("Assumptions", tabName = "settings", icon = icon("gears"))
    ),
    hr(),
    # User Inputs for Dynamic Changes
    numericInput("monthly_income", "Monthly Take-home (£):", value = 2350),
    numericInput("monthly_expenses", "Monthly Expenses (£):", value = 2044),
    sliderInput("investment_rate", "Annual ETF Return (%):", 0, 12, 7),
    helpText("Note: 7% is a common benchmark for Halal ETFs like ISWD.")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dash",
              fluidRow(
                # Emergency Fund Card
                valueBoxOutput("ef_timer", width = 4),
                # House Fund Card
                valueBoxOutput("house_timer", width = 4),
                # Investment Projection
                valueBoxOutput("investment_value", width = 4)
              ),
              fluidRow(
                box(title = "10-Year Growth Projection (House + Investment)", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("growth_plot"))
              )
      )
    )
  )
)

# --- Server Section ---
server <- function(input, output) {
  
  # Reactive calculations
  finance_data <- reactive({
    surplus <- input$monthly_income - input$monthly_expenses
    
    # Goals
    ef_target <- 7500
    house_target <- 80000
    
    # 1. Emergency Fund Timeline
    months_to_ef <- ef_target / max(surplus, 1)
    
    # 2. House Timeline (assuming we save after EF is done)
    months_to_house <- house_target / max(surplus, 1)
    
    # 3. Compounding Investment (T212)
    # Let's assume you put £150/month into the ETF as per previous plan
    years <- 0:15
    monthly_rate <- (1 + (input$investment_rate/100))^(1/12) - 1
    investment_monthly <- 150 
    
    # Formula for Future Value of Annuity
    fv_vec <- sapply(years * 12, function(m) {
      if(m == 0) return(0)
      investment_monthly * ((1 + monthly_rate)^m - 1) / monthly_rate
    })
    
    list(
      months_ef = round(months_to_ef, 1),
      months_house = round(months_to_house, 1),
      fv_data = data.frame(Year = years, Balance = fv_vec),
      current_surplus = surplus
    )
  })
  
  # UI Output: Emergency Fund Card
  output$ef_timer <- renderValueBox({
    data <- finance_data()
    valueBox(
      paste0(data$months_ef, " Months"), "To Emergency Fund (£7.5k)",
      icon = icon("shield-halved"), color = "teal"
    )
  })
  
  # UI Output: House Fund Card
  output$house_timer <- renderValueBox({
    data <- finance_data()
    valueBox(
      paste0(round(data$months_house / 12, 1), " Years"), "To Oujda Villa (£80k)",
      icon = icon("house"), color = "orange"
    )
  })
  
  # UI Output: Compounded ETF Value
  output$investment_value <- renderValueBox({
    data <- finance_data()
    future_val <- tail(data$fv_data$Balance, 1)
    valueBox(
      paste0("£", format(round(future_val), big.mark=",")), "ETF Value (15 Years)",
      icon = icon("arrow-up-right-dots"), color = "purple"
    )
  })
  
  # Plotting the compounding growth
  output$growth_plot <- renderPlot({
    ggplot(finance_data()$fv_data, aes(x = Year, y = Balance)) +
      geom_area(fill = "#27ae60", alpha = 0.4) +
      geom_line(color = "#27ae60", size = 1.2) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
      labs(title = "Wealth Accumulation (Compounding @ £150/month)",
           y = "Portfolio Value", x = "Years from Now") +
      theme_minimal()
  })
}

shinyApp(ui, server)