library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)

# --- inital data setup ---
# creating a 10 year projection template
start_date <- as.Date("01-05-2026")
month_seq <- seq(start_date, by = "month", length.out = 120)

init_df <- data.frame(
  Date = month_seq,
  Income = 2350,
  Extra_Income = 0,
  Rent = 1100,
  CouncilTax = 1664/12,
  Gas_electricity = 110,
  Phone = 5 + 5,
  Broadband = 15,
  Food = 300,
  Expense = Rent + CouncilTax + Gas_electricity + Phone + Broadband + Food,
  Emergency_Contribution = 100,
  Investment_Contribution = 55,
  Property_Contribution = 100,
  Travel_Contribution = 50,
)

targets <- c(Emergency = 7500,
             Investment = 50000,
             Property = 80000,
             Travel = 1800)
  
ui <- page_navbar(
  theme = bs_theme(version = 5),
  title = "Goals tracker",
  
  nav_panel(
    "Dashboard",
    layout_column_wrap(
      width = 1/4,
      value_box(
        title = "Emergency Pot",
        value = uiOutput("ef_status"),
        showcase = icon("shield-heart"),
        them = "info"
      ),
      value_box(
        title = "Investment Pot",
        value = uiOutput("if_status"),
        showcase = icon("chart-line"),
        them = "info"
      ),
      value_box(
        title = "Property Pot",
        value = uiOutput("pf_status"),
        showcase = icon("house"),
        them = "info"
      ),
      value_box(
        title = "Travel Pot",
        value = uiOutput("tf_status"),
        showcase = icon("plane"),
        them = "info"
      )
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Emergency fund timeline"),
        plotOutput("plot_ef_timeline")),
      card(
        card_header("Property fund timeline"),
        plotOutput("plot_pf_timeline")),
      card(
        card_header("Investment fund timeline"),
        plotOutput("plot_if_timeline")),
      card(
        card_header("Travel fund timeline"),
        plotOutput("plot_tf_timeline")),
    )
  ),
  nav_panel(
    "Inputs",
    card(
      card_header("Monthly budget projection"),
      markdown("Double-click a cell to edit. Your changes will undate the Dashboard."),
      DTOutput("budget_table")
    )
  )
)

server <- function(input, session, output){
  values <- reactiveVal(init_df)
}

shinyApp(ui, server)