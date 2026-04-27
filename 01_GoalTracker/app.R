library(shiny)
library(bslib)
library(shinydashboard)

ui <- page_navbar(
  theme = bs_theme(version = 5),
  title = "Goals tracker",
  nav_panel("Dashboard",
            "Cards and value boxes go here"),
  nav_panel("Inputs",
            "this is we")
)

server <- function(input, session, output){
  
}

shinyApp(ui, server)