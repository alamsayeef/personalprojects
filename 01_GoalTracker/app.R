library(shiny)
library(bslib)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, session, output){
  
}

shinyApp(ui, server)