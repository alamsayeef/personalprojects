library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(readxl)
library(writexl)

# --- Initial Data Setup ---
start_date <- as.Date("2026-05-01")
month_seq  <- seq(start_date, by = "month", length.out = 120)

init_df <- data.frame(
  Date                    = month_seq,
  Income                  = 2350,
  Extra_Income            = 0,
  Rent                    = 1100,
  CouncilTax              = 138.67,
  Gas_Electricity         = 110,
  Phone                   = 10,
  Broadband               = 15,
  Food                    = 300,
  Emergency_Contribution  = 100,
  Investment_Contribution = 55,
  Property_Contribution   = 100,
  Travel_Contribution     = 50
)
init_df$Expense <- rowSums(init_df[, c("Rent","CouncilTax","Gas_Electricity","Phone","Broadband","Food")])

targets <- c(Emergency = 7500, Investment = 50000, Property = 80000, Travel = 1800)
COLORS  <- c(Emergency = "#17a2b8", Investment = "#007bff", Property = "#28a745", Travel = "#ffc107")

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Goals Tracker",
  nav_spacer(),
  nav_panel(
    "Dashboard",
    layout_column_wrap(
      width = 1/4,
      value_box(title = "Emergency Pot", value = textOutput("ef_val"), showcase = bsicons::bs_icon("shield-check"), theme = "info"),
      value_box(title = "Investment Pot", value = textOutput("if_val"), showcase = bsicons::bs_icon("graph-up-arrow"), theme = "primary"),
      value_box(title = "Property Pot", value = textOutput("pf_val"), showcase = bsicons::bs_icon("house-fill"), theme = "success"),
      value_box(title = "Travel Pot", value = textOutput("tf_val"), showcase = bsicons::bs_icon("airplane-fill"), theme = "warning")
    ),
    layout_column_wrap(
      width = 1/2,
      card(card_header("Emergency Fund"), plotlyOutput("plot_ef", height = "280px")),
      card(card_header("Property Fund"),  plotlyOutput("plot_pf", height = "280px")),
      card(card_header("Investment Fund (compounded)"), plotlyOutput("plot_if", height = "280px")),
      card(card_header("Travel Fund"),  plotlyOutput("plot_tf", height = "280px"))
    )
  ),
  
  nav_panel(
    "Inputs & Data",
    layout_column_wrap(
      width = 1,
      card(
        card_header("Data Management"),
        layout_column_wrap(
          width = 1/2,
          fileInput("upload_excel", "Upload Projection Excel", accept = c(".xlsx")),
          downloadButton("download_excel", "Download Current Data", class = "btn-primary")
        )
      ),
      card(
        card_header("Monthly Budget Projection — edit any cell, then click outside to apply"),
        DTOutput("budget_table")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  values <- reactiveVal(init_df)
  
  # Handle Excel Upload
  observeEvent(input$upload_excel, {
    req(input$upload_excel)
    new_df <- read_excel(input$upload_excel$datapath)
    # Ensure Date column is actually Dates
    new_df$Date <- as.Date(new_df$Date)
    # Re-calculate Expense column just in case
    new_df$Expense <- rowSums(new_df[, c("Rent","CouncilTax","Gas_Electricity","Phone","Broadband","Food")], na.rm = TRUE)
    values(as.data.frame(new_df))
  })
  
  # Handle Excel Download
  output$download_excel <- downloadHandler(
    filename = function() { paste0("budget_projection_", Sys.Date(), ".xlsx") },
    content = function(file) { write_xlsx(values(), file) }
  )
  
  # Cell edits
  observeEvent(input$budget_table_cell_edit, {
    info <- input$budget_table_cell_edit
    df <- DT::editData(values(), info, rownames = FALSE)
    df$Expense <- rowSums(df[, c("Rent","CouncilTax","Gas_Electricity","Phone","Broadband","Food")], na.rm = TRUE)
    values(df)
  })
  
  # Priority Logic & Accumulation
  processed <- reactive({
    df <- values()
    n <- nrow(df)
    cumm_ef <- numeric(n); cumm_pf <- numeric(n); cumm_if <- numeric(n); cumm_tf <- numeric(n)
    bal_ef <- 0; bal_pf <- 0; bal_if <- 0; bal_tf <- 0
    mult <- 1.00797 # Monthly Compounding
    
    for (i in 1:n) {
      extra <- if(is.na(df$Extra_Income[i])) 0 else df$Extra_Income[i]
      
      # 1. Extra Income Priority: Emergency -> Then 60/20/20 split
      if (bal_ef < targets["Emergency"]) {
        bal_ef <- bal_ef + extra
      } else {
        bal_pf <- bal_pf + (extra * 0.60)
        bal_if <- bal_if + (extra * 0.20)
        bal_tf <- bal_tf + (extra * 0.20)
      }
      
      # 2. Add base contributions
      bal_ef <- bal_ef + replace(df$Emergency_Contribution[i], is.na(df$Emergency_Contribution[i]), 0)
      bal_pf <- bal_pf + replace(df$Property_Contribution[i], is.na(df$Property_Contribution[i]), 0)
      bal_tf <- bal_tf + replace(df$Travel_Contribution[i], is.na(df$Travel_Contribution[i]), 0)
      
      # 3. Investment Compounding
      inv_contrib <- replace(df$Investment_Contribution[i], is.na(df$Investment_Contribution[i]), 0)
      bal_if <- (bal_if + inv_contrib) * mult
      
      # 4. Travel Overflow Logic
      if (bal_tf > targets["Travel"]) {
        overflow <- bal_tf - targets["Travel"]
        bal_tf   <- targets["Travel"]
        bal_ef <- bal_ef + (overflow * 0.60)
        bal_pf <- bal_pf + (overflow * 0.20)
        bal_if <- bal_if + (overflow * 0.20)
      }
      
      cumm_ef[i] <- bal_ef; cumm_pf[i] <- bal_pf; cumm_if[i] <- bal_if; cumm_tf[i] <- bal_tf
    }
    df$cumm_ef <- cumm_ef; df$cumm_pf <- cumm_pf; df$cumm_if <- cumm_if; df$cumm_tf <- cumm_tf
    df
  })
  
  # Render Value Boxes, Plots, and DT (keep existing logic from previous response)
  output$ef_val <- renderText({ paste0("£", format(round(last(processed()$cumm_ef)), big.mark = ",")) })
  output$if_val <- renderText({ paste0("£", format(round(last(processed()$cumm_if)), big.mark = ",")) })
  output$pf_val <- renderText({ paste0("£", format(round(last(processed()$cumm_pf)), big.mark = ",")) })
  output$tf_val <- renderText({ paste0("£", format(round(last(processed()$cumm_tf)), big.mark = ",")) })
  
  # Helper: build a plotly line chart (same as before)
  goal_plotly <- function(data, y_var, target, color) {
    plot_ly(data, x = ~Date, y = ~.data[[y_var]], type = "scatter", mode = "lines", line = list(color = color)) %>%
      add_segments(x = min(data$Date), xend = max(data$Date), y = target, yend = target, line = list(color = "red", dash = "dash")) %>%
      layout(showlegend = FALSE, yaxis = list(title = "£"))
  }
  
  output$plot_ef <- renderPlotly({ goal_plotly(processed(), "cumm_ef", targets["Emergency"], COLORS["Emergency"]) })
  output$plot_if <- renderPlotly({ goal_plotly(processed(), "cumm_if", targets["Investment"], COLORS["Investment"]) })
  output$plot_pf <- renderPlotly({ goal_plotly(processed(), "cumm_pf", targets["Property"], COLORS["Property"]) })
  output$plot_tf <- renderPlotly({ goal_plotly(processed(), "cumm_tf", targets["Travel"], COLORS["Travel"]) })
  
  output$budget_table <- renderDT({
    datatable(values(), editable = list(target = "cell", disable = list(columns = c(0, 13))), rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })
}

shinyApp(ui, server)