library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)

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
init_df$Expense <- rowSums(
  init_df[, c("Rent","CouncilTax","Gas_Electricity","Phone","Broadband","Food")]
)

targets <- c(Emergency = 7500, Investment = 50000, Property = 80000, Travel = 1800)

COLORS <- c(
  Emergency  = "#17a2b8",
  Investment = "#007bff",
  Property   = "#28a745",
  Travel     = "#ffc107"
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Goals Tracker",
  nav_spacer(),
  nav_panel(
    "Dashboard",
    
    # Value boxes — use textOutput inside value= for reactivity
    layout_column_wrap(
      width = 1/4,
      value_box(
        title    = "Emergency Pot",
        value    = textOutput("ef_val"),
        showcase = bsicons::bs_icon("shield-check"),
        theme    = "info"
      ),
      value_box(
        title    = "Investment Pot",
        value    = textOutput("if_val"),
        showcase = bsicons::bs_icon("graph-up-arrow"),
        theme    = "primary"
      ),
      value_box(
        title    = "Property Pot",
        value    = textOutput("pf_val"),
        showcase = bsicons::bs_icon("house-fill"),
        theme    = "success"
      ),
      value_box(
        title    = "Travel Pot",
        value    = textOutput("tf_val"),
        showcase = bsicons::bs_icon("airplane-fill"),
        theme    = "warning"
      )
    ),
    
    layout_column_wrap(
      width = 1/2,
      card(card_header("Emergency Fund"), plotlyOutput("plot_ef", height = "280px")),
      card(card_header("Property Fund"),  plotlyOutput("plot_pf", height = "280px")),
      card(card_header("Investment Fund (compounded)"), plotlyOutput("plot_if", height = "280px")),
      card(card_header("Travel Fund"),    plotlyOutput("plot_tf", height = "280px"))
    )
  ),
  
  nav_panel(
    "Inputs",
    card(
      card_header("Monthly Budget Projection — edit any cell, then click outside to apply"),
      DTOutput("budget_table")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  values <- reactiveVal(init_df)
  
  # Cell edits — use editData() which handles 0-based DT column indexing correctly
  observeEvent(input$budget_table_cell_edit, {
    info <- input$budget_table_cell_edit
    df   <- values()
    # editData() maps DT's 0-based col index to the correct R column and
    # coerces the value to match the existing column type (numeric, Date, etc.)
    df <- DT::editData(df, info, rownames = FALSE)
    # Recompute the derived Expense total after any expense column changes
    df$Expense <- rowSums(
      df[, c("Rent","CouncilTax","Gas_Electricity","Phone","Broadband","Food")],
      na.rm = TRUE
    )
    values(df)
  })
  
  # Derived / cumulative columns
  processed <- reactive({
    df <- values()
    
    df$cumm_ef <- cumsum(replace(df$Emergency_Contribution,  is.na(df$Emergency_Contribution),  0))
    df$cumm_pf <- cumsum(replace(df$Property_Contribution,   is.na(df$Property_Contribution),   0))
    df$cumm_tf <- cumsum(replace(df$Travel_Contribution,     is.na(df$Travel_Contribution),     0))
    
    # Monthly compounding at ~9.56% p.a. (0.797 % per month — matches original multiplier)
    inv  <- replace(df$Investment_Contribution, is.na(df$Investment_Contribution), 0)
    bal  <- 0
    mult <- 1.00797
    cif  <- numeric(nrow(df))
    for (i in seq_along(inv)) {
      bal    <- (bal + inv[i]) * mult
      cif[i] <- bal
    }
    df$cumm_if <- cif
    df
  })
  
  # ── Helper: build a plotly line chart ──
  goal_plotly <- function(data, y_var, target, color) {
    y_vals      <- data[[y_var]]
    hit_idx     <- which(y_vals >= target)[1]
    hit_date    <- if (!is.na(hit_idx)) data$Date[hit_idx] else NA
    
    pct         <- round(min(last(y_vals) / target * 100, 100))
    subtitle    <- if (!is.na(hit_date))
      paste0("Target reached: ", format(hit_date, "%b %Y"))
    else
      paste0(pct, "% of £", format(target, big.mark = ","), " target")
    
    p <- plot_ly(data, x = ~Date, y = ~.data[[y_var]],
                 type = "scatter", mode = "lines",
                 line = list(color = color, width = 2.5),
                 hovertemplate = "%{x|%b %Y}<br>£%{y:,.0f}<extra></extra>") |>
      add_trace(y = rep(target, nrow(data)),
                type = "scatter", mode = "lines",
                line = list(color = "red", width = 1.2, dash = "dash"),
                hovertemplate = paste0("Target: £", format(target, big.mark=","), "<extra></extra>")) |>
      layout(
        showlegend = FALSE,
        xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
        yaxis = list(title = "£", tickformat = ",.0f",
                     showgrid = TRUE, gridcolor = "#eee", zeroline = FALSE),
        annotations = list(list(
          x = 0.01, y = 0.97, xref = "paper", yref = "paper",
          text = subtitle, showarrow = FALSE,
          font = list(size = 11, color = "#555"), xanchor = "left"
        )),
        margin = list(t = 10, b = 10, l = 55, r = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      )
    
    if (!is.na(hit_date)) {
      p <- p |> add_segments(
        x = hit_date, xend = hit_date, y = 0, yend = target,
        line = list(color = "darkgrey", width = 1, dash = "dot"),
        hoverinfo = "none"
      )
    }
    p
  }
  
  # ── Value box text outputs ──
  fmt_box <- function(curr, tgt)
    paste0("£", format(round(curr), big.mark = ","),
           "  (", round(min(curr / tgt * 100, 100)), "%)")
  
  output$ef_val <- renderText({ fmt_box(last(processed()$cumm_ef), targets["Emergency"])  })
  output$if_val <- renderText({ fmt_box(last(processed()$cumm_if), targets["Investment"]) })
  output$pf_val <- renderText({ fmt_box(last(processed()$cumm_pf), targets["Property"])   })
  output$tf_val <- renderText({ fmt_box(last(processed()$cumm_tf), targets["Travel"])     })
  
  # ── Plots — all renderPlotly ──
  output$plot_ef <- renderPlotly({
    goal_plotly(processed(), "cumm_ef", targets["Emergency"],  COLORS["Emergency"])
  })
  output$plot_if <- renderPlotly({
    goal_plotly(processed(), "cumm_if", targets["Investment"], COLORS["Investment"])
  })
  output$plot_pf <- renderPlotly({
    goal_plotly(processed(), "cumm_pf", targets["Property"],   COLORS["Property"])
  })
  output$plot_tf <- renderPlotly({
    goal_plotly(processed(), "cumm_tf", targets["Travel"],     COLORS["Travel"])
  })
  
  # ── Editable budget table ──
  output$budget_table <- renderDT({
    datatable(
      values(),
      editable  = list(target = "cell", disable = list(columns = c(0, 13))),  # Date & Expense not editable
      rownames  = FALSE,
      options   = list(
        pageLength = 12,
        scrollX    = TRUE,
        autoWidth  = FALSE,
        columnDefs = list(list(className = "dt-right", targets = "_all"))
      )
    )
  })
}

shinyApp(ui, server)