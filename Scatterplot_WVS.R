# ============================================================
# World Values Survey — Interactive Scatterplot
# app.R
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(scales)

# ── 1. Load data ─────────────────────────────────────────────
# Expects a .RData file in the same directory that contains
# an object called `wvs_data`.  Adjust the filename / object
# name to match your actual file.
if (file.exists("WVS_TimeSeries_1981_2020_R_v2_0.rdata")) {
  load("WVS_TimeSeries_1981_2020_R_v2_0.rdata")          # loads object `wvs_data`
} else {
  # ── Synthetic demo data so the app runs without the real file ──
  set.seed(42)
  countries <- c("USA", "Germany", "China", "India", "Brazil",
                 "Nigeria", "Russia", "Japan", "Mexico", "Sweden",
                 "Poland", "Turkey", "Argentina", "South Korea", "Egypt")
  wvs_data <- expand.grid(
    country = countries,
    year    = c(1994, 1999, 2004, 2009, 2014, 2020)
  ) %>%
    mutate(
      X003        = round(runif(n(), 18, 80)),
      gdp_per_cap = exp(runif(n(), 7, 11)),
      # authoritarian-index components (1–4 scale, higher = more authoritarian)
      E114        = sample(1:4, n(), replace = TRUE),
      E116        = sample(1:4, n(), replace = TRUE),
      E115        = sample(1:4, n(), replace = TRUE),
      E018        = sample(1:4, n(), replace = TRUE),
      A042        = sample(1:4, n(), replace = TRUE),
      E225        = sample(1:4, n(), replace = TRUE),
      E228        = sample(1:4, n(), replace = TRUE),
      A124_06     = sample(1:4, n(), replace = TRUE),
      A124_12     = sample(1:4, n(), replace = TRUE),
      A124_43     = sample(1:4, n(), replace = TRUE)
    )
}

# ── 2. Build authoritarian index (mean of 10 items, NA-tolerant) ──
auth_vars <- c("E114","E116","E115","E018","A042",
               "E225","E228","A124_06","A124_12","A124_43")

wvs_data <- wvs_data %>%
  rowwise() %>%
  mutate(
    auth_index = mean(c_across(all_of(auth_vars)), na.rm = TRUE)
  ) %>%
  ungroup()

# ── 3. Derive country & year lists ───────────────────────────
all_countries <- sort(unique(as.character(wvs_data$country)))
all_years     <- sort(unique(wvs_data$year))

y_choices <- c(
  "Mean Age"               = "X003",
  "GDP per Capita (USD)"   = "gdp_per_cap",
  "Authoritarian Index"    = "auth_index"
)

# ── 4. UI ─────────────────────────────────────────────────────
ui <- fluidPage(

  # ── Google Font + custom CSS ──────────────────────────────
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=DM+Serif+Display&family=DM+Sans:wght@300;400;500&display=swap"
    ),
    tags$style(HTML("

      /* ── Base ── */
      * { box-sizing: border-box; }

      body {
        background: #0f1117;
        color: #e8e6df;
        font-family: 'DM Sans', sans-serif;
        font-weight: 300;
        margin: 0;
        padding: 0;
      }

      /* ── Header ── */
      .app-header {
        background: linear-gradient(135deg, #1a1d27 0%, #0f1117 100%);
        border-bottom: 1px solid #2a2d3a;
        padding: 28px 40px 20px;
      }
      .app-header h1 {
        font-family: 'DM Serif Display', serif;
        font-size: 2rem;
        color: #f0ede6;
        margin: 0 0 4px;
        letter-spacing: -0.5px;
      }
      .app-header p {
        font-size: 0.82rem;
        color: #7a7d8a;
        margin: 0;
        letter-spacing: 0.05em;
        text-transform: uppercase;
      }

      /* ── Layout ── */
      .main-wrap {
        display: flex;
        gap: 0;
        min-height: calc(100vh - 92px);
      }

      /* ── Sidebar ── */
      .sidebar-panel {
        width: 280px;
        min-width: 280px;
        background: #13161f;
        border-right: 1px solid #1e2130;
        padding: 28px 22px;
        display: flex;
        flex-direction: column;
        gap: 22px;
      }

      .ctrl-label {
        font-size: 0.7rem;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        color: #5c6070;
        margin-bottom: 8px;
        font-weight: 500;
      }

      /* Shiny select widgets */
      .selectize-control .selectize-input {
        background: #1a1d27 !important;
        border: 1px solid #2a2d3a !important;
        color: #e8e6df !important;
        border-radius: 6px !important;
        font-family: 'DM Sans', sans-serif !important;
        font-size: 0.85rem !important;
        box-shadow: none !important;
        padding: 8px 12px !important;
      }
      .selectize-control .selectize-input.focus {
        border-color: #c9a96e !important;
        box-shadow: 0 0 0 2px rgba(201,169,110,.15) !important;
      }
      .selectize-dropdown {
        background: #1e2130 !important;
        border: 1px solid #2a2d3a !important;
        color: #e8e6df !important;
        font-family: 'DM Sans', sans-serif !important;
        font-size: 0.85rem !important;
        border-radius: 6px !important;
        box-shadow: 0 8px 32px rgba(0,0,0,.5) !important;
      }
      .selectize-dropdown .option { padding: 8px 12px !important; }
      .selectize-dropdown .option:hover,
      .selectize-dropdown .option.active {
        background: #2a2d3a !important;
        color: #c9a96e !important;
      }
      .item { color: #c9a96e !important; }

      /* Slider */
      .irs--shiny .irs-bar,
      .irs--shiny .irs-bar--single {
        background: #c9a96e !important;
        border-color: #c9a96e !important;
      }
      .irs--shiny .irs-handle {
        background: #c9a96e !important;
        border-color: #c9a96e !important;
      }
      .irs--shiny .irs-from,
      .irs--shiny .irs-to,
      .irs--shiny .irs-single {
        background: #c9a96e !important;
        color: #0f1117 !important;
        font-weight: 500 !important;
      }
      .irs--shiny .irs-line { background: #2a2d3a !important; }
      .irs--shiny .irs-grid-text { color: #5c6070 !important; }

      /* Checkbox group */
      .shiny-input-container label { color: #e8e6df !important; }
      input[type='checkbox']:checked { accent-color: #c9a96e; }

      /* Stat pills */
      .stat-row {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
      }
      .stat-pill {
        background: #1a1d27;
        border: 1px solid #2a2d3a;
        border-radius: 6px;
        padding: 10px 14px;
        flex: 1;
        min-width: 80px;
      }
      .stat-pill .val {
        font-family: 'DM Serif Display', serif;
        font-size: 1.3rem;
        color: #c9a96e;
        display: block;
      }
      .stat-pill .lbl {
        font-size: 0.68rem;
        color: #5c6070;
        text-transform: uppercase;
        letter-spacing: 0.08em;
      }

      /* ── Plot area ── */
      .plot-panel {
        flex: 1;
        padding: 28px 32px;
        display: flex;
        flex-direction: column;
        gap: 16px;
      }
      .plot-title {
        font-family: 'DM Serif Display', serif;
        font-size: 1.25rem;
        color: #f0ede6;
        margin: 0;
      }
      .plot-subtitle {
        font-size: 0.78rem;
        color: #5c6070;
        margin: 2px 0 0;
        letter-spacing: 0.04em;
      }
      .plot-wrap {
        background: #13161f;
        border: 1px solid #1e2130;
        border-radius: 10px;
        overflow: hidden;
        flex: 1;
      }
      .shiny-plot-output { width: 100% !important; }

      /* ── Button ── */
      .btn-gold {
        background: #c9a96e;
        border: none;
        color: #0f1117;
        font-family: 'DM Sans', sans-serif;
        font-weight: 500;
        font-size: 0.8rem;
        letter-spacing: 0.06em;
        text-transform: uppercase;
        padding: 9px 16px;
        border-radius: 6px;
        cursor: pointer;
        width: 100%;
        transition: background .2s;
      }
      .btn-gold:hover { background: #d9be8a; }

      /* ── Divider ── */
      .ctrl-divider {
        border: none;
        border-top: 1px solid #1e2130;
        margin: 0;
      }

      /* ── Note ── */
      .index-note {
        font-size: 0.72rem;
        color: #4a4d5a;
        line-height: 1.5;
        padding: 10px 12px;
        background: #1a1d27;
        border-left: 2px solid #c9a96e44;
        border-radius: 0 6px 6px 0;
      }
    "))
  ),

  # ── Header ────────────────────────────────────────────────
  div(class = "app-header",
    h1("World Values Survey"),
    p("Interactive Scatterplot Explorer · Cross-Wave Analysis")
  ),

  # ── Main layout ───────────────────────────────────────────
  div(class = "main-wrap",

    # ── Sidebar ─────────────────────────────────────────────
    div(class = "sidebar-panel",

      # Y-axis variable
      div(
        div(class = "ctrl-label", "Y-Axis Variable"),
        selectInput("y_var", label = NULL,
                    choices  = y_choices,
                    selected = "auth_index")
      ),

      hr(class = "ctrl-divider"),

      # Country selector
      div(
        div(class = "ctrl-label", "Countries"),
        selectizeInput("countries", label = NULL,
                       choices  = all_countries,
                       selected = head(all_countries, 8),
                       multiple = TRUE,
                       options  = list(placeholder = "Select countries…"))
      ),

      # Quick-select buttons
      div(
        style = "display:flex; gap:8px;",
        actionButton("sel_all",  "All",   class = "btn-gold",
                     style = "padding:7px 10px; font-size:.72rem;"),
        actionButton("sel_none", "None",  class = "btn-gold",
                     style = "padding:7px 10px; font-size:.72rem; background:#2a2d3a; color:#e8e6df;"),
        actionButton("sel_rand", "Sample", class = "btn-gold",
                     style = "padding:7px 10px; font-size:.72rem; background:#2a2d3a; color:#e8e6df;")
      ),

      hr(class = "ctrl-divider"),

      # Year range
      div(
        div(class = "ctrl-label", "Year Range"),
        sliderInput("year_range", label = NULL,
                    min   = min(all_years),
                    max   = max(all_years),
                    value = range(all_years),
                    step  = 1,
                    sep   = "")
      ),

      hr(class = "ctrl-divider"),

      # Show labels toggle
      div(
        div(class = "ctrl-label", "Options"),
        checkboxInput("show_labels",  "Show country labels",  value = TRUE),
        checkboxInput("show_trend",   "Show trend line",      value = TRUE),
        checkboxInput("color_country","Color by country",     value = TRUE)
      ),

      hr(class = "ctrl-divider"),

      # Summary stats
      div(class = "ctrl-label", "Summary"),
      div(class = "stat-row",
        div(class = "stat-pill",
          span(class = "val", textOutput("n_obs",  inline = TRUE)),
          span(class = "lbl", "Obs.")
        ),
        div(class = "stat-pill",
          span(class = "val", textOutput("n_ctry", inline = TRUE)),
          span(class = "lbl", "Countries")
        ),
        div(class = "stat-pill",
          span(class = "val", textOutput("n_wave", inline = TRUE)),
          span(class = "lbl", "Waves")
        )
      ),

      hr(class = "ctrl-divider"),

      # Index composition note
      div(class = "index-note",
        "Authoritarian Index: mean of E114, E116, E115, E018, A042, E225, E228, A124_06, A124_12, A124_43 (higher = more authoritarian attitudes)"
      )
    ),

    # ── Plot panel ──────────────────────────────────────────
    div(class = "plot-panel",
      div(
        p(class = "plot-title",  textOutput("plot_title",    inline = TRUE)),
        p(class = "plot-subtitle", textOutput("plot_subtitle", inline = TRUE))
      ),
      div(class = "plot-wrap",
        plotOutput("scatter", height = "540px")
      )
    )
  )
)

# ── 5. Server ─────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Country quick-select buttons ──
  observeEvent(input$sel_all,  {
    updateSelectizeInput(session, "countries", selected = all_countries)
  })
  observeEvent(input$sel_none, {
    updateSelectizeInput(session, "countries", selected = character(0))
  })
  observeEvent(input$sel_rand, {
    updateSelectizeInput(session, "countries",
                         selected = sample(all_countries,
                                           min(8, length(all_countries))))
  })

  # ── Filtered data ──
  filtered <- reactive({
    req(input$countries, input$year_range)
    wvs_data %>%
      filter(
        country %in% input$countries,
        year    >= input$year_range[1],
        year    <= input$year_range[2]
      )
  })

  # ── Aggregated (mean per country × year) ──
  agg_data <- reactive({
    req(input$y_var)
    filtered() %>%
      group_by(country, year) %>%
      summarise(
        y_val = mean(.data[[input$y_var]], na.rm = TRUE),
        .groups = "drop"
      )
  })

  # ── Y-axis label ──
  y_label <- reactive({
    switch(input$y_var,
      X003        = "Mean Age",
      gdp_per_cap = "GDP per Capita (USD)",
      auth_index  = "Authoritarian Index (1–4)"
    )
  })

  # ── Summary stats ──
  output$n_obs  <- renderText({ nrow(agg_data()) })
  output$n_ctry <- renderText({ n_distinct(agg_data()$country) })
  output$n_wave <- renderText({ n_distinct(agg_data()$year) })

  # ── Dynamic title / subtitle ──
  output$plot_title <- renderText({
    paste(y_label(), "vs. Survey Wave")
  })
  output$plot_subtitle <- renderText({
    ctry <- length(input$countries)
    paste0(ctry, " countr", ifelse(ctry == 1, "y", "ies"), " · ",
           input$year_range[1], "–", input$year_range[2])
  })

  # ── Scatterplot ──
  output$scatter <- renderPlot({
    d <- agg_data()
    validate(need(nrow(d) > 0, "No data for the selected filters."))

    # ggplot base
    p <- ggplot(d, aes(x = year, y = y_val)) +

      # ── Theme ──────────────────────────────────────────────
      theme_minimal(base_size = 13) +
      theme(
        text             = element_text(family = "sans", color = "#e8e6df"),
        plot.background  = element_rect(fill = "#13161f", color = NA),
        panel.background = element_rect(fill = "#13161f", color = NA),
        panel.grid.major = element_line(color = "#1e2130", linewidth = .5),
        panel.grid.minor = element_line(color = "#181b25", linewidth = .3),
        axis.text        = element_text(color = "#7a7d8a", size = 11),
        axis.title       = element_text(color = "#9a9daa", size = 11,
                                        margin = margin(t = 8, r = 8)),
        axis.ticks       = element_blank(),
        legend.background= element_rect(fill = "#13161f", color = NA),
        legend.key       = element_rect(fill = "#13161f", color = NA),
        legend.text      = element_text(color = "#b0b3bf", size = 10),
        legend.title     = element_text(color = "#7a7d8a", size = 9,
                                        hjust = .5),
        plot.margin      = margin(20, 20, 16, 16)
      )

    # ── Trend line ─────────────────────────────────────────
    if (input$show_trend && nrow(d) >= 4) {
      p <- p + geom_smooth(
        method = "loess", formula = y ~ x, se = TRUE,
        color = "#c9a96e", fill = "#c9a96e22",
        linewidth = 1.2, linetype = "solid"
      )
    }

    # ── Points ─────────────────────────────────────────────
    if (input$color_country) {
      n_c <- n_distinct(d$country)
      pal <- if (n_c <= 8) {
        c("#c9a96e","#6ec9a9","#6e9bc9","#c96ea9",
          "#a96ec9","#c9c96e","#6ec9c9","#c96e6e")
      } else {
        scales::hue_pal()(n_c)
      }
      p <- p +
        geom_point(aes(color = country), size = 4, alpha = .88) +
        scale_color_manual(values = pal, name = "Country")
    } else {
      p <- p +
        geom_point(color = "#c9a96e", size = 4, alpha = .88)
    }

    # ── Connecting lines per country ──────────────────────
    if (input$color_country) {
      p <- p +
        geom_line(aes(group = country, color = country),
                  alpha = .3, linewidth = .7)
    } else {
      p <- p +
        geom_line(aes(group = country),
                  color = "#c9a96e", alpha = .3, linewidth = .7)
    }

    # ── Labels ─────────────────────────────────────────────
    if (input$show_labels) {
      # Only label the latest year for each country to reduce clutter
      d_label <- d %>%
        group_by(country) %>%
        filter(year == max(year)) %>%
        ungroup()

      p <- p +
        geom_text_repel(
          data         = d_label,
          aes(label = country),
          color        = "#c9a96e",
          size         = 3.2,
          fontface     = "plain",
          segment.color= "#c9a96e55",
          segment.size = .5,
          box.padding  = .4,
          point.padding= .3,
          max.overlaps = 20
        )
    }

    # ── Y-axis formatting ──────────────────────────────────
    if (input$y_var == "gdp_per_cap") {
      p <- p + scale_y_continuous(labels = scales::dollar_format(
        prefix = "$", big.mark = ",", accuracy = 1
      ))
    } else {
      p <- p + scale_y_continuous(labels = scales::number_format(accuracy = .01))
    }

    # ── X axis: integer years ─────────────────────────────
    p <- p +
      scale_x_continuous(
        breaks = all_years,
        labels = as.character(all_years)
      )

    # ── Axis labels ────────────────────────────────────────
    p + labs(x = "Survey Wave (Year)", y = y_label())
  }, bg = "#13161f")
}

# ── 6. Run ────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
