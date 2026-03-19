library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(readr)

# ── Data ──────────────────────────────────────────────────────────────────────
setwd("~/Uni/Year 5/semester 2/Data")
vdem <- read_csv("V-Dem-CD-v16.csv")

vdem_all <- vdem |>
  filter(year >= 1980, year <= 2025) |>
  select(country_name, year, v2x_polyarchy) |>
  mutate(v2x_polyarchy = round(v2x_polyarchy, 3))

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  select(name, name_long, iso_a3, geometry)

# Manual name-matching patches
# Keys are rnaturalearth `name` values; values are exact V-Dem country_name strings
#Note for me, look into Czech matching (Czechia or Czech Republic)
name_map <- c(
  "United States of America" = "United States of America",
  "Russia"                   = "Russia",
  "United Kingdom"           = "United Kingdom",
  "Dem. Rep. Congo"          = "Democratic Republic of the Congo (Zaire)",
  "Congo"                    = "Republic of the Congo",
  "Tanzania"                 = "Tanzania",
  "Ivory Coast"              = "Ivory Coast",
  "Guinea Bissau"            = "Guinea-Bissau",
  "Czechia"                  = "Czech Republic",
  "Macedonia"                = "North Macedonia",
  "Bosnia and Herz."         = "Bosnia and Herzegovina",
  "eSwatini"                 = "Swaziland",
  "Timor-Leste"              = "East Timor",
  "S. Sudan"                 = "South Sudan",
  "Central African Rep."     = "Central African Republic",
  "Eq. Guinea"               = "Equatorial Guinea",
  "Solomon Is."              = "Solomon Islands",
  "W. Sahara"                = "Western Sahara",
  "São Tomé and Principe"    = "Sao Tome and Principe"
)

world <- world |>
  mutate(vdem_name = case_when(
    name %in% names(name_map) ~ name_map[name],
    name_long == "United States" ~ "United States",   # fallback: ne long name → V-Dem
    name_long == "Russia"        ~ "Russia",
    TRUE                         ~ name_long
  ))

# world_base is the geometry-only frame; data is joined reactively per year
world_base <- world

# ── Colour palette ─────────────────────────────────────────────────────────
pal <- colorNumeric(
  palette = c("#2d1b4e", "#6b3a8c", "#c0392b", "#e67e22", "#f1c40f", "#2ecc71", "#1a7a4a"),
  domain  = c(0, 1),
  na.color = "#3a3a3a"
)

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Syne:wght@400;700;800&family=DM+Mono:wght@300;400&display=swap",
      rel  = "stylesheet"
    ),
    tags$style(HTML("
      * { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        background: #0e0e14;
        color: #e8e4d9;
        font-family: 'DM Mono', monospace;
        overflow-x: hidden;
      }

      /* ── Header ── */
      .header-bar {
        background: linear-gradient(135deg, #0e0e14 0%, #1a1030 100%);
        border-bottom: 1px solid #2a2040;
        padding: 22px 36px 18px;
        display: flex;
        align-items: flex-end;
        gap: 28px;
      }
      .header-title {
        font-family: 'Syne', sans-serif;
        font-weight: 800;
        font-size: 2rem;
        letter-spacing: -0.03em;
        color: #f0ece0;
        line-height: 1;
      }
      .header-title span {
        color: #7ec8a0;
      }
      .header-sub {
        font-size: 0.68rem;
        color: #7a7090;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        padding-bottom: 3px;
      }

      /* ── Main layout ── */
      .main-wrap {
        display: flex;
        height: calc(100vh - 80px);
        gap: 0;
      }

      /* ── Sidebar ── */
      .side-panel {
        width: 310px;
        min-width: 280px;
        background: #12111a;
        border-right: 1px solid #1e1c2a;
        padding: 24px 20px;
        display: flex;
        flex-direction: column;
        gap: 20px;
        overflow-y: auto;
      }
      .panel-label {
        font-size: 0.6rem;
        letter-spacing: 0.18em;
        text-transform: uppercase;
        color: #5a5470;
        margin-bottom: 8px;
      }
      .legend-bar {
        height: 14px;
        border-radius: 7px;
        background: linear-gradient(to right, #2d1b4e, #6b3a8c, #c0392b, #e67e22, #f1c40f, #2ecc71, #1a7a4a);
        margin-bottom: 5px;
      }
      .legend-labels {
        display: flex;
        justify-content: space-between;
        font-size: 0.62rem;
        color: #6a6480;
      }

      .stat-box {
        background: #1a1828;
        border: 1px solid #252235;
        border-radius: 10px;
        padding: 14px 16px;
      }
      .stat-val {
        font-family: 'Syne', sans-serif;
        font-size: 1.6rem;
        font-weight: 700;
        color: #7ec8a0;
        line-height: 1;
      }
      .stat-desc {
        font-size: 0.62rem;
        color: #5a5470;
        margin-top: 4px;
        letter-spacing: 0.06em;
        text-transform: uppercase;
      }

      .tier-row {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 7px;
        font-size: 0.68rem;
        color: #a09ab0;
      }
      .tier-dot {
        width: 11px; height: 11px;
        border-radius: 50%;
        flex-shrink: 0;
      }

      /* filter slider */
      .shiny-input-container { width: 100% !important; }
      .irs--shiny .irs-bar { background: #7ec8a0; border-color: #7ec8a0; }
      .irs--shiny .irs-handle { border-color: #7ec8a0; background: #7ec8a0; }
      .irs--shiny .irs-from, .irs--shiny .irs-to,
      .irs--shiny .irs-single { background: #7ec8a0; color: #0e0e14; font-family: 'DM Mono'; }
      .irs--shiny .irs-line { background: #2a2840; border-color: #2a2840; }

      /* ── Map area ── */
      .map-wrap {
        flex: 1;
        position: relative;
        overflow: hidden;
      }
      #map { width: 100%; height: 100%; }

      /* hover info box */
      .info-overlay {
        position: absolute;
        bottom: 24px;
        right: 24px;
        background: rgba(14,14,20,0.92);
        border: 1px solid #2a2840;
        border-radius: 12px;
        padding: 14px 18px;
        min-width: 200px;
        backdrop-filter: blur(8px);
        z-index: 900;
      }
      .info-country {
        font-family: 'Syne', sans-serif;
        font-weight: 700;
        font-size: 1rem;
        color: #f0ece0;
        margin-bottom: 4px;
      }
      .info-score {
        font-size: 1.5rem;
        font-weight: 700;
        font-family: 'Syne', sans-serif;
      }
      .info-tier-label {
        font-size: 0.62rem;
        text-transform: uppercase;
        letter-spacing: 0.1em;
        margin-top: 3px;
        color: #7a7090;
      }

      .source-note {
        font-size: 0.58rem;
        color: #3a3450;
        letter-spacing: 0.05em;
        line-height: 1.6;
      }

      /* top-n table */
      .rank-table { width: 100%; border-collapse: collapse; }
      .rank-table td { padding: 4px 0; font-size: 0.67rem; color: #a09ab0; border-bottom: 1px solid #1e1c2a; }
      .rank-table td:last-child { text-align: right; color: #7ec8a0; }
      .rank-num { color: #3a3450; width: 18px; }
    "))
  ),
  
  # Header
  div(class = "header-bar",
      div(class = "header-title", "DEMOCRACY", tags$br(), tags$span("INDEX"), " MAP"),
      div(class = "header-sub", textOutput("header_year", inline = TRUE))
  ),
  
  div(class = "main-wrap",
      # ── Sidebar ──
      div(class = "side-panel",
          
          div(
            div(class = "panel-label", "Year"),
            sliderInput("year", label = NULL,
                        min = 1980, max = 2025, value = 2025,
                        step = 1, sep = "", animate = animationOptions(interval = 600, loop = FALSE))
          ),
          
          div(
            div(class = "panel-label", "Score Range (v2x_polyarchy)"),
            sliderInput("range", label = NULL,
                        min = 0, max = 1, value = c(0, 1), step = 0.01)
          ),
          
          div(
            div(class = "panel-label", "Score Legend"),
            div(class = "legend-bar"),
            div(class = "legend-labels",
                span("0 — Autocratic"), span("1 — Full Democracy"))
          ),
          
          div(
            div(class = "panel-label", "Regime Tiers"),
            div(class = "tier-row", div(class = "tier-dot", style = "background:#1a7a4a"), "Liberal Democracy (≥ 0.70)"),
            div(class = "tier-row", div(class = "tier-dot", style = "background:#2ecc71"), "Electoral Democracy (0.50–0.69)"),
            div(class = "tier-row", div(class = "tier-dot", style = "background:#f1c40f"), "Hybrid Regime (0.30–0.49)"),
            div(class = "tier-row", div(class = "tier-dot", style = "background:#e67e22"), "Competitive Authoritarianism (0.15–0.29)"),
            div(class = "tier-dot", style = "background:#c0392b"),
            span(style = "font-size:0.68rem; color:#a09ab0", "Closed Autocracy (< 0.15)")
          ),
          
          fluidRow(
            column(6, div(class = "stat-box",
                          div(class = "stat-val", textOutput("n_countries")),
                          div(class = "stat-desc", "Countries")
            )),
            column(6, div(class = "stat-box",
                          div(class = "stat-val", textOutput("avg_score")),
                          div(class = "stat-desc", "Avg Score")
            ))
          ),
          
          div(
            div(class = "panel-label", "Top Democracies"),
            tableOutput("top_table")
          ),
          
          div(class = "source-note",
              "Source: Varieties of Democracy (V-Dem) Dataset v16.", tags$br(),
              "v2x_polyarchy = Electoral Democracy Index.", tags$br(),
              "Coppedge et al. (2025). SSRN 4000971."
          )
      ),
      
      # ── Map ──
      div(class = "map-wrap",
          leafletOutput("map", width = "100%", height = "100%"),
          div(class = "info-overlay",
              div(class = "info-country", textOutput("hover_country")),
              div(class = "info-score",   uiOutput("hover_score")),
              div(class = "info-tier-label", textOutput("hover_tier"))
          )
      )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  

  # ── Debounce year slider so rapid dragging doesn't queue many redraws ──
  year_d  <- debounce(reactive(input$year),  250)
  range_d <- debounce(reactive(input$range), 250)

  output$header_year <- renderText({
    paste0("V-Dem · Electoral Democracy · ", year_d())
  })
  
  # Tier helper
  get_tier <- function(x) {
    case_when(
      is.na(x)    ~ "No data",
      x >= 0.70   ~ "Liberal Democracy",
      x >= 0.50   ~ "Electoral Democracy",
      x >= 0.30   ~ "Hybrid Regime",
      x >= 0.15   ~ "Competitive Authoritarianism",
      TRUE        ~ "Closed Autocracy"
    )
  }
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      worldCopyJump = FALSE,
      zoomControl   = TRUE
    )) |>
      addProviderTiles("CartoDB.DarkMatter",
                       options = tileOptions(opacity = 0.85)) |>
      setView(lng = 15, lat = 20, zoom = 2)
  })
  
  # Filtered data using debounced inputs
  filtered_d <- reactive({
    yr <- year_d()
    rng <- range_d()
    vdem_yr <- vdem_all |>
      filter(year == yr) |>
      group_by(country_name) |>
      summarise(v2x_polyarchy = round(mean(v2x_polyarchy, na.rm = TRUE), 3),
                .groups = "drop")
    world_base |>
      left_join(vdem_yr, by = c("vdem_name" = "country_name")) |>
      mutate(visible = !is.na(v2x_polyarchy) &
               v2x_polyarchy >= rng[1] &
               v2x_polyarchy <= rng[2])
  })

  # Track whether polygons have been drawn for the first time
  polygons_drawn <- reactiveVal(FALSE)

  make_labels <- function(d) {
    sprintf(
      "<div style='font-family:DM Mono,monospace; color:#0e0e14; padding:4px 6px;'>
         <b style='font-size:13px'>%s</b><br/>
         Score: <b>%s</b><br/>
         <span style='font-size:10px; color:#333'>%s</span>
       </div>",
      d$name_long,
      ifelse(d$visible & !is.na(d$v2x_polyarchy),
             sprintf("%.3f", d$v2x_polyarchy), "N/A"),
      get_tier(d$v2x_polyarchy)
    ) |> lapply(htmltools::HTML)
  }

  # Draw polygons once on startup, then only update colours in-place.
  # observeEvent on map_center fires as soon as the map tile loads.
  observeEvent(input$map_center, {
    if (!polygons_drawn()) {
      d <- filtered_d()
      fill_color <- ifelse(d$visible, pal(d$v2x_polyarchy), "#1e1c2a")
      leafletProxy("map") |>
        clearShapes() |>
        addPolygons(
          data         = d,
          fillColor    = fill_color,
          fillOpacity  = 0.82,
          color        = "#0a0910",
          weight       = 0.5,
          smoothFactor = 1,
          label        = make_labels(d),
          labelOptions = labelOptions(
            style     = list("background" = "rgba(240,236,224,0.95)",
                             "border"     = "none",
                             "border-radius" = "6px"),
            direction = "auto"
          ),
          layerId = d$name_long
        )
      polygons_drawn(TRUE)
    }
  }, ignoreNULL = TRUE, once = TRUE)

  # Subsequent updates: only update fill colour in-place via setShapeStyle,
  # avoiding the clear-and-redraw flash entirely.
  observe({
    req(polygons_drawn())
    d <- filtered_d()
    fill_color <- ifelse(d$visible, pal(d$v2x_polyarchy), "#1e1c2a")

    proxy <- leafletProxy("map")

    # setShapeStyle is available in leaflet.extras2; if not installed, fall back
    # to a minimal clearShapes redraw (still debounced so lag is reduced)
    if (requireNamespace("leaflet.extras2", quietly = TRUE)) {
      leaflet.extras2::setShapeStyle(
        proxy,
        data      = d,
        layerId   = ~name_long,
        fillColor = fill_color,
        fillOpacity = 0.82,
        label     = make_labels(d)
      )
    } else {
      proxy |>
        clearShapes() |>
        addPolygons(
          data         = d,
          fillColor    = fill_color,
          fillOpacity  = 0.82,
          color        = "#0a0910",
          weight       = 0.5,
          smoothFactor = 1,
          label        = make_labels(d),
          labelOptions = labelOptions(
            style     = list("background" = "rgba(240,236,224,0.95)",
                             "border"     = "none",
                             "border-radius" = "6px"),
            direction = "auto"
          ),
          layerId = d$name_long
        )
    }
  })
  
  # Hover panel
  rv <- reactiveValues(country = "Hover over a country", score = NULL, tier = "")
  
  observeEvent(input$map_shape_mouseover, {
    hov <- input$map_shape_mouseover
    clicked_name <- hov$id
    row <- filtered_d() |> filter(name_long == clicked_name)
    if (nrow(row) == 1 && row$visible) {
      rv$country <- row$name_long
      rv$score   <- row$v2x_polyarchy
      rv$tier    <- get_tier(row$v2x_polyarchy)
    }
  })
  
  output$hover_country <- renderText({ rv$country })
  output$hover_score   <- renderUI({
    if (is.null(rv$score) || is.na(rv$score)) {
      span("—", style = "color:#5a5470")
    } else {
      col <- pal(rv$score)
      span(sprintf("%.3f", rv$score), style = paste0("color:", col))
    }
  })
  output$hover_tier <- renderText({ rv$tier })
  
  # Stats
  output$n_countries <- renderText({
    filtered_d() |> filter(visible & !is.na(v2x_polyarchy)) |> nrow()
  })
  output$avg_score <- renderText({
    vals <- filtered_d() |> filter(visible & !is.na(v2x_polyarchy)) |> pull(v2x_polyarchy)
    if (length(vals) == 0) "—" else sprintf("%.2f", mean(vals))
  })
  
  output$top_table <- renderTable({
    filtered_d() |>
      as.data.frame() |>
      filter(!is.na(v2x_polyarchy), visible) |>
      distinct(name_long, .keep_all = TRUE) |>
      arrange(desc(v2x_polyarchy)) |>
      head(8) |>
      mutate(Rank = row_number()) |>
      select("#" = Rank, Country = name_long, Score = v2x_polyarchy) |>
      mutate(Score = sprintf("%.3f", Score))
  },
  striped = FALSE, hover = FALSE, bordered = FALSE,
  spacing = "xs", align = "l",
  rownames = FALSE,
  sanitize.text.function = identity,
  options = list(dom = 't')
  )
}

shinyApp(ui, server)
