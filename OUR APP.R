library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(readr)

# ── Data ──────────────────────────────────────────────────────────────────────
# Locate the CSV relative to wherever this app.R file lives.
# Put V-Dem-CD-v16.csv in the same folder as app.R, or update csv_path below.
app_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
)
csv_path <- file.path(app_dir, "V-Dem-CD-v16.csv")

if (!file.exists(csv_path)) {
  stop(paste0(
    "Cannot find V-Dem-CD-v16.csv.\n",
    "Expected it at: ", csv_path, "\n",
    "Place the CSV in the same folder as app.R, or edit csv_path in the script."
  ))
}

vdem <- read_csv(csv_path, show_col_types = FALSE)

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
ui <- navbarPage(
  title = div(
    style = "font-family:'Syne',sans-serif; font-weight:800; font-size:1.1rem; letter-spacing:-0.02em; color:#f0ece0;",
    "DEMOCRACY ", tags$span("INDEX", style = "color:#7ec8a0;"), " MAP"
  ),
  id = "nav",
  selected = "Overview",
  collapsible = TRUE,
  
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Syne:wght@400;600;700;800&family=DM+Mono:wght@300;400&display=swap",
      rel  = "stylesheet"
    ),
    tags$style(HTML("
      /* ── Global ── */
      * { box-sizing: border-box; }

      body {
        background: #0e0e14;
        color: #e8e4d9;
        font-family: 'DM Mono', monospace;
        overflow-x: hidden;
      }

      /* ── Navbar ── */
      .navbar {
        background: linear-gradient(135deg, #0e0e14 0%, #1a1030 100%) !important;
        border-bottom: 1px solid #2a2040 !important;
        min-height: 56px;
      }
      .navbar-brand { padding: 10px 16px; }
      .navbar-nav > li > a {
        font-family: 'DM Mono', monospace !important;
        font-size: 0.72rem !important;
        letter-spacing: 0.14em !important;
        text-transform: uppercase !important;
        color: #7a7090 !important;
        padding: 18px 18px !important;
        transition: color 0.2s;
      }
      .navbar-nav > li > a:hover { color: #c8c0e0 !important; }
      .navbar-nav > li.active > a,
      .navbar-nav > li.active > a:focus {
        color: #7ec8a0 !important;
        background: transparent !important;
        border-bottom: 2px solid #7ec8a0;
      }
      .navbar-toggle .icon-bar { background: #7a7090; }
      .navbar-collapse { border-top: none !important; box-shadow: none !important; }

      /* ── Tab content padding reset ── */
      .tab-content > .tab-pane { padding: 0; }
      .container-fluid { padding: 0; }

      /* ══════════════════════════════════════════════
         OVERVIEW PAGE
      ══════════════════════════════════════════════ */
      .overview-page {
        min-height: calc(100vh - 56px);
        background: #0e0e14;
        overflow-y: auto;
      }

      /* Hero band */
      .hero {
        background: linear-gradient(160deg, #0e0e14 0%, #15102a 50%, #0e1a14 100%);
        padding: 72px 10% 60px;
        border-bottom: 1px solid #1e1c2a;
        position: relative;
        overflow: hidden;
      }
      .hero::before {
        content: '';
        position: absolute;
        inset: 0;
        background: radial-gradient(ellipse 60% 50% at 70% 50%, rgba(126,200,160,0.07) 0%, transparent 70%);
        pointer-events: none;
      }
      .hero-eyebrow {
        font-size: 0.6rem;
        letter-spacing: 0.22em;
        text-transform: uppercase;
        color: #7ec8a0;
        margin-bottom: 18px;
      }
      .hero-title {
        font-family: 'Syne', sans-serif;
        font-weight: 800;
        font-size: clamp(2rem, 5vw, 3.6rem);
        line-height: 1.05;
        letter-spacing: -0.03em;
        color: #f0ece0;
        max-width: 820px;
        margin-bottom: 24px;
      }
      .hero-title em {
        font-style: normal;
        color: #7ec8a0;
      }
      .hero-lead {
        font-size: 0.88rem;
        line-height: 1.8;
        color: #9a94aa;
        max-width: 640px;
        margin-bottom: 36px;
      }
      .hero-cta {
        display: inline-block;
        background: #7ec8a0;
        color: #0e0e14;
        font-family: 'DM Mono', monospace;
        font-size: 0.7rem;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        padding: 12px 28px;
        border-radius: 4px;
        border: none;
        cursor: pointer;
        text-decoration: none;
        transition: background 0.2s, transform 0.15s;
      }
      .hero-cta:hover { background: #9adbb8; transform: translateY(-1px); color: #0e0e14; text-decoration: none; }

      /* Info grid */
      .info-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
        gap: 1px;
        background: #1e1c2a;
        border-top: 1px solid #1e1c2a;
      }
      .info-card {
        background: #0e0e14;
        padding: 40px 36px;
      }
      .info-card-label {
        font-size: 0.58rem;
        letter-spacing: 0.2em;
        text-transform: uppercase;
        color: #7ec8a0;
        margin-bottom: 14px;
      }
      .info-card-title {
        font-family: 'Syne', sans-serif;
        font-weight: 700;
        font-size: 1.1rem;
        color: #f0ece0;
        margin-bottom: 14px;
        line-height: 1.3;
      }
      .info-card-body {
        font-size: 0.78rem;
        line-height: 1.85;
        color: #7a7090;
      }
      .info-card-body p { margin-bottom: 10px; }
      .info-card-body p:last-child { margin-bottom: 0; }

      /* Key terms strip */
      .terms-strip {
        padding: 40px 10%;
        border-top: 1px solid #1e1c2a;
        border-bottom: 1px solid #1e1c2a;
      }
      .terms-strip-label {
        font-size: 0.58rem;
        letter-spacing: 0.2em;
        text-transform: uppercase;
        color: #5a5470;
        margin-bottom: 20px;
      }
      .terms-list {
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
      }
      .term-chip {
        background: #1a1828;
        border: 1px solid #252235;
        border-radius: 20px;
        padding: 6px 16px;
        font-size: 0.68rem;
        color: #a09ab0;
      }
      .term-chip strong { color: #7ec8a0; font-weight: 400; }

      /* Footer */
      .page-footer {
        padding: 28px 10%;
        display: flex;
        justify-content: space-between;
        align-items: center;
        flex-wrap: wrap;
        gap: 12px;
      }
      .page-footer-left { font-size: 0.6rem; color: #3a3450; letter-spacing: 0.06em; line-height: 1.7; }
      .page-footer-right { font-size: 0.6rem; color: #3a3450; letter-spacing: 0.06em; }

      /* ══════════════════════════════════════════════
         MAP PAGE  (existing styles preserved)
      ══════════════════════════════════════════════ */
      .map-page-wrap {
        display: flex;
        height: calc(100vh - 56px);
      }
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
      .map-page-header {
        padding: 14px 20px 12px;
        background: linear-gradient(135deg, #0e0e14 0%, #1a1030 100%);
        border-bottom: 1px solid #2a2040;
      }
      .map-page-title {
        font-family: 'Syne', sans-serif;
        font-weight: 800;
        font-size: 1rem;
        color: #f0ece0;
        letter-spacing: -0.02em;
      }
      .map-page-sub {
        font-size: 0.6rem;
        color: #7a7090;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        margin-top: 2px;
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
      .tier-dot { width: 11px; height: 11px; border-radius: 50%; flex-shrink: 0; }
      .shiny-input-container { width: 100% !important; }
      .irs--shiny .irs-bar { background: #7ec8a0; border-color: #7ec8a0; }
      .irs--shiny .irs-handle { border-color: #7ec8a0; background: #7ec8a0; }
      .irs--shiny .irs-from, .irs--shiny .irs-to,
      .irs--shiny .irs-single { background: #7ec8a0; color: #0e0e14; font-family: 'DM Mono'; }
      .irs--shiny .irs-line { background: #2a2840; border-color: #2a2840; }
      .map-wrap { flex: 1; position: relative; overflow: hidden; }
      #map { width: 100%; height: 100%; }
      .info-overlay {
        position: absolute; bottom: 24px; right: 24px;
        background: rgba(14,14,20,0.92); border: 1px solid #2a2840;
        border-radius: 12px; padding: 14px 18px; min-width: 200px;
        backdrop-filter: blur(8px); z-index: 900;
      }
      .info-country { font-family: 'Syne',sans-serif; font-weight:700; font-size:1rem; color:#f0ece0; margin-bottom:4px; }
      .info-score   { font-size:1.5rem; font-weight:700; font-family:'Syne',sans-serif; }
      .info-tier-label { font-size:0.62rem; text-transform:uppercase; letter-spacing:0.1em; margin-top:3px; color:#7a7090; }
      .source-note { font-size:0.58rem; color:#3a3450; letter-spacing:0.05em; line-height:1.6; }
      .rank-table { width:100%; border-collapse:collapse; }
      .rank-table td { padding:4px 0; font-size:0.67rem; color:#a09ab0; border-bottom:1px solid #1e1c2a; }
      .rank-table td:last-child { text-align:right; color:#7ec8a0; }
    "))
  ),
  
  # ══════════════════════════════════════════════════════════════════
  #  TAB 1 — OVERVIEW (title / info page)
  # ══════════════════════════════════════════════════════════════════
  tabPanel("Overview",
           div(class = "overview-page",
               
               # ── Hero ──────────────────────────────────────────────────────
               div(class = "hero",
                   div(class = "hero-eyebrow", "V-Dem Dataset v16  ·  1980 – 2025"),
                   div(class = "hero-title",
                       "Mapping Electoral ", tags$em("Democracy"), tags$br(), "Across the World"
                   ),
                   div(class = "hero-lead",
                       # ↓↓ EDIT: replace with your own research question / intro sentence
                       "How has the quality of electoral democracy changed across the world
           between 1980 and 2025? This interactive tool visualises the
           V-Dem Electoral Democracy Index (v2x_polyarchy) — a composite
           measure of free elections, suffrage, freedom of expression, and
           associational autonomy — for 180+ countries over 45 years."
                   ),
                   tags$a(class = "hero-cta", href = "#", onclick = "$('a[data-value=\"Interactive Map\"]').tab('show'); return false;",
                          "Open the Map →"
                   )
               ),
               
               # ── Info cards ────────────────────────────────────────────────
               div(class = "info-grid",
                   
                   div(class = "info-card",
                       div(class = "info-card-label", "Research Question"),
                       div(class = "info-card-title",
                           # ↓↓ EDIT: your research question
                           "Have global levels of electoral democracy declined since the early 2000s peak?"
                       ),
                       div(class = "info-card-body",
                           # ↓↓ EDIT: elaboration / hypothesis
                           tags$p("This project examines the so-called 'democratic recession' hypothesis,
                    asking whether aggregate electoral democracy scores have fallen since
                    their post-Cold War high, and which regions are driving any observed trend."),
                           tags$p("We pay particular attention to backsliding cases — countries that scored
                    above 0.5 in 1995 but have since dropped below that threshold.")
                       )
                   ),
                   
                   div(class = "info-card",
                       div(class = "info-card-label", "Background"),
                       div(class = "info-card-title",
                           # ↓↓ EDIT: background / context
                           "The Third Wave and Its Reversal"
                       ),
                       div(class = "info-card-body",
                           # ↓↓ EDIT: background text
                           tags$p("Huntington's 'Third Wave' of democratisation (1974–1990s) saw dozens of
                    authoritarian regimes transition to competitive elections. By 2006 Freedom
                    House recorded more electoral democracies than ever before."),
                           tags$p("Since then, scholars including Levitsky & Ziblatt and Larry Diamond have
                    documented a reversal — not through dramatic coups, but via incremental
                    erosion of institutions from within by elected leaders.")
                       )
                   ),
                   
                   div(class = "info-card",
                       div(class = "info-card-label", "Data & Method"),
                       div(class = "info-card-title",
                           # ↓↓ EDIT: data/methodology note
                           "V-Dem Electoral Democracy Index"
                       ),
                       div(class = "info-card-body",
                           # ↓↓ EDIT: method description
                           tags$p("The Electoral Democracy Index (v2x_polyarchy) is constructed from five
                    high-level components: elected officials, clean elections, freedom of
                    expression, associational autonomy, and inclusive suffrage."),
                           tags$p("Scores range from 0 (fully autocratic) to 1 (fully democratic) and are
                    aggregated from expert surveys covering 202 countries from 1789 to the
                    present. This analysis uses V-Dem Dataset v16 (Coppedge et al., 2025).")
                       )
                   ),
                   
                   div(class = "info-card",
                       div(class = "info-card-label", "How to Use"),
                       div(class = "info-card-title", "Navigating the Map"),
                       div(class = "info-card-body",
                           tags$p("Click 'Interactive Map' in the navigation bar to explore the choropleth.
                    Use the Year slider (1980–2025) to travel through time, or press ▶ Play
                    to animate the full 45-year sequence automatically."),
                           tags$p("The Score Range filter lets you isolate specific democracy tiers.
                    Hover over any country to see its exact index score and regime classification.")
                       )
                   )
                   
               ), # /info-grid
               
               # ── Key terms ─────────────────────────────────────────────────
               div(class = "terms-strip",
                   div(class = "terms-strip-label", "Key Concepts"),
                   div(class = "terms-list",
                       div(class = "term-chip", tags$strong("v2x_polyarchy"), " Electoral Democracy Index"),
                       div(class = "term-chip", tags$strong("Liberal Democracy"), " ≥ 0.70"),
                       div(class = "term-chip", tags$strong("Electoral Democracy"), " 0.50 – 0.69"),
                       div(class = "term-chip", tags$strong("Hybrid Regime"), " 0.30 – 0.49"),
                       div(class = "term-chip", tags$strong("Competitive Authoritarianism"), " 0.15 – 0.29"),
                       div(class = "term-chip", tags$strong("Closed Autocracy"), " < 0.15"),
                       div(class = "term-chip", tags$strong("Democratic Backsliding")),
                       div(class = "term-chip", tags$strong("Third Wave of Democracy")),
                       div(class = "term-chip", tags$strong("V-Dem"), " Varieties of Democracy Project")
                   )
               ),
               
               # ── Footer ────────────────────────────────────────────────────
               div(class = "page-footer",
                   div(class = "page-footer-left",
                       "Data: Coppedge, Michael et al. (2025). V-Dem Dataset v16. Varieties of Democracy Project.", tags$br(),
                       "Pemstein et al. (2025). The V-Dem Measurement Model: Latent Variable Analysis. V-Dem Working Paper No. 21."
                   ),
                   div(class = "page-footer-right", "Built with R Shiny & Leaflet")
               )
               
           ) # /overview-page
  ), # /tabPanel Overview
  
  # ══════════════════════════════════════════════════════════════════
  #  TAB 2 — INTERACTIVE MAP
  # ══════════════════════════════════════════════════════════════════
  tabPanel("Interactive Map",
           div(class = "map-page-wrap",
               
               # ── Sidebar ──
               div(class = "side-panel",
                   
                   div(class = "map-page-header",
                       div(class = "map-page-title", "Electoral Democracy Index"),
                       div(class = "map-page-sub", textOutput("header_year", inline = TRUE))
                   ),
                   
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
                       "Source: V-Dem Dataset v16.", tags$br(),
                       "v2x_polyarchy = Electoral Democracy Index.", tags$br(),
                       "Coppedge et al. (2025)."
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
  ) # /tabPanel Map
  
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
  
  # Subsequent updates: redraw polygons with updated colours.
  # Debouncing above (250ms) means this only fires after the slider settles,
  # keeping transitions smooth without needing any external packages.
  observe({
    req(polygons_drawn())
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

