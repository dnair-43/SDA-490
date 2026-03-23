suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(dplyr)
  library(sf)
  library(haven)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(RColorBrewer)
  library(readr)
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(tidyr)
  library(htmltools)
  library(purrr)
})

# ── Data ──────────────────────────────────────────────────────────────────────
# Locate the CSV relative to wherever this app.R file lives.
# Put V-Dem-CD-v16.csv in the same folder as app.R, or update csv_path below.

app_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
)
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

# ── WVS Data ──────────────────────────────────────────────────────────────────
wvs_path <- file.path(app_dir, "WVS_TimeSeries_1981_2020_R_v2_0.rdata")

if (file.exists(wvs_path)) {
  load(wvs_path)   # loads object `wvs_data`
} else {
  # Synthetic demo data so the app runs without the real file
  set.seed(42)
  wvs_countries <- c(
    "Denmark", "Sweden", "Belgium", "Norway", "Ireland", "France",
    "Mexico", "Brazil", "Venezuela", "Chile", "Argentina",
    "United States", "Canada", "Russia", "Finland", "Australia",
    "Qatar", "Vietnam", "China", "Sudan", "Iran", "Iraq",
    "Niger", "Israel", "Palestine", "Libya", "Bolivia",
    "Hungary", "Poland", "Romania", "Ukraine", "Italy",
    "United Kingdom", "Pakistan", "Mozambique"
  )
  wvs_data <- expand.grid(
    country = wvs_countries,
    year    = c(1994, 1999, 2004, 2009, 2014, 2020)
  ) %>%
    mutate(
      X003        = round(runif(n(), 18, 80)),
      gdp_per_cap = exp(runif(n(), 7, 11)),
      E114        = sample(1:4, n(), replace = TRUE),
      E116        = sample(1:4, n(), replace = TRUE),
      E115        = sample(1:4, n(), replace = TRUE),
      E018        = sample(1:4, n(), replace = TRUE),
      A042        = sample(1:4, n(), replace = TRUE),
      E225        = sample(1:4, n(), replace = TRUE),
      E228        = sample(1:4, n(), replace = TRUE),
      A124_06     = sample(1:4, n(), replace = TRUE),
      A124_12     = sample(1:4, n(), replace = TRUE),
      A124_43     = sample(1:4, n(), replace = TRUE),
      A007        = sample(1:6, n(), replace = TRUE)   # Individualism (1=family important … 6=not important)
    )
}

# ── GINI Data ──────────────────────────────────────────────────────────────────
gini_path <- file.path(app_dir, "GINI_DATA.csv")

if (file.exists(gini_path)) {
  gini_raw <- read_csv(gini_path, show_col_types = FALSE)
  # Expected columns: country, year, gini  (or similar — we normalise below)
  # Normalise column names to lowercase
  names(gini_raw) <- tolower(names(gini_raw))
  # Rename the gini column if needed (accepts "gini", "gini_index", "gini_coefficient")
  gini_col <- intersect(c("gini", "gini_index", "gini_coefficient"), names(gini_raw))[1]
  gini_data <- gini_raw %>%
    rename(gini = all_of(gini_col)) %>%
    select(country, year, gini) %>%
    mutate(gini = as.numeric(gini))
} else {
  # Synthetic Gini demo data keyed to the same countries & years as WVS
  set.seed(99)
  gini_countries <- c(
    "Denmark", "Sweden", "Belgium", "Norway", "Ireland", "France",
    "Mexico", "Brazil", "Venezuela", "Chile", "Argentina",
    "United States", "Canada", "Russia", "Finland", "Australia",
    "Qatar", "Vietnam", "China", "Sudan", "Iran", "Iraq",
    "Niger", "Israel", "Palestine", "Libya", "Bolivia",
    "Hungary", "Poland", "Romania", "Ukraine", "Italy",
    "United Kingdom", "Pakistan", "Mozambique"
  )
  gini_data <- expand.grid(
    country = gini_countries,
    year    = c(1994, 1999, 2004, 2009, 2014, 2020)
  ) %>%
    mutate(gini = round(runif(n(), 25, 65), 1))
}

auth_vars <- c("E114","E116","E115","E018","A042",
               "E225","E228","A124_06","A124_12","A124_43")

wvs_data <- wvs_data %>%
  rowwise() %>%
  mutate(auth_index = mean(c_across(all_of(auth_vars)), na.rm = TRUE)) %>%
  ungroup()

wvs_countries_list <- c(
  "Denmark", "Sweden", "Belgium", "Norway", "Ireland", "France",
  "Mexico", "Brazil", "Venezuela", "Chile", "Argentina",
  "United States", "Canada", "Russia", "Finland", "Australia",
  "Qatar", "Vietnam", "China", "Sudan", "Iran", "Iraq",
  "Niger", "Israel", "Palestine", "Libya", "Bolivia",
  "Hungary", "Poland", "Romania", "Ukraine", "Italy",
  "United Kingdom", "Pakistan", "Mozambique"
)
PERMANENT_COUNTRIES <- "Canada"
# Filter wvs_data to only include countries in the list (if real data loaded)
wvs_data <- wvs_data %>%
  filter(country %in% wvs_countries_list)
wvs_years          <- sort(unique(wvs_data$year))

wvs_y_choices <- c(
  "Mean Age"                    = "X003",
  "GDP per Capita (USD)"        = "gdp_per_cap",
  "Authoritarian Index"         = "auth_index",
  "Individualism (A007)"        = "A007",
  "Economic Inequality (Gini)"  = "gini"
)


# ── CTM Data (Canada Authoritarian Map) ────────────────────────────────────────
ctm_raw <- read_sav("CTM_DATA.sav")

ctm_auth_vars <- c("HD3", "V3", "ATT10", "HD4", "V2")

ctm <- ctm_raw %>%
  mutate(across(all_of(ctm_auth_vars), as.numeric)) %>%
  rowwise() %>%
  mutate(auth_index = mean(c_across(all_of(ctm_auth_vars)), na.rm = TRUE)) %>%
  ungroup()

region_to_province <- c(
  "BC_REG"   = "British Columbia",
  "NS_REG"   = "Nova Scotia",
  "NL_REG"   = "Newfoundland and Labrador",
  "MB_REG"   = "Manitoba",
  "AB_REG5"  = "Alberta",
  "AB_REG4"  = "Alberta",
  "AB_REG3"  = "Alberta",
  "ON_REG"   = "Ontario",
  "ON_REG4"  = "Ontario",
  "ON_REG2"  = "Ontario",
  "QC_REG2"  = "Quebec",
  "QC_REG4"  = "Quebec",
  "SK_REG"   = "Saskatchewan",
  "SK_REG3"  = "Saskatchewan",
  "NB_REG4"  = "New Brunswick",
  "NB_REG"   = "New Brunswick"
)

region_cols <- intersect(names(region_to_province), names(ctm))

ctm_id <- ctm %>%
  mutate(.row_id = row_number()) %>%
  mutate(across(all_of(region_cols), haven::zap_labels))

ctm_long <- ctm_id %>%
  select(all_of(c(".row_id", "auth_index", region_cols))) %>%
  tidyr::pivot_longer(
    cols      = all_of(region_cols),
    names_to  = "reg_col",
    values_to = "reg_val"
  ) %>%
  filter(!is.na(reg_val)) %>%
  mutate(province = region_to_province[reg_col]) %>%
  filter(!is.na(province), !is.na(auth_index))

prov_auth <- ctm_long %>%
  distinct(.row_id, province, .keep_all = TRUE) %>%
  group_by(province) %>%
  summarise(
    mean_auth = round(mean(auth_index, na.rm = TRUE), 3),
    n_resp    = n(),
    .groups   = "drop"
  )

canada_sf <- ne_download(
  scale       = 50,
  type        = "states",
  category    = "cultural",
  returnclass = "sf"
) %>%
  filter(admin == "Canada") %>%
  select(name, geometry) %>%
  rename(province = name) %>%
  mutate(province = recode(province, "Québec" = "Quebec"))

canada_map_sf <- canada_sf %>%
  left_join(prov_auth, by = "province")

auth_range <- range(prov_auth$mean_auth, na.rm = TRUE)

auth_pal <- colorNumeric(
  palette  = c("#1a7a4a", "#2ecc71", "#f1c40f", "#e67e22", "#c0392b", "#6b3a8c", "#2d1b4e"),
  domain   = auth_range,
  na.color = "#2a2840"
)

make_canada_tooltip <- function(prov_name, mean_val, n) {
  if (is.na(mean_val)) {
    return(sprintf(
      "<div style='font-family:\"DM Mono\",monospace;background:#12111a;
       border:1px solid #2a2040;border-radius:10px;padding:12px 15px;
       color:#7a7090;font-size:12px;'>
       <b style='color:#f0ece0;'>%s</b><br><br>No data available.</div>",
      htmltools::htmlEscape(prov_name)
    ))
  }
  bar_pct <- (mean_val - auth_range[1]) / max(diff(auth_range), 0.001) * 100
  bar_col  <- auth_pal(mean_val)
  auth_label <- dplyr::case_when(
    mean_val >= 4.0 ~ "Very High",
    mean_val >= 3.0 ~ "High",
    mean_val >= 2.0 ~ "Moderate",
    TRUE            ~ "Low"
  )
  sprintf("
    <div style='
      font-family: \"DM Mono\", monospace;
      background: #12111a;
      border: 1px solid #2a2040;
      border-radius: 13px;
      padding: 15px 17px;
      min-width: 230px;
      max-width: 260px;
      box-shadow: 0 6px 30px rgba(0,0,0,0.6);
    '>
      <div style='
        font-family: \"Syne\", sans-serif;
        font-weight: 800; font-size: 15px;
        color: #f0ece0; margin-bottom: 12px;
        padding-bottom: 9px;
        border-bottom: 1px solid #2a2040;
        letter-spacing: -0.01em;
      '>%s</div>
      <div style='margin-bottom:10px;'>
        <div style='display:flex;justify-content:space-between;margin-bottom:5px;'>
          <span style='font-size:11px;color:#7a7090;text-transform:uppercase;
                       letter-spacing:0.1em;'>Authoritarian Index</span>
          <span style='font-size:14px;font-weight:800;color:%s;'>%.3f</span>
        </div>
        <div style='background:#1e1c2a;border-radius:4px;height:9px;width:100%%;overflow:hidden;'>
          <div style='background:%s;width:%.1f%%;height:9px;border-radius:4px;
               transition:width 0.4s ease;'></div>
        </div>
      </div>
      <div style='display:flex;justify-content:space-between;margin-top:8px;'>
        <span style='font-size:11px;color:#5a5470;'>Level</span>
        <span style='font-size:11px;font-weight:700;color:%s;'>%s</span>
      </div>
      <div style='display:flex;justify-content:space-between;margin-top:4px;'>
        <span style='font-size:11px;color:#5a5470;'>Respondents</span>
        <span style='font-size:11px;color:#a09ab0;'>%d</span>
      </div>
      <div style='font-size:9px;color:#3a3450;margin-top:10px;text-align:right;
                  letter-spacing:0.06em;text-transform:uppercase;'>
        Mean of HD3 · V3 · ATT10 · HD4 · V2
      </div>
    </div>",
          htmltools::htmlEscape(prov_name),
          bar_col, mean_val,
          bar_col, min(bar_pct, 100),
          bar_col, auth_label,
          n
  )
}

canada_labels_html <- mapply(
  make_canada_tooltip,
  prov_name = canada_map_sf$province,
  mean_val  = canada_map_sf$mean_auth,
  n         = ifelse(is.na(canada_map_sf$n_resp), 0L, canada_map_sf$n_resp),
  SIMPLIFY  = FALSE
)

prov_rows_ui <- prov_auth %>%
  arrange(desc(mean_auth)) %>%
  purrr::pmap(function(province, mean_auth, n_resp) {
    col <- auth_pal(mean_auth)
    tags$div(class = "ctm-prov-row",
             tags$span(province),
             tags$span(class = "ctm-prov-score", style = paste0("color:", col, ";"),
                       sprintf("%.3f", mean_auth))
    )
  })

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

      /* ══════════════════════════════════════════════
         WVS SCATTERPLOT TAB
      ══════════════════════════════════════════════ */
      .wvs-wrap {
        display: flex;
        min-height: calc(100vh - 56px);
        background: #0f1117;
      }
      .wvs-sidebar {
        width: 280px;
        min-width: 260px;
        background: #13161f;
        border-right: 1px solid #1e2130;
        padding: 24px 20px;
        display: flex;
        flex-direction: column;
        gap: 18px;
        overflow-y: auto;
      }
      .wvs-plot-panel {
        flex: 1;
        padding: 28px 32px;
        display: flex;
        flex-direction: column;
        gap: 14px;
        background: #0f1117;
      }
      .wvs-plot-title {
        font-family: 'Syne', sans-serif;
        font-weight: 700;
        font-size: 1.15rem;
        color: #f0ece0;
        margin: 0;
      }
      .wvs-plot-subtitle {
        font-size: 0.74rem;
        color: #5c6070;
        margin: 2px 0 0;
        letter-spacing: 0.04em;
        font-family: 'DM Mono', monospace;
        text-transform: uppercase;
      }
      .wvs-plot-wrap {
        background: #13161f;
        border: 1px solid #1e2130;
        border-radius: 10px;
        overflow: hidden;
        flex: 1;
      }
      .wvs-ctrl-label {
        font-size: 0.6rem;
        letter-spacing: 0.18em;
        text-transform: uppercase;
        color: #5a5470;
        margin-bottom: 8px;
        font-family: 'DM Mono', monospace;
      }
      .wvs-divider { border: none; border-top: 1px solid #1e2130; margin: 0; }
      .btn-gold {
        background: #7ec8a0;
        border: none;
        color: #0f1117;
        font-family: 'DM Mono', monospace;
        font-weight: 500;
        font-size: 0.72rem;
        letter-spacing: 0.06em;
        text-transform: uppercase;
        padding: 8px 14px;
        border-radius: 6px;
        cursor: pointer;
        transition: background .2s;
      }
      .btn-gold:hover { background: #d9be8a; }
      .btn-dark {
        background: #2a2d3a;
        border: none;
        color: #e8e6df;
        font-family: 'DM Mono', monospace;
        font-size: 0.72rem;
        letter-spacing: 0.06em;
        text-transform: uppercase;
        padding: 8px 14px;
        border-radius: 6px;
        cursor: pointer;
        transition: background .2s;
      }
      .btn-dark:hover { background: #3a3d4a; }
      .wvs-stat-row { display: flex; gap: 8px; flex-wrap: wrap; }
      .wvs-stat-pill {
        background: #1a1828;
        border: 1px solid #252235;
        border-radius: 8px;
        padding: 10px 12px;
        flex: 1;
        min-width: 70px;
      }
      .wvs-stat-pill .val {
        font-family: 'Syne', sans-serif;
        font-size: 1.25rem;
        font-weight: 700;
        color: #7ec8a0;
        display: block;
        line-height: 1;
      }
      .wvs-stat-pill .lbl {
        font-size: 0.6rem;
        color: #5a5470;
        text-transform: uppercase;
        letter-spacing: 0.1em;
        margin-top: 4px;
        font-family: 'DM Mono', monospace;
      }
      .wvs-index-note {
        font-size: 0.68rem;
        color: #4a4d5a;
        line-height: 1.55;
        padding: 10px 12px;
        background: #1a1828;
        border-left: 2px solid #7ec8a044;
        border-radius: 0 6px 6px 0;
        font-family: 'DM Mono', monospace;
      }
      /* Override slider colours for WVS tab to match overview green */
      .wvs-sidebar .irs--shiny .irs-bar { background: #7ec8a0 !important; border-color: #7ec8a0 !important; }
      .wvs-sidebar .irs--shiny .irs-handle { background: #7ec8a0 !important; border-color: #7ec8a0 !important; }
      .wvs-sidebar .irs--shiny .irs-from,
      .wvs-sidebar .irs--shiny .irs-to,
      .wvs-sidebar .irs--shiny .irs-single { background: #7ec8a0 !important; color: #0e0e14 !important; }
      /* Selectize in WVS — white box, dark text */
      .wvs-sidebar .selectize-control .selectize-input {
        background: #ffffff !important; border: 1px solid #c8cad4 !important;
        color: #1a1a2e !important; border-radius: 6px !important;
        font-size: 0.82rem !important; box-shadow: none !important; padding: 7px 10px !important;
      }
      .wvs-sidebar .selectize-control .selectize-input input {
        color: #1a1a2e !important;
      }
      .wvs-sidebar .selectize-dropdown {
        background: #ffffff !important; border: 1px solid #c8cad4 !important;
        color: #1a1a2e !important; border-radius: 6px !important;
        box-shadow: 0 4px 16px rgba(0,0,0,0.18) !important;
      }
      .wvs-sidebar .selectize-dropdown .option { color: #1a1a2e !important; }
      .wvs-sidebar .selectize-dropdown .option:hover,
      .wvs-sidebar .selectize-dropdown .option.active {
        background: #e8f7f0 !important; color: #0e6644 !important;
      }
      .wvs-sidebar .selectize-input .item {
        background: #e8f7f0 !important; border: 1px solid #7ec8a0 !important;
        color: #0e4d33 !important; border-radius: 4px !important;
        padding: 1px 6px !important; font-size: 0.78rem !important;
      }
      .wvs-sidebar .selectize-input .item.active {
        background: #7ec8a0 !important; color: #0e0e14 !important;
      }

      /* ══════════════════════════════════════════════
         CANADA AUTH MAP TAB
      ══════════════════════════════════════════════ */
      .ctm-wrap {
        display: flex;
        height: calc(100vh - 56px);
        background: #0e0e14;
      }
      .ctm-sidebar {
        width: 250px;
        min-width: 220px;
        background: #12111a;
        border-right: 1px solid #1e1c2a;
        padding: 20px 16px;
        display: flex;
        flex-direction: column;
        gap: 10px;
        overflow-y: auto;
      }
      .ctm-map-wrap { flex: 1; position: relative; overflow: hidden; }
      #canada_map { width: 100%; height: 100%; }
      .ctm-section-title {
        font-size: 0.57rem; letter-spacing: 0.18em;
        text-transform: uppercase; color: #5a5470;
        margin-bottom: 4px; margin-top: 6px;
      }
      .ctm-section-title:first-of-type { margin-top: 0; }
      .ctm-grad-bar {
        height: 14px; border-radius: 7px;
        background: linear-gradient(to right, #1a7a4a, #2ecc71, #f1c40f, #e67e22, #c0392b, #6b3a8c, #2d1b4e);
        margin-bottom: 5px;
      }
      .ctm-grad-labels {
        display: flex; justify-content: space-between;
        font-size: 0.6rem; color: #5a5470;
      }
      .ctm-divider { border: none; border-top: 1px solid #1e1c2a; margin: 4px 0; }
      .ctm-hint {
        font-size: 0.62rem; color: #7a7090; line-height: 1.6;
        background: #1a1828; border: 1px solid #252235;
        border-radius: 8px; padding: 9px 11px;
      }
      .ctm-prov-row {
        display: flex; justify-content: space-between; align-items: center;
        font-size: 0.64rem; color: #a09ab0;
        padding: 3px 0; border-bottom: 1px solid #1a1828;
      }
      .ctm-prov-score {
        font-family: 'Syne', sans-serif; font-weight: 700; font-size: 0.72rem;
      }
      .ctm-note {
        font-size: 0.55rem; color: #3a3450;
        line-height: 1.65; margin-top: auto;
        padding-top: 12px; border-top: 1px solid #1e1c2a;
      }
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
  ), # /tabPanel Map
  
  # ══════════════════════════════════════════════════════════════════
  #  TAB 3 — WVS SCATTERPLOT
  # ══════════════════════════════════════════════════════════════════
  tabPanel("WVS Scatterplot",
           div(class = "wvs-wrap",
               
               # ── Sidebar ──────────────────────────────────────────────────
               div(class = "wvs-sidebar",
                   
                   div(
                     div(class = "wvs-ctrl-label", "Y-Axis Variable"),
                     selectInput("y_var", label = NULL,
                                 choices  = wvs_y_choices,
                                 selected = "auth_index")
                   ),
                   
                   hr(class = "wvs-divider"),
                   
                   div(
                     div(class = "wvs-ctrl-label", "Countries"),
                     selectizeInput("wvs_countries", label = NULL,
                                    choices  = wvs_countries_list,
                                    selected = head(wvs_countries_list, 8),
                                    multiple = TRUE,
                                    options  = list(placeholder = "Select countries…"))
                   ),
                   
                   div(
                     style = "display:flex; gap:8px;",
                     actionButton("sel_all",  "All",    class = "btn-gold",
                                  style = "padding:7px 10px; font-size:.7rem; flex:1;"),
                     actionButton("sel_none", "None",   class = "btn-dark",
                                  style = "padding:7px 10px; font-size:.7rem; flex:1;"),
                     actionButton("sel_rand", "Sample", class = "btn-dark",
                                  style = "padding:7px 10px; font-size:.7rem; flex:1;")
                   ),
                   
                   hr(class = "wvs-divider"),
                   
                   div(
                     div(class = "wvs-ctrl-label", "Year Range"),
                     sliderInput("wvs_year_range", label = NULL,
                                 min   = 1980,
                                 max   = 2020,
                                 value = c(1980, 2020),
                                 step  = 1,
                                 sep   = "")
                   ),
                   
                   hr(class = "wvs-divider"),
                   
                   div(
                     div(class = "wvs-ctrl-label", "Options"),
                     checkboxInput("show_labels",   "Show country labels", value = TRUE),
                     checkboxInput("show_trend",    "Show trend line",     value = TRUE),
                     checkboxInput("color_country", "Color by country",    value = TRUE)
                   ),
                   
                   hr(class = "wvs-divider"),
                   
                   div(
                     div(class = "wvs-ctrl-label", "Summary"),
                     div(class = "wvs-stat-row",
                         div(class = "wvs-stat-pill",
                             span(class = "val", textOutput("wvs_n_obs",  inline = TRUE)),
                             span(class = "lbl", "Obs.")
                         ),
                         div(class = "wvs-stat-pill",
                             span(class = "val", textOutput("wvs_n_ctry", inline = TRUE)),
                             span(class = "lbl", "Countries")
                         ),
                         div(class = "wvs-stat-pill",
                             span(class = "val", textOutput("wvs_n_wave", inline = TRUE)),
                             span(class = "lbl", "Waves")
                         )
                     )
                   ),
                   
                   hr(class = "wvs-divider"),
                   
                   div(class = "wvs-index-note",
                       "Authoritarian Index: mean of E114, E116, E115, E018, A042, E225, E228,
           A124_06, A124_12, A124_43 (higher = more authoritarian attitudes). ",
                       tags$br(),
                       "Individualism (A007): importance of family (1 = very important … 6 = not at all). ",
                       tags$br(),
                       "Economic Inequality: Gini coefficient from GINI_DATA.csv (0 = perfect equality, 100 = maximum inequality)."
                   )
               ),
               
               # ── Plot panel ───────────────────────────────────────────────
               div(class = "wvs-plot-panel",
                   div(
                     p(class = "wvs-plot-title",    textOutput("wvs_plot_title",    inline = TRUE)),
                     p(class = "wvs-plot-subtitle", textOutput("wvs_plot_subtitle", inline = TRUE))
                   ),
                   div(class = "wvs-plot-wrap",
                       plotOutput("wvs_scatter", height = "100%", width = "100%")
                   )
               )
           )
  ) # /tabPanel WVS
  ,
  
  # ══════════════════════════════════════════════════════════════════
  #  TAB 4 — CANADA AUTHORITARIAN MAP
  # ══════════════════════════════════════════════════════════════════
  tabPanel("Canada Auth Map",
           div(class = "ctm-wrap",
               
               # ── Sidebar ─────────────────────────────────────────────────
               div(class = "ctm-sidebar",
                   
                   div(class = "ctm-section-title", "Index Gradient"),
                   div(class = "ctm-grad-bar"),
                   div(class = "ctm-grad-labels",
                       tags$span("Low"), tags$span("High")),
                   
                   tags$hr(class = "ctm-divider"),
                   
                   div(class = "ctm-hint",
                       tags$b(style = "color:#7ec8a0;", "Authoritarian Index", .noWS = "outside"),
                       tags$br(),
                       "Mean of HD3, V3, ATT10, HD4, V2. Higher = stronger authoritarian attitudes."
                   ),
                   
                   tags$hr(class = "ctm-divider"),
                   
                   div(class = "ctm-section-title", "Province Scores"),
                   tagList(prov_rows_ui),
                   
                   div(class = "ctm-note",
                       "Source: CTM_DATA.sav", tags$br(), tags$br(),
                       "Index variables:", tags$br(),
                       "HD3 · V3 · ATT10 · HD4 · V2", tags$br(), tags$br(),
                       "Provinces with multiple region codes are averaged."
                   )
               ),
               
               # ── Map ─────────────────────────────────────────────────────
               div(class = "ctm-map-wrap",
                   leafletOutput("canada_map", width = "100%", height = "100%")
               )
           )
  ) # /tabPanel Canada Auth Map
  
  
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Explicitly capture globals so they are always available inside the server,
  # even if Shiny is launched from a different working directory than the script.
  .vdem_all   <- vdem_all
  .world_base <- world_base
  
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
    vdem_yr <- .vdem_all |>
      filter(year == yr) |>
      group_by(country_name) |>
      summarise(v2x_polyarchy = round(mean(v2x_polyarchy, na.rm = TRUE), 3),
                .groups = "drop")
    .world_base |>
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
  
  # ══════════════════════════════════════════════════════════════════
  #  WVS SCATTERPLOT SERVER
  # ══════════════════════════════════════════════════════════════════
  
  # Capture WVS globals into server scope
  .wvs_data            <- wvs_data
  .wvs_years           <- wvs_years
  .wvs_ctry            <- wvs_countries_list
  .gini_data           <- gini_data
  .permanent_countries <- PERMANENT_COUNTRIES
  
  # ── Country quick-select buttons ──────────────────────────────────
  observeEvent(input$sel_all, {
    updateSelectizeInput(session, "wvs_countries", selected = .wvs_ctry)
  })
  observeEvent(input$sel_none, {
    updateSelectizeInput(session, "wvs_countries", selected = character(0))
  })
  observeEvent(input$sel_rand, {
    updateSelectizeInput(session, "wvs_countries",
                         selected = sample(.wvs_ctry, min(8, length(.wvs_ctry))))
  })
  
  # ── Filtered WVS data ─────────────────────────────────────────────
  wvs_filtered <- reactive({
    req(input$wvs_countries, input$wvs_year_range)
    selected_ctry <- union(input$wvs_countries, .permanent_countries)
    .wvs_data %>%
      filter(
        country %in% selected_ctry,
        year    >= input$wvs_year_range[1],
        year    <= input$wvs_year_range[2]
      )
  })
  
  # ── Aggregated (mean per country × year) ─────────────────────────
  wvs_agg <- reactive({
    req(input$y_var)
    y <- input$y_var
    selected_ctry <- union(input$wvs_countries, .permanent_countries)
    
    if (y == "gini") {
      # Gini comes from separate dataset
      .gini_data %>%
        filter(
          country %in% selected_ctry,
          year    >= input$wvs_year_range[1],
          year    <= input$wvs_year_range[2]
        ) %>%
        group_by(country, year) %>%
        summarise(y_val = mean(gini, na.rm = TRUE), .groups = "drop")
    } else {
      wvs_filtered() %>%
        group_by(country, year) %>%
        summarise(
          y_val = mean(.data[[y]], na.rm = TRUE),
          .groups = "drop"
        )
    }
  })
  
  # ── Y-axis label ──────────────────────────────────────────────────
  wvs_y_label <- reactive({
    switch(input$y_var,
           X003        = "Mean Age",
           gdp_per_cap = "GDP per Capita (USD)",
           auth_index  = "Authoritarian Index (1–4)",
           A007        = "Individualism — Family Importance (A007)",
           gini        = "Economic Inequality (Gini Coefficient)"
    )
  })
  
  # ── Summary stats ─────────────────────────────────────────────────
  output$wvs_n_obs  <- renderText({ nrow(wvs_agg()) })
  output$wvs_n_ctry <- renderText({ n_distinct(wvs_agg()$country) })
  output$wvs_n_wave <- renderText({ n_distinct(wvs_agg()$year) })
  
  # ── Dynamic title / subtitle ──────────────────────────────────────
  output$wvs_plot_title <- renderText({
    paste(wvs_y_label(), "vs. Survey Wave")
  })
  output$wvs_plot_subtitle <- renderText({
    ctry <- length(input$wvs_countries)
    paste0(ctry, " countr", ifelse(ctry == 1, "y", "ies"), " · ",
           input$wvs_year_range[1], "–", input$wvs_year_range[2],
           "  ·  World Values Survey")
  })
  
  # ── Scatterplot ───────────────────────────────────────────────────
  output$wvs_scatter <- renderPlot({
    d <- wvs_agg()
    validate(need(nrow(d) > 0, "No data for the selected filters."))
    
    p <- ggplot(d, aes(x = year, y = y_val)) +
      theme_minimal(base_size = 13) +
      theme(
        text             = element_text(family = "sans", color = "#f0ede6"),
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
        legend.title     = element_text(color = "#7a7d8a", size = 9, hjust = .5),
        plot.margin      = margin(20, 20, 16, 16)
      )
    
    # Trend line
    if (isTRUE(input$show_trend) && nrow(d) >= 4) {
      p <- p + geom_smooth(
        method = "loess", formula = y ~ x, se = TRUE,
        color = "#7ec8a0", fill = "#7ec8a044",
        linewidth = 1.2
      )
    }
    
    # Points + connecting lines
    # Canada is permanent — pin it to a fixed colour (#f1c40f, the map's yellow)
    # and assign the rest of the palette to other countries
    canada_colour <- "#f1c40f"
    other_countries <- setdiff(unique(d$country), .permanent_countries)
    n_other <- length(other_countries)
    overview_pal_base <- c(
      "#7ec8a0", "#2ecc71", "#1a7a4a", "#6b3a8c", "#9b59b6",
      "#2d1b4e", "#c0392b", "#e67e22", "#3498db",
      "#1abc9c", "#e91e63", "#ff5722", "#8bc34a", "#00bcd4",
      "#ff9800", "#9c27b0", "#4caf50", "#f44336", "#2196f3",
      "#cddc39", "#795548", "#607d8b", "#e040fb", "#00e5ff",
      "#76ff03", "#ff6d00", "#d500f9", "#00b0ff", "#69f0ae",
      "#ffd740", "#ff4081", "#40c4ff", "#b2ff59"
    )
    other_pal <- overview_pal_base[seq_len(n_other)]
    names(other_pal) <- other_countries
    full_pal <- c(setNames(canada_colour, "Canada"), other_pal)
    
    if (isTRUE(input$color_country)) {
      p <- p +
        geom_line(aes(group = country, color = country),
                  alpha = .3, linewidth = .7) +
        geom_point(aes(color = country,
                       size  = ifelse(country == "Canada", 5.5, 4),
                       shape = ifelse(country == "Canada", 18L, 16L)),
                   alpha = .9) +
        scale_color_manual(values = full_pal, name = "Country") +
        scale_size_identity() +
        scale_shape_identity()
    } else {
      p <- p +
        geom_line(aes(group = country), color = "#7ec8a0", alpha = .3, linewidth = .7) +
        geom_point(aes(size  = ifelse(country == "Canada", 5.5, 4),
                       shape = ifelse(country == "Canada", 18L, 16L)),
                   color = "#7ec8a0", alpha = .88) +
        scale_size_identity() +
        scale_shape_identity()
    }
    # Labels (latest year per country only)
    if (isTRUE(input$show_labels)) {
      d_label <- d %>%
        group_by(country) %>%
        filter(year == max(year)) %>%
        ungroup()
      
      d_label_canada <- d_label %>% filter(country == "Canada")
      d_label_others <- d_label %>% filter(country != "Canada")
      
      if (nrow(d_label_others) > 0) {
        p <- p + geom_text_repel(
          data          = d_label_others,
          aes(label     = country),
          color         = "#e8e4d9",
          size          = 3.2,
          segment.color = "#7ec8a055",
          segment.size  = .5,
          box.padding   = .4,
          point.padding = .3,
          max.overlaps  = 20
        )
      }
      if (nrow(d_label_canada) > 0) {
        p <- p + geom_text_repel(
          data          = d_label_canada,
          aes(label     = country),
          color         = "#f1c40f",
          fontface      = "bold",
          size          = 3.6,
          segment.color = "#f1c40f88",
          segment.size  = .6,
          box.padding   = .5,
          point.padding = .4,
          max.overlaps  = 20
        )
      }
    }
    
    # Y-axis formatting
    if (input$y_var == "gdp_per_cap") {
      p <- p + scale_y_continuous(
        labels = scales::dollar_format(prefix = "$", big.mark = ",", accuracy = 1)
      )
    } else if (input$y_var == "gini") {
      p <- p + scale_y_continuous(
        labels = scales::number_format(accuracy = 0.1),
        limits = c(0, 100)
      )
    } else {
      p <- p + scale_y_continuous(labels = scales::number_format(accuracy = .01))
    }
    
    p +
      scale_x_continuous(breaks = .wvs_years, labels = as.character(.wvs_years)) +
      labs(x = "Survey Wave (Year)", y = wvs_y_label())
    
  }, bg = "#13161f")
  
}

# ══════════════════════════════════════════════════════════════════
#  CANADA AUTHORITARIAN MAP SERVER
# ══════════════════════════════════════════════════════════════════
.canada_map_sf        <- canada_map_sf
.auth_pal             <- auth_pal
.canada_labels_html   <- canada_labels_html

output$canada_map <- renderLeaflet({
  label_list <- lapply(.canada_labels_html, HTML)
  
  leaflet(
    .canada_map_sf,
    options = leafletOptions(zoomControl = TRUE, minZoom = 3, maxZoom = 10)
  ) %>%
    addProviderTiles(
      "CartoDB.DarkMatter",
      options = tileOptions(opacity = 0.88)
    ) %>%
    setView(lng = -96, lat = 62, zoom = 4) %>%
    addPolygons(
      fillColor    = ~ifelse(is.na(mean_auth), "#2a2840", .auth_pal(mean_auth)),
      fillOpacity  = 0.82,
      color        = "#0e0e14",
      weight       = 1.0,
      smoothFactor = 1.8,
      highlightOptions = highlightOptions(
        weight       = 2.8,
        color        = "#f0ece0",
        fillOpacity  = 0.95,
        bringToFront = TRUE
      ),
      label        = label_list,
      labelOptions = labelOptions(
        style = list(
          "padding"          = "0",
          "border"           = "none",
          "background-color" = "transparent",
          "box-shadow"       = "none"
        ),
        direction = "auto",
        sticky    = TRUE
      )
    ) %>%
    addLegend(
      position  = "bottomright",
      pal       = .auth_pal,
      values    = ~mean_auth,
      title     = "<span style='font-family:DM Mono,monospace;font-size:11px;color:#a09ab0;'>Auth. Index</span>",
      opacity   = 0.85,
      labFormat = labelFormat(digits = 2)
    )
})


shinyApp(ui, server)