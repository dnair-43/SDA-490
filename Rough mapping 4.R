library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(haven)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)
library(htmltools)
library(readr)
library(purrr)

# ══════════════════════════════════════════════════════════════════════════════
#  PATHS — place CTM_DATA.sav AND table_tableau08.csv in the same folder
# ══════════════════════════════════════════════════════════════════════════════
DATA_DIR <- if (exists("rstudioapi") && rstudioapi::isAvailable()) {
  dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  getwd()
}

# ══════════════════════════════════════════════════════════════════════════════
#  1. ELECTION DATA  (table_tableau08.csv)
# ══════════════════════════════════════════════════════════════════════════════
party_short <- c(
  "Animal Protection Party of Canada/Le Parti pour la Protection des Animaux du Canada" = "Animal Protection",
  "Bloc Québécois/Bloc Québécois"                                                        = "Bloc Québécois",
  "Canadian Future Party/Parti Avenir Canadien"                                          = "Canadian Future",
  "Centrist Party of Canada/Parti Centriste du Canada"                                   = "Centrist",
  "Christian Heritage Party of Canada/Parti de l'Héritage Chrétien du Canada"           = "Christian Heritage",
  "Communist Party of Canada/Parti communiste du Canada"                                 = "Communist",
  "Conservative Party of Canada/Parti conservateur du Canada"                            = "Conservative",
  "Green Party of Canada/Le Parti Vert du Canada"                                        = "Green",
  "Liberal Party of Canada/Parti libéral du Canada"                                      = "Liberal",
  "Libertarian Party of Canada/Parti Libertarien du Canada"                              = "Libertarian",
  "Marijuana Party/Parti Marijuana"                                                       = "Marijuana",
  "Marxist-Leninist Party of Canada/Parti Marxiste-Léniniste du Canada"                 = "Marxist-Leninist",
  "New Democratic Party/Nouveau Parti démocratique"                                       = "NDP",
  "Parti Rhinocéros Party/Parti Rhinocéros Party"                                        = "Rhinocéros",
  "People's Party of Canada/Parti populaire du Canada"                                   = "PPC",
  "United Party of Canada (UP)/Parti Uni du Canada (UP)"                                = "United",
  "Independent/Indépendant(e)"                                                            = "Independent",
  "No Affiliation/Aucune appartenance"                                                    = "No Affiliation"
)

party_colours <- c(
  "Conservative"       = "#1A4782",
  "Liberal"            = "#D71920",
  "NDP"                = "#F37021",
  "Bloc Québécois"     = "#033F8A",
  "Green"              = "#3D9B35",
  "PPC"                = "#4B286D",
  "Independent"        = "#888888",
  "No Affiliation"     = "#AAAAAA",
  "Animal Protection"  = "#78C753",
  "Canadian Future"    = "#5B9BD5",
  "Centrist"           = "#808080",
  "Christian Heritage" = "#8B0000",
  "Communist"          = "#CC0000",
  "Libertarian"        = "#C8A400",
  "Marijuana"          = "#228B22",
  "Marxist-Leninist"   = "#AA0000",
  "Rhinoceros"         = "#FF69B4",
  "United"             = "#6B4C9A"
)

col_to_prov <- c(
  "N.L. Valid Votes/Votes valides T.-N.-L."   = "Newfoundland and Labrador",
  "P.E.I. Valid Votes/Votes valides I.-P.-E." = "Prince Edward Island",
  "N.S. Valid Votes/Votes valides N.-E."       = "Nova Scotia",
  "N.B. Valid Votes/Votes valides N.-B."       = "New Brunswick",
  "Que. Valid Votes/Votes valides Qc"          = "Quebec",
  "Ont. Valid Votes/Votes valides Ont."        = "Ontario",
  "Man. Valid Votes/Votes valides Man."        = "Manitoba",
  "Sask. Valid Votes/Votes valides Sask."      = "Saskatchewan",
  "Alta. Valid Votes/Votes valides Alb."       = "Alberta",
  "B.C. Valid Votes/Votes valides C.-B."       = "British Columbia",
  "Y.T. Valid Votes/Votes valides Yn"          = "Yukon",
  "N.W.T. Valid Votes/Votes valides T.N.-O."  = "Northwest Territories",
  "Nun. Valid Votes/Votes valides Nt"          = "Nunavut"
)

elec_raw <- read_csv(
  file.path(DATA_DIR, "table_tableau08.csv"),
  name_repair = "minimal",
  show_col_types = FALSE
)
names(elec_raw)[1] <- "party_long"

# Strip BOM from first column name if present
names(elec_raw)[1] <- gsub("^\xef\xbb\xbf", "", names(elec_raw)[1])
names(elec_raw)[1] <- gsub("^\\xef\\xbb\\xbf", "", names(elec_raw)[1])

# Normalise province column names: strip accented chars for robust matching
# Build a mapping from actual CSV column names to province names
# by matching the canonical endings
actual_prov_cols <- names(elec_raw)[-1]

# Manual fuzzy match: strip accents/special chars for comparison
strip_accents <- function(x) iconv(x, to = "ASCII//TRANSLIT")

norm_actual  <- strip_accents(actual_prov_cols)
norm_desired <- strip_accents(names(col_to_prov))

col_map <- setNames(
  col_to_prov[match(norm_actual, norm_desired)],
  actual_prov_cols
)
col_map <- col_map[!is.na(col_map)]

elec_long <- elec_raw %>%
  mutate(party = party_short[party_long]) %>%
  filter(!is.na(party)) %>%
  select(-party_long) %>%
  pivot_longer(cols = -party, names_to = "prov_col", values_to = "votes") %>%
  mutate(
    province = col_map[prov_col],
    votes    = suppressWarnings(as.numeric(gsub(",", "", as.character(votes))))
  ) %>%
  filter(!is.na(province), !is.na(votes), votes > 0)

# Top-3 parties per province
top3 <- elec_long %>%
  group_by(province) %>%
  arrange(desc(votes), .by_group = TRUE) %>%
  slice_head(n = 3) %>%
  mutate(rank = row_number()) %>%
  ungroup()

prov_totals <- elec_long %>%
  group_by(province) %>%
  summarise(total_votes = sum(votes, na.rm = TRUE), .groups = "drop")

top3 <- top3 %>%
  left_join(prov_totals, by = "province") %>%
  mutate(pct = round(votes / total_votes * 100, 1))

winner <- top3 %>%
  filter(rank == 1) %>%
  select(province, winner = party, winner_pct = pct)

# ══════════════════════════════════════════════════════════════════════════════
#  2. AUTHORITARIAN INDEX  (CTM_DATA.sav)
# ══════════════════════════════════════════════════════════════════════════════
ctm_raw <- read_sav(file.path(DATA_DIR, "CTM_DATA.sav"))
names(ctm_raw) <- toupper(names(ctm_raw))

auth_vars <- c("HD3", "V3", "ATT10", "HD4", "V2")
missing_auth <- setdiff(auth_vars, names(ctm_raw))
if (length(missing_auth) > 0) {
  warning("Auth vars not found: ", paste(missing_auth, collapse = ", "))
  auth_vars <- intersect(auth_vars, names(ctm_raw))
}

ctm <- ctm_raw %>%
  mutate(across(all_of(auth_vars), as.numeric)) %>%
  rowwise() %>%
  mutate(auth_index = mean(c_across(all_of(auth_vars)), na.rm = TRUE)) %>%
  ungroup()

region_to_province <- c(
  "BC_REG"  = "British Columbia",
  "NS_REG"  = "Nova Scotia",
  "NL_REG"  = "Newfoundland and Labrador",
  "MB_REG"  = "Manitoba",
  "AB_REG5" = "Alberta", "AB_REG4" = "Alberta", "AB_REG3" = "Alberta",
  "ON_REG"  = "Ontario", "ON_REG4" = "Ontario", "ON_REG2" = "Ontario",
  "QC_REG2" = "Quebec",  "QC_REG4" = "Quebec",
  "SK_REG"  = "Saskatchewan", "SK_REG3" = "Saskatchewan",
  "NB_REG4" = "New Brunswick", "NB_REG"  = "New Brunswick"
)

region_cols <- intersect(names(region_to_province), names(ctm))

ctm_id <- ctm %>%
  mutate(.row_id = row_number()) %>%
  mutate(across(all_of(region_cols), haven::zap_labels))

ctm_long <- ctm_id %>%
  select(all_of(c(".row_id", "auth_index", region_cols))) %>%
  pivot_longer(cols = all_of(region_cols), names_to = "reg_col", values_to = "reg_val") %>%
  filter(!is.na(reg_val)) %>%
  mutate(province = region_to_province[reg_col]) %>%
  filter(!is.na(province), !is.na(auth_index))

prov_auth <- ctm_long %>%
  distinct(.row_id, province, .keep_all = TRUE) %>%
  group_by(province) %>%
  summarise(mean_auth = round(mean(auth_index, na.rm = TRUE), 3),
            n_resp    = n(), .groups = "drop")

# ══════════════════════════════════════════════════════════════════════════════
#  3. GEOGRAPHY
# ══════════════════════════════════════════════════════════════════════════════
canada_sf <- ne_download(
  scale = 50, type = "states", category = "cultural", returnclass = "sf"
) %>%
  filter(admin == "Canada") %>%
  select(name, geometry) %>%
  rename(province = name) %>%
  mutate(province = recode(province, "Québec" = "Quebec"))

canada_map_sf <- canada_sf %>%
  left_join(prov_auth, by = "province") %>%
  left_join(winner,    by = "province")

# ══════════════════════════════════════════════════════════════════════════════
#  4. PALETTES & TOOLTIP BUILDERS
# ══════════════════════════════════════════════════════════════════════════════
auth_range <- range(prov_auth$mean_auth, na.rm = TRUE)
auth_pal <- colorNumeric(
  palette  = c("#1a7a4a", "#2ecc71", "#f1c40f", "#e67e22", "#c0392b", "#6b3a8c", "#2d1b4e"),
  domain   = auth_range,
  na.color = "#2a2840"
)

elec_fill_vec <- sapply(canada_map_sf$province, function(prov) {
  w <- winner$winner[winner$province == prov]
  if (length(w) == 0 || is.na(w)) return("#2a2840")
  col <- party_colours[w]
  if (is.na(col)) "#555555" else col
})

# Auth tooltip
make_auth_tooltip <- function(prov_name, mean_val, n) {
  if (is.na(mean_val)) {
    return(sprintf(
      "<div style='font-family:\"DM Mono\",monospace;background:#12111a;border:1px solid #2a2040;border-radius:10px;padding:12px 15px;color:#7a7090;font-size:12px;'><b style='color:#f0ece0;'>%s</b><br><br>No data available.</div>",
      htmlEscape(prov_name)
    ))
  }
  bar_pct   <- (mean_val - auth_range[1]) / max(diff(auth_range), 0.001) * 100
  bar_col   <- auth_pal(mean_val)
  auth_label <- dplyr::case_when(
    mean_val >= 4.0 ~ "Very High", mean_val >= 3.0 ~ "High",
    mean_val >= 2.0 ~ "Moderate",  TRUE             ~ "Low"
  )
  sprintf("
    <div style='font-family:\"DM Mono\",monospace;background:#12111a;border:1px solid #2a2040;border-radius:13px;padding:15px 17px;min-width:230px;max-width:260px;box-shadow:0 6px 30px rgba(0,0,0,0.6);'>
      <div style='font-family:\"Syne\",sans-serif;font-weight:800;font-size:15px;color:#f0ece0;margin-bottom:12px;padding-bottom:9px;border-bottom:1px solid #2a2040;'>%s</div>
      <div style='margin-bottom:10px;'>
        <div style='display:flex;justify-content:space-between;margin-bottom:5px;'>
          <span style='font-size:11px;color:#7a7090;text-transform:uppercase;letter-spacing:0.1em;'>Authoritarian Index</span>
          <span style='font-size:14px;font-weight:800;color:%s;'>%.3f</span>
        </div>
        <div style='background:#1e1c2a;border-radius:4px;height:9px;width:100%%;overflow:hidden;'>
          <div style='background:%s;width:%.1f%%;height:9px;border-radius:4px;'></div>
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
      <div style='font-size:9px;color:#3a3450;margin-top:10px;text-align:right;letter-spacing:0.06em;text-transform:uppercase;'>Mean of HD3 · V3 · ATT10 · HD4 · V2</div>
    </div>",
          htmlEscape(prov_name), bar_col, mean_val,
          bar_col, min(bar_pct, 100), bar_col, auth_label, n
  )
}

# Election tooltip
make_elec_tooltip <- function(prov_name) {
  rows <- top3 %>% filter(province == prov_name) %>% arrange(rank)
  if (nrow(rows) == 0) {
    return(sprintf(
      "<div style='font-family:\"DM Mono\",monospace;background:#12111a;border:1px solid #2a2040;border-radius:10px;padding:12px 15px;color:#7a7090;font-size:12px;'><b style='color:#f0ece0;'>%s</b><br><br>No election data.</div>",
      htmlEscape(prov_name)
    ))
  }
  medals <- c("1st", "2nd", "3rd")
  medal_cols <- c("#FFD700", "#C0C0C0", "#CD7F32")
  bars_html <- paste(sapply(seq_len(nrow(rows)), function(i) {
    r   <- rows[i, ]
    col <- party_colours[r$party]
    if (is.na(col)) col <- "#888888"
    mc  <- if (i <= 3) medal_cols[i] else "#888888"
    ml  <- if (i <= 3) medals[i]     else paste0("#", i)
    sprintf("
      <div style='margin-bottom:10px;'>
        <div style='display:flex;justify-content:space-between;align-items:center;margin-bottom:4px;'>
          <div style='display:flex;align-items:center;gap:6px;'>
            <span style='font-size:10px;font-weight:800;color:%s;background:#1e1c2a;padding:1px 5px;border-radius:3px;'>%s</span>
            <b style='color:%s;font-size:12px;'>%s</b>
          </div>
          <span style='font-size:14px;font-weight:800;color:%s;'>%s%%</span>
        </div>
        <div style='background:#1e1c2a;border-radius:4px;height:8px;width:100%%;overflow:hidden;'>
          <div style='background:%s;width:%s%%;height:8px;border-radius:4px;'></div>
        </div>
        <div style='font-size:10px;color:#5a5470;margin-top:2px;text-align:right;'>%s votes</div>
      </div>",
            mc, ml, col, htmlEscape(r$party), col, r$pct,
            col, min(r$pct, 100),
            format(r$votes, big.mark = ",")
    )
  }), collapse = "")
  
  sprintf("
    <div style='font-family:\"DM Mono\",monospace;background:#12111a;border:1px solid #2a2040;border-radius:13px;padding:15px 17px;min-width:255px;max-width:285px;box-shadow:0 6px 30px rgba(0,0,0,0.6);'>
      <div style='font-family:\"Syne\",sans-serif;font-weight:800;font-size:15px;color:#f0ece0;margin-bottom:4px;padding-bottom:9px;border-bottom:1px solid #2a2040;'>%s</div>
      <div style='font-size:10px;color:#5a5470;margin-bottom:12px;text-transform:uppercase;letter-spacing:0.1em;'>Top 3 by Valid Votes</div>
      %s
      <div style='font-size:9px;color:#3a3450;margin-top:8px;text-align:right;letter-spacing:0.06em;text-transform:uppercase;'>Total valid votes: %s</div>
    </div>",
          htmlEscape(prov_name), bars_html,
          format(rows$total_votes[1], big.mark = ",")
  )
}

auth_labels_html <- mapply(
  make_auth_tooltip,
  prov_name = canada_map_sf$province,
  mean_val  = canada_map_sf$mean_auth,
  n         = ifelse(is.na(canada_map_sf$n_resp), 0L, canada_map_sf$n_resp),
  SIMPLIFY  = FALSE
)

elec_labels_html <- lapply(canada_map_sf$province, make_elec_tooltip)

# ══════════════════════════════════════════════════════════════════════════════
#  5. CSS
# ══════════════════════════════════════════════════════════════════════════════
app_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Syne:wght@400;700;800&family=DM+Mono:wght@300;400&display=swap');
  * { box-sizing: border-box; margin: 0; padding: 0; }
  html, body { height: 100%; overflow: hidden; }
  body { background: #0e0e14; color: #e8e4d9; font-family: 'DM Mono', monospace; }

  .app-header {
    background: linear-gradient(135deg, #0e0e14 0%, #1a1030 50%, #0e1a14 100%);
    border-bottom: 1px solid #2a2040;
    padding: 0 30px;
    display: flex; align-items: center; justify-content: space-between;
    height: 62px;
  }
  .app-title {
    font-family: 'Syne', sans-serif; font-weight: 800;
    font-size: 1.3rem; letter-spacing: -0.025em;
    color: #f0ece0; line-height: 1.1;
  }
  .app-title .accent-green { color: #7ec8a0; }
  .app-title .accent-red   { color: #e07070; }

  .tab-switch { display: flex; gap: 6px; }
  .tab-btn {
    background: #1a1828; border: 1px solid #2a2040;
    color: #7a7090; font-family: 'DM Mono', monospace;
    font-size: 0.65rem; letter-spacing: 0.12em; text-transform: uppercase;
    padding: 7px 14px; border-radius: 20px; cursor: pointer; transition: all 0.2s;
  }
  .tab-btn:hover { background: #252235; color: #c0b8d0; }
  .tab-btn.active-auth { background: #2d1b4e; border-color: #6b3a8c; color: #e8e4d9; }
  .tab-btn.active-elec { background: #1a0a0a; border-color: #D71920; color: #e8e4d9; }

  .page-wrap { display: flex; height: calc(100vh - 62px); }

  .legend-panel {
    width: 240px; min-width: 210px;
    background: #12111a; border-right: 1px solid #1e1c2a;
    padding: 18px 15px;
    display: flex; flex-direction: column; gap: 10px;
    overflow-y: auto;
  }
  .leg-section-title {
    font-size: 0.57rem; letter-spacing: 0.18em;
    text-transform: uppercase; color: #5a5470;
    margin-bottom: 4px; margin-top: 6px;
  }
  .grad-bar {
    height: 14px; border-radius: 7px;
    background: linear-gradient(to right, #1a7a4a, #2ecc71, #f1c40f, #e67e22, #c0392b, #6b3a8c, #2d1b4e);
    margin-bottom: 5px;
  }
  .grad-labels { display: flex; justify-content: space-between; font-size: 0.6rem; color: #5a5470; }
  .leg-divider { border: none; border-top: 1px solid #1e1c2a; margin: 4px 0; }
  .leg-hint {
    font-size: 0.62rem; color: #7a7090; line-height: 1.6;
    background: #1a1828; border: 1px solid #252235;
    border-radius: 8px; padding: 9px 11px;
  }
  .prov-row {
    display: flex; justify-content: space-between; align-items: center;
    font-size: 0.64rem; color: #a09ab0;
    padding: 3px 0; border-bottom: 1px solid #1a1828;
  }
  .prov-score { font-family: 'Syne', sans-serif; font-weight: 700; font-size: 0.72rem; }
  .leg-note {
    font-size: 0.55rem; color: #3a3450; line-height: 1.65;
    margin-top: auto; padding-top: 12px; border-top: 1px solid #1e1c2a;
  }
  .party-row {
    display: flex; align-items: center; gap: 8px;
    font-size: 0.63rem; color: #a09ab0;
    padding: 3px 0; border-bottom: 1px solid #1a1828;
  }
  .party-swatch { width: 12px; height: 12px; border-radius: 3px; flex-shrink: 0; }
  .party-pct { font-family: 'Syne',sans-serif; font-weight:700; font-size:0.7rem; margin-left:auto; }

  .map-wrap { flex: 1; position: relative; overflow: hidden; }
  #canada_map { width: 100%; height: 100%; }
  .leaflet-tooltip {
    background: transparent !important; border: none !important;
    box-shadow: none !important; padding: 0 !important;
  }
"

# ══════════════════════════════════════════════════════════════════════════════
#  6. UI HELPERS
# ══════════════════════════════════════════════════════════════════════════════
auth_rows_ui <- prov_auth %>%
  arrange(desc(mean_auth)) %>%
  pmap(function(province, mean_auth, n_resp) {
    col <- auth_pal(mean_auth)
    div(class = "prov-row",
        span(province),
        span(class = "prov-score", style = paste0("color:", col, ";"),
             sprintf("%.3f", mean_auth)))
  })

natl_top <- elec_long %>%
  group_by(party) %>%
  summarise(total = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice_head(n = 8)
natl_total_votes <- sum(natl_top$total)

elec_legend_ui <- natl_top %>%
  pmap(function(party, total) {
    col <- party_colours[party]
    if (is.na(col)) col <- "#888888"
    pct <- round(total / natl_total_votes * 100, 1)
    div(class = "party-row",
        div(class = "party-swatch", style = paste0("background:", col, ";")),
        span(party),
        span(class = "party-pct", style = paste0("color:", col, ";"), paste0(pct, "%"))
    )
  })

# ══════════════════════════════════════════════════════════════════════════════
#  7. UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  
  div(class = "app-header",
      div(class = "app-title",
          "CANADA ",
          tags$span(class = "accent-green", "AUTH"),
          " & ",
          tags$span(class = "accent-red", "ELECTION"),
          " MAP"
      ),
      div(class = "tab-switch",
          tags$button("Authoritarian Index", id = "btn_auth",
                      class = "tab-btn active-auth",
                      onclick = "Shiny.setInputValue('active_tab','auth',{priority:'event'});
                                 document.getElementById('btn_auth').className='tab-btn active-auth';
                                 document.getElementById('btn_elec').className='tab-btn';"),
          tags$button("Election Results", id = "btn_elec",
                      class = "tab-btn",
                      onclick = "Shiny.setInputValue('active_tab','elec',{priority:'event'});
                                 document.getElementById('btn_elec').className='tab-btn active-elec';
                                 document.getElementById('btn_auth').className='tab-btn';")
      )
  ),
  
  div(class = "page-wrap",
      uiOutput("sidebar_ui"),
      div(class = "map-wrap",
          leafletOutput("canada_map", width = "100%", height = "100%")
      )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
#  8. SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  cur_tab <- reactive({
    if (is.null(input$active_tab)) "auth" else input$active_tab
  })
  
  output$sidebar_ui <- renderUI({
    if (cur_tab() == "auth") {
      div(class = "legend-panel",
          div(class = "leg-section-title", "Index Gradient"),
          div(class = "grad-bar"),
          div(class = "grad-labels", span("Low"), span("High")),
          tags$hr(class = "leg-divider"),
          div(class = "leg-hint",
              tags$b(style = "color:#7ec8a0;", "Authoritarian Index"),
              tags$br(),
              "Mean of HD3, V3, ATT10, HD4, V2. Higher = stronger authoritarian attitudes."
          ),
          tags$hr(class = "leg-divider"),
          div(class = "leg-section-title", "Province Scores"),
          tagList(auth_rows_ui),
          div(class = "leg-note",
              "Source: CTM_DATA.sav", tags$br(), tags$br(),
              "Variables: HD3 · V3 · ATT10 · HD4 · V2"
          )
      )
    } else {
      div(class = "legend-panel",
          div(class = "leg-hint",
              tags$b(style = "color:#e07070;", "Election Results"),
              tags$br(),
              "Provinces coloured by winning party. Hover to see top 3 parties & vote shares."
          ),
          tags$hr(class = "leg-divider"),
          div(class = "leg-section-title", "National Vote Share (Top 8)"),
          tagList(elec_legend_ui),
          tags$hr(class = "leg-divider"),
          div(class = "leg-note",
              "Source: table_tableau08.csv", tags$br(), tags$br(),
              "Fill = 1st-place party colour.", tags$br(),
              "Hover for ranked breakdown."
          )
      )
    }
  })
  
  # Base map (rendered once)
  output$canada_map <- renderLeaflet({
    leaflet(
      canada_map_sf,
      options = leafletOptions(zoomControl = TRUE, minZoom = 3, maxZoom = 10)
    ) %>%
      addProviderTiles("CartoDB.DarkMatter", options = tileOptions(opacity = 0.88)) %>%
      setView(lng = -96, lat = 62, zoom = 4)
  })
  
  # Swap polygons when tab changes
  observe({
    t <- cur_tab()
    proxy <- leafletProxy("canada_map", data = canada_map_sf)
    
    if (t == "auth") {
      fill_vec   <- ifelse(is.na(canada_map_sf$mean_auth), "#2a2840", auth_pal(canada_map_sf$mean_auth))
      label_list <- lapply(auth_labels_html, HTML)
      
      proxy %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(
          fillColor = fill_vec, fillOpacity = 0.82,
          color = "#0e0e14", weight = 1.0, smoothFactor = 1.8,
          highlightOptions = highlightOptions(weight = 2.8, color = "#f0ece0", fillOpacity = 0.95, bringToFront = TRUE),
          label = label_list,
          labelOptions = labelOptions(
            style = list("padding" = "0", "border" = "none",
                         "background-color" = "transparent", "box-shadow" = "none"),
            direction = "auto", sticky = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomright", pal = auth_pal, values = ~mean_auth,
          title = "<span style='font-family:DM Mono,monospace;font-size:11px;color:#a09ab0;'>Auth. Index</span>",
          opacity = 0.85, labFormat = labelFormat(digits = 2)
        )
      
    } else {
      label_list <- lapply(elec_labels_html, HTML)
      
      proxy %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(
          fillColor = elec_fill_vec, fillOpacity = 0.78,
          color = "#0e0e14", weight = 1.0, smoothFactor = 1.8,
          highlightOptions = highlightOptions(weight = 2.8, color = "#f0ece0", fillOpacity = 0.95, bringToFront = TRUE),
          label = label_list,
          labelOptions = labelOptions(
            style = list("padding" = "0", "border" = "none",
                         "background-color" = "transparent", "box-shadow" = "none"),
            direction = "auto", sticky = TRUE
          )
        )
    }
  })
}

shinyApp(ui, server)