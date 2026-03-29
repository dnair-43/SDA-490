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
library(broom)


# ── Data ──────────────────────────────────────────────────────────────────────
# All data files should be in your working directory.
app_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
)

setwd("~/Uni/Year 5/semester 2/Data")

vdem <- read_csv("V-Dem-CD-v16.csv", show_col_types = FALSE)

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
#wvs_path <- "WVS_TimeSeries_1981_2020_R_v2_0.rdata"
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
gini_path <- "GINI_DATA.csv"

if (file.exists(gini_path)) {
  gini_raw <- read_csv(
    gini_path,
    show_col_types  = FALSE,
    skip_empty_rows = TRUE
  )
  
  # ── Detect format: wide (World Bank) vs. long ─────────────────────────────
  # World Bank exports: "Country Name", "Country Code", "Indicator Name",
  # "Indicator Code", then year columns (e.g. 1960, 1961, ..., 2025).
  is_wide <- any(grepl("country.name", names(gini_raw), ignore.case = TRUE)) ||
    any(grepl("^\\d{4}$", names(gini_raw)))
  
  if (is_wide) {
    country_col <- names(gini_raw)[1]
    year_cols   <- names(gini_raw)[grepl("^\\d{4}$", names(gini_raw))]
    
    gini_data <- gini_raw %>%
      select(country = all_of(country_col), all_of(year_cols)) %>%
      pivot_longer(
        cols      = all_of(year_cols),
        names_to  = "year",
        values_to = "gini"
      ) %>%
      mutate(
        year = as.integer(year),
        gini = suppressWarnings(as.numeric(gini))
      ) %>%
      filter(!is.na(gini))
  } else {
    names(gini_raw) <- tolower(names(gini_raw))
    gini_col  <- intersect(c("gini", "gini_index", "gini_coefficient"), names(gini_raw))[1]
    gini_data <- gini_raw %>%
      rename(gini = all_of(gini_col)) %>%
      select(country, year, gini) %>%
      mutate(year = as.integer(year), gini = as.numeric(gini))
  }
  
  # ── Harmonise World Bank country names → WVS country names ─────────────────
  wb_to_wvs <- c(
    "United States"                        = "United States",
    "United Kingdom"                       = "United Kingdom",
    "Russian Federation"                   = "Russia",
    "Korea, Rep."                          = "South Korea",
    "Korea, Dem. People\u2019s Rep."      = "North Korea",
    "Iran, Islamic Rep."                   = "Iran",
    "Egypt, Arab Rep."                     = "Egypt",
    "Venezuela, RB"                        = "Venezuela",
    "Syrian Arab Republic"                 = "Syria",
    "Yemen, Rep."                          = "Yemen",
    "Congo, Dem. Rep."                     = "Democratic Republic of the Congo",
    "Congo, Rep."                          = "Republic of the Congo",
    "Gambia, The"                          = "Gambia",
    "Lao PDR"                              = "Laos",
    "West Bank and Gaza"                   = "Palestine",
    "Micronesia, Fed. Sts."                = "Micronesia",
    "Slovak Republic"                      = "Slovakia",
    "Czechia"                              = "Czech Republic",
    "Kyrgyz Republic"                      = "Kyrgyzstan",
    "Turkiye"                              = "Turkey",
    "Cote d\u2019Ivoire"                  = "Ivory Coast",
    "Cabo Verde"                           = "Cape Verde",
    "Eswatini"                             = "Swaziland",
    "North Macedonia"                      = "North Macedonia",
    "Bosnia and Herzegovina"               = "Bosnia and Herzegovina"
  )
  gini_data <- gini_data %>%
    mutate(country = dplyr::recode(country, !!!wb_to_wvs))
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
.permanent_countries <- "Canada"
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
#makes changes according to the logistic regression formula 

# ── Election Data (Canada Votes Map) ─────────────────────────────────────────
election_path <- "table_tableau08.csv"

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
  "Conservative"      = "#1A4782",
  "Liberal"           = "#D71920",
  "NDP"               = "#F37021",
  "Bloc Québécois"    = "#033F8A",
  "Green"             = "#3D9B35",
  "PPC"               = "#4B286D",
  "Independent"       = "#888888",
  "No Affiliation"    = "#AAAAAA",
  "Animal Protection" = "#78C753",
  "Canadian Future"   = "#5B9BD5",
  "Centrist"          = "#808080",
  "Christian Heritage"= "#8B0000",
  "Communist"         = "#CC0000",
  "Libertarian"       = "#C8A400",
  "Marijuana"         = "#228B22",
  "Marxist-Leninist"  = "#AA0000",
  "Rhinocéros"        = "#FF69B4",
  "United"            = "#6B4C9A"
)

col_to_prov <- c(
  "N.L. Valid Votes/Votes valides T.-N.-L."   = "Newfoundland and Labrador",
  "P.E.I. Valid Votes/Votes valides Î.-P.-É." = "Prince Edward Island",
  "N.S. Valid Votes/Votes valides N.-É."       = "Nova Scotia",
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

if (file.exists(election_path)) {
  election_raw <- read_csv(
    election_path,
    locale         = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )
  
  votes_long <- election_raw |>
    dplyr::rename(party_full = 1) |>
    pivot_longer(-party_full, names_to = "col_name", values_to = "votes") |>
    mutate(
      province = col_to_prov[col_name],
      party    = party_short[party_full],
      votes    = suppressWarnings(as.numeric(votes))
    ) |>
    dplyr::filter(!is.na(province), !is.na(party), !is.na(votes))
  
  prov_top3 <- votes_long |>
    group_by(province) |>
    mutate(total = sum(votes, na.rm = TRUE)) |>
    ungroup() |>
    dplyr::filter(votes > 0, total > 0) |>
    mutate(pct = votes / total * 100) |>
    group_by(province) |>
    slice_max(pct, n = 3, with_ties = FALSE) |>
    mutate(rank = row_number()) |>
    ungroup()
  
  election_loaded <- TRUE
} else {
  prov_top3      <- data.frame()
  election_loaded <- FALSE
}

make_election_tooltip <- function(prov_name) {
  rows <- prov_top3 |> dplyr::filter(province == prov_name) |> arrange(rank)
  if (nrow(rows) == 0) {
    return(sprintf(
      "<div style='font-family:\"DM Mono\",monospace;background:#12111a;border:1px solid #2a2040;
       border-radius:10px;padding:12px 15px;color:#7a7090;font-size:12px;'>
       <b style='color:#f0ece0;'>%s</b><br><br>No data available.</div>",
      htmltools::htmlEscape(prov_name)
    ))
  }
  medals   <- c("\U0001F947", "\U0001F948", "\U0001F949")
  bar_html <- ""
  for (i in seq_len(nrow(rows))) {
    p   <- rows$party[i]
    pct <- rows$pct[i]
    col <- party_colours[p]; if (is.na(col)) col <- "#888888"
    bar_html <- paste0(bar_html, sprintf("
      <div style='margin-bottom:10px;'>
        <div style='display:flex;justify-content:space-between;align-items:center;margin-bottom:4px;'>
          <span style='font-size:12px;font-weight:700;color:%s;'>%s&nbsp;%s</span>
          <span style='font-size:13px;font-weight:800;color:%s;'>%.1f%%</span>
        </div>
        <div style='background:#1e1c2a;border-radius:4px;height:8px;width:100%%;overflow:hidden;'>
          <div style='background:%s;width:%.1f%%;height:8px;border-radius:4px;'></div>
        </div>
      </div>",
                                         col, medals[i], htmltools::htmlEscape(p), col, pct, col, min(pct, 100)
    ))
  }
  sprintf("
    <div style='font-family:\"DM Mono\",monospace;background:#12111a;border:1px solid #2a2040;
      border-radius:13px;padding:15px 17px;min-width:240px;max-width:270px;
      box-shadow:0 6px 30px rgba(0,0,0,0.6);'>
      <div style='font-family:\"Syne\",sans-serif;font-weight:800;font-size:15px;
        color:#f0ece0;margin-bottom:13px;padding-bottom:9px;border-bottom:1px solid #2a2040;'>%s</div>
      %s
      <div style='font-size:9px;color:#3a3450;margin-top:9px;text-align:right;
        letter-spacing:0.06em;text-transform:uppercase;'>Top 3 parties · vote share</div>
    </div>",
          htmltools::htmlEscape(prov_name), bar_html
  )
}

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

# ── Canada Regression Data ────────────────────────────────────────────────────
# Build province-level predictors for:
# Auth_i = β0 + β1·Pol + β2·Res + β3·ln(GNI) + β4·Gini + β5·Dig + β6·Party + ε
#
# All predictors are aggregated from individual CTM respondents to province level.
# Unit of analysis = Canadian province (n = up to 10).
#
# Pol  = SPREAD of authoritarian opinions within a province (sd of auth_index
#         across individual respondents). High sd = people disagree a lot = polarised.
#
# Res  = Mean of ATT10 (democratic resilience / civic attitudes).
#         ATT10 asks how important it is to live in a democracy.
#
# Dig  = Mean of V3 (digital/media freedom attitudes).
#         V3 captures attitudes toward free expression and information access.
#
# Party (Strength) = Winning party's vote share in that province from election data.
#         High value = one party dominates = strong party grip.
#
# ln(GNI) = We use a fixed Canada-wide GNI proxy (log scale) since we have no
#         province-level GNI in CTM. This term will have zero within-province
#         variance, so it is included but noted as a national-level control.
#         Users can toggle it off for a cleaner within-Canada model.
#
# Gini  = Canada has low Gini variation by province; we use a synthetic
#         province-level proxy scaled from HD4 (economic grievance attitudes).
#         HD4 captures perceptions of economic unfairness — a valid proxy for
#         felt inequality even when official Gini data isn't province-granular.

# Step 1: individual-level CTM data with province assigned
ctm_for_reg <- ctm_id %>%
  # bring in all CTM auth sub-vars for province-level aggregation
  select(all_of(c(".row_id", "auth_index",
                  intersect(c("HD3","V3","ATT10","HD4","V2"), names(ctm_id)),
                  region_cols))) %>%
  tidyr::pivot_longer(
    cols      = all_of(region_cols),
    names_to  = "reg_col",
    values_to = "reg_val"
  ) %>%
  filter(!is.na(reg_val)) %>%
  mutate(province = region_to_province[reg_col]) %>%
  filter(!is.na(province)) %>%
  distinct(.row_id, province, .keep_all = TRUE)

# Step 2: aggregate to province level
ctm_prov_reg <- ctm_for_reg %>%
  group_by(province) %>%
  summarise(
    # Outcome: mean authoritarian attitude score
    mean_auth   = mean(auth_index, na.rm = TRUE),
    # Pol: standard deviation of auth_index across respondents in the province
    # — how SPREAD OUT opinions are. High = polarised province.
    pol         = sd(auth_index,  na.rm = TRUE),
    # Res: mean of ATT10 (importance of democracy)
    res         = mean(ATT10, na.rm = TRUE),
    # Dig: mean of V3 (free expression / digital attitudes)
    dig         = mean(V3,    na.rm = TRUE),
    # Party (Strength proxy): mean of HD3 (institutional trust / party loyalty)
    # — will be replaced by election vote share below if election data is loaded
    party_ctm   = mean(HD3,   na.rm = TRUE),
    # Gini proxy: mean of HD4 (perceived economic unfairness)
    gini_proxy  = mean(HD4,   na.rm = TRUE),
    n_resp      = n(),
    .groups     = "drop"
  )

# Step 3: join winning-party vote share from election data as Party Strength
if (exists("prov_top3") && nrow(prov_top3) > 0) {
  winner_pct <- prov_top3 %>%
    filter(rank == 1) %>%
    select(province, party_strength = pct)
  ctm_prov_reg <- ctm_prov_reg %>%
    left_join(winner_pct, by = "province") %>%
    # fall back to CTM proxy if election data missing for a province
    mutate(party = coalesce(party_strength, party_ctm))
} else {
  ctm_prov_reg <- ctm_prov_reg %>%
    mutate(party = party_ctm)
}

# Step 4: add a national-level ln(GNI) placeholder (same for all provinces)
# Canada GNI per capita ~$52,000 USD → log(52000) ≈ 10.86
ctm_prov_reg <- ctm_prov_reg %>%
  mutate(ln_gni = log(52000))

# Province list for the regression UI selector
canada_prov_list <- sort(unique(ctm_prov_reg$province))

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
    tags$div(class = "prov-row",
             tags$span(province),
             tags$span(class = "prov-score", style = paste0("color:", col, ";"),
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

      /* ── Canada Votes tab toggle ── */
      .ctm-toggle-wrap {
        display: flex; gap: 6px; margin-bottom: 4px;
      }
      .ctm-toggle-btn {
        flex: 1; padding: 7px 4px;
        font-family: 'DM Mono', monospace; font-size: 0.62rem;
        letter-spacing: 0.08em; text-transform: uppercase;
        border: 1px solid #2a2040; border-radius: 5px;
        background: #1a1828; color: #7a7090; cursor: pointer;
        transition: background 0.18s, color 0.18s, border-color 0.18s;
      }
      .ctm-toggle-btn.active-auth {
        background: #1a4a2e; color: #7ec8a0; border-color: #2ecc71;
      }
      .ctm-toggle-btn.active-votes {
        background: #1a1e3a; color: #7aacf0; border-color: #3a6abf;
      }

      /* Party legend swatches */
      .votes-leg-item {
        display: flex; align-items: center; gap: 8px;
        font-size: 0.64rem; color: #a09ab0; padding: 2px 0;
      }
      .votes-leg-swatch {
        width: 10px; height: 10px; border-radius: 3px; flex-shrink: 0;
      }

      /* ══════════════════════════════════════════════
         REGRESSION TAB — Canada Model
      ══════════════════════════════════════════════ */
      .reg-wrap {
        display: flex; min-height: calc(100vh - 56px); background: #0f1117;
      }
      .reg-sidebar {
        width: 310px; min-width: 280px; background: #12111a;
        border-right: 1px solid #1e1c2a; padding: 24px 20px;
        display: flex; flex-direction: column; gap: 16px; overflow-y: auto;
      }
      .reg-main {
        flex: 1; padding: 28px 36px;
        display: flex; flex-direction: column; gap: 20px;
        overflow-y: auto; background: #0f1117;
      }
      .reg-ctrl-label {
        font-size: 0.6rem; letter-spacing: 0.18em; text-transform: uppercase;
        color: #5a5470; margin-bottom: 8px; font-family: 'DM Mono', monospace;
      }
      .reg-section-title {
        font-family: 'Syne', sans-serif; font-weight: 700; font-size: 1rem;
        color: #f0ece0; border-bottom: 1px solid #1e2130; padding-bottom: 8px;
        margin-bottom: 4px;
      }
      .reg-formula-box {
        background: #1a1828; border: 1px solid #252235; border-radius: 10px;
        padding: 16px 18px; font-family: 'DM Mono', monospace;
        font-size: 0.78rem; color: #c0b8d8; line-height: 2;
      }
      .eq-main  { color: #f0ece0; font-size: 0.85rem; font-weight: 500; }
      .eq-active { color: #7ec8a0; font-weight: 700; }
      .eq-muted  { color: #3a3450; text-decoration: line-through; }
      .reg-stat-grid {
        display: grid; grid-template-columns: repeat(auto-fit, minmax(110px,1fr)); gap: 10px;
      }
      .reg-stat-card {
        background: #1a1828; border: 1px solid #252235; border-radius: 10px; padding: 14px 16px;
      }
      .reg-stat-card .val {
        font-family: 'Syne', sans-serif; font-size: 1.25rem; font-weight: 700;
        color: #7ec8a0; display: block; line-height: 1;
      }
      .reg-stat-card .lbl {
        font-size: 0.58rem; color: #5a5470; text-transform: uppercase;
        letter-spacing: 0.1em; margin-top: 5px; font-family: 'DM Mono', monospace;
      }
      .reg-hint {
        font-size: 0.62rem; color: #4a4d5a; line-height: 1.65; padding: 10px 12px;
        background: #1a1828; border-left: 2px solid #7ec8a044;
        border-radius: 0 6px 6px 0; font-family: 'DM Mono', monospace;
      }
      .reg-plot-wrap {
        background: #13161f; border: 1px solid #1e2130; border-radius: 10px; overflow: hidden;
      }
      .reg-divider { border: none; border-top: 1px solid #1e2130; margin: 0; }
      /* Override Shiny's default white table background */
      #reg_coef_table table { width:100%; border-collapse:collapse;
        font-family:'DM Mono',monospace; font-size:0.75rem; background:#13161f !important; }
      #reg_coef_table th { color:#5a5470 !important; background:#13161f !important;
        font-size:0.6rem !important; letter-spacing:0.14em !important;
        text-transform:uppercase !important; padding:6px 12px; border-bottom:1px solid #2a2840; }
      #reg_coef_table td { color:#c0b8d8 !important; padding:10px 12px;
        border-bottom:1px solid #1e1c2a; background:transparent !important; }
      #reg_coef_table tr:hover td { background:#1a1828 !important; }
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
  #  TAB 4 — CANADA AUTHORITARIAN MAP + VOTES
  # ══════════════════════════════════════════════════════════════════
  tabPanel("Canada Auth Map",
           div(class = "ctm-wrap",
               
               # ── Sidebar ─────────────────────────────────────────────────
               div(class = "ctm-sidebar",
                   
                   # Toggle buttons
                   div(class = "ctm-section-title", "Map View"),
                   div(class = "ctm-toggle-wrap",
                       actionButton("ctm_view_auth",  "Auth Index", class = "ctm-toggle-btn active-auth"),
                       actionButton("ctm_view_votes", "Election",   class = "ctm-toggle-btn")
                   ),
                   
                   tags$hr(class = "ctm-divider"),
                   
                   # Sidebar content rendered server-side based on active mode
                   uiOutput("ctm_sidebar_content")
               ),
               
               # ── Map ─────────────────────────────────────────────────────
               div(class = "ctm-map-wrap",
                   leafletOutput("canada_map", width = "100%", height = "100%")
               )
           )
  ) # /tabPanel Canada Auth Map
  ,
  
  # ══════════════════════════════════════════════════════════════════
  #  TAB 5 — CANADA REGRESSION MODEL
  #  Auth_i = β0 + β1·Pol + β2·Res + β3·ln(GNI) + β4·Gini + β5·Dig + β6·Party + ε
  #  Unit of analysis: Canadian provinces (from CTM survey data)
  # ══════════════════════════════════════════════════════════════════
  tabPanel("Canada Regression",
           div(class = "reg-wrap",
               
               # ── Sidebar ────────────────────────────────────────────────────
               div(class = "reg-sidebar",
                   
                   # Outcome description
                   div(
                     div(class = "reg-ctrl-label", "Outcome (What we are predicting)"),
                     tags$div(
                       style = "font-size:0.76rem; color:#c0b8d8; font-family:'DM Mono',monospace;
                     background:#1a1828; border:1px solid #252235; border-radius:6px; padding:8px 10px;",
                       tags$b(style="color:#7ec8a0;", "Auth Index"),
                       " — Province-level mean authoritarian attitude score (from CTM survey data).
            Higher = respondents in that province hold stronger authoritarian views."
                     )
                   ),
                   
                   tags$hr(class = "reg-divider"),
                   
                   # Predictor toggles
                   div(
                     div(class = "reg-ctrl-label", "Predictors (Toggle On/Off)"),
                     div(class = "reg-hint",
                         tags$b(style="color:#7ec8a0;","Pol"), " — How divided opinions are within a province",    tags$br(),
                         tags$b(style="color:#7ec8a0;","Res"), " — How strongly people value democracy",            tags$br(),
                         tags$b(style="color:#7ec8a0;","ln(GNI)"), " — National wealth (log scale)",               tags$br(),
                         tags$b(style="color:#7ec8a0;","Gini"), " — Felt economic unfairness",                      tags$br(),
                         tags$b(style="color:#7ec8a0;","Dig"), " — Attitudes toward free expression",               tags$br(),
                         tags$b(style="color:#7ec8a0;","Party"), " — How dominant the winning party is (vote %)"
                     ),
                     tags$br(),
                     checkboxInput("reg_pol",   "Pol   — Political Polarisation",   value = TRUE),
                     checkboxInput("reg_res",   "Res   — Democratic Resilience",    value = TRUE),
                     checkboxInput("reg_lngni", "ln(GNI) — National Wealth",        value = FALSE),
                     checkboxInput("reg_gini",  "Gini  — Felt Economic Unfairness", value = TRUE),
                     checkboxInput("reg_dig",   "Dig   — Free Expression Attitudes",value = TRUE),
                     checkboxInput("reg_party", "Party — Party Strength",           value = TRUE)
                   ),
                   
                   tags$hr(class = "reg-divider"),
                   
                   # Province filter
                   div(
                     div(class = "reg-ctrl-label", "Include Provinces"),
                     selectizeInput("reg_provinces", label = NULL,
                                    choices  = sort(unique(ctm_prov_reg$province)),
                                    selected = sort(unique(ctm_prov_reg$province)),
                                    multiple = TRUE,
                                    options  = list(placeholder = "Select provinces…")),
                     div(style = "display:flex; gap:8px; margin-top:6px;",
                         actionButton("reg_sel_all",  "All",  class = "btn-gold",
                                      style = "flex:1; padding:6px 8px; font-size:.65rem;"),
                         actionButton("reg_sel_none", "None", class = "btn-dark",
                                      style = "flex:1; padding:6px 8px; font-size:.65rem;")
                     )
                   ),
                   
                   tags$hr(class = "reg-divider"),
                   
                   div(class = "reg-hint",
                       tags$b(style="color:#5a8a70;","Note on sample size: "),
                       "This model has up to 10 provinces. With few observations, results
           are exploratory — treat patterns as descriptive, not causal proof.",
                       tags$br(), tags$br(),
                       "ln(GNI) is a national-level constant (same for all provinces) and adds
           no within-Canada information — best left off unless testing a specific hypothesis."
                   )
                   
               ), # /reg-sidebar
               
               # ── Main panel ──────────────────────────────────────────────────
               div(class = "reg-main",
                   
                   div(class = "reg-section-title", "Model Specification"),
                   uiOutput("reg_formula_display"),
                   
                   div(class = "reg-stat-grid",
                       div(class="reg-stat-card", span(class="val", textOutput("reg_r2",     inline=TRUE)), span(class="lbl","R²")),
                       div(class="reg-stat-card", span(class="val", textOutput("reg_adj_r2", inline=TRUE)), span(class="lbl","Adj. R²")),
                       div(class="reg-stat-card", span(class="val", textOutput("reg_n",      inline=TRUE)), span(class="lbl","Provinces")),
                       div(class="reg-stat-card", span(class="val", textOutput("reg_f",      inline=TRUE)), span(class="lbl","F-statistic")),
                       div(class="reg-stat-card", span(class="val", textOutput("reg_rmse",   inline=TRUE)), span(class="lbl","RMSE"))
                   ),
                   
                   div(class="reg-section-title", style="margin-top:8px;", "Coefficient Estimates"),
                   tableOutput("reg_coef_table"),
                   div(class="reg-hint", style="margin-top:-8px;",
                       "*** p<0.001 · ** p<0.01 · * p<0.05 · † p<0.1  |  ",
                       "β > 0 means the predictor is associated with HIGHER authoritarian attitudes. ",
                       "β < 0 means LOWER authoritarian attitudes."
                   ),
                   
                   div(class="reg-section-title", style="margin-top:8px;", "Coefficient Plot (95% CI)"),
                   div(class="reg-plot-wrap", plotOutput("reg_coef_plot", height="320px")),
                   
                   div(class="reg-section-title", style="margin-top:8px;", "Fitted vs. Residuals"),
                   div(class="reg-plot-wrap", plotOutput("reg_resid_plot", height="260px"))
                   
               ) # /reg-main
           ) # /reg-wrap
  ) # /tabPanel Canada Regression
  
  
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
  sanitize.text.function = identity
  )
  
  # ══════════════════════════════════════════════════════════════════
  #  WVS SCATTERPLOT SERVER
  # ══════════════════════════════════════════════════════════════════
  
  # Capture WVS globals into server scope
  .wvs_data            <- wvs_data
  .wvs_years           <- wvs_years
  .wvs_ctry            <- wvs_countries_list
  .gini_data           <- gini_data
  .permanent_countries <- .permanent_countries
  
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
      scale_x_continuous(breaks = wvs_years, labels = as.character(wvs_years)) +
      labs(x = "Survey Wave (Year)", y = wvs_y_label())
    
  }, bg = "#13161f")
  
  # ══════════════════════════════════════════════════════════════════
  #  CANADA MAP — AUTH INDEX + ELECTION VOTES TOGGLE
  # ══════════════════════════════════════════════════════════════════
  
  ctm_mode <- reactiveVal("auth")
  
  observeEvent(input$ctm_view_auth, {
    ctm_mode("auth")
  })
  observeEvent(input$ctm_view_votes, {
    ctm_mode("votes")
  })
  
  # Render the correct sidebar panel based on active mode
  output$ctm_sidebar_content <- renderUI({
    if (ctm_mode() == "auth") {
      tagList(
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
      )
    } else {
      tagList(
        div(class = "ctm-section-title", "Leading Party"),
        div(class = "votes-leg-item", div(class="votes-leg-swatch", style="background:#1A4782"), "Conservative"),
        div(class = "votes-leg-item", div(class="votes-leg-swatch", style="background:#D71920"), "Liberal"),
        div(class = "votes-leg-item", div(class="votes-leg-swatch", style="background:#F37021"), "NDP"),
        div(class = "votes-leg-item", div(class="votes-leg-swatch", style="background:#033F8A"), "Bloc Québécois"),
        div(class = "votes-leg-item", div(class="votes-leg-swatch", style="background:#3D9B35"), "Green"),
        div(class = "votes-leg-item", div(class="votes-leg-swatch", style="background:#4B286D"), "PPC"),
        div(class = "votes-leg-item", div(class="votes-leg-swatch", style="background:#888888"), "Other"),
        tags$hr(class = "ctm-divider"),
        div(class = "ctm-hint",
            tags$b(style = "color:#7aacf0;", "Election Results", .noWS = "outside"),
            tags$br(),
            "Hover any province to see top 3 parties and their vote shares."
        ),
        div(class = "ctm-note",
            "Source: Elections Canada", tags$br(),
            "table_tableau08.csv", tags$br(), tags$br(),
            "Colours show the winning party per province.",
            tags$br(), tags$br(),
            "Percentages are share of all valid votes cast."
        )
      )
    }
  })
  
  # Build votes sf once
  votes_sf <- reactive({
    req(election_loaded)
    winner_df <- prov_top3 |>
      dplyr::filter(rank == 1) |>
      dplyr::select(province, winner = party, winner_pct = pct)
    
    canada_sf |>
      dplyr::left_join(winner_df, by = "province") |>
      dplyr::mutate(
        fill_col = party_colours[winner],
        fill_col = ifelse(is.na(fill_col), "#2a2840", fill_col),
        winner   = ifelse(is.na(winner), "No data", winner)
      )
  })
  
  # Base map — rendered once, updated via proxy
  output$canada_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE, minZoom = 3, maxZoom = 10)) %>%
      addProviderTiles("CartoDB.DarkMatter", options = tileOptions(opacity = 0.88)) %>%
      setView(lng = -96, lat = 62, zoom = 4)
  })
  
  observe({
    mode <- ctm_mode()
    
    if (mode == "auth") {
      label_list <- lapply(canada_labels_html, HTML)
      
      leafletProxy("canada_map") %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data         = canada_map_sf,
          fillColor    = ~ifelse(is.na(mean_auth), "#2a2840", auth_pal(mean_auth)),
          fillOpacity  = 0.82,
          color        = "#0e0e14",
          weight       = 1.0,
          smoothFactor = 1.8,
          highlightOptions = highlightOptions(
            weight = 2.8, color = "#f0ece0", fillOpacity = 0.95, bringToFront = TRUE
          ),
          label        = label_list,
          labelOptions = labelOptions(
            style = list("padding"="0","border"="none",
                         "background-color"="transparent","box-shadow"="none"),
            direction = "auto", sticky = TRUE
          )
        ) %>%
        addLegend(
          position  = "bottomright",
          pal       = auth_pal,
          values    = canada_map_sf$mean_auth,
          title     = "<span style='font-family:DM Mono,monospace;font-size:11px;color:#a09ab0;'>Auth. Index</span>",
          opacity   = 0.85,
          labFormat = labelFormat(digits = 2)
        )
      
    } else {
      req(election_loaded)
      vsf <- votes_sf()
      
      vote_labels <- lapply(vsf$province, function(p) {
        HTML(make_election_tooltip(p))
      })
      
      leafletProxy("canada_map") %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data         = vsf,
          fillColor    = ~fill_col,
          fillOpacity  = 0.76,
          color        = "#0e0e14",
          weight       = 1.0,
          smoothFactor = 1.8,
          highlightOptions = highlightOptions(
            weight = 2.8, color = "#f0ece0", fillOpacity = 0.93, bringToFront = TRUE
          ),
          label        = vote_labels,
          labelOptions = labelOptions(
            style = list("padding"="0","border"="none",
                         "background-color"="transparent","box-shadow"="none"),
            direction = "auto", sticky = TRUE
          )
        )
    }
  })
  
  # ══════════════════════════════════════════════════════════════════
  #  CANADA REGRESSION SERVER
  #  Auth_i = β0 + β1·Pol + β2·Res + β3·ln(GNI) + β4·Gini + β5·Dig + β6·Party + ε
  # ══════════════════════════════════════════════════════════════════
  
  observeEvent(input$reg_sel_all, {
    updateSelectizeInput(session, "reg_provinces",
                         selected = sort(unique(ctm_prov_reg$province)))
  })
  observeEvent(input$reg_sel_none, {
    updateSelectizeInput(session, "reg_provinces", selected = character(0))
  })
  
  # Which predictors are currently switched on
  reg_active_vars <- reactive({
    v <- character(0)
    if (isTRUE(input$reg_pol))   v <- c(v, "pol")
    if (isTRUE(input$reg_res))   v <- c(v, "res")
    if (isTRUE(input$reg_lngni)) v <- c(v, "ln_gni")
    if (isTRUE(input$reg_gini))  v <- c(v, "gini_proxy")
    if (isTRUE(input$reg_dig))   v <- c(v, "dig")
    if (isTRUE(input$reg_party)) v <- c(v, "party")
    req(length(v) > 0)
    v
  })
  
  # Filtered province dataset — drop rows with NA in any active predictor
  reg_data <- reactive({
    provs <- input$reg_provinces
    req(length(provs) >= 3)    # need at least 3 provinces to fit anything
    vars <- reg_active_vars()
    d <- ctm_prov_reg %>%
      filter(province %in% provs) %>%
      filter(if_all(all_of(c("mean_auth", vars)), ~ !is.na(.)))
    req(nrow(d) >= max(length(vars) + 1, 3))
    d
  })
  
  # Fitted OLS model
  reg_model <- reactive({
    d    <- reg_data()
    vars <- reg_active_vars()
    fmla <- as.formula(paste("mean_auth ~", paste(vars, collapse = " + ")))
    lm(fmla, data = d)
  })
  
  # Human-readable term names for table & plot
  reg_term_label <- function(term) {
    lookup <- c(
      "(Intercept)" = "Intercept",
      "pol"         = "Pol — Political Polarisation",
      "res"         = "Res — Democratic Resilience",
      "ln_gni"      = "ln(GNI) — National Wealth",
      "gini_proxy"  = "Gini — Felt Economic Unfairness",
      "dig"         = "Dig — Free Expression Attitudes",
      "party"       = "Party — Party Strength"
    )
    ifelse(term %in% names(lookup), lookup[term], term)
  }
  
  # Significance stars
  sig_stars <- function(p) {
    dplyr::case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.1   ~ "†",
      TRUE      ~ ""
    )
  }
  
  # Live formula display — active terms in green, inactive struck through
  output$reg_formula_display <- renderUI({
    vars    <- reg_active_vars()
    var_ids <- c("pol",   "res",   "ln_gni",   "gini_proxy", "dig",   "party")
    labels  <- c("Pol",  "Res",  "ln(GNI)", "Gini",      "Dig",  "Party")
    betas   <- c("β₁",  "β₂",  "β₃",      "β₄",        "β₅",  "β₆")
    coef_tags <- lapply(seq_along(var_ids), function(i) {
      if (var_ids[i] %in% vars)
        tags$span(class = "eq-active", paste0(" + ", betas[i], "\u00b7", labels[i]))
      else
        tags$span(class = "eq-muted",  paste0(" + ", betas[i], "\u00b7", labels[i]))
    })
    div(class = "reg-formula-box",
        div(class = "eq-main",
            tags$span("Auth"), tags$sub("i"),
            tags$span(" = \u03b2\u2080"),
            tagList(coef_tags),
            tags$span(" + \u03b5"), tags$sub("i")
        ),
        tags$br(),
        tags$span(style = "font-size:0.62rem; color:#5a5470;",
                  "Outcome: province-level mean authoritarian attitude index (CTM survey). ",
                  "i = Canadian province. Active predictors shown in ",
                  tags$span(style="color:#7ec8a0;","green"), ".")
    )
  })
  
  # Fit statistics
  output$reg_r2     <- renderText({
    tryCatch(sprintf("%.3f", summary(reg_model())$r.squared),     error = function(e) "—")
  })
  output$reg_adj_r2 <- renderText({
    tryCatch(sprintf("%.3f", summary(reg_model())$adj.r.squared), error = function(e) "—")
  })
  output$reg_n      <- renderText({ tryCatch(nrow(reg_data()),    error = function(e) "—") })
  output$reg_f      <- renderText({
    tryCatch({
      fs <- summary(reg_model())$fstatistic
      if (is.null(fs)) "—" else sprintf("%.2f", fs["value"])
    }, error = function(e) "—")
  })
  output$reg_rmse   <- renderText({
    tryCatch(sprintf("%.4f", sqrt(mean(residuals(reg_model())^2))), error = function(e) "—")
  })
  
  # Coefficient table
  output$reg_coef_table <- renderTable({
    broom::tidy(reg_model(), conf.int = TRUE) %>%
      mutate(
        Term        = reg_term_label(term),
        Estimate    = sprintf("%+.4f", estimate),
        `Std Error` = sprintf("%.4f",  std.error),
        `t-value`   = sprintf("%+.3f", statistic),
        `p-value`   = sprintf("%.4f",  p.value),
        `95% CI`    = sprintf("[%+.3f, %+.3f]", conf.low, conf.high),
        Sig         = sig_stars(p.value)
      ) %>%
      select(Term, Estimate, `Std Error`, `t-value`, `p-value`, `95% CI`, Sig)
  }, striped = FALSE, hover = TRUE, bordered = FALSE, width = "100%", rownames = FALSE)
  
  # Coefficient plot
  output$reg_coef_plot <- renderPlot({
    tryCatch({
      broom::tidy(reg_model(), conf.int = TRUE) %>%
        filter(term != "(Intercept)") %>%
        mutate(
          label = reg_term_label(term),
          col   = ifelse(estimate > 0, "#7ec8a0", "#e06c75"),
          sig   = p.value < 0.05
        ) %>%
        ggplot(aes(x = reorder(label, estimate), y = estimate)) +
        geom_hline(yintercept = 0, colour = "#2a2840", linewidth = 0.8) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0.18, colour = "#5a5470", linewidth = 0.7) +
        geom_point(aes(colour = col, size = sig), shape = 16) +
        scale_colour_identity() +
        scale_size_manual(values = c(`TRUE` = 4.2, `FALSE` = 2.4), guide = "none") +
        coord_flip() +
        labs(x = NULL, y = "Coefficient (β)",
             subtitle = "Larger point = p < 0.05  ·  green = positive association  ·  red = negative") +
        theme_minimal(base_family = "mono") +
        theme(
          plot.background    = element_rect(fill = "#13161f", colour = NA),
          panel.background   = element_rect(fill = "#13161f", colour = NA),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "#1e2130", linewidth = 0.4),
          panel.grid.minor   = element_blank(),
          axis.text          = element_text(colour = "#a09ab0", size = 9),
          axis.title.x       = element_text(colour = "#7a7090", size = 8.5, margin = margin(t=8)),
          plot.subtitle      = element_text(colour = "#4a4d5a", size = 8, margin = margin(b=10)),
          plot.margin        = margin(14, 20, 14, 14)
        )
    }, error = function(e) {
      ggplot() +
        annotate("text", x=0.5, y=0.5, label="Not enough data to plot.\nSelect more provinces or predictors.",
                 colour="#5a5470", size=4, family="mono", hjust=0.5) +
        theme_void() +
        theme(plot.background = element_rect(fill="#13161f", colour=NA))
    })
  }, bg = "#13161f")
  
  # Fitted vs Residuals plot
  output$reg_resid_plot <- renderPlot({
    tryCatch({
      m <- reg_model()
      data.frame(
        fitted    = fitted(m),
        residual  = residuals(m),
        province  = reg_data()$province
      ) %>%
        ggplot(aes(x = fitted, y = residual, label = province)) +
        geom_hline(yintercept = 0, colour = "#2a2840", linewidth = 0.8) +
        geom_point(colour = "#7ec8a0", size = 3, alpha = 0.8) +
        ggrepel::geom_text_repel(colour = "#a09ab0", size = 3, family = "mono",
                                 box.padding = 0.4, max.overlaps = 10) +
        labs(x = "Fitted values  (what the model predicted)",
             y = "Residual  (prediction error)",
             subtitle = "Points close to the zero line = good fit  ·  each point is a province") +
        theme_minimal(base_family = "mono") +
        theme(
          plot.background  = element_rect(fill = "#13161f", colour = NA),
          panel.background = element_rect(fill = "#13161f", colour = NA),
          panel.grid.major = element_line(colour = "#1e2130", linewidth = 0.4),
          panel.grid.minor = element_blank(),
          axis.text        = element_text(colour = "#a09ab0", size = 9),
          axis.title       = element_text(colour = "#7a7090", size = 8.5),
          plot.subtitle    = element_text(colour = "#4a4d5a", size = 8, margin = margin(b=10)),
          plot.margin      = margin(14, 20, 14, 14)
        )
    }, error = function(e) {
      ggplot() +
        annotate("text", x=0.5, y=0.5, label="Not enough data to plot.\nSelect more provinces.",
                 colour="#5a5470", size=4, family="mono", hjust=0.5) +
        theme_void() +
        theme(plot.background = element_rect(fill="#13161f", colour=NA))
    })
  }, bg = "#13161f")
  
  # ── End Canada Regression Server ──────────────────────────────────────────
  
}

shinyApp(ui, server)