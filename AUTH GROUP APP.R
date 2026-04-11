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
library(WDI)
library(countrycode)
library(zoo)


# ── Data ──────────────────────────────────────────────────────────────────────
# All data files should be in your working directory.
app_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
)
# Make app_dir available as a static file path so images placed alongside
# the app file (not in www/) can still be served by Shiny.
addResourcePath("appimgs", app_dir)



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
wvs_path <- "WVS.csv"

# Also check app_dir in case working directory differs
if (!file.exists(wvs_path)) {
  wvs_path <- file.path(app_dir, "WVS.csv")
}

if (file.exists(wvs_path)) {
  wvs_data <- read_csv(wvs_path, show_col_types = FALSE)
}

if (!exists("wvs_data") || !is.data.frame(wvs_data)) {
  # Synthetic demo data so the app runs without the real file
  set.seed(42)
  wvs_countries <- c(
    "Belarus", "India", "Hungary", "United States", "Russia",
    "Venezuela", "Burkina Faso", "Nicaragua", "Mali", "Niger",
    "Tunisia", "Turkey", "Canada"
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
      A007        = sample(1:6, n(), replace = TRUE)
    )
} else {
  # Real WVS CSV loaded — normalise column names if needed.
  # If it uses S003 (numeric country code) and S020 (year), convert them.
  if ("S003" %in% names(wvs_data) && !"country" %in% names(wvs_data)) {
    wvs_iso_map <- c(
      "112" = "Belarus",
      "356" = "India",
      "348" = "Hungary",
      "840" = "United States",
      "643" = "Russia",
      "862" = "Venezuela",
      "854" = "Burkina Faso",
      "558" = "Nicaragua",
      "466" = "Mali",
      "562" = "Niger",
      "788" = "Tunisia",
      "792" = "Turkey",
      "124" = "Canada"
    )
    wvs_data <- wvs_data %>%
      mutate(
        country = wvs_iso_map[as.character(as.integer(S003))],
        year    = as.integer(S020)
      ) %>%
      filter(!is.na(country))
  } else if ("country_name" %in% names(wvs_data) && !"country" %in% names(wvs_data)) {
    wvs_data <- wvs_data %>% rename(country = country_name)
  }
  # Ensure year column is integer
  if ("year" %in% names(wvs_data)) {
    wvs_data <- wvs_data %>% mutate(year = as.integer(year))
  }
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
    "Belarus", "India", "Hungary", "United States", "Russia",
    "Venezuela", "Burkina Faso", "Nicaragua", "Mali", "Niger",
    "Tunisia", "Turkey", "Canada"
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
  "Belarus", "India", "Hungary", "United States", "Russia",
  "Venezuela", "Burkina Faso", "Nicaragua", "Mali", "Niger",
  "Tunisia", "Turkey", "Canada"
)
.permanent_countries <- "Canada"
# Filter wvs_data to only include countries in the list (if real data loaded)
wvs_data <- wvs_data %>%
  filter(country %in% wvs_countries_list)
wvs_years          <- sort(unique(wvs_data$year))

wvs_y_choices <- c(
  "Authoritarian Values Index"        = "auth_index",
  "Digital Freedom (v2mecenefm)"      = "digital_proxy",
  "Party Strength (v2xps_party)"      = "party_strength",
  "Wealth — ln(GNI per capita)"       = "logged_wealth",
  "Inequality (Gini Coefficient)"     = "gini",
  "Social Polarization (v2cacamps)"   = "polarization",
  "Legislature Resource Funding (v2lgfunds)" = "resource_rents"
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
  "PPC"               = "#1e40af",
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
  "United"            = "#1d4ed8"
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
      "<div style='font-family:\"DM Mono\",monospace;background:#f7f9fc;border:1px solid #d0dae8;
       border-radius:10px;padding:12px 15px;color:#6b7280;font-size:14px;'>
       <b style='color:#0f172a;'>%s</b><br><br>No data available.</div>",
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
          <span style='font-size:14px;font-weight:700;color:%s;'>%s&nbsp;%s</span>
          <span style='font-size:15px;font-weight:800;color:%s;'>%.1f%%</span>
        </div>
        <div style='background:#d0dae8;border-radius:4px;height:8px;width:100%%;overflow:hidden;'>
          <div style='background:%s;width:%.1f%%;height:8px;border-radius:4px;'></div>
        </div>
      </div>",
                                         col, medals[i], htmltools::htmlEscape(p), col, pct, col, min(pct, 100)
    ))
  }
  sprintf("
    <div style='font-family:\"DM Mono\",monospace;background:#f7f9fc;border:1px solid #d0dae8;
      border-radius:13px;padding:15px 17px;min-width:240px;max-width:270px;
      box-shadow:0 6px 30px rgba(0,0,0,0.6);'>
      <div style='font-family:\"Syne\",sans-serif;font-weight:800;font-size:15px;
        color:#0f172a;margin-bottom:13px;padding-bottom:9px;border-bottom:1px solid #d0dae8;'>%s</div>
      %s
      <div style='font-size:14px;color:#6b7280;margin-top:9px;text-align:right;
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

# ── CTM Canada Auth Index value (for use as Canada's 2025 WVS auth point) ──
canada_ctm_auth_2025 <- prov_auth %>%
  summarise(auth_index = mean(mean_auth, na.rm = TRUE)) %>%
  pull(auth_index)

# Ensure it's a single scalar; fall back to NA if not available
if (length(canada_ctm_auth_2025) != 1 || is.nan(canada_ctm_auth_2025)) {
  canada_ctm_auth_2025 <- NA_real_
}

# Province list for the regression UI selector
canada_prov_list <- sort(unique(ctm_prov_reg$province))

# ── Predictor Scatterplot data ────────────────────────────────────────────────
# Built here (after canada_ctm_auth_2025 is available) so the Canada 2025
# auth point from CTM can be appended correctly.
#
# Pulls V-Dem predictors for all WVS countries + merges WVS auth_index and Gini.
# Falls back to synthetic data if the vdem CSV wasn't loaded as a full dataframe.

if (exists("vdem") && is.data.frame(vdem)) {
  scatter_vdem <- vdem %>%
    filter(year >= 1980, year <= 2025,
           country_name %in% wvs_countries_list) %>%
    select(any_of(c("country_name", "year",
                    "v2cacamps", "v2xps_party", "v2mecenefm",
                    "v2lgfunds"))) %>%
    rename(
      country        = country_name,
      polarization   = any_of("v2cacamps"),
      party_strength = any_of("v2xps_party"),
      digital_proxy  = any_of("v2mecenefm"),
      resource_rents = any_of("v2lgfunds")
    )
} else {
  # Fallback: synthetic data so the scatterplot tab still opens
  set.seed(77)
  scatter_vdem <- expand.grid(country = wvs_countries_list,
                              year    = seq(1980, 2020, by = 5)) %>%
    mutate(
      polarization   = runif(n(), -3, 3),
      party_strength = runif(n(),  0, 1),
      digital_proxy  = runif(n(), -3, 3),
      resource_rents = runif(n(), -3, 3)
    )
}

# Aggregate WVS auth_index to country × year
wvs_auth_agg <- wvs_data %>%
  group_by(country, year) %>%
  summarise(auth_index = mean(auth_index, na.rm = TRUE), .groups = "drop")

# ── logged_wealth: fetch GNI per capita (PPP) directly from WDI ──────────────
# Indicator NY.GNP.PCAP.PP.CD = GNI per capita, PPP (current international $)
# We need iso2c codes for the WVS countries list.
wdi_iso2_map <- c(
  "Belarus"       = "BY", "India"         = "IN", "Hungary"       = "HU",
  "United States" = "US", "Russia"        = "RU", "Venezuela"     = "VE",
  "Burkina Faso"  = "BF", "Nicaragua"     = "NI", "Mali"          = "ML",
  "Niger"         = "NE", "Tunisia"       = "TN", "Turkey"        = "TR",
  "Canada"        = "CA"
)

wb_gni_scatter <- tryCatch({
  raw <- WDI(
    indicator = "NY.GNP.PCAP.PP.CD",
    country   = unname(wdi_iso2_map),
    start     = 1980,
    end       = 2025
  )
  iso2_to_name <- setNames(names(wdi_iso2_map), wdi_iso2_map)
  raw %>%
    filter(!is.na(NY.GNP.PCAP.PP.CD)) %>%
    transmute(
      country       = iso2_to_name[iso2c],
      year          = as.integer(year),
      logged_wealth = log(NY.GNP.PCAP.PP.CD)
    ) %>%
    filter(!is.na(country))
}, error = function(e) {
  message("WDI fetch failed, falling back to wb_data_clean: ", conditionMessage(e))
  # fallback to wb_data_clean if WDI call fails (offline etc.)
  if (exists("wb_data_clean") && is.data.frame(wb_data_clean) && "gni_pc" %in% names(wb_data_clean)) {
    iso_to_name <- setNames(names(wdi_iso2_map), wdi_iso2_map)
    wb_data_clean %>%
      distinct(iso3c, year, .keep_all = TRUE) %>%
      filter(!is.na(gni_pc)) %>%
      mutate(country = countrycode(iso3c, "iso3c", "country.name")) %>%
      filter(!is.na(country)) %>%
      transmute(country, year = as.integer(year), logged_wealth = log(gni_pc))
  } else {
    data.frame(country = character(), year = integer(), logged_wealth = numeric())
  }
})

# Merge V-Dem predictors + auth_index + Gini + logged_wealth
# Countries without a value for a given predictor are automatically excluded
# from that predictor's plot because wvs_agg() already filters !is.na(y_val).
scatter_full <- scatter_vdem %>%
  left_join(wvs_auth_agg, by = c("country", "year")) %>%
  left_join(gini_data %>% select(country, year, gini), by = c("country", "year")) %>%
  left_join(wb_gni_scatter,                             by = c("country", "year"))

# Append Canada 2025 CTM auth point (canada_ctm_auth_2025 now exists)
if (!is.na(canada_ctm_auth_2025)) {
  canada_rows <- scatter_full %>% filter(country == "Canada")
  if (nrow(canada_rows) > 0) {
    canada_2025_row <- canada_rows %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      slice(1) %>%
      mutate(year = 2025L, auth_index = canada_ctm_auth_2025)
    scatter_full <- bind_rows(scatter_full, canada_2025_row)
  }
}

scatter_countries_list <- sort(unique(scatter_full$country))

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
  palette  = c("#1e40af", "#3b82f6", "#60a5fa", "#f59e0b", "#e67e22", "#c0392b", "#7c3aed"),
  domain   = auth_range,
  na.color = "#c4d0e4"
)

make_canada_tooltip <- function(prov_name, mean_val, n) {
  if (is.na(mean_val)) {
    return(sprintf(
      "<div style='font-family:\"DM Mono\",monospace;background:#f7f9fc;
       border:1px solid #d0dae8;border-radius:10px;padding:12px 15px;
       color:#6b7280;font-size:14px;'>
       <b style='color:#0f172a;'>%s</b><br><br>No data available.</div>",
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
      background: #f7f9fc;
      border: 1px solid #d0dae8;
      border-radius: 13px;
      padding: 15px 17px;
      min-width: 230px;
      max-width: 260px;
      box-shadow: 0 6px 30px rgba(0,0,0,0.6);
    '>
      <div style='
        font-family: \"Syne\", sans-serif;
        font-weight: 800; font-size: 15px;
        color: #0f172a; margin-bottom: 12px;
        padding-bottom: 9px;
        border-bottom: 1px solid #d0dae8;
        letter-spacing: -0.01em;
      '>%s</div>
      <div style='margin-bottom:10px;'>
        <div style='display:flex;justify-content:space-between;margin-bottom:5px;'>
          <span style='font-size:13px;color:#6b7280;text-transform:uppercase;
                       letter-spacing:0.1em;'>Authoritarian Index</span>
          <span style='font-size:14px;font-weight:800;color:%s;'>%.3f</span>
        </div>
        <div style='background:#d0dae8;border-radius:4px;height:9px;width:100%%;overflow:hidden;'>
          <div style='background:%s;width:%.1f%%;height:9px;border-radius:4px;
               transition:width 0.4s ease;'></div>
        </div>
      </div>
      <div style='display:flex;justify-content:space-between;margin-top:8px;'>
        <span style='font-size:13px;color:#9ca3af;'>Level</span>
        <span style='font-size:13px;font-weight:700;color:%s;'>%s</span>
      </div>
      <div style='display:flex;justify-content:space-between;margin-top:4px;'>
        <span style='font-size:13px;color:#9ca3af;'>Respondents</span>
        <span style='font-size:13px;color:#6b7280;'>%d</span>
      </div>
      <div style='font-size:14px;color:#6b7280;margin-top:10px;text-align:right;
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
  na.color = "#cccccc"
)

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = div(
    style = "font-family:'Syne',sans-serif; font-weight:800; font-size:1.1rem; letter-spacing:-0.02em; color:#0f172a;",
    "DEMOCRACY ", tags$span("INDEX", style = "color:#3b82f6;"), " MAP"
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
        background: #ffffff;
        color: #0f172a;
        font-family: 'DM Mono', monospace;
        overflow-x: hidden;
      }

      /* ── Navbar ── */
      .navbar {
        background: linear-gradient(135deg, #ffffff 0%, #eef2fa 100%) !important;
        border-bottom: 1px solid #d0dae8 !important;
        min-height: 56px;
      }
      .navbar-brand { padding: 10px 16px; }
      .navbar-nav > li > a {
        font-family: 'DM Mono', monospace !important;
        font-size: 0.90rem !important;
        letter-spacing: 0.14em !important;
        text-transform: uppercase !important;
        color: #6b7280 !important;
        padding: 18px 18px !important;
        transition: color 0.2s;
      }
      .navbar-nav > li > a:hover { color: #1d4ed8 !important; }
      .navbar-nav > li.active > a,
      .navbar-nav > li.active > a:focus {
        color: #3b82f6 !important;
        background: transparent !important;
        border-bottom: 2px solid #3b82f6;
      }
      .navbar-toggle .icon-bar { background: #6b7280; }
      .navbar-collapse { border-top: none !important; box-shadow: none !important; }

      /* ── Tab content padding reset ── */
      .tab-content > .tab-pane { padding: 0; }
      .container-fluid { padding: 0; }

      /* ══════════════════════════════════════════════
         OVERVIEW PAGE
      ══════════════════════════════════════════════ */
      .overview-page {
        min-height: calc(100vh - 56px);
        background: #ffffff;
        overflow-y: auto;
      }

      /* Hero band */
      .hero {
        background: linear-gradient(160deg, #ffffff 0%, #eef2fa 50%, #f0f7ee 100%);
        padding: 72px 10% 60px;
        border-bottom: 1px solid #d0dae8;
        position: relative;
        overflow: hidden;
      }
      .hero::before {
        content: '';
        position: absolute;
        inset: 0;
        background: radial-gradient(ellipse 60% 50% at 70% 50%, rgba(59,130,246,0.06) 0%, transparent 70%);
        pointer-events: none;
      }
      .hero-eyebrow {
        font-size: 0.82rem;
        letter-spacing: 0.18em;
        text-transform: uppercase;
        color: #3b82f6;
        margin-bottom: 18px;
      }
      .hero-title {
        font-family: 'Syne', sans-serif;
        font-weight: 800;
        font-size: clamp(2rem, 5vw, 3.6rem);
        line-height: 1.05;
        letter-spacing: -0.03em;
        color: #0f172a;
        max-width: 820px;
        margin-bottom: 24px;
      }
      .hero-title em {
        font-style: normal;
        color: #3b82f6;
      }
      .hero-lead {
        font-size: 1.05rem;
        line-height: 1.85;
        color: #6b7280;
        max-width: 720px;
        margin-bottom: 36px;
      }
      .hero-cta {
        display: inline-block;
        background: #3b82f6;
        color: #ffffff;
        font-family: 'DM Mono', monospace;
        font-size: 0.90rem;
        letter-spacing: 0.10em;
        text-transform: uppercase;
        padding: 12px 28px;
        border-radius: 4px;
        border: none;
        cursor: pointer;
        text-decoration: none;
        transition: background 0.2s, transform 0.15s;
      }
      .hero-cta:hover { background: #2563eb; transform: translateY(-1px); color: #ffffff; text-decoration: none; }

      /* Info grid */
      .info-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
        gap: 1px;
        background: #d0dae8;
        border-top: 1px solid #d0dae8;
      }
      .info-card {
        background: #ffffff;
        padding: 40px 36px;
      }
      .info-card-label {
        font-size: 0.78rem;
        letter-spacing: 0.15em;
        text-transform: uppercase;
        color: #3b82f6;
        margin-bottom: 14px;
      }
      .info-card-title {
        font-family: 'Syne', sans-serif;
        font-weight: 700;
        font-size: 1.1rem;
        color: #0f172a;
        margin-bottom: 14px;
        line-height: 1.3;
      }
      .info-card-body {
        font-size: 1.0rem;
        line-height: 1.9;
        color: #6b7280;
      }
      .info-card-body p { margin-bottom: 10px; }
      .info-card-body p:last-child { margin-bottom: 0; }

      /* Key terms strip */
      .terms-strip {
        padding: 40px 10%;
        border-top: 1px solid #d0dae8;
        border-bottom: 1px solid #d0dae8;
      }
      .terms-strip-label {
        font-size: 0.82rem;
        letter-spacing: 0.15em;
        text-transform: uppercase;
        color: #9ca3af;
        margin-bottom: 20px;
      }
      .terms-list {
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
      }
      .term-chip {
        background: #eef2fa;
        border: 1px solid #b8c8de;
        border-radius: 20px;
        padding: 6px 16px;
        font-size: 0.90rem;
        color: #6b7280;
      }
      .term-chip strong { color: #3b82f6; font-weight: 400; }

      /* Footer */
      .page-footer {
        padding: 28px 10%;
        display: flex;
        justify-content: space-between;
        align-items: center;
        flex-wrap: wrap;
        gap: 12px;
      }
      .page-footer-left { font-size: 0.82rem; color: #9ca3af; letter-spacing: 0.04em; line-height: 1.7; }
      .page-footer-right { font-size: 0.82rem; color: #9ca3af; letter-spacing: 0.04em; }

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
        background: #f7f9fc;
        border-right: 1px solid #d0dae8;
        padding: 24px 20px;
        display: flex;
        flex-direction: column;
        gap: 20px;
        overflow-y: auto;
      }
      .map-page-header {
        padding: 14px 20px 12px;
        background: linear-gradient(135deg, #ffffff 0%, #eef2fa 100%);
        border-bottom: 1px solid #d0dae8;
      }
      .map-page-title {
        font-family: 'Syne', sans-serif;
        font-weight: 800;
        font-size: 1rem;
        color: #0f172a;
        letter-spacing: -0.02em;
      }
      .map-page-sub {
        font-size: 0.82rem;
        color: #6b7280;
        letter-spacing: 0.10em;
        text-transform: uppercase;
        margin-top: 2px;
      }
      .panel-label {
        font-size: 0.84rem;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        color: #6b7280;
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
        font-size: 0.82rem;
        color: #6b7280;
      }
      .stat-box {
        background: #eef2fa;
        border: 1px solid #b8c8de;
        border-radius: 10px;
        padding: 14px 16px;
      }
      .stat-val {
        font-family: 'Syne', sans-serif;
        font-size: 1.6rem;
        font-weight: 700;
        color: #3b82f6;
        line-height: 1;
      }
      .stat-desc {
        font-size: 0.82rem;
        color: #6b7280;
        margin-top: 4px;
        letter-spacing: 0.05em;
        text-transform: uppercase;
      }
      .tier-row {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 7px;
        font-size: 0.88rem;
        color: #374151;
      }
      .tier-dot { width: 11px; height: 11px; border-radius: 50%; flex-shrink: 0; }
      .shiny-input-container { width: 100% !important; }
      .irs--shiny .irs-bar { background: #3b82f6; border-color: #3b82f6; }
      .irs--shiny .irs-handle { border-color: #3b82f6; background: #3b82f6; }
      .irs--shiny .irs-from, .irs--shiny .irs-to,
      .irs--shiny .irs-single { background: #3b82f6; color: #ffffff; font-family: 'DM Mono'; }
      .irs--shiny .irs-line { background: #c4d0e4; border-color: #c4d0e4; }
      .map-wrap { flex: 1; position: relative; overflow: hidden; }
      #map { width: 100%; height: 100%; }
      .info-overlay {
        position: absolute; bottom: 24px; right: 24px;
        background: rgba(255,255,255,0.97); border: 1px solid #c4d0e4;
        border-radius: 12px; padding: 14px 18px; min-width: 200px;
        backdrop-filter: blur(8px); z-index: 900;
      }
      .info-country { font-family: 'Syne',sans-serif; font-weight:700; font-size:1rem; color:#0f172a; margin-bottom:4px; }
      .info-score   { font-size:1.5rem; font-weight:700; font-family:'Syne',sans-serif; }
      .info-tier-label { font-size:0.80rem; text-transform:uppercase; letter-spacing:0.1em; margin-top:3px; color:#6b7280; }
      .source-note { font-size:1.0rem; color:#9ca3af; letter-spacing:0.05em; line-height:1.6; }
      .rank-table { width:100%; border-collapse:collapse; }
      .rank-table td { padding:4px 0; font-size:1.00rem; color:#6b7280; border-bottom:1px solid #d0dae8; }
      .rank-table td:last-child { text-align:right; color:#3b82f6; }

      /* ══════════════════════════════════════════════
         WVS SCATTERPLOT TAB
      ══════════════════════════════════════════════ */
      .wvs-wrap {
        display: flex;
        min-height: calc(100vh - 56px);
        background: #f7f9fc;
      }
      .wvs-sidebar {
        width: 280px;
        min-width: 260px;
        background: #ffffff;
        border-right: 1px solid #d0dae8;
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
        background: #f7f9fc;
      }
      .wvs-plot-title {
        font-family: 'Syne', sans-serif;
        font-weight: 700;
        font-size: 1.15rem;
        color: #0f172a;
        margin: 0;
      }
      .wvs-plot-subtitle {
        font-size: 0.90rem;
        color: #6b7280;
        margin: 2px 0 0;
        letter-spacing: 0.04em;
        font-family: 'DM Mono', monospace;
        text-transform: uppercase;
      }
      .wvs-plot-wrap {
        background: #ffffff;
        border: 1px solid #d0dae8;
        border-radius: 10px;
        overflow: hidden;
        flex: 1;
      }
      .wvs-ctrl-label {
        font-size: 0.84rem;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        color: #6b7280;
        margin-bottom: 8px;
        font-family: 'DM Mono', monospace;
      }
      .wvs-divider { border: none; border-top: 1px solid #d0dae8; margin: 0; }
      .btn-gold {
        background: #3b82f6;
        border: none;
        color: #f7f9fc;
        font-family: 'DM Mono', monospace;
        font-weight: 500;
        font-size: 0.88rem;
        letter-spacing: 0.05em;
        text-transform: uppercase;
        padding: 8px 14px;
        border-radius: 6px;
        cursor: pointer;
        transition: background .2s;
      }
      .btn-gold:hover { background: #93c5fd; }
      .btn-dark {
        background: #e4eaf6;
        border: none;
        color: #374151;
        font-family: 'DM Mono', monospace;
        font-size: 0.88rem;
        letter-spacing: 0.05em;
        text-transform: uppercase;
        padding: 8px 14px;
        border-radius: 6px;
        cursor: pointer;
        transition: background .2s;
      }
      .btn-dark:hover { background: #d0dae8; }
      .wvs-stat-row { display: flex; gap: 8px; flex-wrap: wrap; }
      .wvs-stat-pill {
        background: #eef2fa;
        border: 1px solid #b8c8de;
        border-radius: 8px;
        padding: 10px 12px;
        flex: 1;
        min-width: 70px;
      }
      .wvs-stat-pill .val {
        font-family: 'Syne', sans-serif;
        font-size: 1.25rem;
        font-weight: 700;
        color: #3b82f6;
        display: block;
        line-height: 1;
      }
      .wvs-stat-pill .lbl {
        font-size: 0.82rem;
        color: #6b7280;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        margin-top: 4px;
        font-family: 'DM Mono', monospace;
      }
      .wvs-index-note {
        font-size: 0.68rem;
        color: #9ca3af;
        line-height: 1.55;
        padding: 10px 12px;
        background: #eef2fa;
        border-left: 2px solid #3b82f644;
        border-radius: 0 6px 6px 0;
        font-family: 'DM Mono', monospace;
      }
      /* Override slider colours for WVS tab to match overview green */
      .wvs-sidebar .irs--shiny .irs-bar { background: #3b82f6 !important; border-color: #3b82f6 !important; }
      .wvs-sidebar .irs--shiny .irs-handle { background: #3b82f6 !important; border-color: #3b82f6 !important; }
      .wvs-sidebar .irs--shiny .irs-from,
      .wvs-sidebar .irs--shiny .irs-to,
      .wvs-sidebar .irs--shiny .irs-single { background: #3b82f6 !important; color: #ffffff !important; }
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
        background: #dbeafe !important; color: #1d4ed8 !important;
      }
      .wvs-sidebar .selectize-input .item {
        background: #dbeafe !important; border: 1px solid #3b82f6 !important;
        color: #1e40af !important; border-radius: 4px !important;
        padding: 1px 6px !important; font-size: 0.78rem !important;
      }
      .wvs-sidebar .selectize-input .item.active {
        background: #3b82f6 !important; color: #ffffff !important;
      }

      /* ══════════════════════════════════════════════
         CANADA AUTH MAP TAB
      ══════════════════════════════════════════════ */
      .ctm-wrap {
        display: flex;
        height: calc(100vh - 56px);
        background: #ffffff;
      }
      .ctm-sidebar {
        width: 250px;
        min-width: 220px;
        background: #f7f9fc;
        border-right: 1px solid #d0dae8;
        padding: 20px 16px;
        display: flex;
        flex-direction: column;
        gap: 10px;
        overflow-y: auto;
      }
      .ctm-map-wrap { flex: 1; position: relative; overflow: hidden; }
      #canada_map { width: 100%; height: 100%; }
      .ctm-section-title {
        font-size: 0.82rem; letter-spacing: 0.12em;
        text-transform: uppercase; color: #9ca3af;
        margin-bottom: 4px; margin-top: 6px;
      }
      .ctm-section-title:first-of-type { margin-top: 0; }
      .ctm-grad-bar {
        height: 14px; border-radius: 7px;
        background: linear-gradient(to right, #1d4ed8, #60a5fa, #f1c40f, #e67e22, #c0392b, #1d4ed8, #1e40af);
        margin-bottom: 5px;
      }
      .ctm-grad-labels {
        display: flex; justify-content: space-between;
        font-size: 0.82rem; color: #6b7280;
      }
      .ctm-divider { border: none; border-top: 1px solid #d0dae8; margin: 4px 0; }
      .ctm-hint {
        font-size: 0.62rem; color: #6b7280; line-height: 1.6;
        background: #eef2fa; border: 1px solid #b8c8de;
        border-radius: 8px; padding: 9px 11px;
      }
      .ctm-prov-row {
        display: flex; justify-content: space-between; align-items: center;
        font-size: 0.64rem; color: #6b7280;
        padding: 3px 0; border-bottom: 1px solid #eef2fa;
      }
      .ctm-prov-score {
        font-family: 'Syne', sans-serif; font-weight: 700; font-size: 0.90rem;
      }
      .ctm-note {
        font-size: 0.80rem; color: #6b7280;
        line-height: 1.65; margin-top: auto;
        padding-top: 12px; border-top: 1px solid #d0dae8;
      }

      /* ── Canada Votes tab toggle ── */
      .ctm-toggle-wrap {
        display: flex; gap: 6px; margin-bottom: 4px;
      }
      .ctm-toggle-btn {
        flex: 1; padding: 7px 4px;
        font-family: 'DM Mono', monospace; font-size: 0.84rem;
        letter-spacing: 0.08em; text-transform: uppercase;
        border: 1px solid #d0dae8; border-radius: 5px;
        background: #eef2fa; color: #6b7280; cursor: pointer;
        transition: background 0.18s, color 0.18s, border-color 0.18s;
      }
      .ctm-toggle-btn.active-auth {
        background: #dbeafe; color: #3b82f6; border-color: #60a5fa;
      }
      .ctm-toggle-btn.active-votes {
        background: #dbeafe; color: #1d4ed8; border-color: #3b82f6;
      }

      /* Party legend swatches */
      .votes-leg-item {
        display: flex; align-items: center; gap: 8px;
        font-size: 0.86rem; color: #374151; padding: 3px 0;
      }
      .votes-leg-swatch {
        width: 10px; height: 10px; border-radius: 3px; flex-shrink: 0;
      }

      /* ══════════════════════════════════════════════
         REGRESSION TAB — Canada Model
      ══════════════════════════════════════════════ */
      .reg-wrap {
        display: flex; min-height: calc(100vh - 56px); background: #f7f9fc;
      }
      .reg-sidebar {
        width: 310px; min-width: 280px; background: #f7f9fc;
        border-right: 1px solid #d0dae8; padding: 24px 20px;
        display: flex; flex-direction: column; gap: 16px; overflow-y: auto;
      }
      .reg-main {
        flex: 1; padding: 28px 36px;
        display: flex; flex-direction: column; gap: 20px;
        overflow-y: auto; background: #f7f9fc;
      }
      .reg-ctrl-label {
        font-size: 0.88rem; letter-spacing: 0.12em; text-transform: uppercase;
        color: #6b7280; margin-bottom: 8px; font-family: 'DM Mono', monospace;
      }
      .reg-section-title {
        font-family: 'Syne', sans-serif; font-weight: 700; font-size: 1rem;
        color: #0f172a; border-bottom: 1px solid #d0dae8; padding-bottom: 8px;
        margin-bottom: 4px;
      }
      .reg-formula-box {
        background: #eef2fa; border: 1px solid #b8c8de; border-radius: 10px;
        padding: 16px 18px; font-family: 'DM Mono', monospace;
        font-size: 0.95rem; color: #374151; line-height: 2.1;
      }
      .eq-main  { color: #0f172a; font-size: 1.0rem; font-weight: 500; }
      .eq-active { color: #3b82f6; font-weight: 700; }
      .eq-muted  { color: #9ca3af; text-decoration: line-through; }
      .reg-stat-grid {
        display: grid; grid-template-columns: repeat(auto-fit, minmax(110px,1fr)); gap: 10px;
      }
      .reg-stat-card {
        background: #eef2fa; border: 1px solid #b8c8de; border-radius: 10px; padding: 14px 16px;
      }
      .reg-stat-card .val {
        font-family: 'Syne', sans-serif; font-size: 1.25rem; font-weight: 700;
        color: #3b82f6; display: block; line-height: 1;
      }
      .reg-stat-card .lbl {
        font-size: 0.88rem; color: #6b7280; text-transform: uppercase;
        letter-spacing: 0.08em; margin-top: 5px; font-family: 'DM Mono', monospace;
      }
      .reg-hint {
        font-size: 0.95rem; color: #374151; line-height: 1.75; padding: 12px 14px;
        background: #eef2fa; border-left: 3px solid #3b82f6;
        border-radius: 0 6px 6px 0; font-family: 'DM Mono', monospace;
      }
      .reg-plot-wrap {
        background: #ffffff; border: 1px solid #d0dae8; border-radius: 10px; overflow: hidden;
      }
      .reg-divider { border: none; border-top: 1px solid #d0dae8; margin: 0; }
      /* Override Shiny's default white table background */
      #reg_coef_table table { width:100%; border-collapse:collapse;
        font-family:'DM Mono',monospace; font-size:0.93rem; background:#ffffff !important; }
      #reg_coef_table th { color:#9ca3af !important; background:#ffffff !important;
        font-size:0.96rem !important; letter-spacing:0.14em !important;
        text-transform:uppercase !important; padding:6px 12px; border-bottom:1px solid #c4d0e4; }
      #reg_coef_table td { color:#374151 !important; padding:10px 12px;
        border-bottom:1px solid #d0dae8; background:transparent !important; }
      #reg_coef_table tr:hover td { background:#eef2fa !important; }
    "))
  ),
  
  # ══════════════════════════════════════════════════════════════════
  #  TAB 1 — OVERVIEW (title / info page)
  # ══════════════════════════════════════════════════════════════════
  tabPanel("Overview",
           div(class = "overview-page",
               
               # ── Hero ──────────────────────────────────────────────────────
               div(class = "hero",
                   div(class = "hero-eyebrow", "Divya Nair  ·  Kyleigh Harris  ·  Yolanda Setiawan  ·  SDA 490: Capstone Project (Spring 2026)"),
                   div(class = "hero-title",
                       "Rise in Authoritarianism ", tags$em("Around the World"), tags$br(), "(and in Canada?)"
                   ),
                   div(class = "hero-lead",
                       tags$b("Which institutional-level and individual-level factors predict a rise in authoritarianism in the government? And what is the likelihood of a rise of authoritarianism in the Canadian government?"),
                       tags$br(), tags$br(),
                       "We focused on this topic as we are all interested in understanding political conflicts and
           international relations around the world. The rise in authoritarianism
           has created more conflict internationally, has disrupted the stock market
           and is negatively affecting everyone."
                   ),
                   # Timeline chart — Timeline_picture.png in same folder as app.R
                   tags$div(
                     style = "margin: 24px 0 20px; background:#eef2fa; border:1px solid #b8c8de; border-radius:10px; padding:20px;",
                     tags$img(
                       src   = "appimgs/Timeline_picture.png",
                       alt   = "Electoral Democracy Index Timeline 1980–2025",
                       style = "max-width:100%; width:100%; height:auto; display:block; margin:0 auto; border-radius:6px;",
                       onerror = "this.style.display='none'"),
                     tags$p(
                       style = "font-size:1.05rem; color:#374151; line-height:1.85; margin-top:14px;",
                       "This graph highlights the timeline for the electoral democracy index from 1980 till 2025.
                       The electoral democracy index measures democracy scores in each country. This line shows
                       the avg democracy scores around the world for our timeline.",
                       tags$br(), tags$br(),
                       "In 1990s, we can see a significant increase in the democratic score at the end of the Cold War
                       which was a war of ideologies between freedom and democracy vs communism. When the west defeated
                       the Soviet Union, we see a sharp rise in democracy around the world.",
                       tags$br(), tags$br(),
                       "On the graph we highlighted ",
                       tags$b(style="color:#f59e0b;","2013"),
                       " which is the first dip in democracy score after a steady increase. Since then there
                       has only been a decrease in the democratic score.",
                       tags$br(), tags$br(),
                       "As we look closer to present day, there is a large decrease in the democracy score as it
                       drops from 2019 at 0.52 till most recent in 2025 at 0.48. This indicates a ",
                       tags$b(style="color:#c0392b;","significant rise in authoritarianism around the world.")
                     )
                   ),
                   tags$a(class = "hero-cta", href = "#", onclick = "$('a[data-value=\"Interactive Map\"]').tab('show'); return false;",
                          "Open the Map →"
                   )
               ),
               
               div(class = "info-grid",
                   
                   div(class = "info-card",
                       div(class = "info-card-label", "Research Questions"),
                       div(class = "info-card-title",
                           "Which factors predict a rise in authoritarianism? Is Canada at risk?"
                       ),
                       div(class = "info-card-body",
                           tags$p(tags$b(style="color:#3b82f6;","Primary Research Question:"),
                                  " Which institutional-level and individual-level factors predict a rise in authoritarianism in the government?"),
                           tags$p(tags$b(style="color:#3b82f6;","Secondary Research Question:"),
                                  " What is the likelihood of a rise of authoritarianism in the Canadian government?")
                       )
                   ),
                   
                   div(class = "info-card",
                       div(class = "info-card-label", "Definition"),
                       div(class = "info-card-title",
                           HTML("authoritarian <span style='color:#6b7280;font-size:0.85rem;font-weight:400;'>(adj.)</span>")
                       ),
                       div(class = "info-card-body",
                           tags$p("\"favoring imposed order over freedom,\" 1862, from authority + -an. Compare authoritative,
                                   which originally had this meaning to itself. Authoritarian has also been used in the sense
                                   of \"authoritative\" (by 1857)."),
                           tags$p("The noun in the sense of \"one advocating or practicing the principle of authority over
                                   individual freedom\" is attested by 1859."),
                           tags$p(style="font-size:1.02rem;color:#9ca3af;",
                                  "Source: ",
                                  tags$a(href="https://www.etymonline.com/word/authoritarian",
                                         target="_blank",
                                         style="color:#3b82f6;",
                                         "etymonline.com/word/authoritarian"))
                       )
                   ),
                   
                   div(class = "info-card",
                       div(class = "info-card-label", "Authoritarian Indices"),
                       div(class = "info-card-title",
                           "How we measure authoritarianism"
                       ),
                       div(class = "info-card-body",
                           tags$p(tags$b(style="color:#3b82f6;","Authoritarian Values Index:"),
                                  " Uses questions from the World Values Survey to understand authoritarian
                                  values and attitudes held by individuals around the world."),
                           tags$p(tags$b(style="color:#3b82f6;","Authoritarian Government Index:"),
                                  " Uses V-Dem scores to measure countries on a scale of 0–1
                                  (0 = autocracy, 1 = the most liberal democracy)."),
                           tags$p(tags$b(style="color:#3b82f6;","Canadian Authoritarian Values Index:"),
                                  " Uses questions from the CTM dataset to understand authoritarian
                                  values and attitudes held by Canadians.")
                       )
                   ),
                   
                   div(class = "info-card",
                       div(class = "info-card-label", "Data Sources"),
                       div(class = "info-card-title", "Datasets Used"),
                       div(class = "info-card-body",
                           tags$p(tags$b(style="color:#3b82f6;","V-Dem (Varieties of Democracy):"),
                                  " Provides hundreds of indicators on \"liberal\" vs. \"authoritarian\" practices
                                  (e.g., media censorship, judicial independence)."),
                           tags$p(tags$b(style="color:#3b82f6;","World Values Survey (WVS):"),
                                  " The gold standard. Contains specific questions on \"Support for democracy\" vs.
                                  \"Support for a strong leader\" or \"Military rule.\" Canada is included in several waves."),
                           tags$p(tags$b(style="color:#3b82f6;","CTM Data:"),
                                  " Survey data provided by Innovative Research used to represent the attitudes
                                  and views of Canadians in the wave of December 2025.")
                       )
                   )
                   
               ), # /info-grid
               
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
                       div(class = "term-chip", tags$strong("Authoritarianism"), " favoring imposed order over freedom"),
                       div(class = "term-chip", tags$strong("Political Polarization")),
                       div(class = "term-chip", tags$strong("Resource Curse"), " (Resource Dependency)"),
                       div(class = "term-chip", tags$strong("Party Strength"), " institutionalization"),
                       div(class = "term-chip", tags$strong("Gini Coefficient"), " income inequality"),
                       div(class = "term-chip", tags$strong("V-Dem"), " Varieties of Democracy Project"),
                       div(class = "term-chip", tags$strong("WVS"), " World Values Survey"),
                       div(class = "term-chip", tags$strong("CTM Data"), " Innovative Research Canada")
                   )
               ),
               
               div(class = "page-footer",
                   div(class = "page-footer-left",
                       "V-Dem: Coppedge, Michael et al. (2025). V-Dem Dataset v16. Varieties of Democracy Project.", tags$br(),
                       "WVS: World Values Survey Time Series 1981–2022. www.worldvaluessurvey.org", tags$br(),
                       "CTM Data: Survey data provided by Innovative Research Group. Wave: December 2025."
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
                     style = "background:#eef2fa; border:1px solid #b8c8de; border-radius:8px; padding:12px 14px; font-size:1.02rem; color:#6b7280; line-height:1.7;",
                     "This map visualises the Electoral Democracy Index (v2x_polyarchy) from the Varieties of Democracy
                     (V-Dem) dataset, tracking democratic quality across 180+ countries from 1980 to 2025. It shows
                     the categories at which countries are classified: liberal democracy, electoral democracy, hybrid
                     regime, competitive authoritarianism, and closed autocracy. It highlights how electoral democracy
                     has expanded, contracted, and shifted across regions over more than four decades. By sliding through
                     the years, the map reveals key historical turning points — from the post-Cold War wave of
                     democratisation in the early 1990s to the more recent patterns of democratic backsliding observed
                     across parts of Europe, Asia, and Latin America."
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
                     span(style = "font-size:1.02rem; color:#6b7280", "Closed Autocracy (< 0.15)")
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
  tabPanel("Predictor Scatterplot",
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
                                    choices  = scatter_countries_list,
                                    selected = head(scatter_countries_list, 8),
                                    multiple = TRUE,
                                    options  = list(placeholder = "Select countries…"))
                   ),
                   
                   div(
                     style = "display:flex; gap:8px;",
                     actionButton("sel_all",  "All",    class = "btn-gold",
                                  style = "padding:8px 12px; font-size:0.88rem; flex:1;"),
                     actionButton("sel_none", "None",   class = "btn-dark",
                                  style = "padding:8px 12px; font-size:0.88rem; flex:1;"),
                     actionButton("sel_rand", "Sample", class = "btn-dark",
                                  style = "padding:8px 12px; font-size:0.88rem; flex:1;")
                   ),
                   
                   hr(class = "wvs-divider"),
                   
                   div(
                     div(class = "wvs-ctrl-label", "Year Range"),
                     sliderInput("wvs_year_range", label = NULL,
                                 min   = 1980,
                                 max   = 2025,
                                 value = c(1980, 2025),
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
                       "Predictors from the regression model: ",
                       tags$b("Digital Freedom"), " (v2mecenefm, V-Dem — higher = freer), ",
                       tags$b("Party Strength"), " (v2xps_party, V-Dem), ",
                       tags$b("Inequality"), " (Gini, SWIID), ",
                       tags$b("Polarization"), " (v2cacamps, V-Dem), ",
                       tags$b("Legislature Resource Funding"), " (v2lgfunds, V-Dem), ",
                       tags$b("Auth Values"), " (WVS index). ",
                       tags$br(),
                       "Canada's 2025 auth point is sourced from CTM survey data.",
                       tags$br(),
                       "Countries without data for a selected variable are automatically excluded."
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
  ), # /tabPanel WVS
  
  # ══════════════════════════════════════════════════════════════════
  #  TAB 4 — GLOBAL REGRESSION MODEL
  #  scale(v2x_polyarchy) ~ 7 predictors
  #  Unit of analysis: country × WVS wave (global cross-national)
  # ══════════════════════════════════════════════════════════════════
  tabPanel("Regression Model",
           div(class = "reg-wrap",
               
               # ── Sidebar ────────────────────────────────────────────────────
               div(class = "reg-sidebar",
                   
                   div(
                     div(class = "reg-ctrl-label", "Outcome"),
                     tags$div(
                       style = "font-size:1.0rem; color:#374151; font-family:'DM Mono',monospace;
                         background:#eef2fa; border:1px solid #d0dae8; border-radius:6px; padding:10px 12px;",
                       tags$b(style="color:#1d4ed8;", "v2x_polyarchy"),
                       " — V-Dem Electoral Democracy Index (0–1). Higher = more democratic. All predictors standardized."
                     )
                   ),
                   
                   tags$hr(class = "reg-divider"),
                   
                   div(
                     div(class = "reg-ctrl-label", "Predictors"),
                     div(class = "reg-hint",
                         tags$b(style="color:#1d4ed8;","Digital Freedom (Censorship)"),
                         " — V-Dem v2mecenefm. Higher = freer media/digital.", tags$br(), tags$br(),
                         tags$b(style="color:#1d4ed8;","Political Party Strength"),
                         " — V-Dem v2xps_party. Party institutionalization.", tags$br(), tags$br(),
                         tags$b(style="color:#1d4ed8;","Logged Wealth (GNI pc)"),
                         " — ln(GNI per capita, PPP) via WDI.", tags$br(), tags$br(),
                         tags$b(style="color:#1d4ed8;","Social Polarization"),
                         " — V-Dem v2cacamps.", tags$br(), tags$br(),
                         tags$b(style="color:#1d4ed8;","Wealth Inequality (Gini)"),
                         " — SWIID gini_disp.", tags$br(), tags$br(),
                         tags$b(style="color:#1d4ed8;","Natural Resource Curse"),
                         " — World Bank resource rents % GDP.", tags$br(), tags$br(),
                         tags$b(style="color:#1d4ed8;","Authoritarian Values Index"),
                         " — WVS composite index."
                     )
                   ),
                   
                   tags$hr(class = "reg-divider"),
                   
                   div(class = "reg-hint",
                       tags$b(style="color:#1d4ed8;","Global cross-national model."),
                       " Unit of analysis: country × WVS wave.",
                       tags$br(), tags$br(),
                       "Sources: V-Dem v16 · World Bank WDI · SWIID v9.91 · WVS.",
                       tags$br(), tags$br(),
                       "All predictors standardized — coefficients are directly comparable.",
                       tags$br(), tags$br(),
                       tags$span(style="color:#e74c3c; font-weight:700;", "●"),
                       " Red = significant (p < 0.05)",
                       tags$br(),
                       tags$span(style="color:#2c3e50; font-weight:700;", "●"),
                       " Dark = not significant"
                   )
                   
               ), # /reg-sidebar
               
               # ── Main panel ──────────────────────────────────────────────────
               div(class = "reg-main",
                   
                   div(class = "reg-section-title", "Which Factors Drive Authoritarianism?"),
                   
                   # Fit stats
                   div(class = "reg-stat-grid",
                       div(class="reg-stat-card", span(class="val", textOutput("reg_r2",     inline=TRUE)), span(class="lbl","R²")),
                       div(class="reg-stat-card", span(class="val", textOutput("reg_adj_r2", inline=TRUE)), span(class="lbl","Adj. R²")),
                       div(class="reg-stat-card", span(class="val", textOutput("reg_n",      inline=TRUE)), span(class="lbl","Observations")),
                       div(class="reg-stat-card", span(class="val", textOutput("reg_f",      inline=TRUE)), span(class="lbl","F-statistic")),
                       div(class="reg-stat-card", span(class="val", textOutput("reg_rmse",   inline=TRUE)), span(class="lbl","RMSE"))
                   ),
                   
                   # Coefficient table (Variable | Est. | p — matches screenshot 1)
                   div(class="reg-section-title", style="margin-top:8px;", "Coefficient Estimates"),
                   tableOutput("reg_coef_table"),
                   div(class="reg-hint", style="margin-top:-8px;",
                       "*** p<0.001 · ** p<0.01 · * p<0.05  |  ",
                       "Positive = predictor associated with higher democracy score. ",
                       "Negative = associated with lower democracy score."
                   ),
                   
                   # Coefficient plot (matches screenshot 2)
                   div(class="reg-section-title", style="margin-top:8px;",
                       "Standardized Coefficients — Impact on Democracy Score"),
                   div(class="reg-plot-wrap", plotOutput("reg_coef_plot", height="400px")),
                   
                   # Backsliding image
                   div(class="reg-section-title", style="margin-top:16px;",
                       "Top 10 Countries: Most Severe Democratic Backsliding Since 1980"),
                   div(
                     style = "background:#eef2fa; border:1px solid #d0dae8; border-radius:10px; padding:20px; text-align:center; overflow:visible;",
                     tags$img(
                       src   = "appimgs/Top_10_countries.png",
                       alt   = "Top 10 countries experiencing democratic backsliding",
                       style = "max-width:100%; width:100%; height:auto; display:block; margin:0 auto; border-radius:6px;"
                     ),
                     tags$p(
                       style = "font-size:1.05rem; color:#6b7280; margin-top:12px;",
                       "Countries ranked by largest decline in V-Dem Electoral Democracy Index (v2x_polyarchy) from 1980 to 2025."
                     )
                   )
                   
               ) # /reg-main
           ) # /reg-wrap
  ), # /tabPanel Regression Model
  
  # ══════════════════════════════════════════════════════════════════
  #  TAB 6 — CONCLUSION
  # ══════════════════════════════════════════════════════════════════
  tabPanel("Conclusion",
           div(
             style = "min-height: calc(100vh - 56px); background: #ffffff; overflow-y: auto;",
             
             # ── Hero band ─────────────────────────────────────────────────
             div(
               style = "background: linear-gradient(160deg, #ffffff 0%, #eef2fa 50%, #f0f7ee 100%);
                        padding: 60px 10% 48px; border-bottom: 1px solid #d0dae8; position: relative;",
               div(style = "font-size:0.96rem; letter-spacing:0.22em; text-transform:uppercase;
                            color:#3b82f6; margin-bottom:16px;", "Summary of Findings"),
               div(
                 style = "font-family:'Syne',sans-serif; font-weight:800;
                          font-size:clamp(1.8rem,4vw,3rem); line-height:1.08;
                          letter-spacing:-0.03em; color:#0f172a; max-width:760px; margin-bottom:0;",
                 "Conclusions"
               )
             ),
             
             # ── Two-column grid ────────────────────────────────────────────
             div(
               style = "display:grid; grid-template-columns: repeat(auto-fit,minmax(320px,1fr));
                        gap:1px; background:#d0dae8;",
               
               # ── Findings ──────────────────────────────────────────────
               div(
                 style = "background:#ffffff; padding:44px 40px;",
                 div(style = "font-size:0.93rem; letter-spacing:0.2em; text-transform:uppercase;
                              color:#3b82f6; margin-bottom:14px;", "Key Findings"),
                 div(
                   style = "font-family:'Syne',sans-serif; font-weight:700; font-size:1.1rem;
                            color:#0f172a; margin-bottom:18px; line-height:1.3;",
                   "Institutional-Level Factors Drive Authoritarianism"
                 ),
                 tags$p(
                   style = "font-size:0.96rem; line-height:1.85; color:#6b7280; margin-bottom:12px;",
                   "Given its \"top-down\" process, we found institutional-level factors to be more
                    significant predictors of authoritarianism. Institutional-level predictors
                    ", tags$em("precede"), " authoritarian governments. Individual-level predictors
                    ", tags$em("proceed"), " authoritarian governments."
                 ),
                 tags$p(
                   style = "font-size:0.96rem; line-height:1.85; color:#6b7280; margin-bottom:0;",
                   "At the moment Canada is not at risk of becoming authoritarian. Strong democratic
                    guardrails such as party strength protect Canada's democratic institutions from
                    the significant predictors we identified."
                 )
               ),
               
               # ── Limitations ───────────────────────────────────────────
               div(
                 style = "background:#ffffff; padding:44px 40px;",
                 div(style = "font-size:0.93rem; letter-spacing:0.2em; text-transform:uppercase;
                              color:#c0392b; margin-bottom:14px;", "Limitations"),
                 div(
                   style = "font-family:'Syne',sans-serif; font-weight:700; font-size:1.1rem;
                            color:#0f172a; margin-bottom:20px; line-height:1.3;",
                   "What Our Analysis Cannot Fully Capture"
                 ),
                 
                 # Limitation 1
                 div(
                   style = "margin-bottom:24px;",
                   div(
                     style = "display:flex; align-items:flex-start; gap:12px; margin-bottom:10px;",
                     div(style = "width:4px; min-width:4px; height:100%; background:#c0392b;
                                  border-radius:2px; margin-top:3px; align-self:stretch;"),
                     div(
                       style = "font-family:'Syne',sans-serif; font-weight:700; font-size:1.02rem;
                                color:#0f172a; line-height:1.4;",
                       "Authoritarianism is Multidimensional"
                     )
                   ),
                   tags$p(
                     style = "font-size:0.93rem; line-height:1.8; color:#6b7280; padding-left:16px;",
                     "Our indices incorporate both the CTM data and WVS data. However, these questions
                      cannot fully encapsulate \"authoritarianism\" because there are so many aspects and
                      definitions — we had to curate specific questions that relate to the aspects of
                      authoritarianism most relevant for the scope of our project."
                   )
                 ),
                 
                 # Limitation 2
                 div(
                   div(
                     style = "display:flex; align-items:flex-start; gap:12px; margin-bottom:10px;",
                     div(style = "width:4px; min-width:4px; background:#c0392b;
                                  border-radius:2px; margin-top:3px; align-self:stretch;"),
                     div(
                       style = "font-family:'Syne',sans-serif; font-weight:700; font-size:1.02rem;
                                color:#0f172a; line-height:1.4;",
                       "Impossible to Determine Which Factors Cause Authoritarianism (Causality)"
                     )
                   ),
                   tags$p(
                     style = "font-size:0.93rem; line-height:1.8; color:#6b7280; padding-left:16px;",
                     "There are too many overlapping variables influencing authoritarianism all over
                      the world at any given time. We cannot control for certain factors because it is
                      impossible to untangle any of these factors from the variables we are analysing."
                   )
                 )
               )
             ), # /grid
             
             # ── Footer ──────────────────────────────────────────────────
             div(
               style = "padding:28px 10%; display:flex; justify-content:space-between;
                        align-items:center; flex-wrap:wrap; gap:12px;",
               div(
                 style = "font-size:0.96rem; color:#9ca3af; letter-spacing:0.06em; line-height:1.7;",
                 "V-Dem: Coppedge et al. (2025). V-Dem Dataset v16.",
                 tags$br(),
                 "WVS: World Values Survey Time Series 1981–2022.",
                 tags$br(),
                 "CTM Data: Innovative Research Group. December 2025."
               ),
               div(style = "font-size:0.96rem; color:#9ca3af; letter-spacing:0.06em;",
                   "Built with R Shiny & Leaflet")
             )
           )
  ) # /tabPanel Conclusion
  
)
# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Explicitly capture globals so they are always available inside the server,
  # even if Shiny is launched from a different working directory than the script.
  .vdem_all             <- vdem_all
  .world_base           <- world_base
  canada_ctm_auth_2025  <- canada_ctm_auth_2025
  
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
      "<div style='font-family:DM Mono,monospace; color:#0f172a; padding:4px 6px;'>
         <b style='font-size:15px'>%s</b><br/>
         Score: <b>%s</b><br/>
         <span style='font-size:13px; color:#374151'>%s</span>
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
      fill_color <- ifelse(d$visible, pal(d$v2x_polyarchy), "#cccccc")
      leafletProxy("map") |>
        clearShapes() |>
        addPolygons(
          data         = d,
          fillColor    = fill_color,
          fillOpacity  = 0.82,
          color        = "#c4d0e4",
          weight       = 0.5,
          smoothFactor = 1,
          label        = make_labels(d),
          labelOptions = labelOptions(
            style     = list("background" = "rgba(255,255,255,0.98)",
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
    fill_color <- ifelse(d$visible, pal(d$v2x_polyarchy), "#cccccc")
    
    leafletProxy("map") |>
      clearShapes() |>
      addPolygons(
        data         = d,
        fillColor    = fill_color,
        fillOpacity  = 0.82,
        color        = "#c4d0e4",
        weight       = 0.5,
        smoothFactor = 1,
        label        = make_labels(d),
        labelOptions = labelOptions(
          style     = list("background" = "rgba(255,255,255,0.98)",
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
      span("—", style = "color:#9ca3af")
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
  #  PREDICTOR SCATTERPLOT SERVER
  # ══════════════════════════════════════════════════════════════════
  
  # Capture globals into server scope
  .scatter_full        <- scatter_full
  .scatter_ctry        <- scatter_countries_list
  .wvs_years           <- wvs_years
  .permanent_countries <- .permanent_countries
  
  # ── Country quick-select buttons ──────────────────────────────────
  observeEvent(input$sel_all, {
    updateSelectizeInput(session, "wvs_countries", selected = .scatter_ctry)
  })
  observeEvent(input$sel_none, {
    updateSelectizeInput(session, "wvs_countries", selected = character(0))
  })
  observeEvent(input$sel_rand, {
    updateSelectizeInput(session, "wvs_countries",
                         selected = sample(.scatter_ctry, min(8, length(.scatter_ctry))))
  })
  
  # ── Filtered scatter data ──────────────────────────────────────────
  wvs_filtered <- reactive({
    req(input$wvs_countries, input$wvs_year_range)
    selected_ctry <- union(input$wvs_countries, .permanent_countries)
    .scatter_full %>%
      filter(
        country %in% selected_ctry,
        year    >= input$wvs_year_range[1],
        year    <= input$wvs_year_range[2]
      )
  })
  
  # ── Aggregated (mean per country × year for selected y_var) ───────
  wvs_agg <- reactive({
    req(input$y_var)
    y <- input$y_var
    d <- wvs_filtered()
    if (!y %in% names(d)) {
      return(data.frame(country = character(), year = integer(), y_val = numeric()))
    }
    d %>%
      group_by(country, year) %>%
      summarise(y_val = mean(.data[[y]], na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(y_val))
  })
  
  # ── Y-axis label ──────────────────────────────────────────────────
  wvs_y_label <- reactive({
    switch(input$y_var,
           auth_index         = "Authoritarian Values Index (0–1)",
           digital_proxy      = "Digital Freedom / v2mecenefm (V-Dem)",
           party_strength     = "Party Strength / v2xps_party (V-Dem)",
           logged_wealth      = "Wealth — ln(GNI per capita)",
           gini               = "Economic Inequality (Gini Coefficient)",
           polarization       = "Social Polarization / v2cacamps (V-Dem)",
           resource_rents     = "Legislature Resource Funding (v2lgfunds, V-Dem)",
           input$y_var
    )
  })
  
  # ── Summary stats ─────────────────────────────────────────────────
  output$wvs_n_obs  <- renderText({ nrow(wvs_agg()) })
  output$wvs_n_ctry <- renderText({ n_distinct(wvs_agg()$country) })
  output$wvs_n_wave <- renderText({ n_distinct(wvs_agg()$year) })
  
  # ── Dynamic title / subtitle ──────────────────────────────────────
  output$wvs_plot_title <- renderText({
    paste(wvs_y_label(), "vs. Year")
  })
  output$wvs_plot_subtitle <- renderText({
    ctry <- length(input$wvs_countries)
    paste0(ctry, " countr", ifelse(ctry == 1, "y", "ies"), " · ",
           input$wvs_year_range[1], "–", input$wvs_year_range[2],
           "  ·  V-Dem / WVS / SWIID / World Bank · Canada always shown")
  })
  
  # ── Scatterplot ───────────────────────────────────────────────────
  output$wvs_scatter <- renderPlot({
    d <- wvs_agg()
    validate(need(nrow(d) > 0, "No data for the selected filters."))
    
    p <- ggplot(d, aes(x = year, y = y_val)) +
      theme_minimal(base_size = 13) +
      theme(
        text             = element_text(family = "sans", color = "#0f172a"),
        plot.background  = element_rect(fill = "#ffffff", color = NA),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        panel.grid.major = element_line(color = "#d0dae8", linewidth = .5),
        panel.grid.minor = element_line(color = "#e8eef8", linewidth = .3),
        axis.text        = element_text(color = "#6b7280", size = 11),
        axis.title       = element_text(color = "#6b7280", size = 11,
                                        margin = margin(t = 8, r = 8)),
        axis.ticks       = element_blank(),
        legend.background= element_rect(fill = "#ffffff", color = NA),
        legend.key       = element_rect(fill = "#ffffff", color = NA),
        legend.text      = element_text(color = "#6b7280", size = 10),
        legend.title     = element_text(color = "#6b7280", size = 9, hjust = .5),
        plot.margin      = margin(20, 20, 16, 16)
      )
    
    # Trend line
    if (isTRUE(input$show_trend) && nrow(d) >= 4) {
      p <- p + geom_smooth(
        method = "loess", formula = y ~ x, se = TRUE,
        color = "#3b82f6", fill = "#3b82f644",
        linewidth = 1.2
      )
    }
    
    # Points + connecting lines
    # Canada is permanent — pin it to a fixed colour (#f1c40f, the map's yellow)
    # and assign the rest of the palette to other countries
    canada_colour <- "#f59e0b"
    other_countries <- setdiff(unique(d$country), .permanent_countries)
    n_other <- length(other_countries)
    overview_pal_base <- c(
      "#3b82f6", "#60a5fa", "#1d4ed8", "#1d4ed8", "#3b82f6",
      "#1e40af", "#c0392b", "#e67e22", "#3498db",
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
        geom_line(aes(group = country), color = "#3b82f6", alpha = .3, linewidth = .7) +
        geom_point(aes(size  = ifelse(country == "Canada", 5.5, 4),
                       shape = ifelse(country == "Canada", 18L, 16L)),
                   color = "#3b82f6", alpha = .88) +
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
          color         = "#0f172a",
          size          = 3.2,
          segment.color = "#3b82f655",
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
          color         = "#f59e0b",
          fontface      = "bold",
          size          = 3.6,
          segment.color = "#f59e0b88",
          segment.size  = .6,
          box.padding   = .5,
          point.padding = .4,
          max.overlaps  = 20
        )
      }
    }
    
    # Y-axis formatting
    if (input$y_var == "gini") {
      p <- p + scale_y_continuous(
        labels = scales::number_format(accuracy = 0.1),
        limits = c(0, 100)
      )
    } else {
      p <- p + scale_y_continuous(labels = scales::number_format(accuracy = .01))
    }
    
    # Use ~8 evenly-spaced year breaks to avoid label crowding
    yr_range  <- range(d$year, na.rm = TRUE)
    yr_breaks <- scales::pretty_breaks(n = 8)(yr_range[1]:yr_range[2])
    yr_breaks <- yr_breaks[yr_breaks >= yr_range[1] & yr_breaks <= yr_range[2]]
    
    p +
      scale_x_continuous(
        breaks = yr_breaks,
        labels = as.character(yr_breaks)
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
      ) +
      labs(x = "Year", y = wvs_y_label())
    
  }, bg = "#ffffff")
  
  # ══════════════════════════════════════════════════════════════════
  
  # ══════════════════════════════════════════════════════════════════
  #  GLOBAL REGRESSION SERVER
  #  Uses scatter_full — the merged dataset built at app startup from:
  #  V-Dem (digital_proxy/v2mecenefm, party_strength/v2xps_party,
  #         polarization/v2cacamps, resource_rents/v2lgfunds),
  #  WDI (logged_wealth = log(GNI per capita PPP)),
  #  World Bank Gini (gini), WVS (auth_index).
  #  v2x_polyarchy is joined from vdem_all by country_name + year.
  # ══════════════════════════════════════════════════════════════════
  
  # ── Build regression dataset from scatter_full ───────────────────────────────
  reg_df <- tryCatch({
    # scatter_full already has all predictors; rename to match Rmd labels
    base <- scatter_full %>%
      rename(
        gini_disp                  = gini,
        authoritarian_values_index = auth_index,
        resource_filled            = resource_rents
      )
    
    # Join v2x_polyarchy from vdem_all (uses country_name matching scatter_full$country)
    base %>%
      left_join(
        vdem_all %>% rename(country = country_name),
        by = c("country", "year")
      ) %>%
      drop_na(v2x_polyarchy, digital_proxy, party_strength, polarization,
              logged_wealth, gini_disp, resource_filled, authoritarian_values_index)
  }, error = function(e) NULL)
  
  # ── Fit the linear model ──────────────────────────────────────────────────────
  reg_model <- tryCatch({
    if (is.null(reg_df) || nrow(reg_df) < 5) stop("insufficient data")
    lm(scale(v2x_polyarchy) ~
         scale(digital_proxy)             +
         scale(party_strength)            +
         scale(logged_wealth)             +
         scale(polarization)              +
         scale(gini_disp)                 +
         scale(resource_filled)           +
         scale(authoritarian_values_index),
       data = reg_df)
  }, error = function(e) NULL)
  
  # ── Labels ───────────────────────────────────────────────────────────────────
  reg_label <- function(term) {
    lkp <- c(
      "(Intercept)"                       = "Constant (Intercept)",
      "scale(digital_proxy)"              = "Digital Freedom (Censorship)",
      "scale(party_strength)"             = "Political Party Strength",
      "scale(logged_wealth)"              = "Logged Wealth (GNI pc)",
      "scale(polarization)"               = "Social Polarization",
      "scale(gini_disp)"                  = "Wealth Inequality (Gini)",
      "scale(resource_filled)"            = "Natural Resource Curse",
      "scale(authoritarian_values_index)" = "Authoritarian Values Index"
    )
    ifelse(term %in% names(lkp), lkp[term], term)
  }
  
  sig_stars <- function(p) {
    dplyr::case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "")
  }
  
  # ── Fit statistics ────────────────────────────────────────────────────────────
  output$reg_r2 <- renderText({
    if (is.null(reg_model)) return("N/A")
    sprintf("%.3f", summary(reg_model)$r.squared)
  })
  output$reg_adj_r2 <- renderText({
    if (is.null(reg_model)) return("N/A")
    sprintf("%.3f", summary(reg_model)$adj.r.squared)
  })
  output$reg_n <- renderText({
    if (is.null(reg_df) || nrow(reg_df) == 0) return("N/A")
    as.character(nrow(reg_df))
  })
  output$reg_f <- renderText({
    if (is.null(reg_model)) return("N/A")
    fs <- summary(reg_model)$fstatistic
    if (is.null(fs)) "—" else sprintf("%.2f", fs["value"])
  })
  output$reg_rmse <- renderText({
    if (is.null(reg_model)) return("N/A")
    sprintf("%.4f", sqrt(mean(residuals(reg_model)^2)))
  })
  
  # ── Coefficient table ─────────────────────────────────────────────────────────
  output$reg_coef_table <- renderTable({
    if (is.null(reg_model)) {
      return(data.frame(
        Status = "Model not yet fitted — insufficient overlapping observations across predictors."
      ))
    }
    broom::tidy(reg_model) %>%
      mutate(
        Variable = reg_label(term),
        Est.     = sprintf("%.3f%s", estimate, sig_stars(p.value)),
        p        = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
      ) %>%
      select(Variable, Est., p)
  }, striped = FALSE, bordered = FALSE, width = "100%", rownames = FALSE)
  
  # ── Coefficient plot ──────────────────────────────────────────────────────────
  output$reg_coef_plot <- renderPlot({
    if (is.null(reg_model)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Model not yet available.",
                   colour = "#6b7280", size = 5, hjust = 0.5) +
          theme_void() +
          theme(plot.background = element_rect(fill = "#ffffff", colour = NA))
      )
    }
    pd <- broom::tidy(reg_model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(Variable = reg_label(term), sig = p.value < 0.05)
    
    ggplot(pd, aes(x = reorder(Variable, estimate), y = estimate)) +
      geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                      colour = "#2c3e50", linewidth = 0.7, size = 0.5) +
      geom_point(data = dplyr::filter(pd, sig), colour = "#e74c3c", size = 4) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "gray50", linewidth = 0.6) +
      coord_flip() +
      labs(title    = "Which Factors Drive Authoritarianism?",
           subtitle = "Standardized Coefficients",
           x = "", y = "Impact on Democracy Score",
           caption  = "Source: V-Dem, World Bank, SWIID, World Values Survey") +
      theme_minimal(base_size = 14) +
      theme(
        plot.background  = element_rect(fill = "#ffffff", colour = NA),
        panel.background = element_rect(fill = "#ffffff", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e5e7eb", linewidth = 0.4),
        axis.text        = element_text(colour = "#374151", size = 12),
        axis.title.x     = element_text(colour = "#374151", size = 12),
        plot.title       = element_text(face = "bold", colour = "#0f172a", size = 15),
        plot.subtitle    = element_text(colour = "#6b7280", size = 11, hjust = 0.35),
        plot.caption     = element_text(colour = "#9ca3af", size = 9),
        plot.margin      = margin(16, 28, 16, 16)
      )
  }, bg = "#ffffff")
  
  # ── End Global Regression Server ─────────────────────────────────────────
  
  
}

shinyApp(ui, server)
