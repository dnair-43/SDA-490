library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(haven)
library(WDI)

setwd("~/Uni/Year 5/semester 2/Data")

# ── Output directory ──────────────────────────────────────────────────────────
out_dir <- "predictor_plots"
dir.create(out_dir, showWarnings = FALSE)

# =============================================================================
# COUNTRY LOOKUPS
# =============================================================================
# WVS COW alpha-3 -> display label (10 countries)
cow_to_label <- c(
  "IND" = "India",
  "BLR" = "Belarus",
  "HUN" = "Hungary",
  "VEN" = "Venezuela",
  "RUS" = "Russia",
  "USA" = "United States"
)

target_cow <- names(cow_to_label)

# Display label -> V-Dem country_name
label_to_vdem <- c(
  "India"         = "India",
  "Belarus"       = "Belarus",
  "Hungary"       = "Hungary",
  "Venezuela"     = "Venezuela",
  "Russia"        = "Russia",
  "United States" = "United States of America",
  "Canada"        = "Canada"
)

# Display label -> ISO2 for WDI
label_to_iso2 <- c(
  "India"         = "IN",
  "Belarus"       = "BY",
  "Hungary"       = "HU",
  "Venezuela"     = "VE",
  "Russia"        = "RU",
  "United States" = "US",
  "Canada"        = "CA"
)

# World Bank GINI name -> display label
wb_to_label <- c(
  "India"              = "India",
  "Belarus"            = "Belarus",
  "Hungary"            = "Hungary",
  "Venezuela, RB"      = "Venezuela",
  "Russian Federation" = "Russia",
  "United States"      = "United States",
  "Canada"             = "Canada"
)

all_countries <- unname(cow_to_label)   # the 10 WVS countries (Canada added separately)

# =============================================================================
# 1. WVS: authoritarian attitudes index (10 countries only — Canada is CTM)
# =============================================================================
wvs_path <- "WVS_TimeSeries_1981_2020_R_v2_0.rdata"
load(wvs_path)
wvs_data <- `WVS_TimeSeries_1981_2020_v2_0.`

auth_vars_wvs <- c("E114", "E116", "E115", "E018", "A042",
                   "E225", "E228", "A124_06", "A124_12", "A124_43")

wvs_year_col <- intersect(c("S020", "year", "YEAR"), names(wvs_data))[1]
wvs_cow_col  <- intersect(c("COW_ALPHA", "S003_ISO", "B_COUNTRY_ALPHA"), names(wvs_data))[1]

wvs_auth <- wvs_data |>
  rename(cow_code = all_of(wvs_cow_col),
         Year     = all_of(wvs_year_col)) |>
  filter(cow_code %in% target_cow,
         Year >= 1980, Year <= 2025) |>
  mutate(
    country = cow_to_label[cow_code],
    across(all_of(auth_vars_wvs), ~ suppressWarnings(as.numeric(.)))
  ) |>
  rowwise() |>
  mutate(auth_index = mean(c_across(all_of(auth_vars_wvs)), na.rm = TRUE)) |>
  ungroup() |>
  group_by(country, Year) |>
  summarise(auth_index = mean(auth_index, na.rm = TRUE), .groups = "drop")

# =============================================================================
# 2. CTM: authoritarian attitudes index for Canada (single wave, 2025)
# =============================================================================
ctm_raw <- read_sav("CTM_DATA.sav")

auth_vars_ctm <- c("HD3", "V3", "ATT10", "HD4", "V2")

ctm_auth <- ctm_raw |>
  rowwise() |>
  mutate(auth_index = mean(c_across(all_of(auth_vars_ctm)), na.rm = TRUE)) |>
  ungroup() |>
  summarise(auth_index = mean(auth_index, na.rm = TRUE)) |>
  mutate(country = "Canada", Year = 2025L)

# =============================================================================
# 3. V-Dem: structural predictors for all 7 countries (6 + Canada)
# =============================================================================
vdem_raw <- read_csv("V-Dem-CY-Full+Others-v16.csv", show_col_types = FALSE)

vdem_all <- vdem_raw |>
  filter(country_name %in% label_to_vdem,
         year >= 1980, year <= 2025) |>
  transmute(
    country         = names(label_to_vdem)[match(country_name, label_to_vdem)],
    Year            = year,
    vdem_score      = v2x_polyarchy,
    polarization    = v2cacamps,
    resource_curse  = v2lgfunds,
    digital_control = v2mecenefm,
    party_strength  = v2xps_party
  )

# =============================================================================
# 4. World Bank: GNI per capita (PPP) -> logged_wealth for all 7 countries
# =============================================================================
iso2_to_label <- setNames(names(label_to_iso2), label_to_iso2)

wb_gni_raw <- WDI(
  indicator = "NY.GNP.PCAP.PP.CD",
  country   = unname(label_to_iso2),
  start     = 1980,
  end       = 2025
)

wb_gni <- wb_gni_raw |>
  filter(!is.na(NY.GNP.PCAP.PP.CD)) |>
  transmute(
    country       = iso2_to_label[iso2c],
    Year          = year,
    logged_wealth = log(NY.GNP.PCAP.PP.CD)
  ) |>
  filter(!is.na(country))

# =============================================================================
# 5. GINI for all 7 countries
# =============================================================================
gini_raw <- read_csv("GINI_DATA.csv", show_col_types = FALSE)

year_cols <- names(gini_raw)[grepl("^\\d{4}$", names(gini_raw))]

gini_all <- gini_raw |>
  filter(`Country Name` %in% names(wb_to_label)) |>
  mutate(country = wb_to_label[`Country Name`]) |>
  select(country, all_of(year_cols)) |>
  pivot_longer(-country, names_to = "Year", values_to = "gini") |>
  mutate(Year = as.integer(Year),
         gini = suppressWarnings(as.numeric(gini))) |>
  filter(Year >= 1980, Year <= 2025, !is.na(gini))

# =============================================================================
# 6. Merge V-Dem + WDI + GINI + WVS auth index + CTM for Canada
# =============================================================================
# Combine WVS auth (6 countries) and CTM auth (Canada) into one auth table
auth_combined <- bind_rows(wvs_auth, ctm_auth)

df <- vdem_all |>
  left_join(wb_gni,        by = c("country", "Year")) |>
  left_join(gini_all,      by = c("country", "Year")) |>
  left_join(auth_combined, by = c("country", "Year"))

# =============================================================================
# 7. Standardize all predictors (z-score within each country)
#    This puts every variable on the same scale for comparison
# =============================================================================
pred_vars <- c("vdem_score", "polarization", "resource_curse",
               "digital_control", "party_strength", "logged_wealth",
               "gini", "auth_index")

df_scaled <- df |>
  group_by(country) |>
  mutate(across(all_of(pred_vars), ~ as.numeric(scale(.)), .names = "{.col}")) |>
  ungroup()

# =============================================================================
# 8. Pivot to long format for faceted plot
# =============================================================================
pred_labels <- c(
  vdem_score      = "Electoral Democracy Score",
  polarization    = "Political Polarization",
  resource_curse  = "Legislature Resource Funding",
  digital_control = "Digital Control",
  party_strength  = "Party Strength",
  logged_wealth   = "Logged Wealth",
  gini            = "Gini Index",
  auth_index      = "Authoritarian Attitudes Index"
)

df_long <- df_scaled |>
  pivot_longer(cols = all_of(pred_vars),
               names_to  = "predictor",
               values_to = "z_score") |>
  filter(!is.na(z_score)) |>
  mutate(
    predictor = factor(predictor, levels = names(pred_labels),
                       labels = pred_labels),
    is_canada = country == "Canada"
  )

# =============================================================================
# 9. Colour palettes
# =============================================================================
canada_colour <- "#e74c3c"

comparison_colours <- c(
  "India"         = "#e67e22",
  "Belarus"       = "#9b59b6",
  "Hungary"       = "#2ecc71",
  "Venezuela"     = "#1abc9c",
  "Russia"        = "#3498db",
  "United States" = "#e74c3c"
)

# Shared theme
plot_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.text      = element_text(size = 8),
    strip.text       = element_text(face = "bold", size = 8),
    axis.text.x      = element_text(size = 7)
  )

# =============================================================================
# 10. Plot 1: Canada only
# =============================================================================
df_canada <- df_long |> filter(country == "Canada")

p_canada <- ggplot(df_canada, aes(x = Year, y = z_score, group = 1)) +
  geom_line(colour = canada_colour, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  facet_wrap(~ predictor, ncol = 4, scales = "free_y") +
  scale_x_continuous(breaks = seq(1980, 2025, by = 15)) +
  labs(
    title    = "Canada: Standardized Predictors Over Time",
    subtitle = "Z-scores (mean = 0, sd = 1) within Canada",
    x        = "Year",
    y        = "Standardized Score (z)",
    caption  = "Sources: V-Dem v16; World Bank (GNI per capita PPP, GINI); CTM Survey (Auth Index)"
  ) +
  plot_theme

# =============================================================================
# 11. Plot 2: 10 comparison countries
# =============================================================================
df_comparison <- df_long |> filter(country != "Canada")

p_comparison <- ggplot(df_comparison, aes(x = Year, y = z_score,
                                          colour = country, group = country)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  scale_colour_manual(values = comparison_colours, name = NULL) +
  facet_wrap(~ predictor, ncol = 4, scales = "free_y") +
  scale_x_continuous(breaks = seq(1980, 2025, by = 15)) +
  labs(
    title    = "Comparison Countries: Standardized Predictors Over Time",
    subtitle = "Z-scores (mean = 0, sd = 1) within each country",
    x        = "Year",
    y        = "Standardized Score (z)",
    caption  = "Sources: V-Dem v16; World Bank (GNI per capita PPP, GINI); WVS Time Series 1981-2020"
  ) +
  plot_theme +
  guides(colour = guide_legend(nrow = 2))

# =============================================================================
# 12. Save both plots
# =============================================================================
ggsave(file.path(out_dir, "01_canada_standardized_predictors.png"),
       plot = p_canada, width = 14, height = 8, dpi = 150)
message("Saved: 01_canada_standardized_predictors.png")

ggsave(file.path(out_dir, "02_comparison_standardized_predictors.png"),
       plot = p_comparison, width = 16, height = 10, dpi = 150)
message("Saved: 02_comparison_standardized_predictors.png")

message("\nDone. Both plots saved to: ", out_dir, "/")