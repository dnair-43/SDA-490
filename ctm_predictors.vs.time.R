library(dplyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(haven)

setwd("~/Uni/Year 5/semester 2/Data")

# ── Load CTM (Canadian survey data) ──────────────────────────────
ctm_raw <- read_sav("CTM_DATA.sav")

auth_vars_ctm <- c("HD3", "V3", "ATT10", "HD4", "V2")

ctm <- ctm_raw |>
  rowwise() |>
  mutate(auth_index = mean(c_across(all_of(auth_vars_ctm)), na.rm = TRUE)) |>
  ungroup()

# ── Province mapping (same as your main script) ───────────────────
region_cols <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12","R13")

region_to_province <- c(
  R1  = "Newfoundland and Labrador",
  R2  = "Prince Edward Island",
  R3  = "Nova Scotia",
  R4  = "New Brunswick",
  R5  = "Quebec",
  R6  = "Ontario",
  R7  = "Manitoba",
  R8  = "Saskatchewan",
  R9  = "Alberta",
  R10 = "British Columbia",
  R11 = "Yukon",
  R12 = "Northwest Territories",
  R13 = "Nunavut"
)

# Assign province per respondent
ctm_prov <- ctm |>
  mutate(.row_id = row_number()) |>
  tidyr::pivot_longer(
    cols      = any_of(region_cols),
    names_to  = "reg_col",
    values_to = "reg_val"
  ) |>
  filter(!is.na(reg_val)) |>
  mutate(province = region_to_province[reg_col]) |>
  filter(!is.na(province)) |>
  distinct(.row_id, province, .keep_all = TRUE)

# ── Aggregate to province level ───────────────────────────────────
df <- ctm_prov |>
  group_by(province) |>
  summarise(
    auth_index      = mean(auth_index,  na.rm = TRUE),  # outcome
    polarization    = sd(auth_index,    na.rm = TRUE),  # spread of opinions
    resource_curse  = mean(ATT10,       na.rm = TRUE),  # democratic resilience proxy
    digital_control = mean(V3,          na.rm = TRUE),  # free expression attitudes
    party_strength  = mean(HD3,         na.rm = TRUE),  # institutional trust proxy
    gini            = mean(HD4,         na.rm = TRUE),  # felt economic unfairness
    national_income = mean(V2,          na.rm = TRUE),  # economic wellbeing proxy
    .groups = "drop"
  )

# ── Plot function ─────────────────────────────────────────────────
make_plot <- function(x_var, x_label) {
  ggplot(df |> filter(!is.na(.data[[x_var]])),
         aes(x = .data[[x_var]], y = auth_index, label = province)) +
    geom_point(size = 3, colour = "#c0392b", alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE,
                colour = "black", fill = "grey80", linewidth = 0.8, alpha = 0.2) +
    geom_text_repel(size = 2.8, colour = "grey30",
                    box.padding = 0.4, max.overlaps = 13) +
    labs(title = paste("Canadian Auth Index vs.", x_label),
         subtitle = "Each point = one Canadian province (CTM survey data)",
         x = x_label, y = "Mean Authoritarian Attitude Index") +
    theme_minimal()
}

# ── Save all 7 plots ──────────────────────────────────────────────
plots <- list(
  "CA_01_polarization.png"    = make_plot("polarization",    "Polarization (SD of Auth Index)"),
  "CA_02_resource_curse.png"  = make_plot("resource_curse",  "Democratic Resilience (ATT10)"),
  "CA_03_national_income.png" = make_plot("national_income", "Economic Wellbeing (V2)"),
  "CA_04_gini.png"            = make_plot("gini",            "Felt Economic Unfairness / Gini Proxy (HD4)"),
  "CA_05_digital_control.png" = make_plot("digital_control", "Free Expression Attitudes / Digital Control (V3)"),
  "CA_06_party_strength.png"  = make_plot("party_strength",  "Institutional Trust / Party Strength (HD3)"),
  "CA_07_wvs_auth_index.png"  = make_plot("auth_index",      "Auth Index (outcome — self vs. self check)")
)

for (fname in names(plots)) {
  ggsave(fname, plot = plots[[fname]], width = 10, height = 6, dpi = 150)
  message("Saved: ", fname)
}