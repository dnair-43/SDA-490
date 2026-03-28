library(dplyr)
library(ggplot2)
library(ggrepel)
library(readr)

setwd("~/Uni/Year 5/semester 2/Data")

# ── Load V-Dem ────────────────────────────────────────────────────
vdem_raw <- read_csv("V-Dem-CD-v16.csv", show_col_types = FALSE)

countries <- c("Iran", "Venezuela", "United States of America", "Mexico", "China",
               "Russia", "Algeria", "Libya", "Australia", "United Kingdom",
               "Turkey", "Norway", "Poland", "Japan", "South Korea",
               "Indonesia", "Vietnam", "Argentina")

vdem <- vdem_raw |>
  filter(country_name %in% countries, year >= 1990, year <= 2024) |>
  select(country = country_name, year,
         vdem_score      = v2x_polyarchy,
         polarization    = v2smpolsoc,
         resource_curse  = v2xnp_pres,
         digital_control = v2smgovdom,
         party_strength  = v2x_partipdem) |>
  mutate(digital_control = -digital_control)

# ── Load WVS ─────────────────────────────────────────────────────
wvs_data <- read_csv("WVS.csv")

auth_vars <- c("E114","E116","E115","E018","A042","E225","E228","A124_06","A124_12","A124_43")

wvs <- wvs_data |>
  rowwise() |>
  mutate(auth_index = mean(c_across(all_of(auth_vars)), na.rm = TRUE)) |>
  ungroup() |>
  mutate(country = recode(country,
                          "United States" = "United States of America",
                          "Great Britain" = "United Kingdom")) |>
  filter(country %in% countries) |>
  group_by(country, year) |>
  summarise(
    wvs_auth_index  = mean(auth_index,   na.rm = TRUE),
    national_income = mean(gdp_per_cap,  na.rm = TRUE),
    gini            = mean(gini,         na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(national_income = log(national_income + 1))

# ── Snap V-Dem years to nearest WVS wave and join ─────────────────
waves <- c(1994, 1999, 2004, 2009, 2014, 2020)

df <- vdem |>
  mutate(wvs_year = waves[sapply(year, \(y) which.min(abs(waves - y)))]) |>
  left_join(wvs, by = c("country", "wvs_year" = "year"))

# ── Labels at most recent year per country ────────────────────────
lab <- df |> group_by(country) |> slice_max(year, n = 1) |> ungroup()


# ── Plot 1: Polarization ──────────────────────────────────────────
p1 <- ggplot(df |> filter(!is.na(polarization)),
             aes(x = polarization, y = vdem_score, colour = country, group = country)) +
  geom_path(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE,
              colour = "black", fill = "grey80", linewidth = 0.8, alpha = 0.2) +
  geom_text_repel(data = lab |> filter(!is.na(polarization)),
                  aes(label = country), size = 2.5, show.legend = FALSE) +
  labs(title = "V-Dem Score vs. Polarization",
       x = "Polarization", y = "V-Dem Score (0–1)", colour = "Country") +
  theme_minimal()

ggsave("01_polarization.png", plot = p1, width = 10, height = 6, dpi = 150)


# ── Plot 2: Resource Curse ────────────────────────────────────────
p2 <- ggplot(df |> filter(!is.na(resource_curse)),
             aes(x = resource_curse, y = vdem_score, colour = country, group = country)) +
  geom_path(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE,
              colour = "black", fill = "grey80", linewidth = 0.8, alpha = 0.2) +
  geom_text_repel(data = lab |> filter(!is.na(resource_curse)),
                  aes(label = country), size = 2.5, show.legend = FALSE) +
  labs(title = "V-Dem Score vs. Resource Curse",
       x = "Resource Curse", y = "V-Dem Score (0–1)", colour = "Country") +
  theme_minimal()

ggsave("02_resource_curse.png", plot = p2, width = 10, height = 6, dpi = 150)


# ── Plot 3: National Income ───────────────────────────────────────
p3 <- ggplot(df |> filter(!is.na(national_income)),
             aes(x = national_income, y = vdem_score, colour = country, group = country)) +
  geom_path(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE,
              colour = "black", fill = "grey80", linewidth = 0.8, alpha = 0.2) +
  geom_text_repel(data = lab |> filter(!is.na(national_income)),
                  aes(label = country), size = 2.5, show.legend = FALSE) +
  labs(title = "V-Dem Score vs. National Income ln(GDP/cap)",
       x = "National Income ln(GDP/cap)", y = "V-Dem Score (0–1)", colour = "Country") +
  theme_minimal()

ggsave("03_national_income.png", plot = p3, width = 10, height = 6, dpi = 150)


# ── Plot 4: Gini Inequality ───────────────────────────────────────
p4 <- ggplot(df |> filter(!is.na(gini)),
             aes(x = gini, y = vdem_score, colour = country, group = country)) +
  geom_path(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE,
              colour = "black", fill = "grey80", linewidth = 0.8, alpha = 0.2) +
  geom_text_repel(data = lab |> filter(!is.na(gini)),
                  aes(label = country), size = 2.5, show.legend = FALSE) +
  labs(title = "V-Dem Score vs. Gini Inequality",
       x = "Gini Inequality", y = "V-Dem Score (0–1)", colour = "Country") +
  theme_minimal()

ggsave("04_gini.png", plot = p4, width = 10, height = 6, dpi = 150)


# ── Plot 5: Digital Control ───────────────────────────────────────
p5 <- ggplot(df |> filter(!is.na(digital_control)),
             aes(x = digital_control, y = vdem_score, colour = country, group = country)) +
  geom_path(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE,
              colour = "black", fill = "grey80", linewidth = 0.8, alpha = 0.2) +
  geom_text_repel(data = lab |> filter(!is.na(digital_control)),
                  aes(label = country), size = 2.5, show.legend = FALSE) +
  labs(title = "V-Dem Score vs. Digital Control",
       x = "Digital Control", y = "V-Dem Score (0–1)", colour = "Country") +
  theme_minimal()

ggsave("05_digital_control.png", plot = p5, width = 10, height = 6, dpi = 150)


# ── Plot 6: Party Strength ────────────────────────────────────────
p6 <- ggplot(df |> filter(!is.na(party_strength)),
             aes(x = party_strength, y = vdem_score, colour = country, group = country)) +
  geom_path(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE,
              colour = "black", fill = "grey80", linewidth = 0.8, alpha = 0.2) +
  geom_text_repel(data = lab |> filter(!is.na(party_strength)),
                  aes(label = country), size = 2.5, show.legend = FALSE) +
  labs(title = "V-Dem Score vs. Party Strength",
       x = "Party Strength", y = "V-Dem Score (0–1)", colour = "Country") +
  theme_minimal()

ggsave("06_party_strength.png", plot = p6, width = 10, height = 6, dpi = 150)


# ── Plot 7: WVS Auth Index ────────────────────────────────────────
p7 <- ggplot(df |> filter(!is.na(wvs_auth_index)),
             aes(x = wvs_auth_index, y = vdem_score, colour = country, group = country)) +
  geom_path(alpha = 0.3) +
  geom_point(alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE,
              colour = "black", fill = "grey80", linewidth = 0.8, alpha = 0.2) +
  geom_text_repel(data = lab |> filter(!is.na(wvs_auth_index)),
                  aes(label = country), size = 2.5, show.legend = FALSE) +
  labs(title = "V-Dem Score vs. WVS Auth Index",
       x = "WVS Auth Index", y = "V-Dem Score (0–1)", colour = "Country") +
  theme_minimal()

ggsave("07_wvs_auth_index.png", plot = p7, width = 10, height = 6, dpi = 150)