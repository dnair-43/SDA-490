library(dplyr)
library(ggplot2)
library(readr)

setwd("~/Uni/Year 5/semester 2/Data")

vdem_raw <- read_csv("V-Dem-CD-v16.csv", show_col_types = FALSE)

df <- vdem_raw |>
  filter(country_name == "Canada", year >= 1980, year <= 2024) |>
  select(year, vdem_score = v2x_polyarchy)

ggplot(df, aes(x = year, y = vdem_score)) +
  geom_line(colour = "#c0392b", linewidth = 1) +
  geom_point(colour = "#c0392b", size = 2) +
  labs(title = "Canada — V-Dem Electoral Democracy Score Over Time",
       x = "Year", y = "V-Dem Score (0–1)") +
  theme_minimal()

ggsave("canada_vdem_timeseries.png", width = 10, height = 6, dpi = 150)
message("Saved: canada_vdem_timeseries.png")
