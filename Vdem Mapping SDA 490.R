rm(list=ls())

library(tidyverse)
library(rio)
library(plotly)
library(sf)
library(ggplot2)

#install.packages("choroplethr", dependencies = TRUE)
install.packages("ggrepel")

install.packages("choroplethrMaps")
library(choroplethrMaps)
vdem_data <- read_csv("VDEM.csv")
head(vdem_data)
data(vdem_data)

install.packages("ggmap")
library(ggmap)


# load the world map into R 
install.packages("rnaturalearth")
#install.packages("sf")
library(rnaturalearth)
library(sf)
library(ggplot2)


world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(world)+
  geom_sf(aes(geometry = geometry)) +
  theme_void()

# Canada 
Canada <- ne_countries(country = "Canada")
ggplot(Canada) +
  geom_sf() 


# USA
us_states <- ne_states(country = "United States of America")
ggplot(us_states) +
  geom_sf()+
  coord_sf(
    xlim = c(-125, -66), # longitude
    ylim = c(24,50), # latitude
    expand = FALSE
  )
#v2x_polyarchy is our choice variable 

#filter for only nato countries 
nato_members <- c(
  "Albania","Belgium","Bulgaria","Canada","Croatia","Czechia","Denmark",
  "Estonia","Finland","France","Germany","Greece","Hungary","Iceland",
  "Italy","Latvia","Lithuania","Luxembourg","Montenegro","Netherlands",
  "North Macedonia","Norway","Poland","Portugal","Romania","Slovakia",
  "Slovenia","Spain","Sweden","Turkey","United Kingdom","United States"
)

vdem_data <- vdem_data[vdem_data$country_name %in% nato_members, c("country_name", "v2x_polyarchy")]



ggplot(vdem_data, aes(long, lat, group = group, fill = v2x_polyarchy)) +
  geom_polygon(color = "white", size = 0.2) +
  coord_fixed(1.3) +
  scale_fill_gradientn(
    colours = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    labels = c("0", "0.25", "0.50", "0.75", "1"),
    name = "Electoral Democracy (v2x_polyarchy)"
  ) +
  labs(title = "V-Dem Electoral Democracy Index (v2x_polyarchy)") +
  theme_void()


# library(dplyr)
# ggplot(us_states, aes(geometry=geometry, fill=Rate)) +
#    geom_sf() +
#    theme_void() +
#    geom_sf_text(aes(label=STUSPS), size=2) +
#    scale_fill_steps(low="yellow", high="royalblue", 
#                     n.breaks = 10) +
#    labs(title="Literacy Rates by State",
#         fill = "% literate",
#         x = "", y = "",
#         subtitle="Updated May 2023",
#         caption="source: https://worldpopulationreview.com")
