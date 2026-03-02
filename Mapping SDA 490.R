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
data(continental_us_states)

library(tidygeocoder)
df <- tidygeocoder::geocode(df, address = addr, method = "osm")

library(ggmap)


library(mapview)
library(sf)
mymap <- st_as_sf(homicide, coords = c("lon", "lat"), crs = 4326)
mapview(mymap)


# load the world map into R 
world <- ne_countries()
ggplot(world, aes(geometry = geometry )) +
  geom_sf()

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

install.packages("electiondata")
states <- map_data("state")

election2020 <- data.frame(
  region = c(
    "alabama","alaska","arizona","arkansas","california","colorado","connecticut","delaware",
    "florida","georgia","hawaii","idaho","illinois","indiana","iowa","kansas","kentucky",
    "louisiana","maine","maryland","massachusetts","michigan","minnesota","mississippi",
    "missouri","montana","nebraska","nevada","new hampshire","new jersey","new mexico",
    "new york","north carolina","north dakota","ohio","oklahoma","oregon","pennsylvania",
    "rhode island","south carolina","south dakota","tennessee","texas","utah","vermont",
    "virginia","washington","west virginia","wisconsin","wyoming"
  ),
  party = c(
    "Republican","Republican","Democrat","Republican","Democrat","Democrat","Democrat","Democrat",
    "Republican","Democrat","Democrat","Republican","Democrat","Republican","Republican",
    "Republican","Republican","Republican","Democrat","Democrat","Democrat","Democrat",
    "Democrat","Republican","Republican","Republican","Republican","Democrat","Democrat",
    "Democrat","Democrat","Democrat","Republican","Republican","Republican","Republican",
    "Democrat","Democrat","Democrat","Republican","Republican","Republican","Republican",
    "Republican","Democrat","Democrat","Democrat","Republican","Democrat","Republican"
  )
)

map_df <- left_join(states, election2020, by="region")

ggplot(map_df, aes(long, lat, group=group, fill=party)) +
  geom_polygon(color="white") +
  coord_fixed(1.3) +
  scale_fill_manual(values=c("Democrat"="blue","Republican"="red")) +
  labs(title="2020 U.S. Presidential Election by State") +
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
