rm(list = ls())

# https://urbaninstitute.github.io/r-at-urban/graphics-guide.html
# install.packages("devtools")
# devtools::install_github("UrbanInstitute/urbnthemes")

library(tidyverse)
library(urbnmapr)
library(scales)
library(urbnthemes)

set_urbn_defaults(style = "print")


countydata %>% 
  left_join(counties, by = "county_fips") %>% 
  filter(state_name =="California") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = horate)) +
  geom_polygon(color = "#ffffff", size = .25) +
  urbnthemes::scale_fill_gradientn(labels = scales::percent,
                       guide = guide_colorbar(title.position = "top")) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Homeownership rate") +
  theme_urbn_map()


household_data <- left_join(countydata, counties, by = "county_fips") 

household_data %>%
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Median Household Income")

require(urbnthemes)
household_data %>%
  ggplot(aes(long, lat, group = group, fill = horate)) +
  geom_polygon(color = NA) +
  urbnthemes::scale_fill_gradientn(labels = scales::percent,
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Homeownership rate") +
  theme_urbn_map()

