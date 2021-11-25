rm(list = ls())

# https://urbaninstitute.github.io/r-at-urban/graphics-guide.html
# install.packages("devtools")
# devtools::install_github("UrbanInstitute/urbnthemes")
# https://stackoverflow.com/questions/40980189/removing-latitude-and-longitude-labels-in-ggplot

library(tidyverse)
library(urbnmapr)
library(scales)
library(urbnthemes)
library(ggthemes)
library(plotly)

set_urbn_defaults(style = "print")


p1 <- countydata %>% 
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
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) 

ggsave("ca_plot.jpg", plot = p1)
  # theme_urbn_map()

# ggplotly(p1)
household_data <- left_join(countydata, counties, by = "county_fips") 

household_data %>%
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Median Household Income")

household_data %>%
  ggplot(aes(long, lat, group = group, fill = horate)) +
  geom_polygon(color = NA) +
  urbnthemes::scale_fill_gradientn(labels = scales::percent,
                       guide = guide_colorbar(title.position = "bottom")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Homeownership rate") +
  # hide original axis labels
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) 
  # theme_map() + theme(legend.position = "top")
  # theme_urbn_map()
  # theme(legend.position = "top")

