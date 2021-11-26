rm(list = ls())

# https://urbaninstitute.github.io/r-at-urban/graphics-guide.html
# install.packages("devtools")
# devtools::install_github("UrbanInstitute/urbnthemes")
# https://stackoverflow.com/questions/40980189/removing-latitude-and-longitude-labels-in-ggplot
# https://urbaninstitute.github.io/urbnmapr/articles/introducing-urbnmapr.html

library(tidyverse)
library(urbnmapr)
library(scales)
library(urbnthemes)
library(ggthemes)
library(plotly)
library(urbnthemes)
library(ggsci)
library(sf)

set_urbn_defaults(style = "map")

# set_urbn_defaults(style = "print")


p1 <- countydata %>% 
  left_join(counties, by = "county_fips") %>% 
  filter(state_name == "California") %>% 
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
        panel.grid.minor.x = element_blank()) +
  theme_urbn_map()

p1
# ggsave("ca_plot.jpg", plot = p1)
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
        panel.grid.minor.x = element_blank()) +
  theme_urbn_map()
# theme_map() + theme(legend.position = "top")
# theme_urbn_map()
# theme(legend.position = "top")

library(urbnthemes)


statedata %>% 
  left_join(states, by = "state_name") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = horate)) +
  geom_polygon(color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn(labels = scales::percent) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(face = "bold", size = 11),
        legend.key.height = unit(.2, "in")) +
  labs(fill = "Homeownership rate")

blah <- household_data %>%
  rename("Median Household Income" = medhhincome)

g2 <- blah %>%
  filter(state_name == "New Mexico") %>%
  ggplot(aes(long, lat, group = group, fill = `Median Household Income`)) +
  geom_polygon(color = "black", size = 0.05)+
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn(labels = scales::dollar) + 
  # scale_fill_gradient(low = "yellow", high = "red", labels = scales::dollar) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(face = "bold", size = 11),
        legend.key.height = unit(.25, "in")) +
  labs(fill = "Median Household Income",
       title = "Median Household Income in New Mexico")

g2
ggplotly(g2)

g1 <- household_data %>%
  filter(state_name %in% c("Virginia", "Maryland", "District of Columbia")) %>%
  # rename("Median Household Income" = medhhincome) %>%
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  geom_polygon(color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn(labels = scales::dollar) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(face = "bold", size = 11),
        legend.key.height = unit(.25, "in")) +
  labs(fill = "Median household income")

g1
# ggplotly(g1)

# geom_sf() method --------------------------------------------------------

states_sf <- get_urbn_map("states", sf = TRUE)

counties_sf <- get_urbn_map("counties", sf = TRUE)

territories_counties <- get_urbn_map(map = "territories_counties", sf = TRUE)

spatial_data <- left_join(statedata,
                          get_urbn_map(map = "states", sf = TRUE),
                          by = "state_name")

states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")

counties_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")

ggplot() +
  geom_sf(territories_counties,
          mapping = aes(),
          fill = "grey", color = "#ffffff")

states_sf %>%
  ggplot() +
  geom_sf(aes(), 
          fill = "grey", color = "#ffffff", size = 0.25) +
  geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE), 
               aes(label = state_abbv), 
               size = 3)

# need to add geometry = geometry inside aes
# https://community.rstudio.com/t/stats-sf-cant-find-geometry-aesthetics/90643
ggplot() +
  geom_sf(spatial_data,
          mapping = aes(fill = horate, geometry = geometry),
          color = "#ffffff", size = 0.25) +
  labs(fill = "Homeownership rate")


counties_sf <- get_urbn_map(map = "counties", sf = TRUE)
county_groups <- countydata %>% 
  mutate(cat_var = paste0("Group ",
                          sample(1:4, nrow(countydata), replace = TRUE)))

household_data <- left_join(county_groups, counties_sf, by = "county_fips")

household_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = cat_var, geometry = geometry),
          color = NA, size = 0.05) +
  labs(fill = "Categorical variable")

household_data2 <- household_data
# "+init=epsg:3857" whatever that does, makes the state not "titled"
# don't know what it means, but it looks nicer
# https://stackoverflow.com/questions/44887635/plotting-a-sf-object-with-geom-sf-with-any-projection-other-than-lat-long
household_data2$geometry <- st_transform(household_data2$geometry, "+init=epsg:3857")


household_data2 %>%
  filter(state_name %in% c("Utah", "Arizona", "New Mexico", "Colorado")) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = medhhincome, geometry = geometry),
          color = "black", size = 0.25) +
  coord_sf(datum = NA, crs = 5070) +
  # coord_sf(crs = 102003) +
  scale_fill_gradientn(labels = scales::dollar,
                       guide_colorbar(title = "Median Household Income",
                                      title.position = "top",
                                      barwidth = 0.5,
                                      barheight = 4)) +
  guides(fill = guide_colorbar(title = "Median Household Income",
                               barwidth = 12,
                               barheight = 1)) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("map1.jpg", dpi = 800, width = 12, height = 8)

household_data2 %>%
  filter(state_name %in% c("New Mexico", "Texas")) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = medhhincome, geometry = geometry),
          color = "black", size = 0.25) +
  coord_sf(datum = NA, crs = 5070) +
  # coord_sf(crs = 102003) +
  scale_fill_gradientn(labels = scales::dollar,
                       guide_colorbar(title = "Median Household Income",
                                      title.position = "top",
                                      barwidth = 0.5,
                                      barheight = 4)) +
  guides(fill = guide_colorbar(title = "Median Household Income",
                               barwidth = 16,
                               barheight = 1)) +
  theme_minimal() +
  theme(legend.position = "top")


  # labs(fill = "Median household income") +
  # theme(legend.position = "bottom")
  # theme_urbn_map()

# coord_map(projection = "albers", lat0 = 39, lat1 = 45)
  
# https://datavizm20.classes.andrewheiss.com/example/12-example/
