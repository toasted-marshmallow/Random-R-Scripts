rm(list = ls())

library(tidyverse)
library(urbnmapr)
library(scales)
library(urbnthemes)
library(ggthemes)
library(plotly)
library(urbnthemes)
library(ggsci)
library(sf)
library(lubridate)

setwd("C:\\Users\\natha\\CDC Case Rate County Data")

data <- read.csv("United_States_COVID19_County_Level_of_Community_Transmission.csv", header = TRUE,
                 fileEncoding = "UTF-8-BOM", na = c("suppressed", "", " "))

data$report_date <- as.Date(data$report_date, format = "%m/%d/%Y")
data$cases_per_100K_7_day_count_change <- as.numeric(data$cases_per_100K_7_day_count_change)
data$community_transmission_level <- ordered(data$community_transmission_level, levels = c("low",
                                                                                           "moderate",
                                                                                           "high",
                                                                                           "substantial"))

counties$county_fips <- as.numeric(counties$county_fips)

data <- data %>%
  filter(report_date >= "2021-11-18" & report_date <= "2021-11-25")

data2 <- data %>%
  group_by(fips_code) %>%
  na.omit() %>%
  mutate(current_7_day_avg <- sum(cases_per_100K_7_day_count_change) / 7)

counties_sf <- get_urbn_map(map = "counties", sf = TRUE)
counties_sf$county_fips <- as.numeric(counties_sf$county_fips)

# joining CDC data with county data
# there are duplications because in the counties data set 
# each counties have more than one long/lat entries
# this is because these long/lats are used to create the geom_polygon
CDC_data <- left_join(x = data, y = counties, by = c("fips_code" = "county_fips"))

CDC_data <- left_join(x = data, y = counties_sf, by = c("fips_code" = "county_fips"))

library(plotly)
CDC_data %>%
  # filter(state_name.x == "New Mexico") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = cases_per_100K_7_day_count_change, geometry = geometry),
          color = "black", size = 0.25) +
  coord_sf(datum = NA, crs = 5070) +
  scale_fill_gradientn(labels = scales::comma,
                       guide_colorbar(title = "Cases per 100k \n 7-day average",
                                      title.position = "top",
                                      barwidth = 12,
                                      barheight = 1))

ggplotly(p1)

ggsave("cases_per_100k_7_day_avg.jpg", dpi = 800, width = 12, height = 8)
  