# Time Series Analysis Script

# This script runs the time series analysis of soil temperature for the R shiny app. 
# soil temperature data obtained from: https://dendra.science/
# Author: Tanvi Shah
# Date: 3/16/2025

library(tidyverse)
library(patchwork)
library(janitor)
library(here)
library(tsibble)
library(feasts)
library(fable)
library(zoo)
library(ggplot2)

soil_data <- read_csv(here("shiny_sierra_biodiversity_or_ts_ct/data/soil_data.csv"))
soil_data_daily <- soil_data %>% clean_names() %>%
  mutate(date = lubridate::mdy_hm(time)) %>% 
  mutate(date = floor_date(date, unit = "day")) %>%
  group_by(date) %>%
  summarize(mean_50mm = mean(x50mm_deg_c), mean_500mm = mean(x500mm_deg_c))


# use lubridate package to reformat date
soil_ts <- soil_data_daily %>%
  mutate(date = as.Date(date)) %>%
  as_tsibble(key = NULL, 
             index = date)  ## time index, here our column is `date`






# season plots
soil_plot_50mm <- 
  gg_season(data = soil_ts, y = mean_50mm, pal = hcl.colors(n = 9)) +
  scale_fill_gradientn(colours = terrain.colors(13),
    limits = as.Date(c("2012-10-01", "2025-01-01")),  # Specify limits (if needed)
    breaks = seq(as.Date("2012-10-01"), as.Date("2025-01-01"), by = "1 year"))+ # Define the tick marks (e.g., by year)
  theme_minimal()+
  labs(x = "Month")+
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 13))+
  ggtitle("50 mm Soil Depth")

soil_plot_500mm <- 
  soil_ts %>% 
  gg_season(y = mean_500mm, pal = hcl.colors(n = 9)) +
  scale_fill_discrete()+
  theme_minimal() +
  theme(axis.title.y = element_blank(), axis.text = element_text(size = 13))+
  labs(x = "Month")+
  ggtitle("500 mm Soil Depth")



soil_plot <- soil_plot_50mm / soil_plot_500mm + plot_layout(guides = "collect")+
  plot_annotation(caption = "Seasonal analysis of soil temperature at 500mm depth shows slight decrease, while temperature at 50mm shows no observable trend")
soil_plot


save(soil_plot, file = here("shiny_sierra_biodiversity_or_ts_ct/data/soil_plot.rdata" ))

