# Author: Olivia Ross, Charlie Thrift, Tanvi Shah
# Date: 16 February 2025

# Objective: This script downloads data, wrangles data, creates figures, and writes figures
# to the project folder to then read into the Shiny App script. 

# Note: All outputs made in this script are stored in the data folder of this project; 
# this script does not need to be run to run the app script. 

# Packages 
librarian::shelf("prism", "here", "ggplot2", "tmap", "sf", "lubridate", "tidyverse", "rlist")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# READING IN DATA

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# This portion of the script downloads climate data from PRISM, which contains
# large rasters. Once you have downloaded these data once to create the plots, 
# do not run this code again. 

# Setting directory for prism data
#prism_set_dl_dir(here("shiny_sierra_biodiversity_or_ts_ct/data", "climate"))

# Downloading climate normals
#get_prism_normals("tmax", "4km", annual = TRUE, keepZip = FALSE)
#get_prism_normals("ppt", "4km", annual = TRUE, keepZip = FALSE)

# Downloading annual climate variables from 2000-2023
#get_prism_annual("tmax", years = 2000:2023, keepZip = FALSE)
#get_prism_annual("ppt", years = 2000:2023, keepZip = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Reading in SNARL spatial data 

# SNARL coordinates 
snarl <- c(-118.83317, 37.61404)

# SNARL reserve boundary
snarl_poly <- st_read(here("shiny_sierra_biodiversity_or_ts_ct/data/SNARL", "SNARL_boundary.shp"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Reading in fire perimeter data & Sierra Nevada border

fire <- st_read(here("shiny_sierra_biodiversity_or_ts_ct/data", "fire_perimeters", "mtbs_perims_DD.shp"))

# Reading in Sierra Nevada Border
snv <- st_read(here("shiny_sierra_biodiversity_or_ts_ct/data", "snv", "Sierra_Nevada_Conservancy_Boundary.shp"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# CREATING FIGURES

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Plot 1: Maximum Annual Temperature at SNARL----------------------------------#

# Grabbing our climate data from the directory
# Note: run code for PRISM download above to run the following two lines of code
tmax <- prism_archive_subset("tmax", "annual", years = 2000:2023)
tmax_normal <- prism_archive_subset("tmax", "annual normals", resolution = "4km")

# slicing climate stack to SNARL location
tmax_snarl <- pd_plot_slice(tmax, snarl)

tmax_snarl <- as.data.frame(tmax_snarl)

# results in a list with 30 year normal value for our chart 
tmax_snarl_normal <- pd_plot_slice(tmax_normal, snarl)

# tmax annuals and tmax normal at SNARL
tmax_snarl_plot <- tmax_snarl +
  geom_hline(yintercept = 16.1, linetype = "dashed", lwd=1.0)+
  geom_line(color="coral2", lwd=1.5) +
  geom_point(alpha = 0.5)+
  labs(x = "Year") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black"))

tmax_snarl_plot

# Saving ggplot object for app
save(tmax_snarl_plot, file = here("shiny_sierra_biodiversity_or_ts_ct/data/tmax_snarl_plot.rdata" ))

# Plot 2: Annual Precipitation at SNARL----------------------------------------#

# Grabbing precipitation data from the directory
# Note: run code for PRISM data download above to run the following two lines of code
ppt <- prism_archive_subset("ppt", "annual", years = 2000:2023)
ppt_normal <- prism_archive_subset("ppt", "annual normals", resolution = "4km")

# Slicing climate stack to SNARL location
ppt_snarl <- pd_plot_slice(ppt, snarl)

# Results in a list with 30 year normal value
ppt_snarl_normal <- pd_plot_slice(ppt_normal, snarl)

# Ppt annuals and ppt normal at SNARL
ppt_snarl_plot <- ppt_snarl +
  geom_hline(yintercept = 309, linetype = "dashed", lwd=1.0)+
  geom_line(color="deepskyblue", lwd=1.5) +
  geom_point(alpha = 0.5)+
  labs(x = "Year") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black"))

ppt_snarl_plot

# Saving object for app 
save(ppt_snarl_plot, file = here("shiny_sierra_biodiversity_or_ts_ct/data/ppt_snarl_plot.rdata" ))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Fire Data Wrangling

# Creating a year column for fire ignition date and selecting wildfires in 2000-2023 

fire_years <- fire |>
  mutate(Year = year(Ig_Date))|>
  filter(Year %in% 2000:2023, Incid_Type == "Wildfire") |>
  select(Year, Incid_Name, Incid_Type, geometry)

# Overlapping vertices, make geometry valid
fire_years <- st_make_valid(fire_years)

# Clipping wildfire perimeters to SNV only (changing crs first)
snv <- st_transform(snv, coords=c("lat", "long"), crs=4326)
fire_years <- st_transform(fire_years, coords=c("lat", "long"), crs=4326)

fire_snv <- st_intersection(fire_years, snv)

# Changing fire year to a factor for the map
fire_snv$Year <- as.factor(fire_snv$Year)

# Making snarl an sf object for the map
y <- 37.61404
x <- -118.83317
snarl_df <- cbind(x, y)
snarl_df <- as.data.frame(snarl_df)
snarl_sf <- st_as_sf(snarl_df, coords = c("x", "y"), crs = 4326)

# Creating a time column and time series for our fire data
fire_snv$Year_numeric <- as.numeric(as.character(fire_snv$Year)) 

class(fire_snv)

fire_time <- ts(fire_snv$Year_numeric, start = 2000, frequency = 1)

# Writing the fire shape file for app script 
fire_snv <- st_write(fire_snv, here("shiny_sierra_biodiversity_or_ts_ct/data", "fire_perimeters","fire_snv.shp"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Interactive Wildfire Map-----------------------------------------------------#
tmap_mode("view")

 tm_shape(snv) +
  tm_borders("black", lwd=1.0) +
  
  tm_shape(fire_snv) +
  tm_fill(col = "red") +

  tm_shape(snarl_poly) +
  tm_dots(col = "orange") + #symbol type not supported in view mode
  
  tm_add_legend(type="fill", label = "SNARL", col = "orange") +
  tm_add_legend(type="fill", label = "Sierra Nevada", col = "black")

# This tmap script was added to the server and made reactive on the app script

 #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
 # Soil Data ------------------------------------------------------------------#

 
 # NOTE: the below script is commented out due to the fact that the soil data is
 # a large file size (18 MB). This script was run previously to generate the soil_plot
 # it remains commented to avoid errors due to the exclusion of the soil_data.csv in 
 # the Github Repository. 
 
 # Soil data is obtained from Dendrascience.org
 # https://dendra.science/orgs/ucnrs/datastreams?faceted=true&scheme=dq&selectStationId=58e68cacdf5ce600012602d7
 
# ####################### COMMENTED SCRIPT STARTS HERE #########################
# 
# # read data and wrangle
#  soil_data <- read_csv(here("shiny_sierra_biodiversity_or_ts_ct/data/soil_data.csv"))
#  soil_data_daily <- soil_data %>% clean_names() %>%
#    mutate(date = lubridate::mdy_hm(time)) %>% 
#    mutate(date = floor_date(date, unit = "day")) %>%
#    group_by(date) %>%
#    summarize(mean_50mm = mean(x50mm_deg_c), mean_500mm = mean(x500mm_deg_c))
#  
#  
#  # use lubridate package to reformat date
#  soil_ts <- soil_data_daily %>%
#    mutate(date = as.Date(date)) %>%
#    as_tsibble(key = NULL, 
#               index = date)  ## time index, here our column is `date`
# 
#  # generate season plots
#  season_plot_50m <- 
#    soil_ts %>% 
#    gg_season(y = mean_50mm, pal = hcl.colors(n = 9)) +
#    theme_minimal()+
#    labs(x = "Month")+
#    theme(axis.title.y = element_blank())+
#    ggtitle("50 mm Soil Depth")
#  
#  season_plot_500m <- 
#    soil_ts %>% 
#    gg_season(y = mean_500mm, pal = hcl.colors(n = 9)) +
#    theme_minimal() +
#    labs(x = "Month")+
#    ggtitle("500 mm Soil Depth")
#  
#  
#  soil_plot <- season_plot_50m / season_plot_500m + plot_layout(guides = "collect")+
#    plot_annotation(caption = "Seasonal analysis of soil temperature at 500mm depth shows slight decrease, while temperature at 50mm shows no observable trend")
#  
#  # save ggplot object for reading
#  save(soil_plot, file = here("shiny_sierra_biodiversity_or_ts_ct/data/soil_plot.rdata" ))

   