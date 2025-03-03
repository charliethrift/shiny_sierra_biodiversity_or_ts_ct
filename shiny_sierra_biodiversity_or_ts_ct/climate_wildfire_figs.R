# Author: Olivia Ross
# Date: 16 February 16 2025

# This script downloads prism and wildfire data and creates draft figures for 
# SNARL biodiversity Shiny app

# Packages 
librarian::shelf("prism", "here", "ggplot2", "tmap", "sf", "lubridate", "tidyverse")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Setting directory for prism data
prism_set_dl_dir(here("data", "climate"))

# Downloading climate normals
get_prism_normals("tmax", "4km", annual = TRUE, keepZip = FALSE)
get_prism_normals("ppt", "4km", annual = TRUE, keepZip = FALSE)

# Downloading annual climate variables from 2000-2023
get_prism_annual("tmax", years = 2000:2023, keepZip = FALSE)
get_prism_annual("ppt", years = 2000:2023, keepZip = FALSE)

# SNARL coordinates 
snarl <- c(-118.83317, 37.61404)

# SNARL reserve boundary
snarl_poly <- st_read(here("data/SNARL", "SNARL_boundary.shp"))

# Plot 1: Maximum Annual Temperature at SNARL----------------------------------#

# Grabbing our climate data from the directory
tmax <- prism_archive_subset("tmax", "annual", years = 2000:2023)
tmax_normal <- prism_archive_subset("tmax", "annual normals", resolution = "4km")

# slicing climate stack to SNARL location
tmax_snarl <- pd_plot_slice(tmax, snarl)

# results in a list with 30 year normal value
tmax_snarl_normal <- pd_plot_slice(tmax_normal, snarl)

# tmax annuals and tmax normal at SNARL
tmax_snarl +
  geom_hline(yintercept = 16.1, linetype = "dashed", lwd=1.0)+
  geom_line(color="coral2", lwd=1.5) +
  geom_point(alpha = 0.5)+
  labs(x = "Year") +
  theme_bw()

# Plot 2: Annual Precipitation at SNARL----------------------------------------#

# Grabbing precipitation data from the directory
ppt <- prism_archive_subset("ppt", "annual", years = 2000:2023)
ppt_normal <- prism_archive_subset("ppt", "annual normals", resolution = "4km")

# slicing climate stack to SNARL location
ppt_snarl <- pd_plot_slice(ppt, snarl)

# results in a list with 30 year normal value
ppt_snarl_normal <- pd_plot_slice(ppt_normal, snarl)

# ppt annuals and ppt normal at SNARL
ppt_snarl +
  geom_hline(yintercept = 309, linetype = "dashed", lwd=1.0)+
  geom_line(color="deepskyblue", lwd=1.5) +
  geom_point(alpha = 0.5)+
  labs(x = "Year") +
  theme_bw()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Interactive wildfire map

# Reading in fire perimeter data
fire <- st_read(here("data", "fire perimeters", "mtbs_perims_DD.shp"))

# Reading in Sierra Nevada Border
snv <- st_read(here("data", "snv", "Sierra_Nevada_Conservancy_Boundary.shp"))

# Creating a year column for fire ignition date and selecting wildfires in 2000-2023 

fire_years <- fire |>
  mutate(Year = year(Ig_Date))|>
  filter(Year %in% 2000:2023, Incid_Type == "Wildfire") |>
  select(Year, Incid_Name, Incid_Type, geometry)

# overlapping verticies, make geometry valid
fire_years <- st_make_valid(fire_years)

#clipping wildfire perimeters to SNV only (changing crs first)
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

fire_time <- ts(fire_snv$Year_numeric, start = 2000, frequency = 1)

# Interactive Wildfire Map-----------------------------------------------------#
tmap_mode("view")
install.packages("gifski")

tm_shape(snv) +
  tm_borders("black", lwd=1.0) +
  
  tm_shape(fire_snv) +
  tmap_options(check.and.fix = TRUE)+
  tm_fill(col = "Year", palette = "PiYG", title="Fire Year") +
  #tm_borders("black") +
  
  #tm_facets(by ="Year_numeric") + 
  
  tm_shape(snarl_poly) +
  tm_dots(col = "orange") + #symbol type not supported in view mode
  
  tm_add_legend(type="fill", label = "SNARL", col = "orange") +
  tm_add_legend(type="fill", label = "Sierra Nevada", col = "black")

tmap_animation(fire_map, time = "Year_numeric", width = 800, height = 600)  

fire_map

# Map of SNARL-----------------------------------------------------------------#
tmap_mode("view")

tm_shape(snarl_poly) +
  tm_polygons(col = "orange") 

# Map of SNARL & Plants--------------------------------------------------------#

# Reading in cal flora data
snarl_calflora <- st_read(here("data/snarl_calflora", "shz1198.shp"))

# Getting just the year 
snarl_calflora <- snarl_calflora |>
  mutate(Year = year(Date))

# Making this a factor for the map
snarl_calflora$Year <- as.factor(snarl_calflora$Year)

tmap_mode("view")

tm_shape(snarl_poly) +
  tm_polygons(col = "orange") +
  
  tm_shape(snarl_calflora) +
  tm_dots(col = "Year", palette = "PiYG", title="Observation Year")








  