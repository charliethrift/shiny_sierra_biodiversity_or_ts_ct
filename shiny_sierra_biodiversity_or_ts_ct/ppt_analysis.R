# Data Analysis Script
# Author: Olivia Ross
# Date: 10 March 2025

# This script investigates nearest neighbor patterns within the plant and animal observations at the SNARL Research
# Station. Each point is an observation from Global Biodiversity Information Facility (GBIF) and INaturalist, and 
# represents one species, geospatial information for that species, and other attributes.

# We completed a nearest neighbor analysis using the G-Function and the K-function for both plants and animals. 

# 1). The G-function calculates the the proportion of plant and observations that occur within a nearest neighbor 
# distance, r. 

# 2). Ripley's K-function calculates the distance between all observations in a study window by observing how the 
# number of observations within the region changes as the radius surrounding a single point increases. 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Installing packages:
librarian::shelf(here, geodata, sf, terra, spatstat, tidyr, ggplot2)

# Loading in GBIF data & SNARL polygon 
animals_raw <- read.csv(here("shiny_sierra_biodiversity_or_ts_ct/data/gbif_snarl_plus_files", "gbif_snarl_plus_surround_animals.csv"))
plants_raw <- read.csv(here("shiny_sierra_biodiversity_or_ts_ct/data/gbif_snarl_plus_files", "gbif_snarl_plus_surround_plants.csv"))
snarl <- st_read(here("shiny_sierra_biodiversity_or_ts_ct", "data/SNARL", "SNARL_boundary.shp"))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Point pattern analysis for animal data

# projecting animal csv
animals_sf <- st_as_sf(animals_raw, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# making sure SNARL has matching crs
snarl <- st_transform(snarl, crs = 4326)

# Intersection of animal data and snarl boundary 
animals_snarl <- st_intersection(animals_sf, snarl)

# projecting on a 2D plane (UTM zone 11) for point pattern analysis 
animals_snarl <- st_transform(animals_snarl, crs = 32611)
snarl <- st_transform(snarl, crs = 32611)

# Converting animal observations to spatial point pattern
animals_ppp <- as.ppp(animals_snarl) 

# Converting snarl boundary to observation window
snarl_window <- as.owin(snarl) 

# Combining as a point pattern object (points + window):
animals_snarl_ppp <- ppp(animals_ppp$x, animals_ppp$y, window = snarl_window, unitname=c("meter", "meters"))

# After running the point pattern analysis, we receive a warning "data contain duplicate points"; we reviewed the initial animals_raw 
# data frame to ensure there were no duplicate observations, however, because the study window is quite small and the 
# observations were close together, after projecting to 2D and creating a point pattern analysis, some points shared the same 
# "UTM Y" or "UTM X" values, producing this warning. 

#G function-Animals-------------------------------------------------------------------------------------------#
# Distance to calculate G(r)
r_vec <- seq(0, 10, by = 0.5) # 10 meters

# calculating the G value over a sequence of 10 m, for 200 simulations
animals_gfunctionout <- envelope(animals_snarl_ppp, fun = Gest, r = r_vec, 
                          nsim = 200) 

animals_gfunctionout

# Creating a data frame and pivoting for ggplot 
animals_gfunctionout_long <- animals_gfunctionout |>
  as.data.frame() |>
  pivot_longer(cols = obs:hi, names_to = "model", values_to = "g_val")

# plotting out g scores and distances 
animals_gdistance <- ggplot(data = animals_gfunctionout_long, aes(x = r, y = g_val, group = model)) +
  geom_line(aes(color = model), lwd = 1.3) +
  scale_color_manual(values = c("blueviolet", "chartreuse3", "deeppink", "cornflowerblue"),
                     name = "Model", labels = c("High", "Low", "Observed", "Theoretical")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black")) +
  labs(x = 'Radius (meters)', y = 'G(r)')+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))

animals_gdistance

# saving to import with app
save(animals_gdistance, file = here("shiny_sierra_biodiversity_or_ts_ct/data/animals_gdistance.rdata" ))

# L function-Animals-------------------------------------------------------------------------------------------#
# Checking window size 
st_area(snarl)

# The SNARL boundary is 232250.3 square meters, took the square root of this for our sequence 
r_vec2 <- seq(0, 482, by = 5) 

animals_lfunctionout <- envelope(animals_snarl_ppp, fun = Lest, r = r_vec2, 
                          nsim = 20)

plot(animals_lfunctionout)
animals_lfunctionout

# Creating a data frame and pivoting results for plotting
animals_lfunctionlong <- animals_lfunctionout %>% 
  as.data.frame() %>% 
  pivot_longer(cols = obs:hi, names_to = "model", values_to = "l")

# Plotting results
animals_ldistance <- ggplot(data = animals_lfunctionlong, aes(x = r, y = l)) +
  geom_line(aes(color = model), lwd = 1.3) +
  scale_color_manual(values = c("blueviolet", "chartreuse3", "deeppink", "cornflowerblue"),
                     name = "Model", labels = c("High", "Low", "Observed", "Theoretical")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black")) +
  labs(x = 'Radius (meters)', y = 'L(r)') +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))

animals_ldistance

# saving to import with app
save(animals_ldistance, file = here("shiny_sierra_biodiversity_or_ts_ct/data/animals_ldistance.rdata"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Point pattern analysis for plants 

# projecting plant csv
plants_sf <- st_as_sf(plants_raw, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Intersection of animal data and snarl boundary 
# Note: if already projected SNARL to UTM, reproject to WGS84 before running this step
plants_snarl <- st_intersection(plants_sf, snarl)

# projecting on a 2D plane (UTM zone 11) for point pattern analysis 
plants_snarl <- st_transform(plants_snarl, crs = 32611)

# Converting animal observations to spatial point pattern
plants_ppp <- as.ppp(plants_snarl) 

# we already have the SNARL window from the section above 

# Combining as a point pattern object (points + window):
plants_snarl_ppp <- ppp(plants_ppp$x, plants_ppp$y, window = snarl_window)

#G function-Plants---------------------------------------------------------------------------------------------#
# Distance to calculate G(r)
r_vec <- seq(0, 9, by = 1) # 15 meters

# calculating the G value over a sequence of 30 m, for 200 simulations
plants_gfunctionout <- envelope(plants_snarl_ppp, fun = Gest, r = r_vec, 
                          nsim = 200) 
plants_gfunctionout
plot(plants_gfunctionout)

# creating a data frame and pivoting for ggplot 
plants_gfunctionoutlong <- plants_gfunctionout |>
  as.data.frame() |>
  pivot_longer(cols = obs:hi, names_to = "model", values_to = "g_val")

# plotting out g scores and distances 
plants_gdistance <- ggplot(data = plants_gfunctionoutlong, aes(x = r, y = g_val, group = model)) +
  geom_line(aes(color = model), lwd = 1.3) +
  scale_color_manual(values = c("blueviolet", "chartreuse3", "deeppink", "cornflowerblue"),
                     name = "Model", labels = c("High", "Low", "Observed", "Theoretical")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black")) +
  labs(x = 'Radius (meters)', y = 'G(r)') +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))

plants_gdistance

# saving to import with app
save(plants_gdistance, file = here("shiny_sierra_biodiversity_or_ts_ct/data/plants_gdistance.rdata" ))

# L function-Plants-------------------------------------------------------------------------------------------#

# distance to search for neighboring points
r_vec2 <- seq(0, 482, by = 2) 

plants_lfunctionout <- envelope(plants_snarl_ppp, fun = Lest, r = r_vec2, 
                          nsim = 20)
plot(plants_lfunctionout)

plants_lfunctionout

# Creating a data frame and pivoting to plot our results
plants_lfunctionlong <- plants_lfunctionout %>% 
  as.data.frame() %>% 
  pivot_longer(cols = obs:hi, names_to = "model", values_to = "l")

# Plotting our results
plants_ldistance <- ggplot(data = plants_lfunctionlong, aes(x = r, y = l)) +
  geom_line(aes(color = model), lwd = 1.3) +
  scale_color_manual(values = c("blueviolet", "chartreuse3", "deeppink", "cornflowerblue"),
                     name = "Model", labels = c("High", "Low", "Observed", "Theoretical")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black")) +
  labs(x = 'Radius (meters)', y = 'L(r)') +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))

plants_ldistance

# saving to import with app
save(plants_ldistance, file = here("shiny_sierra_biodiversity_or_ts_ct/data/plants_ldistance.rdata"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Creating a table for the app

librarian::shelf(knitr, kableExtra, webshot2)

table <- data.frame(
  Analyses = c("Animals G-function", "Plants G-function", "Animals L-function", "Plants L-function"),
  Radius = c("10", "15", "482", "482"),
  Simulations = c("200", "200", "20", "20"),
  Significance = c("0.00995", "0.00995", "0.0952", "0.0952")
)


ppt_table <- kable(table, caption = "G-function and L-function statistics for animals and plants at SNARL") |>
  kable_styling(bootstrap_options = c("striped", "hover"))

class(ppt_table) # checking class

# Saving table 
save_kable(ppt_table, file = here("shiny_sierra_biodiversity_or_ts_ct/data", "ppt_table.jpeg"))
