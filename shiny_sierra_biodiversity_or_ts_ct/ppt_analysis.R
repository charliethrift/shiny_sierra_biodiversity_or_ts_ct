# Data Analysis Script

# This script runs a point pattern analysis for plant and animal observations at the SNARL Research
# Station. Each point is an observation from Global Biodiversity Information Facility (GBIF), and INaturalist.
# The G-function and L-function were calculated for animal and plant observations.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Installing packages:
librarian::shelf(here, geodata, sf, terra, spatstat, tidyr, ggplot2)

# Loading in GBIF data & SNARL polygon 
animals_raw <- read.csv(here("shiny_sierra_biodiversity_or_ts_ct/data/gbif_snarl_plus_files", "gbif_snarl_plus_surround_animals.csv"))
plants_raw <- read.csv(here("shiny_sierra_biodiversity_or_ts_ct/data/gbif_snarl_plus_files", "gbif_snarl_plus_surround_plants.csv"))
  
snarl <- st_read(here("shiny_sierra_biodiversity_or_ts_ct", "data/SNARL", "SNARL_boundary.shp"))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
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
animals_snarl_ppp <- ppp(animals_ppp$x, animals_ppp$y, window = snarl_window)

#G function-Animals------------------------------------------------------------#
# Distance to calculate G(r)
r_vec <- seq(0, 20, by = 1) # 30 meters

# calculating the G value over a sequence of 30 m, for 200 simulations
gfunction_out <- envelope(animals_snarl_ppp, fun = Gest, r = r_vec, 
                          nsim = 200) 

plot(gfunction_out)

# creating a data frame and pivoting for ggplot 
gfunction_out_long <- gfunction_out |>
  as.data.frame() |>
  pivot_longer(cols = obs:hi, names_to = "model", values_to = "g_val")

# plotting out g scores and distances 
g_distance <- ggplot(data = gfunction_out_long, aes(x = r, y = g_val, group = model)) +
  geom_line(aes(color = model), lwd = 1.3) +
  scale_color_manual(values = c("blueviolet", "chartreuse3", "deeppink", "cornflowerblue"),
                     name = "Model", labels = c("High", "Low", "Observed", "Theoretical")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black")) +
  labs(x = 'Radius (m)', y = 'G(r)')

g_distance

# saving to import with app
save(g_distance, file = here("shiny_sierra_biodiversity_or_ts_ct/data/g_distance.rdata" ))

# L function-Animals-----------------------------------------------------------#

#boundary is 57.39 acres, which is 232,249 square meters, took the square root of this 
# for our sequence (check to see if this is correct?)
r_vec2 <- seq(0, 482, by = 40) 

lfunction_out <- envelope(animals_snarl_ppp, fun = Lest, r = r_vec2, 
                          nsim = 10)
plot(lfunction_out)

lfunction_long <- lfunction_out %>% 
  as.data.frame() %>% 
  pivot_longer(cols = obs:hi, names_to = "model", values_to = "l")

l_distance <- ggplot(data = lfunction_long, aes(x = r, y = l)) +
  geom_line(aes(color = model), lwd = 1.3) +
  scale_color_manual(values = c("blueviolet", "chartreuse3", "deeppink", "cornflowerblue"),
                     name = "Model", labels = c("High", "Low", "Observed", "Theoretical")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black")) +
  labs(x = 'Radius (m)', y = 'L(r)')

l_distance

# saving to import with app
save(l_distance, file = here("shiny_sierra_biodiversity_or_ts_ct/data/l_distance.rdata" ))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Point pattern analysis for plants 

# projecting plant csv
plants_sf <- st_as_sf(plants_raw, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Intersection of animal data and snarl boundary 
plants_snarl <- st_intersection(plants_sf, snarl)

# projecting on a 2D plane (UTM zone 11) for point pattern analysis 
plants_snarl <- st_transform(plants_snarl, crs = 32611)

# Converting animal observations to spatial point pattern
plants_ppp <- as.ppp(plants_snarl) 

# we already have the SNARL window from the section above 

# Combining as a point pattern object (points + window):
plants_snarl_ppp <- ppp(plants_ppp$x, plants_ppp$y, window = snarl_window)

#G function-Animals------------------------------------------------------------#
# Distance to calculate G(r)
r_vec <- seq(0, 20, by = 1) # 30 meters

# calculating the G value over a sequence of 30 m, for 200 simulations
gfunction_out <- envelope(plants_snarl_ppp, fun = Gest, r = r_vec, 
                          nsim = 200) 

plot(gfunction_out)

# creating a data frame and pivoting for ggplot 
gfunction_out_long <- gfunction_out |>
  as.data.frame() |>
  pivot_longer(cols = obs:hi, names_to = "model", values_to = "g_val")

# plotting out g scores and distances 
g_distance_plants <- ggplot(data = gfunction_out_long, aes(x = r, y = g_val, group = model)) +
  geom_line(aes(color = model), lwd = 1.3) +
  scale_color_manual(values = c("blueviolet", "chartreuse3", "deeppink", "cornflowerblue"),
                     name = "Model", labels = c("High", "Low", "Observed", "Theoretical")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black")) +
  labs(x = 'Radius (m)', y = 'G(r)')

g_distance_plants

# saving to import with app
save(g_distance_plants, file = here("shiny_sierra_biodiversity_or_ts_ct/data/g_distance_plants.rdata" ))

# L function-Animals-----------------------------------------------------------#

#boundary is 57.39 acres, which is 232,249 square meters, took the square root of this 
# for our sequence (check to see if this is correct?)
r_vec2 <- seq(0, 482, by = 40) 

lfunction_out <- envelope(animals_snarl_ppp, fun = Lest, r = r_vec2, 
                          nsim = 10)
plot(lfunction_out)

lfunction_long <- lfunction_out %>% 
  as.data.frame() %>% 
  pivot_longer(cols = obs:hi, names_to = "model", values_to = "l")

l_distance <- ggplot(data = lfunction_long, aes(x = r, y = l)) +
  geom_line(aes(color = model), lwd = 1.3) +
  scale_color_manual(values = c("blueviolet", "chartreuse3", "deeppink", "cornflowerblue"),
                     name = "Model", labels = c("High", "Low", "Observed", "Theoretical")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black")) +
  labs(x = 'Radius (m)', y = 'L(r)')

l_distance

# saving to import with app
save(l_distance, file = here("shiny_sierra_biodiversity_or_ts_ct/data/l_distance.rdata" ))

