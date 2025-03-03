# Shiny App for Species Distribution and Wildfire Events 
# Authors: Charlie Thrift, Tanvi Shah, Olivia Ross
# Date: 2025 / 02 / 15

# Loading packages
librarian::shelf(shiny, here, dplyr, maps, tidyverse, janitor, 
                 bslib, prism, ggplot2, tmap, sf, lubridate, leaflet) 

# Setting our theme
my_theme <- bs_theme(bootswatch = 'sketchy') %>% 
  bs_theme_update(bg='#ded',
                  fg='#323133',
                  primary="#1e17a6",
                  secondary="#645df5",
                  success = "#26de57",
                  info="#1b8c76",
                  warning = "#f0ec26",
                  danger = "#f04b26",
                  base_font = font_google("Rasa"),
                  code_font = font_google("Atkinson Hyperlegible Mono"),
                  heading_font = font_google("Rubik Dirt"),
                  font_scale = 1.25)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Reading in Data:

# Species Distributions--------------------------------------------------------#
#### Species Distribution Data - only Plants in Mono County
species_cols <- c('year','month', 'day', 'phylum','class','order','family',
                  'scientific_name','decimal_latitude', 'decimal_longitude' )
species_df_all_years <- read_csv(here("shiny_sierra_biodiversity_or_ts_ct",
                            "data", "occurrences.csv")) %>% clean_names() %>%
  select(species_cols) %>% drop_na()

# Calflora Data----------------------------------------------------------------#
snarl_calflora <- st_read(here("data/snarl_calflora", "shz1198.shp"))

# Wildfire Data----------------------------------------------------------------#
fire <- st_read(here("data", "fire perimeters", "mtbs_perims_DD.shp"))

# Sierra Nevada Border---------------------------------------------------------#
snv <- st_read(here("data", "snv", "Sierra_Nevada_Conservancy_Boundary.shp"))

# Climate Data-----------------------------------------------------------------#
## Setting directory for prism data
prism_set_dl_dir(here("data", "climate"))

# Downloading climate normals
get_prism_normals("tmax", "4km", annual = TRUE, keepZip = FALSE)
get_prism_normals("ppt", "4km", annual = TRUE, keepZip = FALSE)

# Downloading annual climate variables from 2000-2023
get_prism_annual("tmax", years = 2000:2023, keepZip = FALSE)
get_prism_annual("ppt", years = 2000:2023, keepZip = FALSE)

# SNARL------------------------------------------------------------------------#
# SNARL coordinates 
snarl <- c(-118.83317, 37.61404)

# SNARL reserve boundary
snarl_poly <- st_read(here("data/SNARL", "SNARL_boundary.shp"))

# Wrappers ------------------------------------------------------------------
## to avoid cluttering UI and Server  

#### CA Species Distribution Wrapper
species_dist_plot_year <- function(input_year){
  data <- species_df_all_years %>% filter(year == input_year)
  CA_counties <- map_data("county", region = "california")
  ggplot(CA_counties, aes(x = long, y = lat, group = subregion)) +
    geom_polygon(color="white") +
    coord_map()+
    geom_point(data = data, aes(x= decimal_longitude, y= decimal_latitude),
               color = 'red',size = 1,
               inherit.aes = FALSE )+
    coord_equal()
}

# Wildfire Map Placeholder
##### this data is static for now
wildfire_plot_year <- function(input_year){
  # input_year is a dummy variable for
  CAcounties <- map_data("county", region = "california")
  CA_wildfire_map <- ggplot(CAcounties, aes(x = long, y = lat, group = subregion)) +
    geom_polygon(color="white") +
    coord_map()
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Olivia- Fire Map-------------------------------------------------------------#

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

# Render Tmap Output-----------------------------------------------------------#
tmap_mode("view")

output$fire_map <- tm_shape(snv) +
  tm_borders("black", lwd=1.0) +
  
  tm_shape(fire_snv) +
  tmap_options(check.and.fix = TRUE)+
  tm_fill(col = "Year", palette = "PiYG", title="Fire Year") +
  
  tm_shape(snarl_poly) +
  tm_dots(col = "orange") +
  
  tm_add_legend(type="fill", label = "SNARL", col = "orange") +
  tm_add_legend(type="fill", label = "Sierra Nevada", col = "black")

#tmap_animation(fire_map, time = "Year_numeric", width = 800, height = 600)  

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Creating the user interface
ui <- navbarPage(
  theme = my_theme,
  title = "Biodiversity of the Sierra Nevada Aquatic Research Laboratory",
  tabPanel("Main Page",
        fluidPage(
          titlePanel("A rich history of occurrence data"),
          sidebarLayout(
            sidebarPanel("Choose Inputs",
                 sliderInput(
                 inputId = "yr", 
                 label = "Year of Recorded Observation:",
                 min = 1980, 
                 max = 2025, 
                 value = 2000),
                 selectInput(
                   inputId = "polygon_color",
                   label = "select polygon color",
                   choices = c("Red" = "red",
                               "Orange" = "orange",
                               "Yellow" = "yellow")
                 )
                 ),
    mainPanel("Map of Species Distribution",
              plotOutput("species_dist_plot"))
    ))),
  tabPanel(
    "Fire History in the Sierras",
    fluidPage(
      titlePanel("Summary of Inputs"),
      verbatimTextOutput("output_summary_page")
    )
  ),
  tabPanel(
    "Species Distribution Models",
    fluidPage(
      titlePanel("Species Distribution Models"),
      p("UI Placeholder."),
      p("Species distribution models of select species from GBIF occurences, indicating where the research station property falls within their range.")
    )
  ),
  tabPanel(
    "About Page",
    fluidPage(
      titlePanel("About This App"),
      p("This app visualizes the biodiversity of the Sierra Nevada Aquatic Research Laboratory in Mammoth Lakes, California."),
      p("User inputs allow you to navigate the past, present, and future of biodiversity in this University of California site."),
      p("Data Used:"),
      p("Plant Occurence Data: CalFlora"),
      p("Wildfire Data: Monitoring Trends in Burns and Severity (MTBS)"),
      p("Cliimate Data: PRISM")
    )
  )
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Creating the server function
server <- function(input, output, session) {
  output$species_dist_plot <- renderPlot({
    species_dist_plot_year(input$yr)
  })
  output$output_summary <- renderPrint({
    summary(list(
      Name = input$text_input,
      Numeric = input$num_input,
      Slider = input$slider_input,
      Checkbox = input$checkbox,
      Radio = input$radio_input
    ))
  })
  output$output_summary_page <- renderPrint({
    summary(list(
      Name = input$text_input,
      Numeric = input$num_input,
      Slider = input$slider_input,
      Checkbox = input$checkbox,
      Radio = input$radio_input
    ))
    
})
}

# Complete app with UI and server components
shinyApp(ui, server)




