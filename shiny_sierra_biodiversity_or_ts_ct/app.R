# Shiny App for Species Distribution and Wildfire Events 
# 2025 / 02 / 15


library(shiny)
library(here)
library(dplyr)
library(maps)

# Data -----------------------------------------------------------------------
#### Species Distribution Data - only Plants in Mono County
species_cols <- c('year','month', 'day', 'phylum','class','order','family',
                  'scientific_name','decimal_latitude', 'decimal_longitude' )
species_df_all_years <- read_csv(here("shiny_sierra_biodiversity_or_ts_ct",
                            "data", "occurrences.csv")) %>% clean_names() %>%
  select(species_cols) %>% drop_na()

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


# UI --------------------------------------------------------------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel("Choose Inputs",
                 sliderInput("yr", "Year of Recorded Observation:",
                             min = 1980, max = 2025, value = 2000)),
    mainPanel("Map of Species Distribution",
              plotOutput("species_dist_plot"))
  )
)

# Server ----------------------------------------------------------------------
server <- function(input, output) {
  output$species_dist_plot <- renderPlot({
    species_dist_plot_year(input$yr)
  })
}

# Complete app with UI and server components
shinyApp(ui, server)




