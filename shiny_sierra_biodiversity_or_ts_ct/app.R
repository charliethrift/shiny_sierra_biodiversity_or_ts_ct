# Shiny App for Species Distribution and Wildfire Events 
# 2025 / 02 / 15


library(shiny)
library(here)
library(dplyr)
library(maps)
library(tidyverse)
library(janitor)
library(bslib)

my_theme <- bs_theme(bootswatch = 'sketchy') %>% 
  bs_theme_update(bg='#dcdec8',
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
    "Fire in the Sierras",
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
      p("MORE INFO ABOUT DATA AND CITATIONS TO BE INCLUDED HERE")
    )
  )
)


# Server ----------------------------------------------------------------------
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




