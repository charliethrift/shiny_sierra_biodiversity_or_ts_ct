# Updated version for file organization, 5March2025
# Shiny app exploring biodiversity at the Sierra Nevada Aquatic Research Laboratory (SNARL)
# Authors: Charlie Thrift, Tanvi Shah, Olivia Ross
# Date: 2025 / 03 / 05

# Loading packages
librarian::shelf(shiny, here, dplyr, maps, tidyverse, janitor, 
                 bslib, prism, ggplot2, tmap, sf, lubridate, leaflet) 
# Setting our theme
my_theme <- bs_theme(bootswatch = 'simplex') %>% 
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
                  heading_font = font_google("Roboto"),
                  font_scale = 1.25)




# Read data
## note: data is being filtered to locations and times of interest in a separate script in this GitHub repository

snarl_tmax <- "a" #note: replace this with the (max temp) filtered prism data, 2000-2023 at snarl
snarl_ppt <- "b" #note: replace this with (avg precip) filtered prism data, 2000-2023 at snarl
snv <- st_read(here("shiny_sierra_biodiversity_or_ts_ct", "data", "snv", "Sierra_Nevada_Conservancy_Boundary.shp"))
fire_snv <- "c" #note: write the output of the original app (where this object is being wrangled) so that we can just read that in here
snarl_sf <- "d" #note: replace with the snarl_sf spatial object
snarl <- c(-118.83317, 37.61404)
snarl_poly <- st_read(here("shiny_sierra_biodiversity_or_ts_ct", "data", "SNARL", "SNARL_boundary.shp"))

snarl_tmax_plotted <- snarl_tmax + 
  geom_hline(yintercept = 16.1, linetype = "dashed", lwd=1.0)+
  geom_line(color="coral2", lwd=1.5) +
  geom_point(alpha = 0.5)+
  labs(x = "Year", y = "Temperature in deg C") +
  theme_bw()

snarl_ppt_plotted <- snarl_ppt + 
  geom_hline(yintercept = 16.1, linetype = "dashed", lwd=1.0)+ #change intercept to be the avg ppt
  geom_line(color="coral2", lwd=1.5) +
  geom_point(alpha = 0.5)+
  labs(x = "Year", y = "Precipitation UNITS") +
  theme_bw()


# User Interface
ui <- navbarPage(
  theme = my_theme,
  title = "Biodiversity of the Sierra Nevada Aquatic Research Laboratory",
  tabPanel(
    "About Page",
    fluidPage(
      titlePanel("About This App"),
      p("This app visualizes the biodiversity of the Sierra Nevada Aquatic Research Laboratory (SNARL) in Mammoth Lakes, California. User inputs allow you to navigate the past, present, 
      and future of biodiversity in this University of California site."),
      p("SNARL is one of 42 natural reserves managed by the University of California system, 
        which together comprise nearly 50,000 acres and represent all of California’s 
        major ecosystems. Protected sites like SNARL allow researchers to ask complex 
        ecological questions about the flora and fauna present. And, these sites can 
        foster an understanding of the rich biodiversity—past, present, and future—that 
        is responding during a time period of intense anthropogenic change."),
      p("Data Used:"),
      p("Animal Occurrence Data: GBIF, Global Biodiversity Information Facility"),
      p("Plant Occurrence Data: CalFlora"),
      p("Wildfire Data: Monitoring Trends in Burns and Severity (MTBS)"),
      p("Climate Data: PRISM")
    )
  ),
  tabPanel(
    "Fire History in the Sierras",
    fluidPage(
      sidebarPanel("Choose Inputs",
                   sliderInput(
                     inputId = "fire_year", 
                     sep = '',
                     label = "Year of Recorded Observation:",
                     min = 2000, #might want to make this slider bin
                     max = 2023, 
                     value = 2000)),
      tmapOutput("fire_map")
    )
  ),
  tabPanel(
    "Climate Data", # note, we want to include both temp and precip figs on here
           fluidPage(
             titlePanel(
               "Annual Max Temperature and Annual Precipitation"),
             p("Max Annual Temperature at SNARL"),
             plotOutput("tmax_plot"),
             p("Average Precipitation at SNARL"),
             plotOutput("ppt_plot")
           )
  ),
  tabPanel(
    "Species that are at SNARL?",
           fluidPage(
             titlePanel("Investigating the relationship between species richness, climate, and wildfire frequency"),
             sidebarLayout(
               sidebarPanel("Choose Inputs",
                            sliderInput(
                              inputId = "species_dist_year", 
                              sep = '',
                              label = "Year of Recorded Observation:",
                              min = 1980, 
                              max = 2025, 
                              value = 2000),
                            selectInput(
                              inputId = "polygon_color",
                              label = "select species",
                              choices = c("Red" = "red",
                                          "Orange" = "orange",
                                          "Yellow" = "yellow")
                            )
               ),
               mainPanel("Map of Species Occurrences",
                         plotOutput("species_dist_plot"))
             ))),
  tabPanel(
    "Lasso Analysis",
           fluidPage(titlePanel("Lasso Analysis"),
                     p("UI Placeholder."),
                     p("Lasso Analysis.")
           )
  )
)


# Server
server <- function(input, output, session) {
  year_reactive <- reactive({
    fy <- as.factor(input$fire_year)
    fire_snv %>% filter(Year %in% fy)
  })
  output$fire_map <- renderTmap({
    tm_shape(snv) +
      tm_borders("black", lwd=1.0) +
      
      tm_shape(year_reactive()) +
      tm_fill(col = "Year", palette = "PiYG", title="Fire Year") +
      
      tm_shape(snarl_poly) +
      tm_dots(col = "orange") +
      
      tm_add_legend(type="fill", label = "SNARL", col = "orange") +
      tm_add_legend(type="fill", label = "Sierra Nevada", col = "black")
  })
  output$tmax_plot <- renderPlot({
    snarl_tmax_plotted
  })
  output$ppt_plot <- renderPlot({
    snarl_ppt_plotted
  })
}



# Complete app by combining UI and server components
shinyApp(ui, server)
