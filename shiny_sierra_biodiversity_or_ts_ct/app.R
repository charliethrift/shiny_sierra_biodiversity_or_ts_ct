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




######  Read data #############################################################

# Climate data
load(here("shiny_sierra_biodiversity_or_ts_ct/data/tmax_snarl_plot.rdata"))
load(here("shiny_sierra_biodiversity_or_ts_ct/data/ppt_snarl_plot.rdata"))

# Shape files
snv <- st_read(here("shiny_sierra_biodiversity_or_ts_ct", "data", "snv", "Sierra_Nevada_Conservancy_Boundary.shp"))
snarl_poly <- st_read(here("shiny_sierra_biodiversity_or_ts_ct", "data", "SNARL", "SNARL_boundary.shp"))
fire_snv <- st_read(here("shiny_sierra_biodiversity_or_ts_ct/data", "fire_perimeters", "fire_snv.shp"))

# Species Occurrences
occurrence_data_for_widget4 <- read_csv("data/occurrence_data_for_widget4_updated.csv")
species_sf <- st_as_sf(occurrence_data_for_widget4, 
                       coords = c("decimalLongitude", "decimalLatitude"), 
                       crs = 4326)


# User Interface
ui <- navbarPage(
  theme = my_theme,
  title = "Biodiversity of the Sierra Nevada Aquatic Research Laboratory",
  tabsetPanel(
    
    tabPanel("About Page",
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
  
  tabPanel("Fire History in the Sierras",
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
  
  tabPanel("Climate Data",
           fluidPage(
             titlePanel(
               "Annual Max Temperature and Annual Precipitation"),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("climate_plot_select", label = "Select Climate Variable to Display",
                              choices = c("Temperature" = "temp_select", "Precipitation" = "ppt_select"),
                              selected = "temp_select")
               ),
               
               mainPanel(
                 plotOutput("climate_plot")  # This will display the selected plot
               )
             )
           #                plotOutput("distPlot"))
           #   p("Max Annual Temperature at SNARL"),
           #   plotOutput("tmax_plot"),
           #   p("Average Precipitation at SNARL"),
           #   plotOutput("ppt_plot")
           # )
  )),
   tabPanel("Species that are at SNARL",
            fluidPage(
              titlePanel("Is the Sierra Nevada Aquatic Research Laboratory a biodiversity hotspot, or is it simply well-studied?"),
              p("Occurrence data is evidence of a species being present in a particular place and time. It is often biased towards places that are easy to sample, or taxa that are easier to find and interesting to look at. Therefore, it tells a snapshot of the story, when a complete picture of all biodiversity in any region is impossible to assemble."),
              p("Here, we present occurrence data from multiple sources within and surrounding the Sierra Nevada Aquatic Research Laboratory. Use the widgets and resulting plots to try and determine your answer—is this a biodiversity hotspot?"),
              
              sidebarPanel(
                "Choose Inputs",
                selectInput("class_common", label = h3("Select class"), 
                            choices = list("Amphibians" = "Amphibians",
                                           "Arachnids" = "Arachnids",
                                           "Birds" = "Birds",
                                           "Gastropods" = "Gastropods",
                                           "Insects" = "Insects",
                                           "Mammals" = "Mammals"),
                            selected = "Amphibians"),
                hr(),
                fluidRow(column(3, verbatimTextOutput("value")))
              ),
              
              mainPanel(
                "Map of Species Occurrences",
                plotOutput("species_dist_plot")
              ),
              fluidRow(
                column(6, plotOutput("species_occurrences_plot", height = "300px")),  # Make plot column larger
                column(6, tmapOutput("species_occurrences_map", height = "300px"))    # Make map column smaller
              )
              
   )),
   
   tabPanel("Point Pattern Analysis",
            fluidPage(
              titlePanel("Point Pattern Analysis"),
              p("UI Placeholder."),
              p("Point Pattern Analysis.")
                      )
            )
                ))


# Server
server <- function(input, output, session) {
  
  # Fire reactive elements
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
  
  # Climate data reactive elements
  output$climate_plot <- renderPlot({
    if (input$climate_plot_select == "temp_select") {
      # Render Tmax Plot 
      tmax_snarl_plot
    } else if (input$climate_plot_select == "ppt_select") {
      # Render Climate Plot
      ppt_snarl_plot
    }
  })
  
  # output$tmax_plot <- renderPlot({
  #   tmax_snarl_plot
  # })
  # output$ppt_plot <- renderPlot({
  #   ppt_snarl_plot
  # })
  
  # Species dist reactive elements
  
  # populate choices from the input in the widget
  occurrence_data <- occurrence_data_for_widget4
  choices_class <- unique(occurrence_data_for_widget4$class_common)
  updateSelectInput(session=session,
                    inputId = "class_common",
                    choices = choices_class)
  
  my_reactive_data <- reactive({
    subset(occurrence_data_for_widget4, class_common == input$class_common)
  })
  reactive_tmap_data <- reactive({
    st_as_sf(my_reactive_data(),
             coords = c("decimalLongitude", "decimalLatitude"),
             crs = 4326) #3857 or 4326
  })
  output$species_occurrences_plot <- renderPlot({
    ggplot(my_reactive_data(),aes(x = location))+
      geom_bar(aes(fill = species), stat = "count") +
      geom_text(aes(y = 0,
                    label = count_print), 
                position = position_stack(vjust = 0.5),  
                vjust = -0.25) +
      theme_minimal()+
      theme(legend.position = "none")+
      xlab("Location")+ 
      ylab("Number of Observations")+
      ggtitle(paste0("Species richness within: ", input$class_common))  # Dynamic Title
  })
  
  
  output$species_occurrences_map <- renderTmap({
    
    data_for_tmap <- reactive_tmap_data()
    
    tmap_mode("view")
    tm_shape(snarl_poly)+
      tm_polygons(col = "red",alpha = 0.3)+
      tm_shape(data_for_tmap) + 
      tm_dots(col = "species", 
              palette = "Set1", 
              size = 0.5,
              legend.show = FALSE,
              id = "species") +  # Color by species
      #tmap_options(max.categories = 214)+
      tm_layout(main.title = paste0("Species Occurrences of ", input$class_common),
                legend.show = FALSE) 
    
  })
  
}




# Complete app by combining UI and server components
shinyApp(ui, server)
