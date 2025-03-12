## charlie's app for widget 4 in isolation

librarian::shelf(shiny, here, dplyr, maps, tidyverse, janitor, 
                 bslib, prism, ggplot2, tmap, sf, lubridate, leaflet) 


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


# Load things here, this will be in the top part of our final app
#animal_data <- read.csv("FILEPATH.csv")
species_sf <- st_as_sf(merged_data, 
                       coords = c("decimalLongitude", "decimalLatitude"), 
                       crs = 4326)
snarl_poly <- st_read("snarl_data/SNARL_boundary.shp")



# User Interface
ui <- fluidPage(
  theme = my_theme,
  

    # Application title
 #   titlePanel("Sample app for widget 4 test"),
  #  tabPanel(
   #   "Species occurrences",
   #   fluidPage(
        titlePanel("Is the Sierra Nevada Aquatic Research Laboratory a biodiversity hotspot, or is it simply well-studied?"),
 p("Occurrence data is evidence of a species being present in a particular place and time. It is often biased towards places that are easy to sample, or taxa that are easier to find and interesting to look at. Therefore, it tells a snapshot of the story, when a complete picture of all biodiversity in any region is impossible to assemble. 
"),
 p("Here, we present occurrence data from multiple sources within and surrounding the Sierra Nevada Aquatic Research Laboratory. Use the widgets and resulting plots to try and determine your answer—is this a biodiversity hotspot? 
"),
    #    sidebarLayout(
          sidebarPanel("Choose Inputs",
                         selectInput(
                           "class_common", 
                           label = h3("Select class"), 
                           choices = list(
                             "Amphibians" = "Amphibians",
                             "Arachnids" = "Arachnids",
                             "Birds" = "Birds",
                             "Gastropods" = "Gastropods",
                             "Insects" = "Insects",
                             "Mammals" = "Mammals"
                             ), 
                            selected = "Amphibians"),
                         hr(),
                         fluidRow(column(3, verbatimTextOutput("value")))
          ),
          mainPanel(
            fluidRow(
              column(4,plotOutput("species_occurrences_plot")),
              column(8,tmapOutput("species_occurrences_map")),
              
          )
        )
)       

# Server
server <- function(input, output, session) {
  
  # give it the data frame
  merged_data <- merged_data

    # populate choices from the input in the widget
  choices_class <- unique(animal_data$class_common)
  
  updateSelectInput(session=session,
                    inputId = "class_common",
                    choices = choices_class)
  
  my_reactive_data <- reactive({
    subset(merged_data, class_common == input$class_common)
  })
  
  reactive_tmap_data <- reactive({
    st_as_sf(my_reactive_data(),
             coords = c("decimalLongitude", "decimalLatitude"),
             crs = 4326) #3857 or 4326
  })
  
    output$species_occurrences_plot <- renderPlot({
      ggplot(my_reactive_data(),aes(x = class_common))+
        geom_bar(aes(fill = species), stat = "count") +
        geom_text(aes(y = 0,
                      label = count_print), 
                  position = position_stack(vjust = 0.5),  # Adjust position for label placement
                  vjust = -0.25) +
        theme_minimal()+
        theme(legend.position = "none")+
        xlab("Animal Class")+
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
                size = 0.1,
                legend.show = FALSE) +  # Color by species
        tmap_options(max.categories = 214)+
        tm_layout(main.title = paste0("Species Occurrences of", input$class_common),
                  legend.show = FALSE) 

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
