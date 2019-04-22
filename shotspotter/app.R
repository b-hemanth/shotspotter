library(shiny)
library(fs)
library(tidyverse)
library(leaflet)
library(tidycensus)
library(mapview)
library(sf)
library(tmap)
library(tmaptools)
library(tigris)
library(ggplot2)
library(viridis)
library(ggthemes)
library(gganimate)
library(shinycssloaders)
library(shinythemes)
# We downloaded the data and saved it in the github repo. By the nature of the
# dataset, we don't expect it to change anythime soon.

data <- read_csv("wash_data.csv",
                 cols(
                   incidentid = col_double(),
                   latitude = col_double(),
                   longitude = col_double(),
                   year = col_double(),
                   month = col_double(),
                   day = col_double(),
                   hour = col_double(),
                   minute = col_double(),
                   second = col_double(),
                   numshots = col_double(),
                   type = col_logical()
                 ),
                 col_names = TRUE
                 )

DC <-  states(cb = TRUE)

DC <- DC[DC$NAME == "District of Columbia", ]

DC <- st_as_sf(DC)
shape_wash_data <- st_as_sf(data, coords = c("longitude", "latitude"),  crs=4326)



# Define UI 

# Based on:
# https://community.rstudio.com/t/different-inputs-sidebars-for-each-tab/1937/2
# We need different sidebar panels for different tabs, so, the traditional one
# panel, multiple tabs method doeesn't work. Instead, we resolve this UI problem
# by making a multi-page Shiny App. Here, the app is built with separate
# sidebars on each page using the navbarPage() layout (the pages are created
# with the tabPanel() function). 

# An example of this:
# https://github.com/Jim89/oyster/blob/master/R/shiny/ui.R 
# A working app example: https://jleach.shinyapps.io/oyster/

ui <- shinyUI(navbarPage("Gun Shots in Washington DC",
                         tabPanel("Across the Years",
                                  sidebarPanel(
                                    sliderInput("year",
                                                "Year:",
                                                min = 2006,
                                                max = 2017,
                                                value = 2006,
                                                sep = "")
                                  ),
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("Across the Years", 
                                               withSpinner(plotOutput("mapplot"), type = 4)
                                               ))
                                  )
                         ),
                         tabPanel("In a Day",
                                  sidebarPanel(
                                    radioButtons("year",
                                                 "Year: ", unique(data$year))
                                  ),
                                  mailPanel(
                                    tabPanel("In a Day",
                                                     plotOutput("hoursPlot"))
                                    )
                         )
))

# fluidPage(
#    
#    # Application title
#    titlePanel("Gun Shots in Washington DC"),
#    
#    # Sidebar with a slider input for number of bins 
#    sidebarLayout(
#       sidebarPanel(
#          sliderInput("year",
#                      "Year:",
#                      min = 2006,
#                      max = 2017,
#                      value = 2006,
#                      sep = "")
#       ),
#       sidebarPanel(
#         radioButtons("year",
#                      "Year: ", unique(data$year))
#       ),
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#         tabsetPanel(
#           tabPanel("Across the Years", 
#                    withSpinner(plotOutput("mapplot"), type = 4),
#           tabPanel("In a day",
#                    plotOutput("hoursPlot"))
#         )
#       )
#    )
# )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$mapplot <- renderPlot({
      # generate bins based on input$bins from ui.R
      shape_wash_data <- st_as_sf(data %>% filter(year == input$year, numshots > 1), coords = c("longitude", "latitude"),  crs=4326)
      
      ggplot(data = DC) +
        geom_sf() +
        geom_sf(data = shape_wash_data) +
        theme_map() + 
        transition_states(year) + 
        labs(title = "Location of DC Shootings by Year",
             subtitle = "Year: {closest_state}",
             caption = "Source: Shotspotter Data")
   })
   
   output$hoursPlot <- renderPlot({
     region_subset <- st_as_sf(data %>% filter(year == input$year, !is.na(numshots)), coords = c("longitude", "latitude"),  crs=4326)
     ggplot(data = DC) +
       geom_sf() +
       geom_sf(data = region_subset) +
       theme_map() +
       transition_states(hour) + 
       labs(
         title = "Location of DC Shootings by Year",
         subtitle = "Hour: {closest_state}",
         caption = "Source: Shotspotter Data"
         )
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

