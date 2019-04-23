library(shiny)
library(fs)
library(tidyverse)
library(leaflet)
library(tidycensus)
library(mapview)
library(sf)
library(tmap)
library(tmaptools)

# After not finding valid shapefiles in the right format for DC, we chose to use
# the `tigris` package's inbuilt states() function for the US that allowed us to
# access shapefiles for American states. Filtering for DC, we found the
# shapefile we wanted.

library(tigris)

library(ggplot2)
library(viridis)
library(ggthemes)
library(gganimate)

# This package allows our tabpanels to call for output enclosed by the
# withSpinner() function. Eg: withSpinner(plotOutput("mapplot"), type = 4). This
# function gives a loading symbol when the Shiny App is rendered, ensuring that
# the otherwise present whitespace when heavy renders like this one with
# animations isn't mistaken for an error or Shiny failure.

library(shinycssloaders)

library(shinythemes)

# 1: PREPROCESSING

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

# Adding a date column 
# For plot #2, we allow the user to pick any date in the
# window in which the data exists and view all the gunshots in that day by hour.

data$date <- as.Date(paste(data$year, data$month, data$day, sep="-"), "%Y-%m-%d")

# After not finding valid shapefiles in the right format for DC, we chose to use
# the `tigris` package's inbuilt states() function for the US that allowed us to
# access shapefiles for American states. Filtering for DC, we found the
# shapefile we wanted.

DC <-  states(cb = TRUE)
DC <- DC[DC$NAME == "District of Columbia", ]

DC <- st_as_sf(DC)
shape_wash_data <- st_as_sf(data, coords = c("longitude", "latitude"),  crs=4326)


# 2: USER INTERFACE

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
                                    helpText("Select a date for which we will display gunshots in DC by hour of day.
                                             All dates are in yyyy-mm-dd formats.
                                             Valid dates go from 27 January, 2016, to December, 2017."),
                                    dateInput("date",
                                              "Date (yyyy-mm-dd from 2006-2017): ", 
                                              value = "2006-01-27",
                                              min = "2006-01-27", 
                                              max = "2017-01-01",
                                              format = "yyyy-mm-dd")
                                  ),
                                  mainPanel(
                                    tabPanel("In a Day",
                                             withSpinner(imageOutput("hoursPlot"), type = 4))
                                    )
                         )
))

# 3: FUNCTIONS FOR ANIMATIONS

# Define server logic 
server <- function(input, output, session) {
   
   output$mapplot <- renderPlot({
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
   
   output$hoursPlot <- renderImage({
     
     if (nrow(filter(date == input$date, numshots != 0)) == 0) {
       # Return a list containing the filename
       list(src = "no_shots.gif",
            contentType = 'image/gif',
            alt = "There were no shots fired on the chosen day"
       )
       
     } else {
     region_subset <- st_as_sf(data %>% filter(date == input$date), coords = c("longitude", "latitude"),  crs=4326)
     
     outfile <- tempfile(fileext='.gif')
     
     p = ggplot(data = DC) +
       geom_sf() +
       geom_sf(data = sample) +
       theme_map() +
       transition_states(hour) + 
       labs(
         title = "Location of DC Shootings Through the Day",
         subtitle = "Shots at {closest_state}00 HRS",
         caption = "Source: Shotspotter Data"
         )
     
     anim_save("outfile.gif", animate(p))
     
     # Return a list containing the filename
     list(src = "outfile.gif",
          contentType = 'image/gif'
          # width = 400,
          # height = 300,
          # alt = "This is alternate text"
     )
     }
})
}

# Run the application 
shinyApp(ui = ui, server = server)

