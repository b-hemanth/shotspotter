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
library(lubridate)
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
) %>%
  mutate(month_1 = as.character(month(ymd(010101) + months(month-1),
                                      label = TRUE,
                                      abbr = FALSE)))

# Adding a date column 
# For plot #2, we allow the user to pick any date in the
# window in which the data exists and view all the gunshots in that day by hour.

data$date <- as.Date(paste(data$year, data$month, data$day, sep="-"), "%Y-%m-%d")

# After not finding valid shapefiles in the right format for DC, we chose to use
# the `tigris` package's inbuilt states() function for the US that allowed us to
# access shapefiles for American states. Filtering for DC, we found the
# shapefile we wanted.

DC <- DC[DC$NAME == "District of Columbia", ]

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


ui <- shinyUI(navbarPage("Gun Shots in Washington DC",
                         theme = shinytheme("darkly"),
                         tabPanel("Across the Years",
                                  sidebarPanel(
                                    
                                    # THis guides the user a little better
                                    
                                    helpText("Please select a year below to iterate over. The graph on the right will show the monthly gunshot locations. Because of the nature of R animations, the graphic may take up to 30 seconds to load. Appreciate your patience!"),
                                    
                                    # I thought it would be more reasonable to
                                    # add a drop-down menu rather than a slider
                                    
                                    selectInput("year_1",
                                                "Year:",
                                                c("2016" = 2016,
                                                  "2015" = 2015,
                                                  "2014" = 2014,
                                                  "2013" = 2013,
                                                  "2012" = 2012,
                                                  "2011" = 2011,
                                                  "2010" = 2010,
                                                  "2009" = 2009,
                                                  "2008" = 2008,
                                                  "2007" = 2007))
                                  ),
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("Gun Shot Locations of a Random Sample by Month", 
                                               
                                               # This adds a spinner when the data loads
                                               
                                               withSpinner(imageOutput("mapplot"), type = 4)
                                      ))
                                  )
                         ),
                         tabPanel("In a Day",
                                  sidebarPanel(
                                    helpText("Select a date for which we will display gunshots in DC by hour of day.
                                             All dates are in yyyy-mm-dd formats.
                                             Valid dates go from 27 January, 2016, to December, 2017.
                                             NOTE: The animation can take upto a minute to load, kindly wait."),
                                    dateInput("date",
                                              "Date (yyyy-mm-dd from 2006-2017): ", 
                                              value = "2006-01-27",
                                              min = "2006-01-27", 
                                              max = "2017-01-01",
                                              format = "yyyy-mm-dd")
                                    ),
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("Location of Washington DC Shootings Through the Day",
                                               
                                               # This adds a spinner when the data loads
                                               
                                               withSpinner(imageOutput("hoursPlot"), type = 4))
                                    ))
                         )
))


# Define server logic 
server <- function(input, output) {
  
  # Because we are in fact plotting .gif files, renderPlot won't work. We
  # actually need to treat the output as an image.
  
  output$mapplot <- renderImage({
    
    # This filter the data according to the input of our user NOTE: I decided to
    # draw a sample of 50 because it took ages for the gganimate to load. Given
    # that we are REQUIRED by the prompt to make an interactive gganimate plot,
    # this was the only solution I could come up with. Another thing is, the
    # graphs still take a lot of time to load. This is just the way gganimate()
    # works.
    
    shape_wash_data <- st_as_sf(data %>% filter(year == input$year_1) %>% sample_n(50), 
                                coords = c("longitude", "latitude"), 
                                crs = 4326)
    
    # This is the way renderImage works
    
    outfile <- tempfile(fileext = '.gif')
    
    # This actually plots the map we need
    
    p <- ggplot(data = DC, aes()) +
      geom_sf() +
      geom_sf(data = shape_wash_data) +
      
      # We iterate over the data montly to see how the crime locations change
      # every month within a certain year
      
      transition_states(month_1) +
      
      # Healy recommends using BW theme
      
      theme_bw() +
      
      # This is to make the user aware of the which month the data represents
      
      labs(title = "Month: {closest_state}",
           caption = "Source: Shotspotter Data")
    
    # This saves the file for embedding
    
    anim_save("outfile.gif", animate(p))
    
    # I also had to do this due to the syntax
    
    list(src = "outfile.gif",
         contentType = 'image/gif'
    )
  })
  
  output$hoursPlot <- renderImage({
    
    region_subset <- st_as_sf(data %>% filter(date == input$date), coords = c("longitude", "latitude"),  crs=4326)
    
    outfile <- tempfile(fileext='.gif')
    
    p = ggplot(data = DC) +
      geom_sf() +
      geom_sf(data = sample) +
      
      # HEALEY Recommends black and white themes on maps. A simple black and
      # white theme is also the most practical theme: with an  interactive AND
      # animated interface, large database, and slow gganimate packaged, using
      # this theme is best. This is particularly so because gganimate makes
      # each frame and then runs them one after the other. 
      
      theme_bw() +
      transition_states(hour) + 
      labs(
        title = "Shot(s) at {closest_state}00 HRS",
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
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

