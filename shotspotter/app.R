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
library(gifski)

# This package allows our tabpanels to call for output enclosed by the
# withSpinner() function. Eg: withSpinner(plotOutput("mapplot"), type = 4). This
# function gives a loading symbol when the Shiny App is rendered, ensuring that
# the otherwise present whitespace when heavy renders like this one with
# animations isn't mistaken for an error or Shiny failure.

library(shinycssloaders)
library(transformr)
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
                 ) 
data_2 <- data
data <- data %>%
  mutate(month_1 = as.character(month(ymd(010101) + months(month-1),
                                    label = TRUE,
                                    abbr = FALSE)))

# Adding a date column 
# For plot #2, we allow the user to pick any date in the
# window in which the data exists and view all the gunshots in that day by hour.

data_2$date <- as.Date(paste(data_2$year, data_2$month, data_2$day, sep="-"), "%Y-%m-%d")

# After not finding valid shapefiles in the right format for DC, we chose to use
# the `tigris` package's inbuilt states() function for the US that allowed us to
# access shapefiles for American states. Filtering for DC, we found the
# shapefile we wanted.

DC <-  states(cb = TRUE)
DC <- DC[DC$NAME == "District of Columbia", ]

DC <- st_as_sf(DC)


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
                         
                         tabPanel("About",
                                  mainPanel(h1("Gun Shots in Washington, DC"),
                                            htmlOutput("about"))),
                         
                         tabPanel("Across the Years",
                                  sidebarPanel(
                                    
                                    # THis guides the user a little better
                                    
                                    helpText("Please select a year below to iterate over. 
                                             The graph on the right will show the monthly gunshot
                                             locations. Because of the nature of R animations, 
                                             the graphic may take up to 30 seconds to load. 
                                             Appreciate your patience!"),
                                    
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
                                             NOTE: The animation can take upto a minute to load, appreciate your patience!"),
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
  
  output$about <- renderText("<b>Authors: Hemanth Bharatha Chakravarthy and Ilkin Bayramli.</b><br><br>This project aims to investigate the gunshot data provided by the Justice Tech Lab. The Justice Tech Lab's ShotSpotter uses acoustic sensors to detect gunfire sounds and record time as well as location of all gunfire incidents in a covered area. We decided to use this data to investigate gunshot locations across months of different years and through the hours of any given day. We built this geospatial analysis using Shiny to be interactive and animated to better inform you, the user.<br><br>To find more information about the Shotspotter data, see:<br> Carr, Jillian B., and Jennifer L. Doleac. 2018. Keep the Kids Inside? Juvenile Curfews and Urban Gun Violence. Review of Economics and Statistics, 100(4): 609-618. <br> Carr, Jillian B., and Jennifer L. Doleac. 2016. The geography, incidence, and underreporting of gun violence: new evidence using ShotSpotter data. Brookings Research Paper.<br><br>You can access our code here, on GitHub: <a href='https://github.com/b-hemanth/shotspotter'>https://github.com/b-hemanth/shotspotter</a>. The dataset we use covers Washington DC for the years 2006-2017. You can find the full ShotSpotter dataset here: <a href=''http://justicetechlab.org/shotspotter-data/'>http://justicetechlab.org/shotspotter-data/</a>.")
  
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
     
     # Note: the above comments on why we need to use renderImage for Shiny to
     # process gifs applies for this function as well.
     
     region_subset <- st_as_sf(data_2 %>% filter(date == input$date), coords = c("longitude", "latitude"),  crs=4326)
     
     outfile <- tempfile(fileext='.gif')
     
     p = ggplot(data = DC) +
       geom_sf() +
       geom_sf(data = region_subset) +
       
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
     list(
       src = "outfile.gif",
       contentType = 'image/gif'
       )
})
}

# Run the application 
shinyApp(ui = ui, server = server)

