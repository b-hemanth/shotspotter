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
library(lubridate)
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
                 ) %>%
  mutate(month = as.character(month(ymd(010101) + months(month-1),
                                    label = TRUE,
                                    abbr = FALSE)))

DC <-  states(cb = TRUE)


DC <- DC[DC$NAME == "District of Columbia", ]

DC <- st_as_sf(DC)
shape_wash_data <- st_as_sf(data %>% filter(year == 2016) %>% sample_n(10), coords = c("longitude", "latitude"),  crs=4326)

ggplot(data = DC) +
  geom_sf() +
  geom_sf(data = shape_wash_data) +
  theme_map() +
  labs(title = "Location of DC Shootings by Year",
       caption = "Source: Shotspotter Data")


ui <- shinyUI(navbarPage("Washington DC Gun Shot Location Data",
                         theme = shinytheme("darkly"),
                         tabPanel("Across the Years",
                                  sidebarPanel(
                                    selectInput("year_1",
                                                "Select the year to iterate over:",
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
                                               withSpinner(imageOutput("mapplot"), type = 4)
                                               ))
                                  )
                         ),
                         tabPanel("In a Day",
                                  sidebarPanel(
                                    radioButtons("year",
                                                 "Year: ", unique(data$year))
                                  ),
                                  mainPanel(
                                    tabPanel("In a Day",
                                                     plotOutput("hoursPlot"))
                                    )
                         )
))


# Define server logic required to draw a histogram

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
        
        transition_states(month) +
        
        # Healy recommends using BW theme
        
        theme_bw() +
        
        # This is to make the user aware of the which month the data represents
        
        labs(title = "Month: {closest_state}")
      
      # This saves the file for embedding
      
      anim_save("outfile.gif", animate(p))
      
      # I also had to do this due to the syntax
      
      list(src = "outfile.gif",
           contentType = 'image/gif'
      )
   })
   
   output$hoursPlot <- renderPlot({
     region_subset <- st_as_sf(data %>% filter(year == input$year, !is.na(numshots)), coords = c("longitude", "latitude"),  crs=4326)
     ggplot(data = DC) +
       geom_sf() +
       geom_sf(data = region_subset) +
       theme_map() +
       transition_states(hour) + 
       labs(
         title = "Location of DC Shootings During the Day",
         subtitle = "Hour: {closest_state}",
         caption = "Source: Shotspotter Data"
         )
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

