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
# We downloaded the data and saved it in the github repo. By the nature of the
# dataset, we don't expect it to change anythime soon.

data <- read_csv("shotspotter/wash_data.csv",
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

ggplot(data = DC) +
  geom_sf() +
  geom_sf(data = shape_wash_data) +
  theme_map() + 
  transition_states(year) + 
  labs(title = "Location of DC Shootings by Year",
       subtitle = "Year: {closest_state}",
       caption = "Source: Shotspotter Data")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("year",
                     "Year:",
                     min = 2006,
                     max = 2017,
                     value = 2006,
                     sep = "")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         withSpinner(plotOutput("mapplot"), type = 4)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$mapplot <- renderPlot({
      # generate bins based on input$bins from ui.R
      shape_wash_data <- st_as_sf(data %>% filter(year == input$year, numshots > 1), coords = c("longitude", "latitude"),  crs=4326)
      
      ggplot(data = DC) +
         geom_sf() +
         geom_sf(data = shape_wash_data) +
         theme_map()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

