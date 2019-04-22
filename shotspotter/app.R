library(shiny)
library(fs)
library(tidyverse)
library(mapview)
library(leaflet)
library(tidycensus)
library(sf)
library(tmap)
library(tmaptools)
library(tigris)
library(ggplot2)
library(viridis)

a = c("shiny",
          "fs",
          "tidyverse",
          "mapview",
          "leaflet",
          "tidycensus",
          "sf",
          "tmap",
          "tmaptools",
          "tigris",
          "ggplot2",
          "viridis")

for (i in a){
   install.packages(i)
}

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

data_16 <- data %>%
  filter(year == 2016)

shape_wash_data <- st_as_sf(data_16, coords = c("longitude", "latitude"),  crs=4326)

shapes_data <- read_sf("shotspotter/Washington_DC_Boundary.shp")

ggplot(data = shapes_data) +
  geom_sf() +
  geom_sf(data = shape_wash_data) +
  theme_map() + 
  labs(title = "Location of DC Gunshots",
       subtitle = "In the year 2016", 
       caption = "Source: Shotspotter Data")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

