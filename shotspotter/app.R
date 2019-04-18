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

shape_data <- st_as_sf(data_16, coords = c("longitude", "latitude"),  crs=4326)


ggplot(data = shapes_data) +
  geom_sf() +
  geom_sf(data = arrest_locations) +
  theme_map() + 
  labs(title = "Location of Hartford Arrests",
       subtitle = "Asian-Americans 2013 -- 2016", 
       caption = "Source: Stanford Open Policing Project" )

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

