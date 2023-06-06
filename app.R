library(shiny)
library(leaflet)
library(dplyr)
library(jsonlite)
library(readr)
library(DT)
library(shinyWidgets)
# Get the current directory path
current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the current directory
setwd(current_dir)

# Load the data from the relative path
data <- jsonlite::fromJSON("./data/significant-earthquake-database.json")

# Löschen von falschen Einträgen
data <- data %>%
  filter(year >= 1968 & year <= 2023)

data <- head(data, 50)
data <- data[c(1,2,3,34,5,7,15,43)]

# UI
ui <- fluidPage(
  titlePanel("Erdbeben Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput("yearInput", "Jahr:", choices=unique(data$year), options = list(`actions-box` = TRUE), multiple = TRUE, selected = unique(data$year)),
      pickerInput("countryInput", "Land:", choices=unique(data$country), options = list(`actions-box` = TRUE), multiple = TRUE, selected = unique(data$country)),
      sliderInput("eq_primaryInput", "Magnitude:", min = 0, max = 10, value = c(0, 10))
    ),
    
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load and filter data
  data_filtered <- reactive({
    data %>%
      filter(year %in% input$yearInput,
             country %in% input$countryInput,
             eq_primary >= input$eq_primaryInput[1] & eq_primary <= input$eq_primaryInput[2])
  })
  
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet(data_filtered()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~coordinates$lon,
        lat = ~coordinates$lat,
        popup = ~paste("ID:", i_d, "<br>",
                       "Year:", year, "<br>",
                       "Magnitude:", eq_primary, "<br>",
                       "Country:", country),
        radius = 5)
      # ) %>%
      # setView(lng = mean(data$coordinates$lon), lat = mean(data$coordinates$lat), zoom = 2)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
