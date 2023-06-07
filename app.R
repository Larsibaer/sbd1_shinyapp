library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(jsonlite)
library(readr)
library(DT)
library(shinyWidgets)
library(rvest)
library(purrr)
# Get the current directory path
current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the current directory
setwd(current_dir)

# Load the data from the relative path
data <- jsonlite::fromJSON("./data/significant-earthquake-database.json")

# Löschen von falschen Einträgen
data <- data %>%
  filter(!is.na(country) & !is.na(coordinates$lon) & !is.na(coordinates$lat) & year >= 1968 & year <= 2023)


data <- data[c(1,2,3,34,5,7,15,43)]

# body
body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "Map",
            tags$style(type = "text/css", "#plot {height: calc(100vh - 80px) !important;}"),
            fluidRow(
              valueBoxOutput("box_1"),
              valueBoxOutput("box_2"),
              valueBoxOutput("box_3")
            ),
            fluidRow(
              leafletOutput("map")
            )
    ),
    
    # Second tab content
    tabItem(tabName = "Tables",
            fluidRow(
              div(style = 'overflow-x: auto', DT::DTOutput("table"))
            )
    ),
    # Third tab content
    tabItem(tabName = "Infos",
            titlePanel("Part of a series on Earthquakes"),
            fluidRow(
              div(style = 'overflow-x: auto', DT::DTOutput("info"))
            )
    )
  )
)

sidebar <-dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "Map"),
    menuItem("Tables", tabName= "Tables"),
    menuItem("Infos", tabName= "Infos"),
    pickerInput("countryInput", "Wähle die Länder:", choices=unique(data$country), options = list(`actions-box` = TRUE), multiple = TRUE, selected = unique(data$country)),
    sliderInput("yearInput", "Wähle eine Zeitspanne:", min = min(data$year), max = max(data$year), value = c(min(data$year), max(data$year)),step = 1, sep = ""),
    sliderInput("eq_primaryInput", "Wähle die Stärke:", min = 0, max = 10, value = c(0, 10))
))

js_code <- '
function copyToClipboard(text) {
    var textarea = document.createElement("textarea");
    textarea.value = text;
    document.body.appendChild(textarea);
    textarea.select();
    document.execCommand("copy");
    document.body.removeChild(textarea);
    alert("Text copied to clipboard: " + text);
}'

css <- "
custom-cursor:hover {
    cursor: pointer;
}
"

ui <- dashboardPage(
  dashboardHeader(
    title = "Hello World"
  ),
  sidebar,
  body,
  tags$head(
    tags$script(HTML(js_code)),
    tags$style(HTML(css)),
  ),
  skin = "purple"
)

# Server
server <- function(input, output, session) {
  
  # Load and filter data
  data_filtered <- reactive({
    data %>%
      filter(year >= input$yearInput[1] & year <= input$yearInput[2],
             country %in% input$countryInput,
             eq_primary >= input$eq_primaryInput[1] & eq_primary <= input$eq_primaryInput[2])
  })
  
  # Render Box 1
  output$box_1 <- renderValueBox({
    valueBox(nrow(data_filtered()), subtitle = "Anzahl Erdbeben", icon = icon(name = "table-list"), color = "yellow")
  })
  
  # Render Box 2
  output$box_2 <- renderValueBox({
    valueBox(nrow(data_filtered()%>%filter(flag_tsunami == "Tsunami")), subtitle = "Anzahl Tsunamis", icon = icon(name = "table-list"), color = "yellow")
  })
  
  # Render Box 2
  output$box_3 <- renderValueBox({
    valueBox(length(unique(data_filtered()$country)), subtitle = "Anzahl Länder", icon = icon(name = "table-list"), color = "yellow")
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
                       "Stärke", eq_primary, "<br>",
                       "Country:", country),
        radius = ~ifelse(!is.na(eq_primary), eq_primary^1.4, 1)) %>%
      setView(lng = mean(data$coordinates$lon), lat = mean(data$coordinates$lat), zoom = 4)
  })
  
  # Table:
  
  output$table <- DT::renderDT({
      data_filtered()
  })
  
  # Info
  
  
  # Scrape the webpage and extract the table
  url <- "https://en.wikipedia.org/wiki/Earthquake"
  page <- read_html(url)
  
  # Find all td elements with class "sidebar-content"
  td_elements <- html_nodes(page, "tbody td.sidebar-content")
  td_elements <- td_elements[1:6]
  
  # Extract the sidebar-list-title and links from each sidebar-content
  data_html <- lapply(td_elements, function(td) {
    title <- html_text(html_node(td, "div.sidebar-list-title"))
    links <- html_nodes(td, "div.sidebar-list-content a")
    name <- html_text(links)
    link <- html_attr(links, "href")
    link <- paste0("<a href='https://en.wikipedia.org/", link, "'>Link</a>")
    list(Title = title,  Name = name, Link = link)
  })
  
  
  # Convert the list to a dataframe
  # df <- data.frame(do.call(rbind, data_html))
  df <- map_df(data_html, as.data.frame)
  
  
  output$info <- renderDT({
    datatable(df, escape = FALSE) %>% 
      formatStyle("Link", textDecoration = "underline", fontWeight = "bold",
                  fontStyle = "italic", cursor = "pointer")
  })
}



# Run the Shiny app
shinyApp(ui = ui, server = server)
