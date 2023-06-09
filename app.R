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

# Code is uploaded on GitHub: https://github.com/Larsibaer/sbd1_shinyapp/

# Get the current directory path
current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the current directory
setwd(current_dir)

# Load the data from the relative path
data <- jsonlite::fromJSON("./data/significant-earthquake-database.json")

# Löschen von lückenhafen Einträgen
data <- data %>%
  filter(!is.na(coordinates$lon) & !is.na(coordinates$lat))


data <- data[c(1,2,3,34,5,6,7,15,19,43)]

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
              valueBoxOutput("info_1"),
              valueBoxOutput("info_2"),
              valueBoxOutput("info_3")
            ),
            fluidRow(
              div(style = 'overflow-x: auto', DT::DTOutput("table"))
            )
    ),
    # Third tab content
    tabItem(
      tabName = "Infos",
      tags$div(
        class = "container",
        tags$h2("Part of a series on Earthquakes"),
        tags$p("Here you find some interesting Links for different parts of series on earthquakes:"),
        tags$div(
          style = 'overflow-x: auto',
          DT::DTOutput("info")
        )
      )
    ),
    # Fourth tab content
    tabItem(tabName = "Metriken",
            titlePanel("Metriken"),
            fluidRow(
              div(style = 'overflow-x: auto', DT::DTOutput("metrics"))
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
    title = "Earthquakes"
  ),
  sidebar,
  body,
  tags$head(
    tags$script(HTML(js_code)),
    tags$style(HTML(css)),
  ),
  skin = "blue"
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
    valueBox(nrow(data_filtered()), subtitle = "Anzahl Erdbeben", icon = icon(name = "house-crack"), color = "aqua")
  })
  
  # Render Box 2
  output$box_2 <- renderValueBox({
    valueBox(nrow(data_filtered()%>%filter(flag_tsunami == "Tsunami")), subtitle = "Anzahl Tsunami", icon = icon(name = "tornado"), color = "aqua")
  })
  
  # Render Box 2
  output$box_3 <- renderValueBox({
    valueBox(length(unique(data_filtered()$country)), subtitle = "Anzahl Länder", icon = icon(name = "globe"), color = "aqua")
  })
  
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet(data_filtered()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~coordinates$lon,
        lat = ~coordinates$lat,
        color = "blue",
        popup = ~paste("<table>",
                       "<tr><td>ID: </td><td>", i_d, "</td></tr>",
                       "<tr><td>Year: </td><td>", year, "</td></tr>",
                       "<tr><td>Stärke: </td><td>", eq_primary, "</td></tr>",
                       "<tr><td>Anzahl Tote: </td><td>", deaths, "</td></tr>",
                       "<tr><td>Country: </td><td>", country, "</td></tr>",
                       "</table>"),
        radius = ~ifelse(!is.na(eq_primary), eq_primary^1.4, 1)) %>%
      setView(lng = mean(data$coordinates$lon), lat = mean(data$coordinates$lat), zoom = 3)
  })
  
  # Table:
  
  output$table <- DT::renderDT({
      data_filtered()
  })
  
  # Table Info:
  # Render Box 1
  output$info_1 <- renderInfoBox({
    valueBox("flag_tsunami", subtitle = "Indicate when a tsunami was generated by an earthquake", icon = icon(name = "tornado"), color = "aqua")
  })
  
  # Render Box 2
  output$info_2 <- renderInfoBox({
    valueBox("focal_depth", subtitle = "The depth of the earthquake is given in kilometers.", icon = icon(name = "ruler-vertical"), color = "aqua")
  })
  
  # Render Box 3
  output$info_3 <- renderInfoBox({
    valueBox("eq_primary", subtitle = "refers to the primary or main earthquake event within a seismic sequence", icon = icon(name = "house-crack"), color = "aqua")
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
  
  # Metriken
  # Scrape the webpage and extract the table
  url2 <- "https://public.opendatasoft.com/explore/dataset/significant-earthquake-database/information/?flg=en&rows=10000&refine.country=INDONESIA&refine.location_name=INDONESIA:++JAVA"
  page2 <- read_html(url2)
  page2 %>% html_element(xpath = '//div[@class="odswidget-dataset-schema__field ng-scope"]') %>% html_text()
}



# Run the Shiny app
shinyApp(ui = ui, server = server)
