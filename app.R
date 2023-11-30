## app.R ##
library(shiny)
library(shinydashboard)
library(readr)
library(leaflet)

# Speaker data ----

speaker_data <- read_csv(file = "speakers/speakers.csv", comment = "#")

# Event data ----

event_data <- read_csv(file = "events/events.csv", comment = "#")

# App ----

ui <- dashboardPage(
  dashboardHeader(title = "Speakers stats"),
  dashboardSidebar(),
  dashboardBody(
    box(
      dataTableOutput("speaker_data_table"),
      title = "Table",
      width = 6
    ),
    box(
      leafletOutput("speaker_map"),
      title = "Map",
      width = 6,
      height = 700
    )
  )
)

server <- function(input, output) {
  output$speaker_data_table <- renderDataTable(speaker_data)

  output$speaker_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 2.3488, lat = 48.85341, zoom = 4) %>% # Paris: 48.85341 2.3488
      addTiles()
  })
}

app <- shinyApp(ui, server)
# shiny::runApp(app)
shiny::runApp(app, launch.browser = TRUE)
