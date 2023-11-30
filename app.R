## app.R ##
library(shiny)
library(shinydashboard)
library(readr)

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
      width = 12
    )
  )
)

server <- function(input, output) {
  output$speaker_data_table <- renderDataTable(speaker_data)
}

app <- shinyApp(ui, server)

shiny::runApp(app, launch.browser = TRUE)
