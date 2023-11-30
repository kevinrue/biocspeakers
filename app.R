## app.R ##
library(shiny)
library(shinydashboard)
library(readr)

# Speaker data ----

speaker_data <- read.csv(file = "speakers.csv", comment.char = "#")


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
