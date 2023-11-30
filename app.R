## app.R ##
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(leaflet)

# Speaker data ----

speaker_data <- read_csv(file = "speakers/speakers.csv", comment = "#") %>%
  mutate(across(c(country, event_type), as.factor))

# Event data ----

event_data <- read_csv(file = "events/events.csv", comment = "#")

# App ----

ui <- dashboardPage(
  dashboardHeader(title = "Speakers stats"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        plotOutput("speaker_country_barplot", click = "speaker_country_barplot_click"),
        title = "Plot",
        width = 6
      ),
      box(
        leafletOutput("speaker_map"),
        h4("Legend"),
        p(strong("Blue markers:"), "events"),
        title = "Map",
        width = 6
      )
    ),
    fluidRow(
      box(
        dataTableOutput("speaker_data_table"),
        title = "Table",
        width = 12
      )
    )
  )
)

server <- function(input, output) {
  output$speaker_data_table <- renderDataTable(speaker_data)

  output$speaker_map <- renderLeaflet({
    suppressWarnings(
      leaflet() %>%
        setView(lng = 2.3488, lat = 48.85341, zoom = 4) %>% # Paris: 48.85341 2.3488
        addTiles() %>%
        addMarkers(~long, ~lat, label = ~as.character(city), data = event_data)
    )
  })

  output$speaker_country_barplot <- renderPlot({
    ggplot(speaker_data) +
      geom_bar(aes(country, fill = country)) +
      guides(
        fill = "none"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text = element_text(size = 16),
        axis.title = element_blank()
      )
  })

  observeEvent(input$speaker_country_barplot_click, {
    click_country <- levels(speaker_data$country)[round(input$speaker_country_barplot_click$x)]
    message("Selected country: ", click_country)
  })
}

app <- shinyApp(ui, server)
# shiny::runApp(app)
shiny::runApp(app, launch.browser = TRUE)
