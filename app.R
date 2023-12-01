## app.R ##
library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)
library(readr)
library(shiny)
library(shinydashboard)

source("functions.R")

# Speaker data ----

speaker_data <- read_csv(file = "speakers/speakers.csv", comment = "#", show_col_types = FALSE) %>%
  mutate(across(c(country, position, event_type, year, event_role, gender), as.factor))

# Event data ----

event_data <- read_csv(file = "events/events.csv", comment = "#", show_col_types = FALSE) %>%
  mutate(across(c(event_type, year, city, country), as.factor))

# App ----

ui <- dashboardPage(
  dashboardHeader(title = "Speakers stats"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        plotOutput("speaker_country_barplot", click = "speaker_country_barplot_click"),
        uiOutput("speaker_country_selected"),
        title = "Plot",
        width = 6
      ),
      box(
        leafletOutput("leaflet_map"),
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
  # Paris: 48.85341 2.3488
  reactive_values <- reactiveValues(
    selected_countries = character(0),
    leaflet_map_center = list(
      lng = 2.3488,
      lat = 48.86471
    ),
    leaflet_map_zoom = 4L
  )

  output$speaker_country_barplot <- renderPlot({
    selected_countries <- reactive_values[["selected_countries"]]
    speaker_data_plot <- speaker_data
    if (length(selected_countries)) {
      speaker_data_plot <- speaker_data_plot %>%
        mutate(
          selected = factor(country %in% selected_countries)
        )
      gg <- ggplot(speaker_data_plot) +
        geom_bar(aes(country, fill = country, alpha = selected, color = selected)) +
        scale_alpha_manual(values = c("FALSE" = 0.25, "TRUE" = 1)) +
        scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "black")) +
        guides(
          fill = "none",
          alpha = "none",
          color = "none"
        )
    } else {
      gg <- ggplot(speaker_data_plot) +
        geom_bar(aes(country, fill = country)) +
        guides(
          fill = "none"
        )
    }
    gg +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text = element_text(size = 16),
        axis.title = element_blank()
      )
  })

  output$speaker_country_selected <- renderUI({
    selected_countries <- reactive_values[["selected_countries"]]
    if (length(selected_countries)) {
      p(strong("Selected countries: "), paste(reactive_values[["selected_countries"]], collapse = " "))
    } else {
      p("Click countries to (un)select them.")
    }
  })

  output$leaflet_map <- renderLeaflet({
    speaker_data_filtered <- speaker_data
    selected_speaker_countries <- reactive_values[["selected_countries"]]
    speaker_data_filtered <- filter_selected_countries(speaker_data_filtered, selected_speaker_countries)
    suppressWarnings(
      leaflet() %>%
        setView(
          lng = reactive_values$leaflet_map_center$lng,
          lat = reactive_values$leaflet_map_center$lat,
          zoom = reactive_values$leaflet_map_zoom
        ) %>%
        addTiles() %>%
        addMarkers(~long, ~lat, label = ~as.character(city), data = event_data) %>%
        addCircleMarkers(~long, ~lat, radius = 2, label = ~as.character(institution), data = speaker_data_filtered)
    )
  })

  output$speaker_data_table <- renderDT({
    speaker_data_filtered <- speaker_data
    selected_countries <- reactive_values[["selected_countries"]]
    speaker_data_filtered <- filter_selected_countries(speaker_data_filtered, selected_countries)
    datatable(
      speaker_data_filtered,
      filter = "top"
    )
  })

  observeEvent(input$speaker_country_barplot_click, {
    click_country <- levels(speaker_data$country)[round(input$speaker_country_barplot_click$x)]
    selected_countries <- reactive_values[["selected_countries"]]
    if (click_country %in% selected_countries) {
      selected_countries <- setdiff(selected_countries, click_country)
    } else {
      selected_countries <- sort(c(selected_countries, click_country))
    }
    reactive_values[["selected_countries"]] <- selected_countries
  })

  observeEvent(input$leaflet_map_center, {
    reactive_values$leaflet_map_center <- input$leaflet_map_center
  })

  observeEvent(input$leaflet_map_zoom, {
    reactive_values$leaflet_map_zoom <- input$leaflet_map_zoom
  })
}

app <- shinyApp(ui, server)

if (interactive()) {
  shiny::runApp(app, launch.browser = TRUE)
} else {
  app
}

# app <- shinyApp(ui, server)
# shiny::runApp(app)
# shiny::runApp(app, launch.browser = TRUE)
