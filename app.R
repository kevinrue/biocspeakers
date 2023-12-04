## app.R ##
library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)
library(readr)
library(shiny)
library(shinydashboard)
library(tidyr)

source("functions.R")

# Speaker data ----

speaker_data <- read_csv(file = "speakers/speakers.csv", comment = "#", show_col_types = FALSE) %>%
  mutate(across(c(person_country, person_position, event_type, event_year, person_role, person_gender), as.factor))

# Event data ----

event_data <- read_csv(file = "events/events.csv", comment = "#", show_col_types = FALSE) %>%
  mutate(across(c(event_type, event_year, event_city, event_country), as.factor))

# join event country to speaker data ----

speaker_data <- speaker_data %>%
  left_join(
    event_data %>%
      select(event_country, event_type, event_year),
    by = c("event_type", "event_year"))

# join speaker countries to event data ----

event_data <- event_data %>%
  left_join(
    speaker_data %>%
      select(person_country, event_type, event_year),
    by = c("event_type", "event_year")
  )

# Choices ----

plot_types <- c("Speakers" = "speakers", "Events" = "events")

# App ----

ui <- dashboardPage(
  dashboardHeader(title = "Speakers stats"),
  dashboardSidebar(
    actionButton("reset", "Reset selections", icon = icon("refresh"), width = "80%")
  ),
  dashboardBody(
    fluidRow(
      box(
        selectizeInput("plot_type", label = "Plot type", choices = plot_types, selected = plot_types[1], width = "200px"),
        plotOutput("barplot", click = "barplot_click"),
        uiOutput("barplot_selection_summary"),
        title = "Plot",
        width = 6
      ),
      box(
        leafletOutput("leaflet_map"),
        h4("Legend"),
        p(strong("Markers:"), "events"),
        p(strong("Circles:"), "speakers"),
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

  # reactive values ----

  reactive_values <- reactiveValues(
    selected_speaker_countries = character(0),
    selected_event_countries = character(0),
    leaflet_map_center = list(
      lng = 2.3488,
      lat = 48.86471
    ),
    leaflet_map_zoom = 4L
  )

  # barplot ----

  output$barplot <- renderPlot({
    plot_type <- input[["plot_type"]]
    # plot_types
    if (identical(plot_type, "speakers")) {
      gg <- speakers_barplot(speaker_data,
        speaker_countries = reactive_values[["selected_speaker_countries"]],
        event_countries = reactive_values[["selected_event_countries"]]
      )
    } else if (identical(plot_type, "events")) {
      gg <- events_barplot(event_data,
        speaker_countries = reactive_values[["selected_speaker_countries"]],
        event_countries = reactive_values[["selected_event_countries"]]
      )
    }
    gg
  })

  # barplot selection summary ----

  output$barplot_selection_summary <- renderUI({
    selected_speaker_countries <- reactive_values[["selected_speaker_countries"]]
    selected_event_countries <- reactive_values[["selected_event_countries"]]
    if (length(selected_speaker_countries)) {
      speaker_summary <- paste(reactive_values[["selected_speaker_countries"]], collapse = " ")
    } else {
      speaker_summary <- "No selection."
    }
    if (length(selected_event_countries)) {
      event_summary <- paste(reactive_values[["selected_event_countries"]], collapse = " ")
    } else {
      event_summary <- "No selection."
    }
    tagList(
      p("Click on the plots to add/remove items to the active selection."),
      p(
        strong("Speakers: "),
        speaker_summary
      ),
      p(
        strong("Events: "),
        event_summary
      )
    )
  })

  # leaflet map ----

  output$leaflet_map <- renderLeaflet({
    speaker_data_filtered <- speaker_data
    event_data_filtered <- event_data
    selected_speaker_countries <- reactive_values[["selected_speaker_countries"]]
    selected_event_countries <- reactive_values[["selected_event_countries"]]
    speaker_data_filtered <- filter_person_countries(speaker_data_filtered, selected_speaker_countries)
    speaker_data_filtered <- filter_event_countries(speaker_data_filtered, selected_event_countries)
    event_data_filtered <- filter_event_countries(event_data_filtered, selected_event_countries) %>%
      filter_person_countries(selected_speaker_countries) %>%
      select(event_city, event_lat, event_long, event_country) %>%
      unique() %>%
      unite("label", event_city, event_country, sep = ", ")
    suppressWarnings(
      leaflet() %>%
        setView(
          lng = reactive_values$leaflet_map_center$lng,
          lat = reactive_values$leaflet_map_center$lat,
          zoom = reactive_values$leaflet_map_zoom
        ) %>%
        addTiles() %>%
        addMarkers(~event_long, ~event_lat, label = ~label, data = event_data_filtered) %>%
        addCircleMarkers(~person_long, ~person_lat, radius = 2, label = ~as.character(person_institution), data = speaker_data_filtered)
    )
  })

  # speaker data table ----

  output$speaker_data_table <- renderDT({
    speaker_data_filtered <- speaker_data %>%
      select(-c(person_lat, person_long))
    selected_speaker_countries <- reactive_values[["selected_speaker_countries"]]
    selected_event_countries <- reactive_values[["selected_event_countries"]]
    speaker_data_filtered <- filter_person_countries(speaker_data_filtered, selected_speaker_countries)
    speaker_data_filtered <- filter_event_countries(speaker_data_filtered, selected_event_countries)
    datatable(
      speaker_data_filtered,
      filter = "top"
    )
  })

  # observeEvent ----

  observeEvent(input[["reset"]], {
    reactive_values[["selected_event_countries"]] <- character(0)
    reactive_values[["selected_speaker_countries"]] <- character(0)
  })

  observeEvent(input[["barplot_click"]], {
    if (identical(input[["plot_type"]], "speakers")) {
      click_country <- levels(speaker_data[["person_country"]])[round(input$barplot_click$x)]
      selected_speaker_countries <- reactive_values[["selected_speaker_countries"]]
      if (click_country %in% selected_speaker_countries) {
        selected_speaker_countries <- setdiff(selected_speaker_countries, click_country)
      } else {
        selected_speaker_countries <- sort(c(selected_speaker_countries, click_country))
      }
      reactive_values[["selected_speaker_countries"]] <- selected_speaker_countries
    } else if (identical(input[["plot_type"]], "events")) {
      click_country <- levels(event_data[["event_country"]])[round(input$barplot_click$x)]
      selected_event_countries <- reactive_values[["selected_event_countries"]]
      if (click_country %in% selected_event_countries) {
        selected_event_countries <- setdiff(selected_event_countries, click_country)
      } else {
        selected_event_countries <- sort(c(selected_event_countries, click_country))
      }
      reactive_values[["selected_event_countries"]] <- selected_event_countries
    }
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
