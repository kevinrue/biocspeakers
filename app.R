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
    ),
    fluidRow(
      box(
        plotOutput("event_country_barplot", click = "event_country_barplot_click"),
        title = "Event country barplot",
        width = 6
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

  # speaker country barplot ----

  output$speaker_country_barplot <- renderPlot({
    selected_speaker_countries <- reactive_values[["selected_speaker_countries"]]
    speaker_data_plot <- speaker_data
    if (length(selected_speaker_countries)) {
      speaker_data_plot <- speaker_data_plot %>%
        mutate(
          selected = factor(country %in% selected_speaker_countries)
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
      labs(
        title = "Speakers by country"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text = element_text(size = 16),
        axis.title = element_blank()
      )
  })

  output$speaker_country_selected <- renderUI({
    selected_speaker_countries <- reactive_values[["selected_speaker_countries"]]
    if (length(selected_speaker_countries)) {
      p(
        strong("Selected countries: "),
        paste(reactive_values[["selected_speaker_countries"]], collapse = " ")
      )
    } else {
      p("Click countries to (un)select them.")
    }
  })

  # event country barplot ----

  output$event_country_barplot <- renderPlot({
    selected_event_countries <- reactive_values[["selected_event_countries"]]
    event_data_plot <- event_data
    if (length(selected_event_countries)) {
      event_data_plot <- event_data_plot %>%
        mutate(
          selected = factor(country %in% selected_event_countries)
        )
      gg <- ggplot(event_data_plot) +
        geom_bar(aes(country, fill = country, alpha = selected, color = selected)) +
        scale_alpha_manual(values = c("FALSE" = 0.25, "TRUE" = 1)) +
        scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "black")) +
        guides(
          fill = "none",
          alpha = "none",
          color = "none"
        )
    } else {
      gg <- ggplot(event_data_plot) +
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

  # leaflet map ----

  output$leaflet_map <- renderLeaflet({
    speaker_data_filtered <- speaker_data
    selected_speaker_countries <- reactive_values[["selected_speaker_countries"]]
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

  # speaker data table ----

  output$speaker_data_table <- renderDT({
    speaker_data_filtered <- speaker_data
    selected_speaker_countries <- reactive_values[["selected_speaker_countries"]]
    speaker_data_filtered <- filter_selected_countries(speaker_data_filtered, selected_speaker_countries)
    datatable(
      speaker_data_filtered,
      filter = "top"
    )
  })

  # observeEvent ----

  observeEvent(input$speaker_country_barplot_click, {
    click_country <- levels(speaker_data$country)[round(input$speaker_country_barplot_click$x)]
    selected_speaker_countries <- reactive_values[["selected_speaker_countries"]]
    if (click_country %in% selected_speaker_countries) {
      selected_speaker_countries <- setdiff(selected_speaker_countries, click_country)
    } else {
      selected_speaker_countries <- sort(c(selected_speaker_countries, click_country))
    }
    reactive_values[["selected_speaker_countries"]] <- selected_speaker_countries
  })

  observeEvent(input$event_country_barplot_click, {
    click_country <- levels(event_data$country)[round(input$event_country_barplot_click$x)]
    selected_event_countries <- reactive_values[["selected_event_countries"]]
    if (click_country %in% selected_event_countries) {
      selected_event_countries <- setdiff(selected_event_countries, click_country)
    } else {
      selected_event_countries <- sort(c(selected_event_countries, click_country))
    }
    reactive_values[["selected_event_countries"]] <- selected_event_countries
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
