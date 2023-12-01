library(dplyr)
library(ggplot2)
library(leaflet)
library(readr)
library(tidygeocoder)

# import data ----

speaker_data <- read_csv(file = "speakers/speakers.csv", comment = "#", show_col_types = FALSE) %>%
  mutate(across(c(country, position, event_type, year, event_role, gender), as.factor))

event_data <- read_csv(file = "events/events.csv", comment = "#", show_col_types = FALSE) %>%
  mutate(across(c(event_type, year, city, country), as.factor))

# get lat/long manually and save to avoid waiting each time
# event_data %>%
#   geocode(city = city, method = 'osm')

speaker_data %>%
  geocode(address = institution, method = 'osm')

# location of events ----

suppressWarnings(
  leaflet() %>%
    setView(lng = 2.3488, lat = 48.85341, zoom = 4) %>% # Paris: 48.85341 2.3488
    addTiles() %>%
    addMarkers(~long, ~lat, label = ~as.character(city), data = event_data)
)

# location of speakers ----

suppressWarnings(
  leaflet() %>%
    setView(lng = 2.3488, lat = 48.85341, zoom = 4) %>% # Paris: 48.85341 2.3488
    addTiles() %>%
    addCircleMarkers(~long, ~lat, radius = 2, color =  label = ~as.character(institution), data = speaker_data)
)

# proportion of speakers from each country ----

selected_countries <- c("Italy", "Spain")

speaker_data_plot <- speaker_data %>%
  mutate(
    selected = factor(country %in% selected_countries)
  )

ggplot(speaker_data_plot) +
  geom_bar(aes(country, fill = country, alpha = selected, color = selected)) +
  scale_alpha_manual(values = c("FALSE" = 0.25, "TRUE" = 1)) +
  scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "black")) +
  guides(
    fill = "none",
    alpha = "none",
    color = "none"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.text = element_text(size = 16),
    axis.title = element_blank()
  )
