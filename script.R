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

# annotate speaker info with event info ----

speaker_data <- speaker_data %>%
  left_join(
    event_data %>%
      select(country, event_type, year) %>%
      rename("event_country" = "country"),
    by = c("event_type", "year"))

# join speaker countries to event data ----

event_data <- event_data %>%
  left_join(
    speaker_data %>%
      select(country, event_type, year) %>%
      rename("speaker_country" = "country"),
    by = c("event_type", "year")
  )

# get lat/long manually and save to avoid waiting each time
# event_data %>%
#   geocode(city = city, method = 'osm')

# speaker_data %>%
#   geocode(address = institution, method = 'osm')

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

# proportion of events in each country ----

selected_countries <- c("Belgium")

event_data_plot <- event_data %>%
  mutate(
    selected = factor(country %in% selected_countries)
  )

ggplot(event_data_plot) +
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


#

speaker_countries <- c("Switzerland")
event_countries <- c("Belgium")

event_data_plot_all <- event_data %>%
  select(country, event_type, year) %>%
  unique()

event_data_plot_speakers <- event_data %>%
  filter(speaker_country %in% speaker_countries) %>%
  select(country, event_type, year) %>%
  unique() %>%
  mutate(
    selected_event_country = factor(country %in% event_countries)
  )

ggplot() +
  geom_bar(aes(country, fill = country), event_data_plot_all, alpha = 0.25, color = "grey") +
  geom_bar(aes(country, fill = country, alpha = selected_event_country, color = selected_event_country), event_data_plot_speakers) +
  scale_alpha_manual(values = c("FALSE" = 0.25, "TRUE" = 1)) +
  scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "black")) +
  guides(
    fill = "none",
    alpha = "none",
    color = "none"
  )
