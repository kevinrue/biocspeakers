library(readr)
library(leaflet)
library(tidygeocoder)
library(ggplot2)

# import data ----

speaker_data <- read_csv(file = "speakers/speakers.csv", comment = "#", show_col_types = FALSE)

event_data <- read_csv(file = "events/events.csv", comment = "#", show_col_types = FALSE)

# get lat/long manually and save to avoid waiting each time
# lat_longs <- event_data %>%
#   geocode(city = city, method = 'osm')

# location of events ----

suppressWarnings(
  leaflet() %>%
    setView(lng = 2.3488, lat = 48.85341, zoom = 4) %>% # Paris: 48.85341 2.3488
    addTiles() %>%
    addMarkers(~long, ~lat, label = ~as.character(city), data = event_data)
)

# proportion of speakers from each country ----

ggplot(speaker_data) +
  geom_bar(aes(country, fill = country)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.title = element_blank()
  )
