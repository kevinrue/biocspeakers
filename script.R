library(readr)
library(leaflet)
library(tidygeocoder)

speaker_data <- read.csv(file = "speakers/speakers.csv", comment.char = "#")

event_data <- read.csv(file = "events/events.csv", comment.char = "#")

lat_longs <- event_data %>%
  geocode(city = city, method = 'osm')

leaflet() %>%
  setView(lng = 2.3488, lat = 48.85341, zoom = 4) %>% # Paris: 48.85341 2.3488
  addTiles() %>%
  addMarkers(~long, ~lat, label = ~as.character(city), data = lat_longs)
