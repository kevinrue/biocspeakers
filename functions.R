filter_country <- function(x, selected) {
  if (length(selected)) {
    x <- x %>%
      filter(country %in% selected) %>%
      mutate(country = factor(country, unique(country)))
  }
  x
}

filter_event_countries <- function(x, selected) {
  if (length(selected)) {
    x <- x %>%
      filter(event_country %in% selected) %>%
      mutate(country = factor(country, unique(country)))
  }
  x
}

speakers_barplot <- function(data, countries) {
  selected_speaker_countries <- countries
  speaker_data_plot <- data
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
}

events_barplot <- function(data, countries) {
  selected_event_countries <- countries
  event_data_plot <- data
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
    labs(
      title = "Events by country"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.text = element_text(size = 16),
      axis.title = element_blank()
    )
}
