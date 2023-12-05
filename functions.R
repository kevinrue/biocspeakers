filter_person_country <- function(x, selected) {
  if (length(selected)) {
    x <- x %>%
      filter(person_country %in% selected) %>%
      mutate(person_country = factor(person_country, unique(person_country)))
  }
  x
}

filter_event_countries <- function(x, selected) {
  if (length(selected)) {
    x <- x %>%
      filter(event_country %in% selected) %>%
      mutate(event_country = factor(event_country, unique(event_country)))
  }
  x
}

filter_person_countries <- function(x, selected) {
  if (length(selected)) {
    x <- x %>%
      filter(person_country %in% selected) %>%
      mutate(person_country = factor(person_country, unique(person_country)))
  }
  x
}

speakers_barplot <- function(data, speaker_countries, event_countries) {
  speaker_data_all <- data %>%
    mutate(
      selected = factor(person_country %in% speaker_countries)
    )
  if (!length(speaker_countries)) {
    speaker_countries <- levels(data[["person_country"]])
  }
  if (!length(event_countries)) {
    event_countries <- levels(data[["event_country"]])
  }
  speaker_data_filtered <- data %>%
    filter(event_country %in% event_countries) %>%
    select(person_country, person_name) %>%
    unique() %>%
    mutate(
      selected_speaker_country = factor(person_country %in% speaker_countries)
    )
  if (length(speaker_countries) || length(event_countries)) {
    gg <- ggplot() +
      geom_bar(aes(person_country, fill = person_country), speaker_data_all, alpha = 0.25, color = "grey") +
      geom_bar(aes(person_country, fill = person_country, alpha = selected_speaker_country, color = selected_speaker_country), speaker_data_filtered) +
      scale_alpha_manual(values = c("FALSE" = 0.25, "TRUE" = 1)) +
      scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "black")) +
      guides(
        fill = "none",
        alpha = "none",
        color = "none"
      )
  } else {
    gg <- ggplot(speaker_data_all) +
      geom_bar(aes(person_country, fill = person_country)) +
      guides(
        fill = "none"
      )
  }
  gg +
    labs(
      title = "Keynote speakers by country"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.text = element_text(size = 16),
      axis.title = element_blank()
    )
}

events_barplot <- function(data, speaker_countries, event_countries) {
  event_data_plot_all <- data %>%
    select(event_country, event_type, event_year) %>%
    unique()
  if (!length(speaker_countries)) {
    speaker_countries <- levels(data[["person_country"]])
  }
  if (!length(event_countries)) {
    event_countries <- levels(data[["event_country"]])
  }
  event_data_plot_speakers <- data %>%
    filter(person_country %in% speaker_countries) %>%
    select(event_country, event_type, event_year) %>%
    unique() %>%
    mutate(
      selected_event_country = factor(event_country %in% event_countries)
    )
  if (length(event_countries) || length(speaker_countries)) {
    gg <- ggplot() +
      geom_bar(aes(event_country, fill = event_country), event_data_plot_all, alpha = 0.25, color = "grey") +
      geom_bar(aes(event_country, fill = event_country, alpha = selected_event_country, color = selected_event_country), event_data_plot_speakers) +
      scale_alpha_manual(values = c("FALSE" = 0.25, "TRUE" = 1)) +
      scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "black")) +
      guides(
        fill = "none",
        alpha = "none",
        color = "none"
      )
  } else {
    gg <- ggplot(event_data_plot_all) +
      geom_bar(aes(event_country, fill = event_country)) +
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
