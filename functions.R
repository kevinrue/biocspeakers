filter_selected_countries <- function(x, selected) {
  if (length(selected)) {
    x <- x %>%
      filter(country %in% selected) %>%
      mutate(country = factor(country, unique(country)))
  }
  x
}
