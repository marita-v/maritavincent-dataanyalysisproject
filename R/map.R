# map.R
# Purpose: Build a world map showing mean annualized salary at each office
#          location, colored by salary level.
#
# Requires the "maps" package for country border polygons:
#   install.packages("maps")

library(ggplot2)
library(dplyr)

#' Known coordinates for the four office locations
#'
#' Hardcoded lookup table, since these are fixed geographic facts (not
#' something that should vary with the data), separate from any dynamic
#' analysis values.
#'
#' @return A data frame with columns location, lat, lon.
office_coordinates <- function() {
  data.frame(
    location = c("Irvine", "Austin", "Cork", "Versailles"),
    lat = c(33.6846, 30.2672, 51.8985, 48.8014),
    lon = c(-117.8265, -97.7431, -8.4756, 2.1301)
  )
}

#' Summarize mean annual salary by location and attach coordinates
#'
#' @param data Cleaned data frame from load_clean_salary_data().
#' @return A data frame with one row per location: location, mean_salary,
#'   n, lat, lon.
summarize_salary_by_location <- function(data) {
  stopifnot("location" %in% names(data))
  stopifnot("annual_salary" %in% names(data))

  summary_df <- data %>%
    group_by(location) %>%
    summarize(
      mean_salary = mean(annual_salary),
      n = dplyr::n(),
      .groups = "drop"
    )

  merged <- merge(summary_df, office_coordinates(), by = "location")
  stopifnot(nrow(merged) == nrow(summary_df)) # catch silent coordinate mismatches

  merged
}

#' Map of mean annualized salary by office location
#'
#' Plots each office on a world map, colored by mean annual salary (darker/
#' warmer = higher pay) with a continuous color key.
#'
#' @param data Cleaned data frame from load_clean_salary_data().
#' @return A ggplot object.
plot_salary_map <- function(data) {
  location_summary <- summarize_salary_by_location(data)

  world_map <- map_data("world")

  ggplot() +
    geom_polygon(
      data = world_map,
      aes(x = long, y = lat, group = group),
      fill = "grey90", color = "white"
    ) +
    geom_point(
      data = location_summary,
      aes(x = lon, y = lat, color = mean_salary, size = n)
    ) +
    geom_text(
      data = location_summary,
      aes(x = lon, y = lat, label = location),
      vjust = -1.2
    ) +
    scale_color_gradient(
      low = "#fee8c8", high = "#e34a33",
      labels = scales::dollar_format(),
      name = "Mean annual\nsalary"
    ) +
    scale_size_continuous(name = "Sample size", range = c(3, 8)) +
    coord_fixed(
      xlim = c(-130, 15), ylim = c(20, 60)
    ) +
    labs(
      title = "Mean annualized salary by office location",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text = element_blank(),
      panel.grid = element_blank()
    )
}
