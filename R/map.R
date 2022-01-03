#' Create a leaflet map
#'
#' Creates a leaflet map from a data frame which contains spatial codes.
#'
#' @param x a data frame
#' @param join_by a column name. This column should contain region codes
#' @param color a column name which contains numeric values for coloring
#' @param cutoff percentile to cutoff the color scale. Defaults to 90%
#' @examples
#' library(sf)
#'
#' ## visualize population
#'
#' nuts_map(iso3_pop())
#' nuts_map(iso5_pop())
#'
#' nuts_map(nuts2_earnings(), color = "employees")
#'
#' library(dplyr)
#'
#' merge(iso5_pop(), iso5_migration()) %>%
#'   mutate(migration_rel = in_migration/pop) %>%
#'   nuts_map(color = "migration_rel")
#'
#' merge(iso3_pop(), iso3_deaths()) %>%
#'   mutate(deaths_rel = deaths/pop) %>%
#'   nuts_map(color = "deaths_rel")
#' @export
nuts_map <- function(x, join_by = NULL, color = NULL, cutoff = 0.9, overlay_nuts2 = TRUE) {
  stopifnot(is.data.frame(x))
  if (is.null(join_by))
    join_by <- names(x)[1]
  if (is.null(color))
    color <- names(x)[2]

  json <- rbind(
    iso3_shapes(),
    iso5_shapes(),
    nuts2_shapes()
  )

  x$nutsr_value <- pmin(x[[color]], stats::quantile(x[[color]], cutoff))
  merged <- merge(json, x)
  pal = leaflet::colorNumeric(
    grDevices::colorRamp(c("green", "red")),
    domain = x$nutsr_value
  )

  l <- leaflet::leaflet()

  if (overlay_nuts2)
    l <- l %>% leaflet::addPolygons(data = nuts2_shapes(), weight = 2, fill = "transparent",
                                color = "black")
  l %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addPolygons(
      data = merged,
      fillColor = pal(x$nutsr_value),
      weight = 0.5, color = 'black',
      label = lapply(
        paste0("<b>", merged$name, "</b>: ", merged[[color]]),
        htmltools::HTML
      )
    ) %>%
    leaflet::addLegend(
      title = color,
      pal = pal,
      values = merged$nutsr_value,
      position = "topleft"
    )
}

