#' kkk
#' @return An html dependency
#' @export
bezirke_geojson <- function() {
  htmltools::htmlDependency(
    "bezirke_geojson",
    "0.1.1",
    src = "",
    script = "bezirke.js",
    package = "nutsr"
  )
}

map_bezirke <- function(x) {
  hc <- highcharter::highchart(
    type = "map"
  )
  hc <- hc %>% highcharter::hc_add_series(
    x,
    #mapData = JS("bezirke_geojson"),
    mapData = JS(readLines('inst/GeoJSON/2021/simplified-95/bezirke_95_geo.json')),
    joinBy = "iso",
    dataLabels = list(
      enabled = TRUE,
      format = '{point.name}'
    )
  )
  hc <- hc %>%
    highcharter::hc_colorAxis(auxpar = NULL)
  hc <- hc %>%
    highcharter:::.hc_opt("mapView", projection = list(
      name = 'LambertConformalConic',
      parallels = c(46, 49),
      rotation = JS("[-12]")
    ))
  hc %>% highcharter::hc_mapNavigation(enabled = TRUE) %>%
    hc_colorAxis(
      maxColor = "#ff0000",
      minColor = '#00ff00'
    )
}
