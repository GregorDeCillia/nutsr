#' Spatial vector data for austrian regions
#'
#' Function that obtain the polygon shapes for austrian regions.
#' The data for these polygons is sourced from geojson files and read
#' via [sf::st_read()].
#' @name nutsr_shapes
#' @source \url{https://github.com/ginseng666/GeoJSON-TopoJSON-Austria}
#' @export
cache_region_shapes <- function() {
  iso3_shapes()
  iso5_shapes()
  nuts2_shapes()
  message("cached 3 'sf' objects in '", nutsr_cache_dir(), "'")
}

#' @name nutsr_shapes
#' @export
iso3_shapes <- function() {
  get_geojson('bezirke_95_geo.rds')
}

#' @name nutsr_shapes
#' @export
iso5_shapes <- function() {
  get_geojson('gemeinden_95_geo.rds')
}

#' @name nutsr_shapes
#' @export
nuts2_shapes <- function() {
  get_geojson('laender_95_geo.rds')
}

get_geojson <- function(file, cache_dir = nutsr_cache_dir()) {
  cache_file <- file.path(cache_dir, file)
  if (!file.exists(cache_file))
    download_geojson(file, cache_file)
  sink('/dev/null')
  on.exit(sink())
  readRDS(gsub("json", "rds", cache_file))
}

download_geojson <- function(file, cache_file) {
  dest_file <- tempfile(fileext = ".json")
  utils::download.file(
    paste0(
      'https://raw.githubusercontent.com/ginseng666/GeoJSON-TopoJSON-Austria/master/2021/simplified-95/',
      gsub('rds', 'json', file)
    ),
    dest_file
  )
  saveRDS(sf::st_read(dest_file), cache_file)
  file.remove(dest_file)
}
