#' Convert between different region codes
#' @description
#' Several dataframes containing identifiers for regions of austria.
#' Every dataset contains three three columns
#'
#' - **`id`**: The identifier of the region
#' - **`parent`**: The identifier of the parent region
#' - **`name`**: The german region name
#'
#' The conversion rules are based on datasets from \url{https://data.statistik.gv.at}.
#' See the Datasets section below for more details.
#' @name conversion_rules
#' @STATcubeR OGD_vgrrgr104_RGR104_1
#' @STATcubeR OGD_f0743_VZ_HIS_GEM_3
#' @STATcubeR OGD_f0743_VZ_HIS_GEM_4
#' @usage
#' ## set up the cache
#'
#' cache_conversion_rules()
#'
#' ## get the data
#'
#' nuts1()
#' nuts2()
#' nuts3()
#'
#' iso1()
#' iso3()
#' iso5()
#' @export
cache_conversion_rules <- function() {
  required_files <- c(
    "iso1.rds", "iso3.rds", "iso5.rds", "nuts1.rds", "nuts2.rds", "nuts3.rds"
  )
  if (!all(required_files %in% dir(nutsr_cache_dir()))) {
    chache_iso3_iso5()
    cache_iso1()
  }
  message("cached 6 'data.frame' objects in '", nutsr_cache_dir(), "'")
}

cache_iso1 <- function() {
  iso1 <- new_conversionrule(
    code = as.character(1:9),
    parent = c('AT11', 'AT21', 'AT12', 'AT31', 'AT32', 'AT22', 'AT33', 'AT34', 'AT13'),
    name = c('Burgenland', 'K\u00E4rnten', 'Nieder\u00F6sterreich', 'Ober\u00F6sterreich',
             'Salzburg', 'Steiermark', 'Tirol', 'Vorarlberg', 'Wien'),
    from = 'iso1',
    to = 'nuts2'
  )
  saveRDS(iso1, file.path(nutsr_cache_dir(), 'iso1.rds'))
}

cache_nuts2 <- function() {
  iso1 <- iso1()
  meta_nuts2 <- iso1[order(iso1$parent), ]
  nuts2 <- new_conversionrule(
    code = meta_nuts2$parent,
    name = meta_nuts2$name,
    parent = substr(meta_nuts2$parent, 1, 3),
    from = "nuts2",
    to = "nuts1"
  )
  saveRDS(nuts2, file.path(nutsr_cache_dir(), 'nuts2.rds'))
}

cache_nuts1 <- function() {
  nuts1 <- new_conversionrule(
    code = c("AT1", "AT2", "AT3"),
    parent = c("AT", "AT", "AT"),
    name = c("Ost\u00F6sterreich", "S\u00FCd\u00F6sterreich", "West\u00F6sterreich"),
    from = "nuts1",
    to = "nuts0"
  )
  saveRDS(nuts1, file.path(nutsr_cache_dir(), 'nuts1.rds'))
}

cache_nuts3 <- function() {
  x <- STATcubeR::od_table('OGD_vgrrgr104_RGR104_1')
  meta_nuts3 <- x$field('NUTS-3')
  meta_nuts3 <- meta_nuts3[nchar(meta_nuts3$code) == 5, ]
  nuts3 <- new_conversionrule(
    code = meta_nuts3$code,
    parent = substr(meta_nuts3$code, 1, 4),
    name = substr(meta_nuts3$label, 1, nchar(meta_nuts3$label) - 8),
    from = "nuts3",
    to = "nuts2"
  )
  saveRDS(nuts3, file.path(nutsr_cache_dir(), 'nuts3.rds'))
}

# generate iso3.rds and iso5.rds
chache_iso3_iso5 <- function() {
  gem_3 <- STATcubeR::od_table('OGD_f0743_VZ_HIS_GEM_3')
  iso5_meta <- gem_3$field('C-GRGEMAKT-0')
  iso5_codes <- substr(iso5_meta$code, 10, 15)
  ind <- (iso5_codes != "0")
  iso5 <- new_conversionrule(
    code = iso5_codes[ind],
    parent = substring(iso5_codes[ind], 1, 3),
    name = substr(iso5_meta$label_de[ind], 1, nchar(iso5_meta$label_de[ind]) - 8),
    from = 'iso5',
    to = 'iso3'
  )
  saveRDS(iso5, file.path(nutsr_cache_dir(), 'iso5.rds'))

  gem_4 <- STATcubeR::od_table('OGD_f0743_VZ_HIS_GEM_4')
  iso3_meta <- gem_4$field('C-GRGEMAKT-0')
  iso3_codes <- substr(iso3_meta$code, 10, 13)
  ind <- which(nchar(iso3_codes) == 3 & iso3_codes != "900")
  iso3_meta <- iso3_meta[ind, ]
  vienna <- iso5[substr(iso5$code,1 , 1) == 9, ]
  vienna$code <- substr(vienna$code, 1, 3)

  iso3 <- new_conversionrule(
    code = c(substr(iso3_meta$code, 10, 13), vienna$code),
    parent = c(substr(iso3_meta$code, 10, 10), substr(vienna$parent, 1, 1)),
    name = c(
      substr(iso3_meta$label_de, 1, nchar(iso3_meta$label_de) - 6),
      vienna$name
    ),
    from = "iso3",
    to = "iso1"
  )

  saveRDS(iso3, file.path(nutsr_cache_dir(), 'iso3.rds'))
}

new_conversionrule <- function(code, parent, name, from, to) {
  stopifnot(is.character(code), is.character(parent), is.character(name))
  stopifnot(is.character(from), is.character(to), length(from) == 1, length(to) == 1)
  res <- data.frame(code = code, parent = parent, name = name, stringsAsFactors = FALSE)
  class(res) <- c('nutsr', 'tbl_df', 'tbl', class(res))
  attr(res, 'nutsr') <- list(from = from, to = to)
  res
}

#' @export
summary.nutsr <- function(object, ...) {
  meta <- attr(object, 'nutsr')
  cat(nrow(object), 'conversion rules between', meta$from, 'and', meta$to)
}

#' @rdname conversion_rules
#' @usage NULL
#' @export
nuts1 <- function() {
  readRDS(file.path(nutsr_cache_dir(), 'nuts1.rds'))
}

#' @rdname conversion_rules
#' @usage NULL
#' @export
nuts2 <- function() {
  readRDS(file.path(nutsr_cache_dir(), 'nuts2.rds'))
}

#' @rdname conversion_rules
#' @usage NULL
#' @export
nuts3 <- function() {
  readRDS(file.path(nutsr_cache_dir(), 'nuts3.rds'))
}

#' @rdname conversion_rules
#' @usage NULL
#' @export
iso1 <- function() {
  readRDS(file.path(nutsr_cache_dir(), 'iso1.rds'))
}

#' @rdname conversion_rules
#' @usage NULL
#' @export
iso3 <- function() {
  readRDS(file.path(nutsr_cache_dir(), 'iso3.rds'))
}

#' @rdname conversion_rules
#' @usage NULL
#' @export
iso5 <- function() {
  readRDS(file.path(nutsr_cache_dir(), 'iso5.rds'))
}
