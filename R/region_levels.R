#' Associate regions with an aggregation level
#'
#' Given a vector of region codes, this function returns the corresponding
#' region level (nuts1, nuts2, nuts2, iso1, iso3 or iso5) for each entry.
#' @examples
#' region_levels(c('AT1', 'AT11', 'AT111', '1', '101', '10101'))
#' @param x a character vector containing region codes
#' @export
region_levels <- function(x) {
  all_codes <- list(nuts3(), nuts2(), nuts1(), iso1(), iso3(), iso5())
  all_codes <- lapply(all_codes, function(x) {
    x$level <- attr(x, "nutsr")$from
    x
  })
  all_codes <- do.call(rbind, all_codes)
  ind <- match(x, all_codes$code)
  if (any(is.na(ind))) {
    stop('Could not find region id "', x[which(is.na(ind))[1]], '"')
  }
  all_codes$level[ind]
}
