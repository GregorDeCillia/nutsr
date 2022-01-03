#' Label a vector of region codes
#'
#' Convert region codes to region labels via a lookup in the
#' region datasets.
#'
#' @param x a vector containing region codes
#' @return  a vector containing region labels
#'
#' @examples
#' region_labels(c('AT1', 'AT11', 'AT111', '10101'))
#' @export
region_labels <- function(x) {
  all_codes <- rbind(iso1(), iso3(), iso5(), nuts3(), nuts2(), nuts1())
  ind <- match(x, all_codes$code)
  if (any(is.na(ind))) {
    stop('Could not find region id "', x[which(is.na(ind))[1]], '"')
  }
  all_codes$name[ind]
}
