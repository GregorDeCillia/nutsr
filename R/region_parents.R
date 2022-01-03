#' Get parent region
#'
#' @param x a vector containing region codes
#' @examples
#' region_parents(c('AT1', 'AT11', 'AT111', '10101'))
#' @export
region_parents <- function(x) {
  all_codes <- rbind(nuts3(), nuts2(), nuts1(), iso1(), iso3(), iso5())
  ind <- match(x, all_codes$code)
  if (any(is.na(ind))) {
    stop('Could not find region id "', x[which(is.na(ind))[1]], '"')
  }
  all_codes$parent[ind]
}
