#' Find region codes ased on names
#'
#' Function to convert region names to region codes. The names must
#' exactly match an entry of the name column in one of the conversion tables
#' in order to find a matching code.
#'
#' @param x a character vector containing region names
#' @param prefer in case of nuts2 regions, this parameter determines
#'   if the nuts2 or the iso representation is returned
#' @examples
#' region_codes("Burgenland")
#' region_codes("Burgenland", prefer = "iso")
#' @export
region_codes <- function(x, prefer = c("nuts", "iso")) {
  prefer <- match.arg(prefer)
  iso_codes <- rbind(iso1(), iso3(), iso5())
  nuts_codes <- rbind(nuts1(), nuts2(), nuts3())
  all_codes <- switch(
    prefer,
    nuts = rbind(nuts_codes, iso_codes),
    iso = rbind(iso_codes, nuts_codes)
  )
  ind <- match(x, all_codes$name)
  if (any(is.na(ind))) {
    stop('Could not find region id "', x[which(is.na(ind))[1]], '"')
  }
  all_codes$code[ind]
}
