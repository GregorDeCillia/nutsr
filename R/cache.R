#' Caching in nutsr
#'
#' The function `nutsr_cache_dir()` is invoked by internal methods
#' of nutsr to dicide where to cache the downloaded files.
#'
#' Use this function or the environment variable `NUTSR_CACHE_DIR` to
#' change the cache from `tempdir()/nutsr` to a persistent storage place.
#'
#' @param new the new cache dir
#' @return path to the cache directory
#'
#' @export
nutsr_cache_dir <- function(new = NULL) {
  if (!is.null(new)) {
    if (!dir.exists(new))
      stop('cache dir must be an existing directory')
    Sys.setenv(NUTSR_CACHE_DIR = new)
  }
  else {
    Sys.getenv('NUTSR_CACHE_DIR', unset = file.path(tempdir(), 'nutsr'))
  }
}
