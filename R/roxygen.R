#' The `@STATcubeR` roxygen tag
#' @description
#' This tag allows you to cite STATcubeR datasets in packages.
#' It Can be used by placing `@STATcubeR` followed by an `id` of an
#' open data dataset into roxygen comment blocks.
#'
#' ```
#' #' My Function
#' #' @STATcubeR OGD_f0743_VZ_HIS_GEM_4
#' #' @STATcubeR OGD_f0743_VZ_HIS_GEM_3
#' my_function() {
#'   # ...
#' }
#' ```
#'
#' This will fetch metadata for the datasets and render the
#' resuts into the documentation pages. The output of this example is shown at
#' the end of this help page.
#' @usage NULL
#' @details
#' It is recommended to set up persistent caching with
#' [STATcubeR::od_cache_dir()] when using this function.
#' It is planned to also support datasets from the STATcube REST API in the
#' future with something like the following syntax
#' ```
#' #' @STATcubeR sc_table 'path/to/request.json'
#' #' @STATcubeR sc_example <example_id>``
#' ````
#' @STATcubeR OGD_f0743_VZ_HIS_GEM_4
#' @STATcubeR OGD_f0743_VZ_HIS_GEM_3
#' @name sc_roxygen
#' @export
roxy_tag_rd.roxy_tag_STATcubeR <- function(x, base_path, env) {
  roxygen2::rd_section("STATcubeR", x$val)
}

#' @rdname sc_roxygen
#' @usage NULL
#' @export
roxy_tag_parse.roxy_tag_STATcubeR <- function(x) {
  id <- x$raw
  url <- paste0('https://data.statistik.gv.at/web/meta.jsp?dataset=', id)
  json_url <- paste0("https://data.statistik.gv.at/ogd/json?dataset=", id)
  obj <- STATcubeR::od_table(id)
  call <- obj$meta$source$label
  code <- paste0("\\strong{\\code{", id, "}}")
  x$val <- paste0(code, " ", call, " \\href{", url, "}{", "metadata", "}", " \\href{",
                  json_url, "}{json}")
  x
}

#' @rdname sc_roxygen
#' @usage NULL
#' @export
format.rd_section_STATcubeR <- function(x, ...) {
  paste0(
    "\\section{Datasets from \\url{https://data.statistik.gv.at}}{\n",
    "\\itemize{\n",
    paste0("  \\item ", x$value, "\n", collapse = ""),
    "}\n",
    "}\n"
  )
}
