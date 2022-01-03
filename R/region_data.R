#' @importFrom dplyr %>%
globalVariables(c(
  "C-POLBEZKW-0", "F-ANZ-1", "region", "in_migration",
  "out_migration", "n"
))

#' Data for regions
#'
#' Example data for the features of the nutsr package
#' @name nutsr_data
#' @STATcubeR OGD_f0743_VZ_HIS_GEM_4
#' @export
iso3_pop <- function() {
  x <- STATcubeR::od_table('OGD_f0743_VZ_HIS_GEM_4')
  y <- x$data
  ## select year 2011
  y <- y[y[[1]] == "H88-15", ]
  ## drop aggregate regions
  y <- y[substr(y[[2]], 1, 5) != "GRBDL", ]
  y <- y[, 2:3]
  y[[1]] <- substr(y[[1]], 10, 13)
  names(y) <- c('iso', 'pop')
  y
}

#' @rdname nutsr_data
#' @STATcubeR OGD_f0743_VZ_HIS_GEM_3
#' @export
iso5_pop <- function() {
  x <- STATcubeR::od_table('OGD_f0743_VZ_HIS_GEM_3')
  y <- x$data
  y <- y[y[["C-H88-0"]] == "H88-15", ]

  y[[1]] <- substr(y[['C-GRGEMAKT-0']], 10, 14)
  y <- y[, c(1, 3)]
  names(y) <- c("iso", "pop")
  y
}

#' @rdname nutsr_data
#' @STATcubeR OGD_veste309_Veste309_1
#' @export
nuts2_earnings <- function() {
  x <- STATcubeR::od_table('OGD_veste309_Veste309_1')
  x$total_codes(
    `C-A11-0` = "A11-1",
    `C-STAATS-0` = "STAATS-9",
    `C-VEBDL-0` = "VEBDL-10",
    `C-BESCHV-0` = "BESCHV-1"
  )
  y <- x$tabulate('Region')
  y[[1]] <- substr(y[[1]], 1, 4)
  y[[1]] <- nuts2_to_iso(y[[1]])
  names(y) <- c("iso", "mean", "q1", "q2", "q3", "employees")
  y
}

#' @rdname nutsr_data
#' @STATcubeR OGD_bevwan020_AUSSENWAND_202
#' @export
iso5_migration <- function() {
  x <- STATcubeR::od_table('OGD_bevwan020_AUSSENWAND_202')
  x$total_codes(Nationality = "Austria")
  y <- x$tabulate("Time", "Commune")
  names(y) <- c("time", "region", "in_migration", "out_migration")
  y <- y %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(in_migration = sum(in_migration), out_migration = sum(out_migration))
  y <- y %>%
    dplyr::mutate(n = nchar(as.character(region))) %>%
    dplyr::mutate(region = substr(region, n - 5, n - 1))
  names(y)[1] <- "iso"
  y
}

#' @rdname nutsr_data
#' @STATcubeR OGD_rate_kalwobez_GEST_KALWOCHE_STR_BZ_100
#' @export
iso3_deaths <- function() {
  x <- STATcubeR::od_table('OGD_rate_kalwobez_GEST_KALWOCHE_STR_BZ_100')
  x$total_codes(Gender = "total")
  y <- x$tabulate("Pol. district", "Calendar week", raw = TRUE) %>%
    dplyr::group_by(`C-POLBEZKW-0`) %>%
    dplyr::summarise(deaths = sum(`F-ANZ-1`))
  names(y) <- c("iso", "deaths")
  y$iso <- substr(y$iso, 10, 100)
  y <- y[nchar(y$iso) == 3, ]
  y
}

nuts2_to_iso <- function(x) {
  mapping <- data.frame(
    stringsAsFactors = FALSE,
    nuts2 = c("AT11", "AT12", "AT13", "AT21", "AT22", "AT31", "AT32", "AT33", "AT34"),
    iso = c(1, 3, 9, 2, 6, 4, 5, 7, 8)
  )
  mapping$iso[match(x, mapping$nuts2)]
}
