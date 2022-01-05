globalVariables(c('value', 'label'))

#' Create a drilldown plots from iso5 data
#'
#' @param x a data frame. The first column should contain valid iso5 ids
#'   and the second column values.
#' @param value_label a label for the value. Used in the tooltip. If missing,
#'   it will be extracted from the column name
#' @param title a chart title
#' @return a htmlwidget object from the highcharter package
#' @examples
#' nuts_drilldown(iso5_pop())
#' @export
nuts_drilldown <- function(x, title = "Chart for ${name}", value_label = NULL) {
  stopifnot(is.data.frame(x), ncol(x) >= 2)
  if (is.null(value_label))
    value_label <- names(x)[2]
  stopifnot(is.character(value_label), length(value_label) == 1)

  drill_data <- create_drilldown_data(x)

  callback_drill <- highcharter::JS(paste0("e => {
    let name = e.seriesOptions.name;
    e.target.setTitle({text: `", title, "`})
  }"))

  highcharter::highchart(list(colors = c("#3B528B", "#2C728E"))) %>%
    # bind data
    highcharter::hc_add_series(drill_data$main, name = "Austria") %>%
    highcharter::hc_drilldown(
      series = drill_data$drilldown,
      drillUpButton = list(position = list(verticalAlign = "bottom", y = -50)),
      activeAxisLabelStyle = list(fontWeight = "bold", textDecoration = "none"),
      activeDataLabelStyle = list(fontWeight = "bold", textDecoration = "none")
    ) %>%
    # styling
    highcharter::hc_chart(
      type = "bar",
      events = list(drilldown = callback_drill, drillup = callback_drill)
    ) %>%
    highcharter::hc_title(text = gsub("${name}", "Austria", title, fixed = TRUE)) %>%
    highcharter::hc_xAxis(type = "category", crosshair = TRUE) %>%
    highcharter::hc_yAxis(crosshair = TRUE, title = list(text = ""), maxPadding = 0.07) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_tooltip(
      headerFormat = '',
      pointFormat = paste0(
        '<b>{point.name}</b><br><span style="color:{point.color}">',
        value_label, '</span>: <b>{point.y}</b><br/>')
    ) %>%
    highcharter::hc_plotOptions(series = list(
      pointPadding = 0.1,
      borderWidth = 0,
      groupPadding = 0,
      dataSorting = list(enabled = TRUE),
      showInLegend = FALSE,
      dataLabels = list(enabled = TRUE)
    )) %>%
    highcharter::hc_exporting(enabled = FALSE)
}

create_drilldown_data <- function(x) {
  stopifnot(all(x[[1]] %in% iso5()$code))

  x <- x[, 1:2]
  names(x) <- c("iso5", "value")

  x <- x %>%
    dplyr::mutate(iso1 = substr(iso5, 1, 1), iso3 = substr(iso5, 1, 3))

  data_nuts2 <- x %>%
    dplyr::group_by(iso1) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::mutate(label = region_labels(iso1)) %>%
    dplyr::select(name = label, y = value, drilldown = iso1)

  data_iso3 <- lapply(unique(x$iso1), function(id) {
    plot_data <- x %>%
      dplyr::filter(iso1 == id) %>%
      dplyr::group_by(iso3) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::mutate(label = region_labels(iso3)) %>%
      dplyr::select(name = label, y = value, drilldown = iso3)
    list(
      id = as.character(id),
      data = highcharter::list_parse(plot_data),
      name = region_labels(id)
    )
  })

  data_iso5 <- lapply(unique(x$iso3), function(id) {
    plot_data <- x %>%
      dplyr::filter(iso3 == id) %>%
      dplyr::mutate(label = region_labels(iso5)) %>%
      dplyr::select(name = label, y = value)

    list(
      id = as.character(id),
      data = highcharter::list_parse(plot_data),
      name = region_labels(id)
    )
  })

  list(
    main = data_nuts2,
    drilldown = c(data_iso3, data_iso5)
  )
}
