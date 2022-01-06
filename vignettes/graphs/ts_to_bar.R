x <- STATcubeR::od_table('OGD_f0743_VZ_HIS_GEM_3')
y <- x$tabulate(raw = TRUE)

years <- substr(x$field('C-H88-0')$label, 1, 4)

names(y) <- c("iso", "year", "pop")
levels(y$year) <- years
y <- y %>%
  mutate(year = as.numeric(as.character(year))) %>%
  mutate(iso = substr(iso, 10, 99))

plot_data <- y %>%
  group_by(year) %>%
  summarise(pop = sum(pop)) %>%
  select(x = year, y = pop) %>%
  mutate(drilldown = as.character(x))

drilldown_data <- y %>% split(y$year) %>% lapply(function(x) {
  this_year <- x$year[1]
  list(
    id = as.character(this_year),
    type = "bar",
    name = as.character(this_year),
    data = x %>%
      filter(year == this_year) %>%
      mutate(iso1 = substr(iso, 1, 1)) %>%
      filter(iso1 != "0") %>%
      group_by(iso1) %>%
      summarize(pop = sum(pop)) %>%
      mutate(iso1 = region_labels(iso1)) %>%
      select(name = iso1, y = pop) %>%
      list_parse()
  )
}) %>% `names<-`(NULL)

callback_drill <- highcharter::JS(paste0("e => {
    let name = e.seriesOptions.name;
    title = 'Bevölkerung von Österreich'
    if (name != 'Zeitreihe') {
      title = title + ', ' + name
      e.target.update({xAxis: {tickPositions: null}})
    } else {
      e.target.update({xAxis: {tickPositions: ",
      jsonlite::toJSON(as.numeric(years)) , "}})
    }
    e.target.setTitle({text: title})
  }"))

hc <- highchart() %>%
  hc_add_series(plot_data, name = "Zeitreihe", type = "spline", showInLegend = FALSE) %>%
  hc_title(text = "Bevölkerung von Österreich") %>%
  hc_drilldown(series = drilldown_data) %>%
  hc_xAxis(type = "category", crosshair = TRUE) %>%
  hc_plotOptions(
    bar = list(
      dataLabels = list(enabled = TRUE),
      dataSorting = list(enabled = TRUE),
      showInLegend = FALSE,
      tooltip = list(
        enabled = FALSE,
        pointFormat = '<b>{point.y}</b>'
      )
    ),
    spline = list(
      tooltip = list(
        pointFormat = '<b>{point.y}</b>'
      )
    )
  ) %>%
  highcharter::hc_chart(
    events = list(drilldown = callback_drill, drillup = callback_drill)
  ) %>%
  hc_colors(c("#3B528B", "#2C728E")) %>%
  hc_xAxis(tickPositions = as.numeric(years))

saveRDS(hc, "vignettes/graphs/ts_to_bar.rds")
