x <- STATcubeR::od_table('OGD_f0743_VZ_HIS_GEM_3')
y <- x$tabulate(raw = TRUE)

y <- y %>%
  `names<-`(c('iso', 'year', 'pop')) %>%
  mutate(iso = substr(iso, 10, 99)) %>%
  mutate(year = 1861L + 10*as.integer(substr(year, 5, 99)))

y <- y %>%
  mutate(iso1 = substr(iso, 1, 1)) %>%
  filter(year %in% c(2001, 2011)) %>%
  mutate(iso3 = substr(iso, 1, 3)) %>%
  group_by(year, iso3) %>%
  summarise(pop = sum(pop))

changes <- y %>%
  group_by(iso3) %>%
  summarise(change = (pop[2] - pop[1])/pop[1], old = pop[1], new  = pop[2]) %>%
  arrange(change) %>%
  filter(iso3 %in% c(906, 907, 304, 103, 302, 107, 702, 502))

hc <- highchartzero() %>%
  hc_chart(type = "variwide"#, inverted = TRUE
           ) %>%
  hc_xAxis(type = "category") %>%
  hc_add_series(
    color = "#000000",
    marker = list(enabled = FALSE),
    #data = parsed,
    changes %>%
      mutate(name = region_labels(iso3)) %>%
      rename(y = change, z = old),
    colorByPoint = TRUE,
    showInLegend = FALSE,
    dataLabels = list(enabled = TRUE, formatter = JS(
      "function() {return Math.round(this.y*1000)/10+'%'}"))
  ) %>%
  hc_add_dependency("modules/variwide.js") %>%
  hc_title(text = "Bevölkerungsentwicklung ausgewählten Bezirken: 2001 - 2011") %>%
  hc_yAxis(
    labels = list(formatter = JS("x => Math.round(x.value*10000)/100+'%'")),
    crosshair = TRUE,
    title = list(text = "Änderung der Bevölkerung in Prozent")
  ) %>%
  hc_tooltip(formatter = JS("function() {
    return 'Die Bevölkerung von <b>' + this.point.name + '</b><br/>ist zwischen ' +
      '<b>1961</b> und <b>2011</b> um <br/><b>' +
      Math.round(this.point.y*1000)/10 + '%</b> gewachsen<br/><br/>' +
      '2001: <b>' + this.point.z + '</b>, ' +
      '2011: <b>' + this.point.new + '</b>'
  }")) %>%
  hc_colors(c("#326996", "#bebebe", "#B0063D", "#e6a06e", "#87786e", "#7daf91",
              "#a0bedc", "#646464", "#D77D82")) %>%
  hc_credits(enabled = FALSE)

saveRDS(hc, 'vignettes/graphs/variwide.rds')
