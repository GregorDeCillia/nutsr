pop_2011 <- STATcubeR::od_table('OGD_bevstandjbab2002_BevStand_2011')

y <- pop_2021$tabulate(raw = TRUE)
y$age <- substr(y$`C-GALTEJ112-0`, 11, 100)
y$iso <- substr(y$`C-GRGEMAKT-0`, 10, 100)
y$sex <- ifelse(y$`C-C11-0` == 'C11-1', 'male', 'female')
y$pop <- y$`F-ISIS-1`
y <- y[, c(7, 8, 6, 9)]


dd <- y %>%
  mutate(iso1 = substr(iso, 1, 1)) %>%
  mutate(iso3 = substr(iso, 1, 3)) %>%
  group_by(iso) %>%
  mutate(age = as.numeric(age)) %>%
  summarise(ratio = sum(pop[age >= 60])/sum(pop), old = sum(pop[age >= 60]), pop = sum(pop))

hc <- highchart() %>%
  hc_add_series(
    type = "bubble",
    data.frame(x = dd$ratio, y = as.numeric(substr(dd$iso, 1, 1)) - 1,
               z = dd$pop, name = region_labels(dd$iso), old = dd$old),
    marker = list(
      lineColor = "transparent"
    ),
    showInLegend = FALSE
  ) %>%
  hc_yAxis(type = "category", categories = c(region_labels(1:9), ""),
           crosshair = TRUE, min = 0, maxPadding = -0.10) %>%
  hc_title(text = "Anteil der über 60 Jährigen in Österreichischen Gemeinden") %>%
  hc_tooltip(formatter = JS(
    "function() {
       return '<b>' + this.point.name + '</b><br/>Anteil: <b>' + Math.round(this.x*1000)/10 + '%</b>' +
         '<br/>Bevölkerung: <b>' + this.point.z + '</b>' +
         '<br/>Über 60-Jährige: <b>' + this.point.old + '</b>'
    }",
    headerFormat = ""
  )) %>%
  hc_xAxis(
    labels = list(formatter = JS("x => x.value*100+'%'")),
    crosshair = TRUE
  ) %>%
  hc_colors(c("#3B528B", "#2C728E"))

saveRDS(hc, "vignettes/graphs/bubble.rds")
