pop_2021 <- STATcubeR::od_table('OGD_bevstandjbab2002_BevStand_2021')
pop_2011 <- STATcubeR::od_table('OGD_bevstandjbab2002_BevStand_2011')


y <- pop_2021$tabulate(raw = TRUE)
y$age <- substr(y$`C-GALTEJ112-0`, 11, 100)
y$iso <- substr(y$`C-GRGEMAKT-0`, 10, 100)
y$sex <- ifelse(y$`C-C11-0` == 'C11-1', 'male', 'female')
y$pop <- y$`F-ISIS-1`
y <- y[, c(7, 8, 6, 9)]

y2 <- pop_2011$tabulate(raw = TRUE)
y2$age <- substr(y2$`C-GALTEJ112-0`, 11, 100)
y2$iso <- substr(y2$`C-GRGEMAKT-0`, 10, 100)
y2$sex <- ifelse(y2$`C-C11-0` == 'C11-1', 'male', 'female')
y2$pop <- y2$`F-ISIS-1`
y2 <- y2[, c(7, 8, 6, 9)]

dist_2021 <- y %>%
  mutate(age = as.integer(age)) %>%
  mutate(agegroup = cut(age, c(0, 14, 29, 44, 59, 74))) %>%
  mutate(iso1 = substr(iso, 1, 1)) %>%
  group_by(iso1, agegroup) %>%
  summarise(pop = sum(pop))

dist_2016 <- y2 %>%
  mutate(age = as.integer(age)) %>%
  mutate(agegroup = cut(age, c(0, 14, 29, 44, 59, 74))) %>%
  mutate(iso1 = substr(iso, 1, 1)) %>%
  group_by(iso1, agegroup) %>%
  summarise(pop = sum(pop))

dist_2021$pop16 <- dist_2016$pop

dist_2021 %>%
  filter(!is.na(agegroup)) %>%
  mutate(change = (pop-pop16)/pop16) %>%
  mutate(agegroup = as.character(agegroup)) %>%
  mutate(iso1 = region_labels(iso1)) %>%
  ggplot() + aes(change, iso1, color = agegroup) +
  geom_path(aes(group = iso1), color = "#cccccc", size = 2) +
  geom_point(size = 5) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

plot_data <- dist_2021 %>%
  filter(!is.na(agegroup)) %>%
  mutate(change = (pop-pop16)/pop16) %>%
  mutate(agegroup = as.character(agegroup)) %>%
  mutate(iso1 = region_labels(iso1)) %>%
  ungroup() %>%
  mutate(y = as.integer(as.factor(iso1))) %>%
  rename(x = change)

plot_data$from <- rep(c(0,15, 30, 45, 60), 9)
plot_data$to <- plot_data$from + 14

hc <- highchart() %>%
  hc_yAxis(type = "category", categories = c("", region_labels(1:9)),
           crosshair = TRUE)

for (i in unique(plot_data$agegroup)) {
  new_data <- plot_data %>%
    filter(agegroup == i)
  name <- paste(new_data$from[1], "bis", new_data$to[1], "Jahre")
  hc <- hc %>%
    hc_add_series(data = new_data, type = "scatter", name = name, marker = list(
      symbol = "circle", radius = 7
    ))
}

tooltip_formatter <- JS("function(e) {
      let verb = this.point.x > 0 ? 'gestiegen' : 'gesunken'
      return 'Zwischen <b>2011</b> und <b>2021</b> ist die Anzahl<br/> der ' +
        '<b><span style=\"color: ' + this.color + '\">' + this.point.from + ' bis ' + this.point.to + ' jährigen' +
        '</span></b> in <b>' + this.point.iso1 + '</b><br/> um <b>' + Math.round(Math.abs(this.point.x)*1000)/10 +
        '%</b> ' + verb + '</br><br/>' +
        '2011: <b>' + Highcharts.numberFormat(this.point.pop16, 0) +
        '</b>, 2021: <b>' + Highcharts.numberFormat(this.point.pop, 0) + '</b>'
    }")


hc2 <- hc %>%
  hc_title(text = "Bevölkerungsänderung: 2011 - 2021", style = list(fontSize = "24px")) %>%
  hc_xAxis(
    labels = list(formatter = JS("x => x.value*100+'%'")),
    crosshair = TRUE
  ) %>%
  hc_tooltip(
    formatter = tooltip_formatter
  ) %>%
  hc_plotOptions(series = list(
    states = list(inactive = list(opacity = 0.5))
  )) %>%
  hc_exporting(
    enabled = TRUE,
    buttons = list(contextButton = list(menuItems = c('source1', 'source2'))),
    menuItemDefinitions = list(
      source1 = list(text = "Daten 2011", onclick = JS("e => window.open('https://data.statistik.gv.at/web/meta.jsp?dataset=OGD_bevstandjbab2002_BevStand_2011', '_blank')")),
      source2 = list(text = "Daten 2021", onclick = JS("e => window.open('https://data.statistik.gv.at/web/meta.jsp?dataset=OGD_bevstandjbab2002_BevStand_2021', '_blank')"))
    )
  ) %>%
  hc_colors(c("#326996", "#bebebe", "#B0063D", "#e6a06e", "#87786e", "#7daf91",
              "#a0bedc", "#646464", "#D77D82"))

saveRDS(hc2, "vignettes/graphs/by_age.rds")
