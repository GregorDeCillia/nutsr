pop_2021 <- STATcubeR::od_table('OGD_bevstandjbab2002_BevStand_2021')

y <- pop_2021$tabulate(raw = TRUE)
y$age <- substr(y$`C-GALTEJ112-0`, 11, 100)
y$iso <- substr(y$`C-GRGEMAKT-0`, 10, 100)
y$sex <- ifelse(y$`C-C11-0` == 'C11-1', 'male', 'female')
y$pop <- y$`F-ISIS-1`
y <- y[, c(7, 8, 6, 9)]

ratios <- y %>%
  mutate(iso1 = substr(iso, 1, 1)) %>%
  filter(iso1 == 3) %>%
  mutate(iso3 = substr(iso, 1, 3)) %>%
  mutate(old = as.integer(age) > 60) %>%
  group_by(iso3, sex, old) %>%
  summarize(pop = sum(pop)) %>%
  group_by(iso3) %>%
  summarise(females = pop[2]/(pop[1]+pop[2]), males = pop[4]/(pop[3]+pop[4]),
            vals = list(pop))

dumbbell_tooltip <- JS("function() {
  let vals = this.point.vals;
  console.log(this);
  return `
  <b>${this.point.name}</b><hr style='margin: 0'/>
  <table>
    <tr>
      <th></th><th>Einwohner</th><th>Über 60</th><th>Quote</th>
    </tr>
    <tr>
      <th>Frauen</th><td>${vals[0]+vals[1]}</td><td>${vals[1]}</td>
      <td>${Highcharts.numberFormat(100*vals[1]/(vals[0]+vals[1]), 2)}%</td>
    </tr>
    <tr>
      <th>Männer</th><td>${vals[2]+vals[3]}</td><td>${vals[3]}</td>
      <td>${Highcharts.numberFormat(100*vals[3]/(vals[2]+vals[3]), 2)}%</td>
    </tr>
    <tr>
      <th>Gesamt</th><td>${vals[0]+vals[1]+vals[2]+vals[3]}</td><td>${vals[1]+vals[3]}</td>
      <td>${Highcharts.numberFormat(100*(vals[3]+vals[1])/(vals[0]+vals[1]+vals[2]+vals[3]), 2)}%</td>
    </tr>
  </table>`
}")

hc <- highchart() %>%
  hc_chart(type = "dumbbell", inverted = TRUE) %>%
  hc_add_series(
    showInLegend = FALSE,
    dataLabels = list(
      enabled = TRUE,
      formatter = JS("function() {return Highcharts.numberFormat(this.y*100, 1) + '%'}"),
      xHigh = -10,
      xLow = 10
    ),
    marker = list(radius = 8),
    connectorWidth = 5,
    connectorColor = "#aaaaaa",
    color = "#326996",
    lowColor = "#B0063D",
    ratios %>%
      mutate(name = region_labels(iso3)) %>%
      rename(low = females, high = males) %>%
      arrange(-low-high)
  ) %>%
  hc_xAxis(type = "category", crosshair = TRUE) %>%
  hc_yAxis(max = 0.38, labels = list(formatter = JS("x => Math.round(x.value*100)+'%'"))) %>%
  hc_title(text = "Anteil der über 60 Jährigen in Niederösterreich: <b style='color: #326996'>Männer</b> und
           <b style='color: #B0063D'>Frauen</b>") %>%
  hc_tooltip(formatter = dumbbell_tooltip, useHTML = TRUE)

saveRDS(hc, "vignettes/graphs/dumbbell.rds")
