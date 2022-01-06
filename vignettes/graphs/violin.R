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
  group_by(sex, iso) %>%
  mutate(age = as.numeric(age)) %>%
  summarise(ratio = sum(pop[age >= 60])/sum(pop), old = sum(pop[age >= 60]), pop = sum(pop))

hc <- highchart()

factors <- c(1, 1.5, 1)
indices <- c(1, 3)

for (j in seq_along(indices)) {
  i <- indices[j]
  dense <- dd %>%
    mutate(iso1 = substr(iso, 1, 1)) %>%
    filter(iso1 == i) %>%
    filter(sex == "female") %$%
    density(ratio, weights = pop/sum(pop), adjust = 0.3, to = 0.45, from = 0.18)

  hc <- hc %>% hc_add_series(
    color = "#B0063D",
    type = "areasplinerange",
    data.frame(x = dense$x, low = j, high = j + factors[i]*dense$y/2/max(dense$y)),
    showInLegend = FALSE
  )
}

for (j in seq_along(indices)) {
  i <- indices[j]
  dense <- dd %>%
    mutate(iso1 = substr(iso, 1, 1)) %>%
    filter(iso1 == i) %>%
    filter(sex != "female") %$%
    density(ratio, weights = pop/sum(pop), adjust = 0.3, to = 0.45, from = 0.18)

  hc <- hc %>% hc_add_series(
    color = "#326996",
    type = "areasplinerange",
    data.frame(x = dense$x, low = j - factors[i]*dense$y/2/max(dense$y), high = j),
    showInLegend = FALSE
  )
}


hc <- hc %>%
  hc_yAxis(type = "category", categories = c("", region_labels(indices), ""), min = 1,
           max = 2) %>%
  hc_xAxis(
    reversed = FALSE,
    labels = list(formatter = JS("x => Math.round(x.value*1000)/10+'%'")),
    crosshair = TRUE
  ) %>%
  hc_chart(inverted = TRUE) %>%
  hc_title(text = "Anteil der über 60 Jährigen") %>%
  hc_plotOptions(
    areasplinerange = list(
      enableMouseTracking = FALSE,
      tooltip = list(enabled = FALSE)
    ),
    line = list(
      enableMouseTracking = FALSE
    ),
    series = list(
      states = list(
        inactive = list(
          opacity = 0.9
        )
      )
    )
  )

weighted.median <- function(x, w, perc = 0.5) {
  w <- w[order(x)]
  x <- x[order(x)]

  prob <- cumsum(w)/sum(w)
  ps <- which(abs(prob - perc) == min(abs(prob - perc)))
  return(x[ps])
}

medians <- dd %>%
  mutate(iso1 = substr(iso, 1, 1)) %>%
  group_by(iso1) %>%
  summarise(med = weighted.median(ratio, pop),
            q1 = weighted.median(ratio, pop, 0.25),
            q3 = weighted.median(ratio, pop, 0.75))

hc2 <- hc %>%
  hc_add_series(
    showInLegend = FALSE,
    type = "line",
    color = "#000000",
    lineWidth = 10,
    data.frame(x = c(medians$q1[1], medians$q3[1]), y = c(1, 1)),
    linecap = "square",
    marker = list(enabled = FALSE)
  ) %>%
  hc_add_series(
    showInLegend = FALSE,
    type = "line",
    color = "#000000",
    lineWidth = 10,
    data.frame(x = c(medians$q1[3], medians$q3[3]), y = c(2, 2)),
    linecap = "square",
    marker = list(enabled = FALSE)
  ) %>%
  hc_add_series(
    showInLegend = FALSE,
    type = "scatter",
    color = "#cccccc",
    marker = list(radius = 4, symbol = "circle"),
    data = medians[indices, ] %>%
      mutate(iso1 = 1:2) %>%
      select(y = iso1, x = med, q1 = q1, q3 = q3) %>%
      list_parse(),
    states = list(
      hover = list(
        enabled = FALSE,
        halo = list(
          size = 0
        )
      )
    )
  ) %>%
  hc_tooltip(
    formatter = JS("function() {
      return  'Median: <b>' + Math.round(this.point.x*1000)/10 + '%</b><br/>' +
        'Q1: <b>' + Math.round(this.point.q1*1000)/10 + '%</b><br/>' +
        'Q3: <b>' + Math.round(this.point.q3*1000)/10 + '%</b>'
    }")
  ) %>%
  hc_title(text = "Anteil der über 60 Jährigen: <b style='color: #326996'>Männer</b> und
           <b style='color: #B0063D'>Frauen</b>") %>%
  hc_xAxis(
    minorTicks = TRUE
  )

saveRDS(hc2, "vignettes/graphs/violin.rds")
