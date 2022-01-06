pop <- iso5_pop()

dout <- pop %>%
  mutate(iso1 = substr(iso, 1, 1)) %>%
  mutate(iso3 = substr(iso, 1, 3)) %>%
  mutate(iso3 = region_labels(iso3)) %>%
  mutate(iso1 = region_labels(iso1)) %>%
  mutate(iso = region_labels(iso)) %>%
  data_to_hierarchical(c(iso1, iso3), pop, colors = c(
    "#326996", "#bebebe", "#B0063D", "#e6a06e", "#87786e", "#7daf91",
    "#a0bedc", "#646464", "#D77D82"))

hc <- hchart(dout, type = "treemap") %>%
  hc_plotOptions(treemap = list(
    allowTraversingTree = TRUE,
    levels = list(
      list(
        level = 1,
        layoutAlgorithm = 'sliceAndDice',
        dataLabels = list(
          enabled = TRUE,
          align = "left",
          verticalAlign = "top",
          style = list(
            fontSize = "30px"
          )
        )
      ),
      list(
        level = 2,
        colorVariation = list(
          key = "brightness",
          to = -0.1
        )
      )
    )
  ))

saveRDS(hc, 'vignettes/graphs/treemap.rds')
