pop <- iso3_pop()
pop$name <- region_labels(ifelse(pop$iso == 900, "AT13", pop$iso))
names(pop) <- c("id", "value", "name")
pop$parent <- substr(pop$id, 1, 1)
pop$parent[pop$id == 900] <- "AT1"
pop2 <- rbind(
  pop,
  data.frame(
    id = 1:8,
    value = NA,
    name = c("Burgenland", "Kärnten", "Niederösterreich", "Oberösterreich",
             "Salzburg", "Steiermark", "Tirol", "Vorarlberg"),
    parent = c("AT1", "AT2", "AT1", "AT3", "AT3", "AT2", "AT3", "AT3")
  ),
  data.frame(
    id = c("AT1", "AT2", "AT3"),
    value = NA,
    name = c("Ost", "Süd", "West"),
    parent = "AT"
  ),
  data.frame(id = 'AT', value = NA, name = "Österreich", parent = "")
)

plot_data <- highcharter::list_parse(pop2)
ind <- which(is.na(pop2$value))
for (i in ind) {
  plot_data[[i]] <- plot_data[[i]][c(1,3,4)]
}

library(highcharter)

hc <- highchart() %>%
  highcharter:::.hc_opt('colors', JS("['red'].concat(Highcharts.getOptions().colors)")) %>%
  hc_add_series(
    startAngle = -35,
    colors = c(substr(viridis::viridis_pal()(9), 1, 7)),
    allowDrillToNode = TRUE,
    data = plot_data,
    type = "sunburst",
    dataLabels = list(
      rotationMode = 'circular'
    ),
    levels = list(
      list(
        level = 1,
        color = "#00000",
        #levelIsConstant = FALSE,
        dataLabels = list(
          style = list(
            fontSize = "34px",
            textOverflow = "inherit"

          )
        ),
        levelSize = list(
          value = 0.8
        )
      ),
      list(
        level = 2,
        colorByPoint = TRUE,
        dataLabels = list(
          style = list(
            fontSize = "20px"
          )
        ),
        levelSize = list(
          value = 0.5
        )
      ),
      list(
        level = 3,
        colorVariation = list(
          key = "brightness",
          to = -0.5
        ),
        dataLabels = list(
          style = list(
            fontSize = "20px"
          )
        )
      ),
      list(
        level = 4,
        colorVariation = list(
          key = "brightness",
          to = 0.5
        )
      )
    )
  )

hc$height <- "800px"
hc$width <- "800px"
saveRDS(hc, 'vignettes/graphs/sunburst.rds')
hc
