migration <- read.table(text = '
Burgenland	6.647	0	89	1.625	134	54	895	75	33	2.172	5.077	1.570
Kärnten	9.535	141	0	542	532	390	1.735	447	112	1.856	5.755	3.780
Niederösterreich	37.676	2.546	1.173	0	2.799	777	1.615	605	258	16.968	26.741	10.935
Oberösterreich	24.040	233	636	2.800	0	2.507	1.572	652	223	4.381	13.004	11.036
Salzburg	15.461	160	704	681	3.186	0	1.076	775	281	2.103	8.966	6.495
Steiermark	20.488	932	1.654	1.225	1.410	706	0	485	234	3.641	10.287	10.201
Tirol	15.984	159	615	483	759	811	826	0	783	1.783	6.219	9.765
Vorarlberg	8.018	64	163	192	251	114	345	496	0	1.265	2.890	5.128
Wien	74.969	2.974	1.377	24.588	3.009	1.189	2.520	1.054	889	0	37.600	37.369' %>%
  gsub("\\.", "", .))

migration <- migration[, c(1, 3:11)]
names(migration) <- c("from", region_labels(1:9))

migration <- tidyr::pivot_longer(migration, cols = 2:10)
names(migration)[2] <- "to"
names(migration)[3] <- "weight"

hc <- highchart() %>%
  hc_add_series(
    migration,
    type = "dependencywheel",
    size = "95%",
    dataLabels = list(
      color = '#333',
      textPath = list(
        enabled = TRUE,
        attributes = list(
          dy = 5
        )
      ),
      distance = 10
    ),
    size = '95%',
    tooltip = list(
      nodeFormatter = JS("function() {
        let sum_from = this.linksFrom.map(x => x.weight).reduce((x,y) => x + y)
        let sum_to = this.linksTo.map(x => x.weight).reduce((x,y) => x + y)
        return `<b>${this.id}</b><br>In migration: <b>${sum_from}</b><br/> Out migration: <b>${sum_to}</b>`
      }"),
      headerFormat = ""
    )
  ) %>%
  hc_colors(c("#326996", "#bebebe", "#B0063D", "#e6a06e", "#87786e", "#7daf91",
              "#a0bedc", "#646464", "#D77D82")) %>%
  hc_title(text = "Domestic Migration, 2020")

saveRDS(hc, "vignettes/graphs/dependencywheel.rds")
