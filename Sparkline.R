library(dplyr)
library(sparkline)

data <- chickwts %>%
  group_by(feed) %>%
  summarise(weight = list(weight)) %>%
  mutate(boxplot = NA, sparkline = NA, bulletchart = NA)

reactable(data, columns = list(
  weight = colDef(cell = function(values) {
    sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(chickwts$weight))
  }),
  boxplot = colDef(cell = function(value, index) {
    sparkline(data$weight[[index]], type = "box")
  }),
  sparkline = colDef(cell = function(value, index) {
    sparkline(data$weight[[index]])
  }), 
  bulletchart = colDef(cell = function(value, index){
    htmltools::as.tags(sparkline(c(100,110,120,70,30), type = "bullet"))
  })
))


# https://omnipotent.net/jquery.sparkline/#s-about