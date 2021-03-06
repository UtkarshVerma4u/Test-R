# BASIC DRILL DOWN Example
getwd()
setwd("D:\\Programs\\R\\R_Sessions")
library(dplyr)
require(highcharter)
require(purrr)
df <- data_frame(
  name = c("Animals", "Fruits", "Cars"),
  y = c(5, 2, 4),
  drilldown = tolower(name)
)


ds <- list_parse(df)
names(ds) <- NULL


hc <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Basic drilldown") %>%
  hc_xAxis(type = "category") %>%
  hc_legend(enabled = FALSE) %>%
  hc_plotOptions(
    series = list(
      boderWidth = 0,
      dataLabels = list(enabled = TRUE)
    )
  ) %>%
  hc_add_series(
    name = "Things",
    colorByPoint = TRUE,
    data = ds
  )

dfan <- data_frame(
  name = c("Cats", "Dogs", "Cows", "Sheep", "Pigs"),
  value = c(4, 3, 1, 2, 1)
)

dffru <- data_frame(
  name = c("Apple", "Organes"),
  value = c(4, 2)
)

dfcar <- data_frame(
  name = c("Toyota", "Opel", "Volkswage"),
  value = c(4, 2, 2)
)

second_el_to_numeric <- function(ls){
  
  map(ls, function(x){
    x[[2]] <- as.numeric(x[[2]])
    x
  })
  
}

dsan <- second_el_to_numeric(list_parse2(dfan))

dsfru <- second_el_to_numeric(list_parse2(dffru))

dscar <- second_el_to_numeric(list_parse2(dfcar))

hc %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list(
      list(
        id = "animals",
        data = dsan
      ),
      list(
        id = "fruits",
        data = dsfru
      ),
      list(
        id = "cars",
        data = dscar
      )
    )
  )