require(caret)
require(ggplot2)
require(highcharter)
require(dplyr)
data("diamonds")
head(diamonds)
sapply(diamonds,class)

ggplot(diamonds,aes(carat,price)) + geom_point(aes(color=cut))+
facet_wrap( ~ cut) +labs(x = "CARAT",y = "PRICE",title="Price of Diamond by Cut",
tag = "ISO 9001",caption = "Data from diamond DB",breaks = seq(0,270,by = 30))

ggplot(diamonds,aes(price,carat))+geom_point() +labs(x="PRICE",y="CARAT",title = "Carat VS Price")

highchart()%>%
  hc_add_series(diamonds, "scatter", hcaes(x = price, y = carat))

highchart()%>%
  hc_add_series_scatter(diamonds,hcaes(x = price, y = carat))

hchart(diamonds, "scatter",hcaes(x = price, y = carat, group = cut))
