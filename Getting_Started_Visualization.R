#Getting Staarted With GGPlot 2
# Plot = data + Aesthetics + Geometry

# Scatter Plot	geom_point(), geom_smooth(), stat_smooth()
# Bar Chart	geom_bar(), geom_errorbar()
# Histogram	geom_histogram(), stat_bin(), position_identity(), position_stack(), position_dodge()
# Box Plot	geom_boxplot(), stat_boxplot(), stat_summary()
# Line Plot	geom_line(), geom_step(), geom_path(), geom_errorbar()
# Pie Chart	coord_polar()

require(ggplot2)
head(iris)

# Plots Histogram
ggplot(data = iris,aes(x=Sepal.Width)) + geom_histogram(bins = 10)

# Plots Density
ggplot(data = iris,aes(x= Sepal.Length,color=Species)) + geom_density()

# Plots Bargraph
head(mpg)
ggplot(data = mpg,aes(x=class)) + geom_bar()

# Plots Bargraph with lables and text
ggplot(data = mpg,aes(x=manufacturer)) +
geom_bar() + #coord_flip() + 
labs(title = "No of Cars in each Class",x = "NO of Car",y = "Type of Car")+
geom_text(stat='count', aes(label=..count..), vjust=-0.25)

# Plots Bargraph ascending order
library(plyr)
library(dplyr)
count(mpg,class) %>% arrange(-n) %>%
  mutate(class = factor(class,levels= class)) %>%
  ggplot(aes(x=class, y=n)) + geom_bar(stat="identity")

#Showing Mean of Continuous Variable by Categorical Variable
str(mpg)
head(mpg)

df = mpg %>% group_by(class) %>% summarise(mean = mean(displ)) %>%
  arrange(-mean) %>% mutate(class = factor(class,levels= class))

p = ggplot(df, aes(x=class, y=mean)) + geom_bar(stat="identity")


p + geom_text(aes(label = sprintf("%0.2f", round(mean, digits = 2))),
              vjust=1.6, color="white", fontface = "bold", size=4)

# Creating Stacked Bar Chart
p <- ggplot(data=mpg, aes(x=class, y=displ, fill=drv))
p + geom_bar(stat = "identity")#+
geom_text(aes(label = sprintf("%0.2f",mean(displ,2))), size = 3, hjust = 0.5, vjust = 3, position ="stack")

# Doge Bar Chart
p <- ggplot(data=mpg, aes(x=class, y=displ, fill=drv))
p + geom_bar(stat="identity", position= "dodge")

range(mpg$dfr)

##########################
# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

# Grouped
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")

# Stacked
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity")
