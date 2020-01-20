setwd("D:\\Programs\\R\\R_Sessions")

library(ggplot2)
library(tidyverse)
library(gganimate)
library(directlabels)
library(png)
library(transformr)
library(grid)
require(gifski)
df = data.frame(A=sample(1:75, 50, replace=TRUE),
                B=sample(1:100, 50, replace=TRUE),
                stringsAsFactors = FALSE)

ggplot(df, aes(A, B)) +
  geom_line() +
  transition_reveal(A) +
  labs(title = 'A: {frame_along}')

p = ggplot(df, aes(A, B)) +
  geom_line() +
  transition_reveal(A) +
  labs(title = 'A: {frame_along}')

animate(p, nframes=100,fps = 2,renderer = gifski_renderer(loop = FALSE),
        duration = 14, width = 800, height = 400)

anim_save("basic_animation.gif", p)

################################## ADVANCE ANIMATION###############################
set.seed(123)
dates = paste(rep(month.abb[1:10], each=10), 2018)
df = data.frame(Product=rep(sample(LETTERS[1:10],10), 10),
                Period=factor(dates, levels=unique(dates)),
                Sales=sample(1:100,100, replace = TRUE))

head(df)

# Ranking by Period and Sales
df = df %>% 
  arrange(Period, Sales) %>% 
  mutate(order = 1:n())

# Animation
p = df %>% 
  ggplot(aes(order, Sales)) +
  geom_bar(stat = "identity", fill = "#ff9933") +
  labs(title='Total Sales in {closest_state}', x=NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  scale_x_continuous(breaks=df$order, labels=df$Product, position = "top") +
  transition_states(Period, transition_length = 1, state_length = 2) +
  view_follow(fixed_y=TRUE) +
  ease_aes('bounce-in-out')

animate(p, nframes=50, fps=2,duration = 14, width = 800, height = 400)

anim_save("bar_animation.gif", p)

#################################### MORE ADVANCE ANIMATION ###############################

library(ggplot2)
library(tidyverse)
library(gganimate)
library(directlabels)
library(png)
library(transformr)
library(grid)

data(survey18)
# Read Data
df = read.table(text = 
                  " Year Perc_Seats Party
                1984 0.79 INC
                1989 0.38 INC
                1991 0.45 INC
                1996 0.27 INC
                1998 0.27 INC
                1999 0.22 INC
                2004 0.28 INC
                2009 0.4   INC
                2014 0.09 INC
                2019 0.1   INC
                1984 0     BJP
                1989 0.17 BJP
                1991 0.23 BJP
                1996 0.31 BJP
                1998 0.35 BJP
                1999 0.35 BJP
                2004 0.27 BJP
                2009 0.23 BJP
                2014 0.52 BJP
                2019 0.56 BJP
                ", header=TRUE)

# Set Theme
theme_set(theme_minimal())

# Plot and animate
p =  
  ggplot(data = df, aes(x= factor(Year), y=Perc_Seats, group=Party, colour=Party)) +
  geom_line(size=2, show.legend = FALSE) +
  scale_color_manual(values=c("#ff9933", "#006400")) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = 'Lok Sabha Election : % of seats won', 
       x = NULL, y = NULL) +
  geom_text(aes(label=scales::percent(Perc_Seats, accuracy = 1),
                vjust= -2), show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_dl(aes(label=Party), method="last.points") +
  transition_reveal(Year) +
  coord_cartesian(clip = 'off') + 
  ease_aes('cubic-in-out')

animate(p, fps = 10, width = 800, height = 400)
anim_save("election.gif", p)

########################### R Shiny APP with Animation ####################################
library(gapminder)
library(ggplot2)
library(shiny)
library(gganimate)
theme_set(theme_bw())

ui <- basicPage(
  imageOutput("plot1"))

server <- function(input, output) {
  output$plot1 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    # now make the animation
    p = ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, 
                              color = continent)) + geom_point() + scale_x_log10() +
      transition_time(year) # New
    
    anim_save("outfile.gif", animate(p)) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         width = 400,
         height = 300,
         alt = "This is alternate text"
    )}, deleteFile = TRUE)}

shinyApp(ui, server)
