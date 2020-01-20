require(shiny)
ui = fluidPage(titlePanel("Hi I am Shiny today you will learn something new about shiny"),
               sidebarLayout(sidebarPanel("Welcome to the world of Shiny App just to give you some idea",
                                         br( a("Click Here:",herf="www.icicidirect.com"))),
                             mainPanel("This is main Panel this has lot of sapace to write anything")))
server = function(input, output) { }
shinyApp(ui, server)

