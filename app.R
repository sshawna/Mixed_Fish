library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Mixed Fisheries"),
  navlistPanel(id="mainpanel", widths=c(2,10), 
               tabPanel("Landings"),
               tabPanel("Effort"),
               tabPanel("Existing Tools"),
               tabPanel("Schenarios")
               ),
  hr(),
  fluidRow(width =12,
           img(src="Logos/Niamh.png", width = "1250px", height = "100px", style="display: block; margin-left: auto; margin-right: auto;margin-top:0em")
           )
)

############################################################################################
############################################################################################
server <- function(input, output, session) {}

shinyApp(ui, server)
