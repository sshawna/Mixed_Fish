#alternative mapping tab
#interactive mapping of species TAC areas and ICES stock areas overlap, want to visualise by species
library(vmstools)
library(shiny)
library(shinythemes)
library(shinyalert)
library(shinyBS)
library(ggplot2)
library(RColorBrewer)
library(ggiraph)
library(ggthemes)
library(plotly)
library(shinycssloaders)
library(tidyverse)
library(viridis)
library(DT)
library(dplyr)
options(scipen=999)

### server ###
server <- function(input, output, session) {
  
  sp <- c("Cod","Haddock","Whiting","Plaice", "Sole", "Hake", "Megrim", "Anglerfish", "Nephrops")
  stocks <- read.csv("H:\\TCM mapping\\maps\\stocks_SS.csv")
  #stocks_list <- list('cod-7e-k','had-7b-k','hke-nrtn','meg-ivvi','meg-rock','mgw-78','whg-7b-k')
  stocks_list <- levels(stocks$stock)
  
  
  #create dropdown to select stock
  updateSelectizeInput(session, 'Stockselector',
                       choices = list(stocks_list),
                       server = TRUE,
                       selected =1)
  
  output$Stockareas <- renderImage({
    filename <- normalizePath(file.path('maps/',
                                        paste(input$Species_selector, '_stocks.png', sep='')))
    # Return a list containing the filename
    list(src = filename,
         width = 690,
         height = 545)
  }, deleteFile = FALSE)
  
  output$Stockoverlap <- renderImage({
    filename <- normalizePath(file.path('maps/',
                                        paste(input$Stockselector, '.png', sep='') 
    ))
    # Return a list containing the filename
    list(src = filename,
         width = 470,
         height = 350)
  }, deleteFile = FALSE)
  
  
}



### ui ###
ui <- fluidPage(
  
  theme = shinytheme("superhero"), #spacelab
  titlePanel("Mixed Fisheries"),
  navlistPanel(id="mainpanel", widths=c(2,10), 
               tabPanel(" Introduction", value = "mp", icon = icon("home"),
                        useShinyalert(),  
                        tags$li(class = "dropdown",
                                actionButton("about", "About"),
                                style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px;")),
               
               tabPanel(" Hackathon Work", value = "hw", icon = icon("folder-open")),
               tabPanel(" Landings",value = "mi", icon = icon("fish")), 
               
               tabPanel(" Effort", value = "sb", icon = icon("ship")),
               
               tabPanel(" Existing Tools", value = "et", icon = icon("wrench")),
               
               tabPanel(" Mapping", value ="sc", icon = icon("map-marked"),
                        fluidRow(column(width=4, 
                                        plotOutput('')),
                                 column(width=4,
                                        plotOutput('')),
                                 column(width=4,
                                        plotOutput(''))
                        ),
                        fluidRow(column(width=4,
                                        plotOutput('')),
                                 column(width=4,
                                        plotOutput('')),
                                 column(width=4,
                                        plotOutput(''))
                        ),
                        fluidRow(column(width=4, 
                                        plotOutput('')),
                                 column(width=4,
                                        plotOutput('')),
                                 column(width=4,
                                        plotOutput(''))
                        )
               ),
               tabPanel(" Schenarios", value ="sc", icon = icon("line-chart"))
  ),
  hr(),
  fluidRow(width =12,
           img(src="Logos/Niamh.png", width = "1250px", height = "100px", 
               style="display: block; margin-left: auto; margin-right: auto;margin-top:0em")
  )
)

shinyApp(ui, server)
