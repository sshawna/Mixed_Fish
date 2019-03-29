library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)

#ui <- fluidPage(
  #theme = shinytheme("superhero"),
  #titlePanel("Mixed Fisheries"),
  #navlistPanel(id="mainpanel", widths=c(2,10), 
              # tabPanel("Landings"),
              # tabPanel("Effort"),
              # tabPanel("Existing Tools"),
              # tabPanel("Schenarios")
              # ),
  #hr(),
  #fluidRow(width =12,
         #  img(src="Logos/Niamh.png", width = "1250px", height = "100px", style="display: block; margin-left: auto; margin-right: auto;margin-top:0em")
          # )
#)

############################################################################################
############################################################################################
#server <- function(input, output, session) {}

#shinyApp(ui, server)

dSiderBar <- dashboardSidebar(
  sidebarMenu( 
    id = "tabs",
    menuItem("First Page", tabName = "mp", icon = icon("home")),
    menuItem("Landings", tabName = "mi", icon = icon("dharmachakra ")),
    menuItem("Efforts", tabName = "sb", icon = icon("dharmachakra ")),
    menuItem("Existing Tools", tabName = "et", icon = icon("dharmachakra")),
    menuItem("Scenarios", tabName = "sc", icon = icon("line-chart "))
  ))

ui <- dashboardPage( title = "Mixed Fisheries",
                     dashboardHeader(title = "Mixed Fisheries",tags$li(class = "dropdown",
                                                                       tags$li(class = "dropdown",
                                                                               socialButton(
                                                                                 url = "https://github.com/sshawna/Mixed_Fish",
                                                                                 type = "github"
                                                                               ),
                                                                               style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px; "),
                                                                       useShinyalert(),  
                                                                       tags$li(class = "dropdown",
                                                                               actionButton("about", "About"),
                                                                               style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px;"))),
                     dSiderBar, 
                     dashboardBody(
                       tabItems( 
                         tabItem("mp", box(title="Introduction" ,width = 12,height = 700), img(src="Logos/Niamh.png", width = "1250px", height = "100px", style="display: block; margin-left: auto; margin-right: auto;margin-top:0em"))
                         ,
                         tabItem("mi"),
                         tabItem("sb"),
                         tabItem("et"),
                         tabItem("sc")
                         
                       ) 
                     ))


server <- function(input, output,session) {
  
  observeEvent(input$about, {
    shinyalert(
      title = "Mixed Fisheries",
      text = "Vizualization Tool for Mixed Fisheries Landings and Effort in Celtic Seas Ecoregion. <br> Try changing the filters on the panel to compare different <b>Metier</b>, <b>years</b> and <b>Species</b>.",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#addd8e",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  }) 
  
}

shinyApp(ui, server)


