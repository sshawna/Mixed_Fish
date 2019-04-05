


shinyUI(fluidPage(theme = shinytheme("superhero"),
tags$h1("Landings "),
fluidRow(
 column(width=3,div(style="display: inline-block;vertical-align:top; width: 125px;",selectInput("set1", label = "Select Metier",c("Choose",levels(test$Metier_lvl5)),selectize = T))
 ), column(width=3,div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("set2", label = "Select Year",c("Choose",c(2009:2017)),selectize = T))
  ),column(width=3,div(style="display: inline-block;vertical-align:top; width: 125px;",selectInput("set3", label = "Select Species",c("Choose",levels(test$Species)),selectize = T))
  ), column(width=3,div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("set4", label = "Select Year",c("Choose",c(2009:2017)),selectize = T))
  )),
fluidRow(
  #column(width=6),uiOutput("LbySpec")))
  column(width = 4,
         ggiraph::ggiraphOutput("plot1")
  ),
  column(width = 2,
         h4("Selected Species"),
         tableOutput("datatab1"),
         actionButton("reset1", label = "Reset selection")
  ),
  column(width = 4,
         ggiraph::ggiraphOutput("plot2")
  ),
  column(width = 2,
         h4("Selected Metier"),
         tableOutput("datatab2"),
         actionButton("reset2", label = "Reset selection")
  )
)
))
