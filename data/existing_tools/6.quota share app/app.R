#####################################
## Shiny app for 
## Quota share
## v.1 - Paul Dolder 19/10/2017
#####################################
# Select area
Area <- "Celtic_Sea"  
#####################################

library(shiny)
library(ggplot2)

load(file.path('data',Area,'QuotaShare.Rdata'))

## Processing

data <- quotashare
colnames(data)[2] <- "stock"
data_tab <- data
data_tab[,c("relstab","catchcomp","landings")] <- round(data_tab[,c("relstab","catchcomp","landings")],2)
colnames(data_tab)[7:8] <- c("proportion_fleet_landings", "stock_landings_share")

## server

server <- function(input, output) {
  
  output$table <- DT::renderDataTable(DT::datatable({
    
    if (input$year != "All") {
      data_tab <- data_tab[data_tab$year == input$year,]
    }
    if (input$Stock != "All") {
      data_tab <- data_tab[data_tab$stock == input$Stock,]
    }
    
    data_tab[,c("year","fleet","stock","landings","proportion_fleet_landings", "stock_landings_share")]
  }))
  
  output$plotCatch <- renderPlot({
    
    data <- aggregate(list(Share=data$relstab),list(year=data$year,stock=data$stock,
                                                    fleet=data$fleet),sum)
    if (input$fleet != "All") {
      data <- data[data$fleet %in% input$fleet,]
    }
    
    if (any(length(input$stock)>1 | input$stock != "All")) {
      data <- data[data$stock %in% input$stock,]
    }
    
    print(ggplot(data, aes(x = year, y = Share,group=paste(fleet,stock))) +
            geom_point(aes(colour = factor(stock)), size = 2) + 
          #  geom_smooth(method = loess, aes(colour = factor(stock), fill = factor(stock)), se = F) +
            geom_line(aes(colour = factor(stock))) +
            facet_wrap(~ fleet,scales="free_y") +
            theme_bw() + theme(axis.text.x = element_text(angle = - 90)) )
    
  })
  
  
output$plotComp <- renderPlot({

       if (input$fleet2 != "All") {
      data <- data[data$fleet %in% input$fleet2,]
    }
    
    print(ggplot(data, aes(x = year, y = catchcomp,group=fleet)) +
            geom_bar(stat = 'identity', aes(fill = stock)) +
            facet_wrap(~ fleet,scales="free_y") + geom_text(aes(x = year, y = 0.9, label = round(fleet_landings,0))) + 
            theme_bw() + theme(axis.text.x = element_text(angle = - 90)) + coord_flip())
    
  })
  
  
  
  
}



# ui

input <- quotashare
input <- input[,c("year","qname","fleet", "relstab","catchcomp")]
colnames(input)[2] <- "stock"
quotashare <- input

ui <- navbarPage("WGMIXFISH fleet data explorer",
                 
                 tabPanel("Fleet Landings share Tables",
                          fluidPage(
                            titlePanel(paste(Area,"Landings share data")),
                            
                            # Create a new Row in the UI for selectInputs
                            fluidRow(
                              column(3,
                                     selectInput("year",
                                                 "Year:",
                                                 c("All",
                                                   sort(unique(as.character(quotashare$year)),decreasing=T))
                                     )
                              ),
                              column(3,
                                     selectInput("Stock",
                                                 "Stock",
                                                 c("All",
                                                   sort(unique(as.character(quotashare$stock)))))
                              )
                              
                            ),
                            
                            # Create a new row for the table.
                            fluidRow(
                              DT::dataTableOutput("table")
                            )
                          )),
                 
                 tabPanel("Landings share time series",
                          fluidPage(
                            titlePanel(paste(Area,"Landings share data")),
                            
                            fluidRow(
                              column(3,
                                     selectInput("fleet",
                                                 "Fleet:",
                                                 c("All",
                                                   sort(unique(as.character(quotashare$fleet)))),
                                                 multiple = TRUE,
                                                 selected = 'All'
                                     )
                              ),
                              column(3,
                                     selectInput("stock",
                                                 "Stock:",
                                                 c("All",
                                                   sort(unique(as.character(quotashare$stock)))),
                                                 multiple=TRUE,
                                                 selected = 'All'
                                     )
                              )
                              
                            ),
                            mainPanel(
                              plotOutput("plotCatch", width = '800px', height = '400px')
                            )
                          ) #end of FluidPage
                 ),#end of tabPanel
                 
                 tabPanel("Landings composition time series",
                          fluidPage(
                            titlePanel(paste(Area,"Landings composition data")),
                            
                            fluidRow(
                              column(3,
                                     selectInput("fleet2",
                                                 "Fleet:",
                                                 c("All",
                                                   sort(unique(as.character(quotashare$fleet)))),
                                                 multiple=TRUE,
                                                 selected = 'All'
                                     )
                              )),
                            
                            mainPanel(
                              plotOutput("plotComp", width = '1200px', height = '800px')
                            )
                          ) #end of FluidPage
                 )#end of tabPanel
                 
                 
                 
) #end of NavBarPage



# Run app
shinyApp(ui = ui, server = server)
