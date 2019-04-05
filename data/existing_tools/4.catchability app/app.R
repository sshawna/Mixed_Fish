#####################################
## Shiny app for catchability 
## exploration
## v.2 - Paul Dolder 19/10/2017
#####################################
# Select area
Area <- "Celtic_Sea"  
#####################################

library(shiny)
library(ggplot2)

load(file.path('..', '4.catchability app/data',Area,'catchability.Rdata'))

## server 
server <- function(input, output) {
  
  output$table <- DT::renderDataTable(DT::datatable({
    #data <- effort[,c("year","iter","data","qname")]
    data <- catchability[,c("year","stock","fleet", "metier","logq","country")]
    
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    if (input$Stock != "All") {
      data <- data[data$stock == input$Stock,]
    }
    
    data[,c("logq")] <- round(data[,c("logq")], 2)
    data[,c("year","country","fleet","metier","stock","logq")]
  }))
  
  output$plotCatch <- renderPlot({
    data <- catchability
    
    if (input$country != "All") {
        data <- data[data$country %in% input$country,]
    }
    
    if (input$stock != "All") {
      data <- data[data$stock %in% input$stock,]
    }
    
    #remove OTH
    data <- data[data$metier!="OTH",]
    
    print(ggplot(data, aes(x = year, y = logq,group=stock)) +
            geom_point(aes(colour=stock)) + geom_smooth(method = loess, fullrange = FALSE,aes(colour=stock)) +
            facet_wrap(fleet ~ metier,scales="free_y") +
            theme_bw())# + scale_x_continuous(breaks = seq(2004,2014,by=4)))
    
  })
  
  
}


## ui

input <- catchability

ui <- navbarPage("WGMIXFISH fleet data explorer",
           
           tabPanel("Fleet Catchability Tables",
                    fluidPage(
                      titlePanel(paste(Area,"Catchability data")),
                      
                      
                      #fluidPage(
                      # titlePanel("Basic DataTable"),
                      
                      # Create a new Row in the UI for selectInputs
                      fluidRow(
                        column(3,
                               selectInput("year",
                                           "Year:",
                                           c("All",
                                             sort(unique(as.character(catchability$year)),decreasing=TRUE))
                               )
                        ),
                        column(3,
                               selectInput("Stock",
                                           "Stock",
                                           c("All",
                                             sort(unique(as.character(catchability$stock))))
                               )
                        )
                        
                      ),
                      
                      # Create a new row for the table.
                      fluidRow(
                        DT::dataTableOutput("table")
                      )
                    )),
           
           tabPanel("Catchability Plots",
                    fluidPage(
                      titlePanel(paste(Area,"Catchability data")),
                      
                      fluidRow(
                        column(3,
                               selectInput("country",
                                           "Country:",
                                           c("All",
                                             sort(unique(as.character(catchability$country))))#,
                                           #selected = 'BE'
                               )
                        ),
                        column(3,
                               selectInput("stock",
                                           "Stock:",
                                           c("All",
                                             sort(unique(as.character(catchability$stock)))),
                                           multiple=TRUE
                                           #selected = 'COD-NS'
                               )
                        )
                        
                      ),
                      mainPanel(
                        plotOutput("plotCatch", width = '800px', height = '800px')
                      )
                    ))
           
)

# Run app
shinyApp(ui = ui, server = server)

