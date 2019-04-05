#####################################
## Shiny app for 
## parital F exploration
## v.2 - Paul Dolder 19/10/2017
#####################################
# Select area
Area <- "Celtic_Sea"  
#####################################

library(shiny)
library(ggplot2)


load(file.path('data',Area,'partF.Rdata'))


## server
server <- function(input, output) {
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- partF[,c("year","stock","fleet", "metier","partF","country")]
    
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    if (input$Stock != "All") {
      data <- data[data$stock == input$Stock,]
    }
    
    data[,c("partF")] <- round(data[,c("partF")],8)
    data[,c("year","country","fleet","metier","stock","partF")]
    
  }))
  
  output$plotCatch <- renderPlot({
    data <- partF
    data <- aggregate(list(partF=data$partF),list(year=data$year,stock=data$stock,
                                                  fleet=data$fleet, metier=data$metier,country=data$country),sum)
    data <- merge(data,aggregate(data$partF,list(year=data$year,stock=data$stock),sum,na.rm=T))
    data$percent <- data$partF/data$x
    
    
    if (input$country != "All") {
          data <- data[data$country %in% input$country,]
    }
    
    if (any(length(input$stock)>1 | input$stock != "All")) {
            data <- data[data$stock %in% input$stock,]
    }
    
    data <- subset(data, !is.na(percent))
    
    #remove OTH
    #data <- data[data$metier!="OTH",]
    
    print(ggplot(data, aes(x = year, y = percent,group=stock)) +
            geom_point(aes(colour = factor(stock))) + geom_smooth(method = lm, fullrange = FALSE, aes(colour = factor(stock))) +
            facet_wrap(fleet ~ metier,scales="free_y") +
            theme_bw() + scale_x_continuous(breaks = seq(2004,2014,by=4)))
    
  })
  
  output$Spiderplot <- renderPlot({
    data <- partF
    
    if (input$year_ != "All") {
            data <- data[data$year %in% input$year_,]
    }
    
    if (input$stocks != "All") {
        data <- data[data$stock %in% input$stocks,]
    }
    
    #remove OTH
    #data <- data[data$metier!="OTH",]
    data <- aggregate(list(partF=data$partF),list(stock=data$stock,fleet=data$fleet),sum)
    data <- merge(data,aggregate(data$partF,list(stock=data$stock),sum))
    data$percent <- data$partF/data$x
    
    nstock <- length(input$stocks)
    
    print(ggplot(data,aes(x=stock,y=percent,col=stock)) + 
            facet_wrap(~fleet,ncol=7) + coord_polar() + 
            geom_bar(stat="identity",fill=NA) + theme_bw() + 
            theme(legend.position="none") + 
            theme(strip.text.x = element_text(size = rel(0.8)),
                  axis.text.x = element_text(size = rel(0.5),
                                             angle = 360/(2*pi)*rev( pi/2 + seq( pi/nstock, 2*pi-pi/nstock, len=nstock))
                  ))
    )
    
    
  })
  
}


## ui

input <- partF



ui <- navbarPage("WGMIXFISH fleet data explorer",
           
           tabPanel("Fleet Partial F Tables",
                    fluidPage(
                      titlePanel(paste(Area,"Partial F data")),
                      
                      
                      #fluidPage(
                      # titlePanel("Basic DataTable"),
                      
                      # Create a new Row in the UI for selectInputs
                      fluidRow(
                        column(3,
                               selectInput("year",
                                           "Year:",
                                           c("All",
                                             sort(unique(as.character(partF$year)),decreasing=T))
                               )
                        ),
                        column(3,
                               selectInput("Stock",
                                           "Stock",
                                           c("All",
                                             sort(unique(as.character(partF$stock)))))
                        )
                        
                      ),
                      
                      # Create a new row for the table.
                      fluidRow(
                        DT::dataTableOutput("table")
                      )
                    )),
           
           tabPanel("Partial F time series",
                    fluidPage(
                      titlePanel(paste(Area,"Partial F data")),
                      
                      fluidRow(
                        column(3,
                               selectInput("country",
                                           "Country:",
                                           c("All",
                                             sort(unique(as.character(partF$country))))#,
                                           #selected = 'BE'
                               )
                        ),
                        column(3,
                               selectInput("stock",
                                           "Stock:",
                                           c("All",
                                             sort(unique(as.character(partF$stock)))),
                                           multiple=TRUE,
                                           selected = 'All'
                               )
                        )
                        
                      ),
                      mainPanel(
                        plotOutput("plotCatch", width = '800px', height = '800px')
                      )
                    ) #end of FluidPage
           ),#end of tabPanel
           
           
           tabPanel("Partial F spider chart",
                    fluidPage(
                      titlePanel(paste(Area,"Partial F data")),
                      
                      fluidRow(
                        column(3,
                               selectInput("year_",
                                           "Year:",
                                           c("All",
                                             sort(unique(as.character(partF$year)),decreasing=T)),
                                           selected = 'All'
                               )
                        ),
                        column(3,
                               selectInput("stocks",
                                           "Stocks:",
                                           c("All",
                                             sort(unique(as.character(partF$stock)))),
                                           multiple=T,
                                           selected = 'All'
                               )
                        )
                        
                      ),
                      mainPanel(
                        plotOutput("Spiderplot", width = '800px', height = '800px')
                      )
                    ) #end of FluidPage
           )#end of tabPanel
           
           
           
) #end of NavBarPage


# Run app
shinyApp(ui = ui, server = server)
