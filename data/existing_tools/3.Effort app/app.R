#####################################
## Shiny app for 
## catchability exploration
## v.2 - Paul Dolder 19/10/2017
#####################################
# Select area
Area <- "North_Sea"  
#####################################

library(shiny)
library(ggplot2)

load(file.path('..','5.partial F app','data',Area,'partF.Rdata'))

# aggregate across stocks (take mean)
data <- partF[,c("year","country", "fleet", "metier","effort","effshare")]
data <- aggregate(list(effort =data$effort, effshare = data$effshare),list(year=data$year,country = data$country, 
                      fleet=data$fleet,metier=data$metier), mean)

## server 

server <- function(input, output) {
  
  output$table <- DT::renderDataTable(DT::datatable({
    
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    if (input$country != "All") {
      data <- data[data$country == input$country,]
    }
    
    data[,c("effort","effshare")] <- round(data[,c("effort","effshare")],2)
    data
  }))
  
  output$plotCatch <- renderPlot({
    data <- partF
    data$effmet <- data$effort*data$effshare
    
    
    
    
    if (input$country1 != "All") {
          data <- data[data$country %in% input$country1,]
    }
    

    print(ggplot(data, aes(x = year, y = effmet,group=metier)) +
            geom_point(aes(colour = factor(metier))) + geom_line(aes(group = metier)) +# geom_smooth(method = lm, fullrange = FALSE, aes(colour = factor(metier))) +
            facet_wrap(~fleet,scales="free_y") +
            theme_bw()) #+ scale_x_continuous(breaks = seq(2004,2014,by=4)))
    
  })
  
  output$plotCatch_Fl <- renderPlot({
    data <- aggregate(list(effort =data$effort),list(year=data$year,country = data$country, 
                                                        fleet=data$fleet), mean)
    
    if(Area == "North_Sea"){ #to avoid distorsion in plot for these two fleets
      data[data$fleet %in% c("EN_Otter24-40","EN_FDF","EN_Otter>=40","EN_Otter<24"),"fleet"] <- "EN_Otter + FDF"
      data[data$fleet %in% c("DK_Seine","DK_Otter<24"),"fleet"] <- "DK_Otter<24 + Seine"
      data <- aggregate(list(effort =data$effort),list(year=data$year,country = data$country, 
                                                       fleet=data$fleet), sum)
    }  
    
    data_ori <- subset(data,year==min(data$year))
    names(data_ori)[4] <- "effort_ori"
    data <- merge(data,data_ori[-1],all.x=T)
    data$eff_rel <- data$effort/data$effort_ori
    
    print(ggplot(data, aes(x = year, y = eff_rel)) +
            geom_point() + geom_line() +# geom_smooth(method = lm, fullrange = FALSE, aes(colour = factor(metier))) +
            facet_wrap(~fleet) +
            geom_hline(yintercept=1,linetype="dashed") +
            theme_bw()) #+ scale_x_continuous(breaks = seq(2004,2014,by=4)))
    
  })
  
  
}

# ui

input <- partF



ui <- navbarPage("WGMIXFISH fleet data explorer",
           
           tabPanel("Fleet Effort Tables",
                    fluidPage(
                      titlePanel(paste(Area,"Effort data")),
                      
                      
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
                               selectInput("country",
                                           "Country",
                                           c("All",
                                             sort(unique(as.character(partF$country)))))
                        )
                        
                      ),
                      
                      # Create a new row for the table.
                      fluidRow(
                        DT::dataTableOutput("table")
                      )
                    )),
           
           tabPanel("Effort time series",
                    fluidPage(
                      titlePanel(paste(Area,"Effort data")),
                      
                      fluidRow(
                        column(3,
                               selectInput("country1",
                                           "Country:",
                                           c("All",
                                             sort(unique(as.character(partF$country))))#,
                                           #selected = 'BE'
                               )
                        )
                        
                        
                      ),
                      mainPanel(
                        plotOutput("plotCatch", width = '800px', height = '800px')
                      )
                    ) #end of FluidPage
           ),#end of tabPanel

           tabPanel("Total effort by fleet",
                    fluidPage(
                      titlePanel(paste(Area,"Effort by fleet relative to first data year")),
                      
                      fluidRow(
                        # column(3,
                        #        selectInput("country2",
                        #                    "Country:",
                        #                    c("All",
                        #                      sort(unique(as.character(partF$country))))#,
                        #                    #selected = 'BE'
                        #        )
                        # )
                        
                        
                      ),
                      mainPanel(
                        plotOutput("plotCatch_Fl", width = '800px', height = '800px')
                      )
                    ) #end of FluidPage
           )#end of tabPanel
           
           
           
) #end of NavBarPage

## Run app 
shinyApp(ui = ui, server = server)

