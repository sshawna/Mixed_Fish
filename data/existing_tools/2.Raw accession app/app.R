#####################################
## Shiny app for 
## Raw accessions
## v.2 - Paul Dolder 19/10/2017
#####################################
# Select area
#Area <- "Bay_of_Biscay"  
#####################################

library(shiny)
library(ggplot2)
library(dplyr)

data_loc <- file.path('..','1.data_prep','Combined_accession_data') # accessions data location
load(file.path(data_loc,'Accessions_Catch.RData'))
load(file.path(data_loc,'Accessions_Effort.RData'))

levels(Ca$Country)[levels(Ca$Country)== "nld"] <- "NLD"

## Order the variables
Ca <- Ca[order(Ca$Country, Ca$Metier,Ca$Area, Ca$Species),]


## Calculate LPUE
lpue <- Ca
lpue <- lpue %>% group_by(Country, Year, Metier, Area, Species) %>% 
  summarise(Landings = sum(Landings), Value = sum(Value))

Ef <- Ef %>% group_by(Country, Year, Metier, Area) %>%
  summarise(kw_days = sum(kw_days), Days_at_sea = sum(Days_at_sea), No_vessels = sum(No_vessels))

lpue$effort <- Ef$kw_days[match(paste(lpue$Country, lpue$Year, lpue$Metier, lpue$Area),
                                paste(Ef$Country, Ef$Year, Ef$Metier, Ef$Area))]

lpue$lpue <- (lpue$Landings / lpue$effort) * 1e6

lpue$vpue <- (lpue$Value / lpue$effort)  # value per tonne

## calculate value-per-tonne
Ca$vpt <- Ca$Value / Ca$Landings

# server

server <- function(input, output) {
  
  # Filter data based on selections
  output$tableCa <- DT::renderDataTable(DT::datatable({
    CaFil <- Ca
    if (input$CaCountry != "All") {
      CaFil <- filter(CaFil, Country %in% input$CaCountry)
    }
    if (input$CaYear != "All") {
      CaFil <- filter(CaFil, Year %in% input$CaYear)
    }
    if (input$CaMetier != "All") {
      CaFil <- filter(CaFil, Metier %in% input$CaMetier)
    }
    if (input$CaSpecies != "All") {
      CaFil <- filter(CaFil, Species %in% input$CaSpecies)
    }
    if (input$CaArea != "All") {
      CaFil <- filter(CaFil, Area %in% input$CaArea)
    }
    CaFil
  }))
  
  
  output$plotCatch <- renderPlot({
    CaFil <- Ca
    if (input$CaPlCountry != "All") {
      CaFil <- filter(CaFil, Country %in% input$CaPlCountry)
    }
    if (input$CaPlYear != "All") {
      CaFil <- filter(CaFil, Year %in% input$CaPlYear)
    }
    if (input$CaPlMetier != "All") {
      CaFil <- filter(CaFil, Metier %in% input$CaPlMetier)
    }
    if (input$CaPlArea != "All") {
      CaFil <- filter(CaFil, Area %in% input$CaPlArea)
    }
    
    if (input$CaPlSpecies != "All") {
      CaFil <- filter(CaFil, Species %in% input$CaPlSpecies)
    }
    
    print(ggplot(CaFil, aes(x = Year, y = Landings)) +
            geom_bar(stat = 'identity', aes(fill = Species)) + 
            facet_wrap(Country ~ Metier) +
            theme_bw() + theme(axis.text.x = element_text(angle = -90)) +
            ylab("Landings (tonnes)"))
    
  })
  
  output$downCatch <- downloadHandler(
    filename <- "CatchPlot.pdf",
    content <- function(file) {
      pdf(file = file, width = 12, 8)
      CaFil <- Ca
      if (input$CaPlCountry != "All") {
        CaFil <- filter(CaFil, Country %in% input$CaPlCountry)
      }
      if (input$CaPlYear != "All") {
        CaFil <- filter(CaFil, Year %in% input$CaPlYear)
      }
      if (input$CaPlMetier != "All") {
        CaFil <- filter(CaFil, Metier %in% input$CaPlMetier)
      }
      if (input$CaPlArea != "All") {
        CaFil <- filter(CaFil, Area %in% input$CaPlArea)
      }
      
      if (input$CaPlSpecies != "All") {
        CaFil <- filter(CaFil, Species %in% input$CaPlSpecies)
      }
      
      p1 <- ggplot(CaFil, aes(x = Year, y = Landings)) +
        geom_bar(stat = 'identity', aes(fill = Species)) + 
        facet_wrap(Country ~ Metier) +
        theme_bw() + theme(axis.text.x = element_text(angle = -90) + 
                             ylab("Landings (tonnes)"))
      
      print(p1)
      dev.off()
    }
  )
  
  # Filter data based on selections
  output$tableEf <- DT::renderDataTable(DT::datatable({
    EfFil <- Ef
    if (input$EfCountry != "All") {
      EfFil <- filter(EfFil, Country %in% input$EfCountry)
    }
    if (input$EfYear != "All") {
      EfFil <- filter(EfFil, Year %in% input$EfYear)
    }
    if (input$EfMetier != "All") {
      EfFil <- filter(EfFil, Metier %in% input$EfMetier)
    }
    if (input$EfArea != "All") {
      EfFil <- filter(EfFil, Area %in% input$EfArea)
    }
    EfFil
  }))
  
  output$plotEffort <- renderPlot({
    EfFil <- Ef
    if (input$EfPlCountry != "All") {
      EfFil <- filter(EfFil, Country %in% input$EfPlCountry)
    }
    if (input$EfPlYear != "All") {
      EfFil <- filter(EfFil, Year %in% input$EfPlYear)
    }
    if (input$EfPlMetier != "All") {
      EfFil <- filter(EfFil, Metier %in% input$EfPlMetier)
    }
    if (input$EfPlArea != "All") {
      EfFil <- filter(EfFil, Area %in% input$EfPlArea)
    }
    
    EfFil <- group_by(EfFil, Country, Year, Metier, Area) %>% summarise(kw_days = sum(kw_days))
    
    print(ggplot(EfFil, aes(x = Year, y = kw_days, group = Area)) +
            geom_line(aes(group = Area, linetype = Area)) +
            geom_point(aes(shape = Area)) +
            facet_wrap(Country ~ Metier) +    theme_bw() +
            theme(axis.text.x = element_text(angle = -90)))
    
  })
  
  output$downEf <- downloadHandler(
    filename <- "EffortPlot.pdf",
    content <- function(file) {
      pdf(file = file, width = 12, 8)
      EfFil <- Ef
      if (input$EfPlCountry != "All") {
        EfFil <- filter(EfFil, Country %in% input$EfPlCountry)
      }
      if (input$EfPlYear != "All") {
        EfFil <- filter(EfFil, Year %in% input$EfPlYear)
      }
      if (input$EfPlMetier != "All") {
        EfFil <- filter(EfFil, Metier %in% input$EfPlMetier)
      }
      if (input$EfPlArea != "All") {
        EfFil <- filter(EfFil, Area %in% input$EfPlArea)
      }
      
      EfFil <- group_by(EfFil, Country, Year, Metier, Area) %>% summarise(kw_days = sum(kw_days))
      
      p2<- print(ggplot(EfFil, aes(x = Year, y = kw_days/1000, group = Area)) +
                   geom_line(aes(group = Area, linetype = Area)) +
                   geom_point(aes(shape = Area)) +
                   facet_wrap(Country ~ Metier) +    theme_bw() +
                   theme(axis.text.x = element_text(angle = -90)) + 
                   ylab("kw days (000)"))
      
      print(p2)
      dev.off()
    }
  )
  
  # lpue
  output$plotlpue <- renderPlot({
    lpueFil <- lpue
    if (input$lpuePlCountry != "All") {
      lpueFil <- filter(lpueFil, Country %in% input$lpuePlCountry)
    }
    if (input$lpuePlYear != "All") {
      lpueFil <- filter(lpueFil, Year %in% input$lpuePlYear)
    }
    if (input$lpuePlMetier != "All") {
      lpueFil <- filter(lpueFil, Metier %in% input$lpuePlMetier)
    }
    if (input$lpuePlArea != "All") {
      lpueFil <- filter(lpueFil, Area %in% input$lpuePlArea)
    }
    
    if (input$lpuePlSpecies != "All") {
      lpueFil <- filter(lpueFil, Species %in% input$lpuePlSpecies)
    }
    
    if(input$vpue == FALSE) {
    
    print(ggplot(lpueFil, aes(x = Year, y = lpue)) +
            geom_line(aes(group = Area, linetype = Area)) + 
            geom_point(aes(shape = Area)) +
            facet_wrap(Country ~ Metier) +
            theme_bw() + theme(axis.text.x = element_text(angle = -90))+
            ylab("LPUE (g per kw day)"))
    }
    
    if(input$vpue == TRUE) {
      
      print(ggplot(lpueFil, aes(x = Year, y = vpue)) +
              geom_line(aes(group = Area, linetype = Area)) + 
              geom_point(aes(shape = Area)) +
              facet_wrap(Country ~ Metier) +
              theme_bw() + theme(axis.text.x = element_text(angle = -90))+
              ylab("VPUE (Euros per kw day)"))
    }
    
  })
  
  output$downlpue <- downloadHandler(
    filename <- "lpuePlot.pdf",
    content <- function(file) {
      pdf(file = file, width = 12, 8)
      lpueFil <- lpue
      if (input$lpuePlCountry != "All") {
        lpueFil <- filter(lpueFil, Country %in% input$lpuePlCountry)
      }
      if (input$lpuePlYear != "All") {
        lpueFil <- filter(lpueFil, Year %in% input$lpuePlYear)
      }
      if (input$lpuePlMetier != "All") {
        lpueFil <- filter(lpueFil, Metier %in% input$lpuePlMetier)
      }
      if (input$lpuePlArea != "All") {
        lpueFil <- filter(lpueFil, Area %in% input$lpuePlArea)
      }
      
      if (input$lpuePlSpecies != "All") {
        lpueFil <- filter(lpueFil, Species %in% input$lpuePlSpecies)
      }
      
      if(input$vpue == FALSE) {
        
        p1<- print(ggplot(lpueFil, aes(x = Year, y = lpue)) +
                geom_line(aes(group = Area, linetype = Area)) + 
                geom_point(aes(shape = Area)) +
                facet_wrap(Country ~ Metier) +
                theme_bw() + theme(axis.text.x = element_text(angle = -90))+
                ylab("LPUE (g per kw day)"))
      }
      
      if(input$vpue == TRUE) {
        
        p1<- print(ggplot(lpueFil, aes(x = Year, y = vpue)) +
                geom_line(aes(group = Area, linetype = Area)) + 
                geom_point(aes(shape = Area)) +
                facet_wrap(Country ~ Metier) +
                theme_bw() + theme(axis.text.x = element_text(angle = -90))+
                ylab("VPUE (Euros per kw day)"))
      }
      
      
      print(p1)
      dev.off()
    }
  )
  
  # value
  
  output$plotVal <- renderPlot({
    ValFil <- Ca
    if (input$ValPlCountry != "All") {
      ValFil <- filter(ValFil, Country %in% input$ValPlCountry)
    }
    if (input$ValPlYear != "All") {
      ValFil <- filter(ValFil, Year %in% input$ValPlYear)
    }
    if (input$ValPlMetier != "All") {
      ValFil <- filter(ValFil, Metier %in% input$ValPlMetier)
    }
    if (input$ValPlArea != "All") {
      ValFil <- filter(ValFil, Area %in% input$ValPlArea)
    }
    
    if (input$ValPlSpecies != "All") {
      ValFil <- filter(ValFil, Species %in% input$ValPlSpecies)
    }
    
    if(input$Price == FALSE) {
      p1 <- ggplot(ValFil, aes(x = Year, y = Value)) +
        geom_bar(stat = 'identity', aes(fill = Species)) + 
        facet_wrap(Country ~ Metier) +
        theme_bw() + theme(axis.text.x = element_text(angle = -90)) + 
                             ylab("Value (Euros)")
    }
    
    if(input$Price == TRUE) {
      
      p1 <- ggplot(ValFil, aes(x = Year, y = Value/(Landings*1000))) +
        geom_bar(stat = 'identity', aes(fill = Species)) + 
        facet_wrap(Country ~ Metier) +
        theme_bw() + theme(axis.text.x = element_text(angle = -90)) + 
                             ylab("Price (Euros per KG)")
    }
    
    
   print(p1)
    
  })
  
  output$downCatch <- downloadHandler(
    filename <- "ValuePlot.pdf",
    content <- function(file) {
      pdf(file = file, width = 12, 8)
      ValFil <- Ca
      if (input$ValPlCountry != "All") {
        ValFil <- filter(ValFil, Country %in% input$ValPlCountry)
      }
      if (input$ValPlYear != "All") {
        ValFil <- filter(ValFil, Year %in% input$ValPlYear)
      }
      if (input$ValPlMetier != "All") {
        ValFil <- filter(ValFil, Metier %in% input$ValPlMetier)
      }
      if (input$ValPlArea != "All") {
        ValFil <- filter(ValFil, Area %in% input$ValPlArea)
      }
      
      if (input$ValPlSpecies != "All") {
        ValFil <- filter(ValFil, Species %in% input$ValPlSpecies)
      }
      
      if(input$Price == FALSE) {
      p1 <- ggplot(ValFil, aes(x = Year, y = Value)) +
        geom_bar(stat = 'identity', aes(fill = Species)) + 
        facet_wrap(Country ~ Metier) +
        theme_bw() + theme(axis.text.x = element_text(angle = -90) + 
                             ylab("Value (Euros)"))
      }
      
      if(input$Price == TRUE) {
      
        p1 <- ggplot(ValFil, aes(x = Year, y = Value/Landings)) +
          geom_bar(stat = 'identity', aes(fill = Species)) + 
          facet_wrap(Country ~ Metier) +
          theme_bw() + theme(axis.text.x = element_text(angle = -90) + 
                               ylab("Price (Euros per KG)"))
      
      }
      
      print(p1)
      dev.off()
    }
  )
  
  
}

## ui
ui <- navbarPage("WGMIXFISH data explorer",
                 tabPanel("Accessions Catch Tables",
                          fluidPage(
                            titlePanel("WGMIXFISH Accessions Catch data"),
                            
                            # Create a new Row in the UI for selectInputs
                            fluidRow(
                              column(2,
                                     selectInput("CaCountry",
                                                 "Country:",
                                                 c("All",
                                                   unique(as.character(Ca$Country))),
                                                 multiple = T, selected = 'All')
                              ),
                              column(2,
                                     selectInput("CaYear",
                                                 "Year:",
                                                 c("All",
                                                   unique(as.character(Ca$Year))),
                                                 multiple = T, selected = 'All')
                              ),
                              column(2,
                                     selectInput("CaMetier",
                                                 "Metier:",
                                                 c("All",
                                                   unique(as.character(Ca$Metier))),
                                                 multiple = T, selected = 'All')
                              ),
                              column(2,
                                     selectInput("CaSpecies",
                                                 "Species:",
                                                 c("All",
                                                   unique(as.character(Ca$Species))),
                                                 multiple = T, selected = 'All')
                              ),
                              column(2,
                                     selectInput("CaArea",
                                                 "Area:",
                                                 c("All",
                                                   unique(as.character(Ca$Area))),
                                                 multiple = T, selected = 'All')
                              )),
                            
                            # Create a new row for the table.
                            fluidRow(
                              DT::dataTableOutput("tableCa")
                            )
                          )),
                 tabPanel("Accessions effort Tables",
                          fluidPage(
                            titlePanel("WGMIXFISH Accessions Effort data"),
                            
                            # Create a new Row in the UI for selectInputs
                            fluidRow(
                              column(2,
                                     selectInput("EfCountry",
                                                 "Country:",
                                                 c("All",
                                                   unique(as.character(Ef$Country))),
                                                 multiple = T, selected = 'All')
                              ),
                              column(2,
                                     selectInput("EfYear",
                                                 "Year:",
                                                 c("All",
                                                   unique(as.character(Ef$Year))),
                                                 multiple = T, selected = 'All')
                              ),
                              column(2,
                                     selectInput("EfMetier",
                                                 "Metier:",
                                                 c("All",
                                                   unique(as.character(Ef$Metier))),
                                                 multiple = T, selected = 'All')
                              ),
                              column(2,
                                     selectInput('EfArea',
                                                 'Area:',
                                                 c('All',
                                                   unique(as.character(Ef$Area))),
                                                 multiple = T, selected = 'All')
                              ),
                              # Create a new row for the table.
                              fluidRow(
                                DT::dataTableOutput("tableEf")
                              )
                            ))),
                 tabPanel("Catch Plots",
                          downloadButton('downCatch', 'Download Plot'),
                          fluidRow(
                            column(2,
                                   selectInput("CaPlCountry",
                                               "Country:",
                                               c("All",
                                                 unique(as.character(Ca$Country))),
                                               multiple = T, selected = 'BEL')
                            ),
                            column(2,
                                   selectInput("CaPlYear",
                                               "Year:",
                                               c("All",
                                                 unique(as.character(Ca$Year))),
                                               multiple = T, selected = 'All')
                            ),
                            column(2,
                                   selectInput("CaPlMetier",
                                               "Metier:",
                                               c("All",
                                                 unique(as.character(Ca$Metier))),
                                               multiple = T, selected = 'All')
                            ),
                            column(2,
                                   selectInput("CaPlArea",
                                               "Area:",
                                               c("All",
                                                 unique(as.character(Ca$Area))),
                                               multiple = T, selected = '27.4.a')
                            ),
                            column(2,
                                   selectInput("CaPlSpecies",
                                               "Species:",
                                               c("All",
                                                 unique(as.character(Ca$Species))),
                                               multiple = T, selected = 'COD')
                            )
                            
                        ),
                          mainPanel(
                            plotOutput("plotCatch", width = '1200px', height = '800px')
                          )
                 ),
                 tabPanel("Effort Plots",
                          downloadButton('downEf', 'Download Plot'),
                          fluidRow(
                            column(2,
                                   selectInput("EfPlCountry",
                                               "Country:",
                                               c("All",
                                                 unique(as.character(Ef$Country))),
                                               multiple = T, selected = 'BEL')
                            ),
                            column(2,
                                   selectInput("EfPlYear",
                                               "Year:",
                                               c("All",
                                                 unique(as.character(Ef$Year))),
                                               multiple = T, selected = 'All')
                            ),
                            column(2,
                                   selectInput("EfPlMetier",
                                               "Metier:",
                                               c("All",
                                                 unique(as.character(Ef$Metier))),
                                               multiple = T, selected = 'All')
                            ),
                            column(2,
                                   selectInput("EfPlArea",
                                               "Area:",
                                               c("All",
                                                 unique(as.character(Ef$Area))),
                                               multiple = T, selected = '27.4.a')
                            ),
                            
                            mainPanel(
                              plotOutput("plotEffort", width = '1200px', height = '800px')
                            )
                          )
                 ),
                 tabPanel("LPUE Plots",
                          downloadButton('downlpue', 'Download Plot'),
                          fluidRow(
                            column(2,
                                   selectInput("lpuePlCountry",
                                               "Country:",
                                               c("All",
                                                 unique(as.character(lpue$Country))),
                                               multiple = T, selected = 'BEL')
                            ),
                            column(2,
                                   selectInput("lpuePlYear",
                                               "Year:",
                                               c("All",
                                                 unique(as.character(lpue$Year))),
                                               multiple = T, selected = 'All')
                            ),
                            column(2,
                                   selectInput("lpuePlMetier",
                                               "Metier:",
                                               c("All",
                                                 unique(as.character(lpue$Metier))),
                                               multiple = T, selected = 'All')
                            ),
                            column(2,
                                   selectInput("lpuePlArea",
                                               "Area:",
                                               c("All",
                                                 unique(as.character(lpue$Area))),
                                               multiple = T, selected = '27.4.a')
                            ),
                            column(2,
                                   selectInput("lpuePlSpecies",
                                               "Species:",
                                               c("All",
                                                 unique(as.character(lpue$Species))),
                                               multiple = F, selected = 'COD')
                            ),
                            column(2,
                                   checkboxInput("vpue",
                                                 label = "Convert to value-per-unit-effort", 
                                                 value = FALSE)
                            )
                            
                          ),
                          mainPanel(
                            plotOutput("plotlpue", width = '1200px', height = '800px')
                          )
                 ),
                 tabPanel("Value Plots",
                          downloadButton('downValue', 'Download Plot'),
                          fluidRow(
                            column(2,
                                   selectInput("ValPlCountry",
                                               "Country:",
                                               c("All",
                                                 unique(as.character(Ca$Country))),
                                               multiple = T, selected = 'BEL')
                            ),
                            column(2,
                                   selectInput("ValPlYear",
                                               "Year:",
                                               c("All",
                                                 unique(as.character(Ca$Year))),
                                               multiple = T, selected = 'All')
                            ),
                            column(2,
                                   selectInput("ValPlMetier",
                                               "Metier:",
                                               c("All",
                                                 unique(as.character(Ca$Metier))),
                                               multiple = T, selected = 'All')
                            ),
                            column(2,
                                   selectInput("ValPlArea",
                                               "Area:",
                                               c("All",
                                                 unique(as.character(Ca$Area))),
                                               multiple = T, selected = '27.4.a')
                            ),
                            column(2,
                                   selectInput("ValPlSpecies",
                                               "Species:",
                                               c("All",
                                                 unique(as.character(Ca$Species))),
                                               multiple = T, selected = 'COD')
                            ),
                            column(2,
                                   checkboxInput("Price",
                                                 label = "Convert to Price", 
                                                 value = FALSE)
                            )
                            
                          ),
                          mainPanel(
                            plotOutput("plotVal", width = '1200px', height = '800px')
                          )
                 )
                 )


# Run app
shinyApp(ui = ui, server = server)



