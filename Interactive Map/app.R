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
library(leaflet)
library(raster)
library(rgdal)

options(scipen=999)

species <- c("Please select", "Cod","Haddock","Whiting","Hake", "Megrim", "Anglerfish/Monkfish","Sole")
stocks <- read.csv("\\Outputs\\stocks_SS.csv")

### server ###
server <- function(input, output, session) {

#stocks_list <- list('cod-7e-k','had-7b-k','hke-nrtn','meg-ivvi','meg-rock','mgw-78','whg-7b-k')
stocks_list <- levels(stocks$stock)


  
#create dropdown to select stock
updateSelectizeInput(session, 'Stockselector',
                     choices = list(stocks_list),
                     server = TRUE,
                     selected =1)
##### Interactive Map #####


# Create foundational leaflet map
# and store it as a reactive expression

foundational.map <- reactive({
  if(input$Species_selector == "Cod"){
    Cod_tac <- readOGR("/Shapefiles","Cod_tac_T")
    Cod_7ek <- readOGR("/Shapefiles","Cod_7ek_T")
    Add_tac <- readOGR("/Shapefiles","Add_tac_T")
    
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                  layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                  attribution = "ICES") %>%
      setView(lng=-14,lat=52,zoom=3) %>% 
      addLegend("bottomleft",col=c('#52BE80','#52BE80','#FF5733'),
                labels = c("Cod TAC area",  "Additional TAC areas","Cod 7ek stock"))%>%
      addPolygons(data=Cod_tac, group="TAC", stroke = FALSE,fill=TRUE,
                  fillColor = '#52BE80', fillOpacity=0.5,
                  popup=paste("<b>Full</b> ",Cod_tac$Area_Full, "<br />",
                              "<b>Area_27:</b> ",Cod_tac$Area_27)) %>%
      addPolygons(data=Cod_7ek,  group="Stocks", stroke =TRUE, weight=1,
                  popup=paste("<b>Stock: </b>7e-k", "<br />",
                              "<b>Area: </b> ",Cod_7ek$Area_Full, "<br />"),fill=TRUE,
                  fillColor = '#FF5733', fillOpacity=0.6,
                  highlight = highlightOptions(weight = 10,
                                               color = "blue",
                                               bringToFront = TRUE)) %>%
      addPolygons(data=Add_tac, group="TAC", stroke = FALSE, fill=TRUE,
                  fillColor = '#52BE80', fillOpacity=0.5,
                  popup=paste("<b>Full</b> ",Add_tac$Area_Full, "<br />",
                              "<b>Area_27:</b> ",Add_tac$Area_27))%>%
      addLayersControl(baseGroups = c("Esri.OceanBasemap", "ICES Areas"),
                       overlayGroups = c("TAC","Stocks"),  
                       options = layersControlOptions(collapsed = FALSE)) 
  }else if(input$Species_selector == "Sole"){
      Sol_tac <- readOGR("H:/TCM mapping/Shapefiles","Sol_tac_T")
      Sol_7e <- readOGR("H:/TCM mapping/Shapefiles","Sol_7e_T")
      Sol_7fg <- readOGR("H:/TCM mapping/Shapefiles","Sol_7fg_T")
      # pal <- colorFactor("YlOrRd", domain = c(Mon_7bk,Mon_8abd))
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>% 
        addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                    layers = "0",
                    options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                    attribution = "ICES") %>%
        setView(lng=-14,lat=52,zoom=5) %>% 
        addLegend("bottomleft",col=c('#3d771e','#c773bd','#590948'),
                  labels = c("Sole TAC area","Sole 7bk stock","Sole 8abd stock"))%>%
        addPolygons(data=Sol_tac, group="TAC", stroke = FALSE,fill=TRUE,
                    fillColor = '#3d771e', fillOpacity=0.4,
                    popup=paste("<b>Full name:</b> ",Sol_tac$Area_Full, "<br />",
                                "<b>Area_27:</b> ",Sol_tac$Area_27, "<br />",
                                "<b>Major_FA:</b> ",Sol_tac$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Sol_tac$SubArea, "<br />",
                                "<b>Division:</b> ",Sol_tac$Division, "<br />",
                                "<b>Sub-Division:</b> ",Sol_tac$SubDivisio, "<br />")) %>%
        addPolygons(data=Sol_7e,  group="Stocks", stroke =TRUE, weight=1,
                    fill=TRUE, fillColor = '#c773bd', fillOpacity=0.7,
                    color = "white",dashArray = "3",
                    popup=paste("<b>Stock: </b>7e", "<br />",
                                "<b>Area: </b> ",Sol_7e$Area_Full, "<br />",
                                "<b>Major_FA:</b> ",Sol_7e$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Sol_7e$SubArea, "<br />",
                                "<b>Division:</b> ",Sol_7e$Division, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addPolygons(data=Sol_7fg,  group="Stocks", stroke =TRUE, weight=1,
                    fill=TRUE, fillColor = '#590948', fillOpacity=0.7,
                    color = "white",dashArray = "3",
                    popup=paste("<b>Stock: </b>8abd", "<br />",
                                "<b>Area: </b> ",Sol_7fg$Area_Full, "<br />",
                                "<b>Area: </b> ",Sol_7fg$Area_Full, "<br />",
                                "<b>Major_FA:</b> ",Sol_7fg$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Sol_7fg$SubArea, "<br />",
                                "<b>Division:</b> ",Sol_7fg$Division, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addLayersControl(baseGroups = c("Esri.OceanBasemap"),
                         overlayGroups = c("Stocks","TAC"),  
                         options = layersControlOptions(collapsed = FALSE))
    }
}) # end of foundational.map()


# render foundational leaflet map
output$map <- leaflet::renderLeaflet({
  #if(input$Species_selector == "Please select"){
   # leaflet() %>%
    #  addProviderTiles(providers$Esri.OceanBasemap) %>% 
    #  addPolylines(color = "grey",data= div, group = "ICES Sub-Areas", weight = 3)%>%
    #  addPolylines(color = "darkgrey",data= cont, group = "ICES Sub-Areas", weight = 3)#%>%
    
  #}else if(input$Species_selector != "Please select"){
  # call reactive map
  foundational.map()
  #}
}) # end of render leaflet

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
                        fluidRow(column(width=5, 
                                        selectInput("Species_selector","Select Species", choices=c(species)),
                                        leafletOutput("map"))#, width='100%', height='100%'))
                                 #column(width=5,offset=1,
                                        #selectizeInput(inputId = "Stockselector", label="Select Stock",
                                                      # choices=NULL, multiple=FALSE),
                                        #plotOutput('Stockoverlap'))
                                 ),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br()
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
