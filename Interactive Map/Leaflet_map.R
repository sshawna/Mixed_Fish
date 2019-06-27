library(leaflet)
library(dplyr)
library(rgdal)
#library(raster)
#paletteLayers <- colorBin(palette = "RdBu", domain = c(minPct, maxPct),
               #           bins = c(0, .4, .45, .5, .55, .6, 1) , pretty=FALSE)
#colorFactor(palette, domain, levels = NULL, ordered = FALSE,
            #na.color = "#808080", alpha = FALSE, reverse = FALSE)

foundational.map <- reactive({
  if(input$Species_selector == "Please select"){
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                  layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                  attribution = "ICES") %>%
      addLegend("bottomleft",col=c('#C70039','#C70039','#FFC300'), 
                labels = c("TAC", "Additional TAC areas", "Stock")) %>%
      setView(lng=-14,lat=52,zoom=3)
    
  }else if(input$Species_selector == "Cod"){
    tac <- readOGR("H:/TCM mapping/Shapefiles","Cod_tac_T")
    Cod_7ek <- readOGR("H:/TCM mapping/Shapefiles","Cod_7ek_T")
    Add_tac <- readOGR("H:/TCM mapping/Shapefiles","Add_tac_T")

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
  }else if(input$Species_selector == "Hake"){
    Hke_tac <- readOGR("H:/TCM mapping/Shapefiles","Hke_tac_T")
    Hke_3a46 <- readOGR("H:/TCM mapping/Shapefiles","Hke_3a46_T")
    Hke_7 <- readOGR("H:/TCM mapping/Shapefiles","Hke_7_T")
    Hke_8abd <- readOGR("H:/TCM mapping/Shapefiles","Hke_8abd_T")
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                  layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                  attribution = "ICES") %>%
      setView(lng=-14,lat=52,zoom=3) %>% 
      addLegend("bottomleft",col=c('#3d771e','#3c2e51','#800080','#000022'),
                labels = c("Hake TAC area","Hake 7 stock","Hake 3a46 stock","Hake 8abd stock"))%>%
      addPolygons(data=Hke_tac, group="TAC", stroke = FALSE,fill=TRUE,
                  fillColor = '#3d771e', fillOpacity=0.5,
                  popup=paste("<b>Full</b> ",Hke_tac$Area_Full, "<br />",
                              "<b>Area_27:</b> ",Hke_tac$Area_27)) %>%
      addPolygons(data=Hke_7,  group="Stocks", stroke =TRUE, weight=1,
                  popup=paste("<b>Stock: </b>7", "<br />",
                              "<b>Area: </b> ",Hke_7$Area_Full, "<br />"),fill=TRUE,
                  fillColor = '#3c2e51', fillOpacity=0.6,
                  highlight = highlightOptions(weight = 10,
                                               color = "blue",
                                               bringToFront = TRUE)) %>%
      addPolygons(data=Hke_3a46,  group="Stocks", stroke =TRUE, weight=1,
                  popup=paste("<b>Stock: </b>3a46", "<br />",
                              "<b>Area: </b> ",Hke_3a46$Area_Full, "<br />"),fill=TRUE,
                  fillColor = '#800080', fillOpacity=0.6,
                  highlight = highlightOptions(weight = 10,
                                               color = "blue",
                                               bringToFront = TRUE)) %>%
      addPolygons(data=Hke_8abd,  group="Stocks", stroke =TRUE, weight=1,
                  popup=paste("<b>Stock: </b>8abd", "<br />",
                              "<b>Area: </b> ",Hke_8abd$Area_Full, "<br />"),fill=TRUE,
                  fillColor = '#000022', fillOpacity=0.6,
                  highlight = highlightOptions(weight = 10,
                                               color = "blue",
                                               bringToFront = TRUE)) %>%
      addLayersControl(baseGroups = c("Esri.OceanBasemap", "ICES Areas"),
                       overlayGroups = c("TAC","Stocks"),  
                       options = layersControlOptions(collapsed = FALSE))
  }else if(input$Species_selector == "Monkfish"){
    Mon_tac <- readOGR("H:/TCM mapping/Shapefiles","Mon_tac_T")
    Mon_7bk <- readOGR("H:/TCM mapping/Shapefiles","Mon_7bk_T")
    Mon_8abd <- readOGR("H:/TCM mapping/Shapefiles","Mon_8abd_T")
   # pal <- colorFactor("YlOrRd", domain = c(Mon_7bk,Mon_8abd))
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                  layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                  attribution = "ICES") %>%
      setView(lng=-14,lat=52,zoom=5) %>% 
      addLegend("bottomleft",col=c('#3d771e','#c773bd','#590948'),
                labels = c("Monkfish TAC area","Monkfish 7bk stock","Monkfish 8abd stock"))%>%
      addPolygons(data=Mon_tac, group="TAC", stroke = FALSE,fill=TRUE,
                  fillColor = '#3d771e', fillOpacity=0.4,
                  popup=paste("<b>Full name:</b> ",Mon_tac$Area_Full, "<br />",
                              "<b>Area_27:</b> ",Mon_tac$Area_27, "<br />",
                              "<b>Major_FA:</b> ",Mon_tac$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Mon_tac$SubArea, "<br />",
                              "<b>Division:</b> ",Mon_tac$Division, "<br />",
                              "<b>Sub-Division:</b> ",Mon_tac$SubDivisio, "<br />")) %>%
      addPolygons(data=Mon_7bk,  group="Stocks", stroke =TRUE, weight=1,
                  fill=TRUE, fillColor = '#c773bd', fillOpacity=0.6,
                  color = "white",dashArray = "3",
                  popup=paste("<b>Stock: </b>7bk", "<br />",
                              "<b>Area: </b> ",Mon_7bk$Area_Full, "<br />",
                              "<b>Major_FA:</b> ",Mon_7bk$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Mon_7bk$SubArea, "<br />",
                              "<b>Division:</b> ",Mon_7bk$Division, "<br />"),
                  highlight = highlightOptions(weight = 5,
                                               bringToFront = TRUE)) %>%
      addPolygons(data=Hke_8abd,  group="Stocks", stroke =TRUE, weight=1,
                  fill=TRUE, fillColor = '#590948', fillOpacity=0.6,
                  color = "white",dashArray = "3",
                  popup=paste("<b>Stock: </b>8abd", "<br />",
                              "<b>Area: </b> ",Hke_8abd$Area_Full, "<br />",
                              "<b>Area: </b> ",Hke_8abd$Area_Full, "<br />",
                              "<b>Major_FA:</b> ",Hke_8abd$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Hke_8abd$SubArea, "<br />",
                              "<b>Division:</b> ",Hke_8abd$Division, "<br />"),
                  highlight = highlightOptions(weight = 5,
                                               bringToFront = TRUE)) %>%
      addLayersControl(baseGroups = c("Esri.OceanBasemap"),
                       overlayGroups = c("ICES Areas","TAC","Stocks"),  
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

leafletProxy("map", data = mapdata1()) %>% 
# clearShapes() %>% 
# addCircles(~Longitude,~Latitude, radius= radius, layerId=~PrimeStation,                  
#stroke=FALSE,fillOpacity=0.7, fillColor=pal(colorData)) %>% 
#addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
#layerId="colorLegend")
 

if(input$Stock_selector == "Please select"){
  
}else if(input$Stock_selector != "Please select"){
  leafletProxy("map",session) %>%
    addPolygons(data=
                paste(input$Species_selector, "_tac_T", sep=""),
              col='red',fillOpacity = 0.2) #%>%
  
}