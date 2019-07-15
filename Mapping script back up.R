
############## Mapping ##########################
# Create foundational leaflet map
# and store it as a reactive expression
foundational.map <- reactive({
  if(input$Species_selector == "Cod"){
    Cod_tac <- readOGR("www/Shapefiles","Cod_tac_T")
    Cod_7ek <- readOGR("www/Shapefiles","Cod_7ek_T")
    Add_tac <- readOGR("www/Shapefiles","Add_tac_T")
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                  layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                  attribution = "ICES") %>%
      setView(lng=-14,lat=52,zoom=4) %>% 
      addLegend("bottomleft",col=c('#3d771e','#91d46d','#c773bd'),
                labels = c("Cod TAC area",  "Additional TAC areas","Cod 7ek stock"))%>%
      addPolygons(data=Cod_tac, group="TAC", stroke = FALSE,fill=TRUE,
                  fillColor = '#3d771e', fillOpacity=0.5,
                  popup=paste("<b>Full</b> ",Cod_tac$Area_Full, "<br />",
                              "<b>Area_27:</b> ",Cod_tac$Area_27, "<br />",
                              "<b>Major_FA:</b> ",Cod_tac$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Cod_tac$SubArea, "<br />",
                              "<b>Division:</b> ",Cod_tac$Division, "<br />",
                              "<b>Sub-Division:</b> ",Cod_tac$SubDivisio, "<br />")) %>%
      addPolygons(data=Add_tac, group="TAC", stroke = FALSE,fill=TRUE,
                  fillColor = '#91d46d', fillOpacity=0.8,
                  popup=paste("<b>ICES Code: </b>",Add_tac$ICESCODE, "<br />",
                              "<b>ICES Area:</b> ",Add_tac$IcesArea, "<br />")) %>%
      addPolygons(data=Cod_7ek,  group="Stocks", stroke =TRUE, 
                  fill=TRUE, fillColor = '#c773bd', fillOpacity=0.7,
                  color = "white",dashArray = "3",
                  popup=paste("<b>Stock: </b>7ek", "<br />",
                              "<b>Area: </b> ",Cod_7ek$Area_Full, "<br />",
                              "<b>Major_FA:</b> ",Cod_7ek$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Cod_7ek$SubArea, "<br />",
                              "<b>Division:</b> ",Cod_7ek$Division, "<br />"),
                  highlight = highlightOptions(weight = 5,
                                               bringToFront = TRUE)) %>%
      addLayersControl(baseGroups = c("Esri.OceanBasemap", "ICES Areas"),
                       overlayGroups = c("TAC","Stocks"),  
                       options = layersControlOptions(collapsed = FALSE)) 
  }else if(input$Species_selector == "Sole"){
    Sol_tac <- readOGR("www/Shapefiles","Sol_tac_T")
    Sol_7e <- readOGR("www/Shapefiles","Sol_7e_T")
    Sol_7fg <- readOGR("www/Shapefiles","Sol_7fg_T")
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
                  popup=paste("<b>Stock: </b>7fg", "<br />",
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
  }else if(input$Species_selector == "Haddock"){
    Had_tac <- readOGR("www/Shapefiles","Had_tac_T")
    Had_7bk <- readOGR("www/Shapefiles","Had_7bk_T")
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                  layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                  attribution = "ICES") %>%
      setView(lng=-14,lat=52,zoom=5) %>% 
      addLegend("bottomleft",col=c('#3d771e','#c773bd'),
                labels = c("Haddock TAC area","Haddock 7b-k stock"))%>%
      addPolygons(data=Had_tac, group="TAC", stroke = FALSE,fill=TRUE,
                  fillColor = '#3d771e', fillOpacity=0.4,
                  popup=paste("<b>Full name:</b> ",Had_tac$Area_Full, "<br />",
                              "<b>Area_27:</b> ",Had_tac$Area_27, "<br />",
                              "<b>Major_FA:</b> ",Had_tac$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Had_tac$SubArea, "<br />",
                              "<b>Division:</b> ",Had_tac$Division, "<br />",
                              "<b>Sub-Division:</b> ",Had_tac$SubDivisio, "<br />")) %>%
      addPolygons(data=Had_7bk,  group="Stocks", stroke =TRUE, weight=1,
                  fill=TRUE, fillColor = '#c773bd', fillOpacity=0.7,
                  color = "white",dashArray = "3",
                  popup=paste("<b>ICES Code: </b>",Had_7bk$ICESCODE, "<br />",
                              "<b>ICES Name: </b> ",Had_7bk$ICESNAM, "<br />",
                              "<b>ICES Area:</b> ",Had_7bk$IcesArea, "<br />"),
                  highlight = highlightOptions(weight = 5,
                                               bringToFront = TRUE)) %>%
      addLayersControl(baseGroups = c("Esri.OceanBasemap"),
                       overlayGroups = c("Stocks","TAC"),  
                       options = layersControlOptions(collapsed = FALSE))
  }else if(input$Species_selector == "Whiting"){
    Whg_tac <- readOGR("www/Shapefiles","Whg_tac_T")
    Whg_7bcek <- readOGR("www/Shapefiles","Whg_7bcek_T")
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                  layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                  attribution = "ICES") %>%
      setView(lng=-14,lat=52,zoom=5) %>% 
      addLegend("bottomleft",col=c('#3d771e','#c773bd','#590948'),
                labels = c("Whiting TAC area","Whiting 7bcek stock"))%>%
      addPolygons(data=Whg_tac, group="TAC", stroke = FALSE,fill=TRUE,
                  fillColor = '#3d771e', fillOpacity=0.4,
                  popup=paste("<b>Full name:</b> ",Whg_tac$Area_Full, "<br />",
                              "<b>Area_27:</b> ",Whg_tac$Area_27, "<br />",
                              "<b>Major_FA:</b> ",Whg_tac$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Whg_tac$SubArea, "<br />",
                              "<b>Division:</b> ",Whg_tac$Division, "<br />",
                              "<b>Sub-Division:</b> ",Whg_tac$SubDivisio, "<br />")) %>%
      addPolygons(data=Whg_7bcek,  group="Stocks", stroke =TRUE, weight=1,
                  fill=TRUE, fillColor = '#590948', fillOpacity=0.8,
                  color = "white",dashArray = "3",
                  popup=paste("<b>ICES Code: </b>",Whg_7bcek$ICESCODE, "<br />",
                              "<b>ICES Name: </b> ",Whg_7bcek$ICESNAM, "<br />",
                              "<b>ICES Area:</b> ",Whg_7bcek$IcesArea, "<br />"),
                  highlight = highlightOptions(weight = 5,
                                               bringToFront = TRUE)) %>%
      addLayersControl(baseGroups = c("Esri.OceanBasemap"),
                       overlayGroups = c("Stocks","TAC"),  
                       options = layersControlOptions(collapsed = FALSE))
  }else if(input$Species_selector == "Hake"){
    Hke_tac <- readOGR("www/Shapefiles","Hke_tac_T")
    Hke_3a46 <- readOGR("www/Shapefiles","Hke_3a46_T")
    Hke_7 <- readOGR("www/Shapefiles","Hke_7_T")
    Hke_8abd <- readOGR("www/Shapefiles","Hke_8abd_T")
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                  layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                  attribution = "ICES") %>%
      setView(lng=-14,lat=52,zoom=5) %>% 
      addLegend("bottomleft",col=c('#3d771e','#c773bd','#590948','#CB8B84'),
                labels = c("Hake TAC area","Hake 3a46 stock", "Hake 7 stock","Hake 8abd stock"))%>%
      addPolygons(data=Hke_tac, group="TAC", stroke = FALSE,fill=TRUE,
                  fillColor = '#3d771e', fillOpacity=0.4,
                  popup=paste("<b>ICES Code: </b>",Hke_tac$ICESCODE, "<br />",
                              "<b>ICES Name: </b> ",Hke_tac$ICESNAM, "<br />",
                              "<b>ICES Area:</b> ",Hke_tac$IcesArea, "<br />"),
                  highlight = highlightOptions(weight = 5,
                                               bringToFront = TRUE)) %>%
      addPolygons(data=Hke_3a46,  group="Stocks", stroke =TRUE, weight=1,
                  fill=TRUE, fillColor = '#c773bd', fillOpacity=0.5,
                  color = "white",dashArray = "3",
                  popup=paste("<b>Stock: </b>3a46", "<br />",
                              "<b>Area: </b> ",Hke_3a46$Area_Full, "<br />",
                              "<b>Major_FA:</b> ",Hke_3a46$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Hke_3a46$SubArea, "<br />",
                              "<b>Division:</b> ",Hke_3a46$Division, "<br />"),
                  highlight = highlightOptions(weight = 5,
                                               bringToFront = TRUE)) %>%
      addPolygons(data=Hke_7,  group="Stocks", stroke =TRUE, weight=1,
                  fill=TRUE, fillColor = '#590948', fillOpacity=0.5,
                  color = "white",dashArray = "3",
                  popup=paste("<b>Stock: </b>7", "<br />",
                              "<b>Area: </b> ",Hke_7$Area_Full, "<br />",
                              "<b>Area: </b> ",Hke_7$Area_Full, "<br />",
                              "<b>Major_FA:</b> ",Hke_7$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Hke_7$SubArea, "<br />",
                              "<b>Division:</b> ",Hke_7$Division, "<br />"),
                  highlight = highlightOptions(weight = 5,
                                               bringToFront = TRUE)) %>%
      addPolygons(data=Hke_8abd,  group="Stocks", stroke =TRUE, weight=1,
                  fill=TRUE, fillColor = '#CB8B84', fillOpacity=0.3,
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
                       overlayGroups = c("Stocks","TAC"),  
                       options = layersControlOptions(collapsed = FALSE))
  }else if(input$Species_selector == "Megrim"){
    Meg_tac <- readOGR("www/Shapefiles","Meg_tac_T")
    Meg_7bk <- readOGR("www/Shapefiles","Meg_7bk_T")
    Meg_8abd <- readOGR("www/Shapefiles","Meg_8abd_T")
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                  layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                  attribution = "ICES") %>%
      setView(lng=-14,lat=52,zoom=5) %>% 
      addLegend("bottomleft",col=c('#3d771e','#c773bd','#590948'),
                labels = c("Megrim TAC area", "Megrim 7bk stock","Megrim 8abd stock"))%>%
      addPolygons(data=Meg_tac, group="TAC", stroke = FALSE,fill=TRUE,
                  fillColor = '#3d771e', fillOpacity=0.4,
                  popup=paste("<b>Full name:</b> ",Meg_tac$Area_Full, "<br />",
                              "<b>Area_27:</b> ",Meg_tac$Area_27, "<br />",
                              "<b>Major_FA:</b> ",Meg_tac$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Meg_tac$SubArea, "<br />",
                              "<b>Division:</b> ",Meg_tac$Division, "<br />",
                              "<b>Sub-Division:</b> ",Meg_tac$SubDivisio, "<br />")) %>%
      addPolygons(data=Meg_7bk,  group="Stocks", stroke =TRUE, weight=1,
                  fill=TRUE, fillColor = '#c773bd', fillOpacity=0.7,
                  color = "white",dashArray = "3",
                  popup=paste("<b>Stock: </b>7bk", "<br />",
                              "<b>Area: </b> ",Meg_7bk$Area_Full, "<br />",
                              "<b>Major_FA:</b> ",Meg_7bk$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Meg_7bk$SubArea, "<br />",
                              "<b>Division:</b> ",Meg_7bk$Division, "<br />"),
                  highlight = highlightOptions(weight = 5,
                                               bringToFront = TRUE)) %>%
      addPolygons(data=Meg_8abd,  group="Stocks", stroke =TRUE, weight=1,
                  fill=TRUE, fillColor = '#590948', fillOpacity=0.7,
                  color = "white",dashArray = "3",
                  popup=paste("<b>Stock: </b>8abd", "<br />",
                              "<b>Area: </b> ",Meg_8abd$Area_Full, "<br />",
                              "<b>Area: </b> ",Meg_8abd$Area_Full, "<br />",
                              "<b>Major_FA:</b> ",Meg_8abd$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Meg_8abd$SubArea, "<br />",
                              "<b>Division:</b> ",Meg_8abd$Division, "<br />"),
                  highlight = highlightOptions(weight = 5,
                                               bringToFront = TRUE)) %>%
      addLayersControl(baseGroups = c("Esri.OceanBasemap"),
                       overlayGroups = c("Stocks","TAC"),  
                       options = layersControlOptions(collapsed = FALSE))
  }else if(input$Species_selector == "Anglerfish/Monkfish"){
    Mon_tac <- readOGR("www/Shapefiles","Mon_tac_T")
    Mon_7bk <- readOGR("www/Shapefiles","Mon_7bk_T")
    Mon_8abd <- readOGR("www/Shapefiles","Mon_8abd_T")
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                  layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                  attribution = "ICES") %>%
      setView(lng=-14,lat=52,zoom=5) %>% 
      addLegend("bottomleft",col=c('#3d771e','#c773bd','#590948'),
                labels = c("Monk/Angler TAC area", "Monk/Angler 7bk stock","Monk/Angler 8abd stock"))%>%
      addPolygons(data=Mon_tac, group="TAC", stroke = FALSE,fill=TRUE,
                  fillColor = '#3d771e', fillOpacity=0.4,
                  popup=paste("<b>Full name:</b> ",Mon_tac$Area_Full, "<br />",
                              "<b>Area_27:</b> ",Mon_tac$Area_27, "<br />",
                              "<b>Major_FA:</b> ",Mon_tac$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Mon_tac$SubArea, "<br />",
                              "<b>Division:</b> ",Mon_tac$Division, "<br />",
                              "<b>Sub-Division:</b> ",Mon_tac$SubDivisio, "<br />")) %>%
      addPolygons(data=Mon_7bk,  group="Stocks", stroke =TRUE, weight=1,
                  fill=TRUE, fillColor = '#c773bd', fillOpacity=0.7,
                  color = "white",dashArray = "3",
                  popup=paste("<b>Stock: </b>7bk", "<br />",
                              "<b>Area: </b> ",Mon_7bk$Area_Full, "<br />",
                              "<b>Major_FA:</b> ",Mon_7bk$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Mon_7bk$SubArea, "<br />",
                              "<b>Division:</b> ",Mon_7bk$Division, "<br />"),
                  highlight = highlightOptions(weight = 5,
                                               bringToFront = TRUE)) %>%
      addPolygons(data=Mon_8abd,  group="Stocks", stroke =TRUE, weight=1,
                  fill=TRUE, fillColor = '#590948', fillOpacity=0.7,
                  color = "white",dashArray = "3",
                  popup=paste("<b>Stock: </b>8abd", "<br />",
                              "<b>Area: </b> ",Mon_8abd$Area_Full, "<br />",
                              "<b>Area: </b> ",Mon_8abd$Area_Full, "<br />",
                              "<b>Major_FA:</b> ",Mon_8abd$Major_FA, "<br />",
                              "<b>Sub-Area:</b> ",Mon_8abd$SubArea, "<br />",
                              "<b>Division:</b> ",Mon_8abd$Division, "<br />"),
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
}
)
