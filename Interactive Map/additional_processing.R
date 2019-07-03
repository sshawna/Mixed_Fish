#Additional processing
#lid=as.vector(row.names(Cod_tac@data))
library(maptools)
library(sf)
library(sp)
library(rgdal)
library(rmapshaper)
crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#states <-readShapePoly('C:/Users/ssanfey/Downloads/statesp020.shp')
str(Had_tac@data)
str(Had_tac@polygons[[1]])
Had_tac@bbox
Had_tac@proj4string
#How to transform shapefile
crsmerc=CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
states_transformed<-spTransform(states,CRS=crsmerc)
#specify CRS 
proj4string(Had_tac) <- CRS("+init=epsg:3857")
#CRS strings obtained from http://spatialreference.org/ 
#and http://prj2epsg.org/epsg/3857
#known issue of CRS info/proj4string being ignored by leaflet (readShapePoly and readOGR (sp and rgdal) both have this problem)
#current suggested workaround is to use spTransform/st_trasform from the package sf


#load in TAC shp
Cod_tac <- readShapePoly("H:/TCM mapping/Shapefiles/Cod_TAC.shp")
 # SET the CRS of the object
Cod_tac@proj4string <- CRS("+init=epsg:3857")
proj4string(Cod_tac_trans)
#crsall <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
#Cod_tac <- spTransform(Cod_tac, CRS(crsall))
#transform
Cod_tac_trans <- spTransform(Cod_tac, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
st_crs(Cod_tac_trans)


Had_tac <- readShapePoly("H:/TCM mapping/Shapefiles/Had_TAC.shp")
Had_tac <- st_as_sf(Had_tac)
Had_tac <- sf::st_transform(Had_tac, CRS("+init=epsg:4326"))

Whg_tac <- readOGR("H:/TCM mapping/Shapefiles","Whg_TAC")
Whg_tac <- sf::st_transform(Whg_tac, CRS("+init=epsg:4326"))

Hke_tac <- readOGR("H:/TCM mapping/Shapefiles","Hke_TAC")
st_crs(Hke_tac)
Hke_tac_trans <- spTransform(Hke_tac, CRS("+init=epsg:4326"))
st_crs(Hke_tac_trans)
plot(Hke_tac_trans)
leaflet() %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolylines(color = "grey",data= div, group = "ICES Sub-Areas", weight = 3)%>%
  addPolylines(color = "darkgrey",data= cont, group = "ICES Sub-Areas", weight = 3)%>%
  addPolygons(data=Hke_tac_trans)
Meg_tac <- readOGR("H:/TCM mapping/Shapefiles","Meg_TAC")
Mon_tac <- readOGR("H:/TCM mapping/Shapefiles","Mon_TAC")
Sol_tac <- readOGR("H:/TCM mapping/Shapefiles","Sol_TAC")


### alt mapping ####

Meg_tac <- readOGR("H:/TCM mapping/Shapefiles","Meg_TAC")
library(raster)
library(leaflet)
Meg_7bk <- shapefile("H:/TCM mapping/Shapefiles/Meg_7bk.shp")
plot(Meg_7bk)
leaflet(Meg_7bk) %>% addTiles()
bbox(Meg_7bk)
projection(Meg_7bk)="+init=epsg:3857"
#then transform to lat/long and check where it is
Meg_7bk_T <- spTransform(Meg_7bk,"+init=epsg:4326")
bbox(Meg_7bk_T)
writeOGR(Meg_7bk_T, ".", "Meg_7bk_T", driver="ESRI Shapefile")

##Works but wrong projection!!###
epsg4326 <- leafletCRS(
  crsClass = "L.CRS.EPSG4326",
  code = "EPSG:7030",
  proj4def = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

###Works!!###
library(dplyr)
library(raster)
library(leaflet)
Meg_7bk <- shapefile("H:/TCM mapping/Shapefiles/Meg_7bk.shp")
plot(Meg_7bk)
leaflet(Meg_7bk) %>% addTiles()
bbox(Meg_7bk)
projection(Meg_7bk)="+init=epsg:3857"
#then transform to lat/long and check where it is
Meg_7bk_T <- spTransform(Meg_7bk,"+init=epsg:4326")
bbox(Meg_7bk_T)
writeOGR(Meg_7bk_T, ".", "Meg_7bk_T", driver="ESRI Shapefile")

leaflet(Meg_7bk_T) %>% #, options = leafletOptions(crs = epsg4326)
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Meg_7bk_T,weight = 1, color = "#444444", opacity = 1)

Cod_tac <- shapefile("H:/TCM mapping/Shapefiles/Cod_TAC.shp")
plot(Cod_tac)
bbox(Cod_tac)
projection(Cod_tac)="+init=epsg:3857"
Cod_tac_T <- spTransform(Cod_tac, "+init=epsg:4326")
leaflet(Cod_tac_T) %>% #, options = leafletOptions(crs = epsg4326)
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Cod_tac_T,weight = 1, color = "#444444", opacity = 1)
bbox(Cod_tac_T)
writeOGR(Cod_tac_T, ".", "Cod_tac_T", driver="ESRI Shapefile")

Had_tac <- shapefile("H:/TCM mapping/Shapefiles/Had_TAC.shp")
projection(Had_tac)="+init=epsg:3857"
Had_tac_T <- spTransform(Had_tac, "+init=epsg:4326")
leaflet(Had_tac_T) %>% #, options = leafletOptions(crs = epsg4326)
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Had_tac_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Had_tac_T, ".", "Had_tac_T", driver="ESRI Shapefile")

Whg_tac <- shapefile("H:/TCM mapping/Shapefiles/Whg_TAC.shp")
projection(Whg_tac)="+init=epsg:3857"
Whg_tac_T <- spTransform(Whg_tac, "+init=epsg:4326")
leaflet(Whg_tac_T) %>% #, options = leafletOptions(crs = epsg4326)
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Whg_tac_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Whg_tac_T, ".", "Whg_tac_T", driver="ESRI Shapefile")

Mon_tac <- shapefile("H:/TCM mapping/Shapefiles/Mon_TAC.shp")
projection(Mon_tac)="+init=epsg:3857"
Mon_tac_T <- spTransform(Mon_tac, "+init=epsg:4326")
leaflet(Mon_tac_T) %>% #, options = leafletOptions(crs = epsg4326)
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Mon_tac_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Mon_tac_T, ".", "Mon_tac_T", driver="ESRI Shapefile")

Sol_tac <- shapefile("H:/TCM mapping/Shapefiles/Sol_TAC.shp")
projection(Sol_tac)="+init=epsg:3857"
Sol_tac_T <- spTransform(Sol_tac, "+init=epsg:4326")
leaflet(Sol_tac_T) %>% #, options = leafletOptions(crs = epsg4326)
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Sol_tac_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Sol_tac_T, ".", "Sol_tac_T", driver="ESRI Shapefile")

Meg_tac <- readOGR("H:/TCM mapping/Shapefiles","Meg_TAC")
projection(Meg_tac)="+init=epsg:3857"
Meg_tac_T <- spTransform(Meg_tac, "+init=epsg:4326")
leaflet(Meg_tac_T) %>% #, options = leafletOptions(crs = epsg4326)
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Meg_tac_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Meg_tac_T, ".", "Meg_tac_T", driver="ESRI Shapefile")

Add_tac <- shapefile("H:/TCM mapping/Shapefiles/Additional_TAC_areas.shp")
bbox(Add_tac)
projection(Add_tac)="+init=epsg:3857"
Add_tac_T <- Add_tac
bbox(Add_tac_T)
leaflet(Add_tac_T) %>% #, options = leafletOptions(crs = epsg4326)
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Add_tac_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Add_tac_T, ".", "Add_tac_T", driver="ESRI Shapefile")

Hke_tac <- shapefile("H:/TCM mapping/Shapefiles/Hke_TAC.shp")
bbox(Hke_tac)
projection(Hke_tac)="+init=epsg:4326"
Hke_tac_T <- Hke_tac
leaflet(Hke_tac_T) %>% #, options = leafletOptions(crs = epsg4326)
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Hke_tac_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Hke_tac_T, ".", "Hke_tac_T", driver="ESRI Shapefile")



#### Stocks Adding projection info and transforming for leaflet #####
Cod_7ek <- shapefile("H:/TCM mapping/Shapefiles/Cod_7ek_newt.shp")
bbox(Cod_7ek)
projection(Cod_7ek)="+init=epsg:3857"
Cod_7ek_T <- spTransform(Cod_7ek, "+init=epsg:4326")
leaflet(Cod_7ek_T) %>% #, options = leafletOptions(crs = epsg4326)
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Cod_7ek_T,weight = 1, color = "#444444", opacity = 1)
plot(Cod_7ek_T)
writeOGR(Cod_7ek_T, ".", "Cod_7ek_T2", driver="ESRI Shapefile")
Cod_7ek_T <-  shapefile("H:/TCM mapping/Shapefiles/Cod_7ek_T.shp")
bbox(Cod_7ek_T)

Had_7bk <- shapefile("H:/TCM mapping/Shapefiles/Had_7bk.shp")
bbox(Had_7bk)
projection(Had_7bk)="+init=epsg:3857"
Had_7bk_T <- Had_7bk
leaflet(Had_7bk_T) %>% #, options = leafletOptions(crs = epsg4326)
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Had_7bk_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Had_7bk_T, ".", "Had_7bk_T", driver="ESRI Shapefile")

Hke_3a46 <- shapefile("H:/TCM mapping/Shapefiles/Hke_3a46.shp")
projection(Hke_3a46)="+init=epsg:3857"
Hke_3a46_T <- spTransform(Hke_3a46, "+init=epsg:4326")
leaflet(Hke_3a46_T) %>% #, options = leafletOptions(crs = epsg4326)
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Hke_3a46_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Hke_3a46_T, ".", "Hke_3a46_T", driver="ESRI Shapefile")

Hke_7<- shapefile("H:/TCM mapping/Shapefiles/Hke_7.shp")
projection(Hke_7)="+init=epsg:3857"
Hke_7_T <- spTransform(Hke_7, "+init=epsg:4326")
leaflet(Hke_7_T) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Hke_7_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Hke_7_T, ".", "Hke_7_T", driver="ESRI Shapefile")

Hke_8abd<- shapefile("H:/TCM mapping/Shapefiles/Hke_8abd.shp")
projection(Hke_8abd)="+init=epsg:3857"
Hke_8abd_T <- spTransform(Hke_8abd, "+init=epsg:4326")
leaflet(Hke_8abd_T) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Hke_8abd_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Hke_8abd_T, ".", "Hke_8abd_T", driver="ESRI Shapefile")

Meg_8abd<- shapefile("H:/TCM mapping/Shapefiles/Meg_8abd.shp")
projection(Meg_8abd)="+init=epsg:3857"
Meg_8abd_T <- spTransform(Meg_8abd, "+init=epsg:4326")
leaflet(Meg_8abd_T) %>% #, options = leafletOptions(crs = epsg4326)
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Meg_8abd_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Meg_8abd_T, ".", "Meg_8abd_T", driver="ESRI Shapefile")

Mon_7bk<- shapefile("H:/TCM mapping/Shapefiles/Mon_7bk.shp")
projection(Mon_7bk)="+init=epsg:3857"
Mon_7bk_T <- spTransform(Mon_7bk, "+init=epsg:4326")
leaflet(Mon_7bk_T) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Mon_7bk_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Mon_7bk_T, ".", "Mon_7bk_T", driver="ESRI Shapefile")

Mon_8abd<- shapefile("H:/TCM mapping/Shapefiles/Mon_8abd.shp")
projection(Mon_8abd)="+init=epsg:3857"
Mon_8abd_T <- spTransform(Mon_8abd, "+init=epsg:4326")
leaflet(Mon_8abd_T) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Mon_8abd_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Mon_8abd_T, ".", "Mon_8abd_T", driver="ESRI Shapefile")

Sol_7e<- shapefile("H:/TCM mapping/Shapefiles/Sol_7e.shp")
projection(Sol_7e)="+init=epsg:3857"
Sol_7e_T <- spTransform(Sol_7e, "+init=epsg:4326")
leaflet(Sol_7e_T) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Sol_7e_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Sol_7e_T, ".", "Sol_7e_T", driver="ESRI Shapefile")

Sol_7fg<- shapefile("H:/TCM mapping/Shapefiles/Sol_7fg.shp")
projection(Sol_7fg)="+init=epsg:3857"
Sol_7fg_T <- spTransform(Sol_7fg, "+init=epsg:4326")
leaflet(Sol_7fg_T) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Sol_7fg_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Sol_7fg_T, ".", "Sol_7fg_T", driver="ESRI Shapefile")

Whg_7bc<- shapefile("H:/TCM mapping/Shapefiles/Whg_7bc.shp")
projection(Whg_7bc)="+init=epsg:3857"
Whg_7bc_T <- Whg_7bc
leaflet(Whg_7bc_T) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Whg_7bc_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Whg_7bc_T, ".", "Whg_7bc_T", driver="ESRI Shapefile")

Whg_7bcek<- shapefile("H:/TCM mapping/Shapefiles/Whg_7bc_ek.shp")
projection(Whg_7bcek)="+init=epsg:3857"
Whg_7bcek_T <- Whg_7bcek
leaflet(Whg_7bcek_T) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Whg_7bcek_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Whg_7bcek_T, ".", "Whg_7bcek_T", driver="ESRI Shapefile")

##Problem
Whg_7ek<- shapefile("H:/TCM mapping/Shapefiles/Whg_7ek.shp")
projection(Whg_7ek)="+init=epsg:3857"
Whg_7ek_T <- Whg_7ek
leaflet(Whg_7ek_T) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  addPolygons(data = Whg_7ek_T,weight = 1, color = "#444444", opacity = 1)
writeOGR(Whg_7ek_T, ".", "Whg_7ek_T", driver="ESRI Shapefile")

