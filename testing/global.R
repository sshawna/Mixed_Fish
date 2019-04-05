
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(maps)


CelticEcoSpecies<-read.csv("CelticEcoSpecies.csv")
test<-aggregate(CelticEcoSpecies$OfficialLanW,by=list(CelticEcoSpecies$Year,CelticEcoSpecies$FishActEUivl5,CelticEcoSpecies$Species),FUN="sum")
names(test)<-c("Year","Metier_lvl5","Species","OfficialLanW")