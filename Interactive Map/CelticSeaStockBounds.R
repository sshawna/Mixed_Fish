library(vmstools)
library(dplyr); library(tidyr);
library(tables); library(reshape2)
library(cowplot)


data(ICESareas)

model <- c("27.7.b", "27.7.c", "27.7.c.1", "27.7.c.2", "27.7.e", "27.7.f", "27.7.g", "27.7.h", "27.7.j", "27.7.j.1", "27.7.j.2", "27.7.k", "27.7.k.1", "27.7.k.2")

## Cod

p1 <- ggplot() +
  geom_polygon(aes(x=long,y=lat,group=group), fill= "blue", data = ICESareas[ICESareas$Area_Full %in%  c("27.7.f","27.7.e","27.7.g","27.7.h","27.7.j", "27.7.j.1","27.7.j.2","27.7.k","27.7.k.1","27.7.k.2"),]) +
  geom_polygon(aes(x=long,y=lat,group=group), fill= "lightblue", data = ICESareas[ICESareas$Area_Full %in%  c("27.7.b","27.7.c","27.7.c.1","27.7.c.2"),]) +
  geom_polygon(aes(x=long,y=lat,group=group),fill="grey",size=0.2,colour="black",data=ICESareas,alpha=0) + 
  annotate("text",x=-3.5,y=49.5,label="7e") + annotate("text",x=-8,y=49,label="7h") +
  annotate("text",x=-11,y=51,label="7j") + annotate("text",x=-5.5,y=50.8,label="7f") +
  annotate("text",x=-8,y=51,label="7g") + annotate("text",x=-14,y=51,label="7k") +
  annotate("text",x=-11,y=53.5,label="7b") + annotate("text",x=-14,y=53.5,label="7c") + 
  theme_bw() + ylim(48,56) + xlim(-18,-2) + ggtitle("Cod stocks")


## Haddock

p2 <- ggplot() +
  geom_polygon(aes(x=long,y=lat,group=group), fill= "blue", data = ICESareas[ICESareas$Area_Full %in%  c("27.7.b","27.7.c","27.7.c.1","27.7.c.2","27.7.d","27.7.f","27.7.e","27.7.g","27.7.h","27.7.j", "27.7.j.1","27.7.j.2","27.7.k","27.7.k.1","27.7.k.2"),]) +
  geom_polygon(aes(x=long,y=lat,group=group),fill="grey",size=0.2,colour="black",data=ICESareas,alpha=0) + 
  annotate("text",x=-3.5,y=49.5,label="7e") + annotate("text",x=-8,y=49,label="7h") +
  annotate("text",x=-11,y=51,label="7j") + annotate("text",x=-5.5,y=50.8,label="7f") +
  annotate("text",x=-8,y=51,label="7g") + annotate("text",x=-14,y=51,label="7k") +
  annotate("text",x=-11,y=53.5,label="7b") + annotate("text",x=-14,y=53.5,label="7c") + 
  theme_bw() + ylim(48,56) + xlim(-18,2) + ggtitle("Haddock stocks")

## Whiting

p3 <- ggplot() +
  geom_polygon(aes(x=long,y=lat,group=group), fill= "green", data = ICESareas[ICESareas$Area_Full %in%  c("27.7.b","27.7.c","27.7.c.1","27.7.c.2","27.7.f","27.7.e","27.7.g","27.7.h","27.7.j", "27.7.j.1","27.7.j.2","27.7.k","27.7.k.1","27.7.k.2"),]) +
  geom_polygon(aes(x=long,y=lat,group=group), fill= "lightgreen", alpha = 0.5, data = ICESareas[ICESareas$Area_Full %in%  c("27.7.d"),]) +
  geom_polygon(aes(x=long,y=lat,group=group),fill="grey",size=0.2,colour="black",data=ICESareas,alpha=0) + 
  annotate("text",x=-3.5,y=49.5,label="7e") + annotate("text",x=-8,y=49,label="7h") +
  annotate("text",x=-11,y=51,label="7j") + annotate("text",x=-5.5,y=50.8,label="7f") +
  annotate("text",x=-8,y=51,label="7g") + annotate("text",x=-14,y=51,label="7k") +
  annotate("text",x=-11,y=53.5,label="7b") + annotate("text",x=-14,y=53.5,label="7c") + 
  theme_bw() + ylim(48,56) + xlim(-18,2) + ggtitle("Whiting stocks")


## Plaice

  p4 <- ggplot() +
    geom_polygon(aes(x=long,y=lat,group=group), fill= "purple", data = ICESareas[ICESareas$Area_Full %in%  c("27.7.g","27.7.f"),]) +
    geom_polygon(aes(x=long,y=lat,group=group), fill= "green", data = ICESareas[ICESareas$Area_Full %in%  c("27.7.e"),]) +
    geom_polygon(aes(x=long,y=lat,group=group), fill= "blue", data = ICESareas[ICESareas$Area_Full %in%  c("27.7.h","27.7.j", "27.7.j.1","27.7.j.2","27.7.k","27.7.k.1","27.7.k.2"),]) +
    geom_polygon(aes(x=long,y=lat,group=group), fill= "grey", data = ICESareas[ICESareas$Area_Full %in%  c("27.7.b", "27.7.c.1","27.7.c.2"),]) +
    geom_polygon(aes(x=long,y=lat,group=group),size=0.2,colour="black",data=ICESareas,alpha=0) + 
    annotate("text",x=-3.5,y=49.5,label="7e") + annotate("text",x=-8,y=49,label="7h") +
  annotate("text",x=-11,y=51,label="7j") + annotate("text",x=-5.5,y=50.8,label="7f") +
  annotate("text",x=-8,y=51,label="7g") + annotate("text",x=-14,y=51,label="7k") +
  annotate("text",x=-11,y=53.5,label="7b") + annotate("text",x=-14,y=53.5,label="7c") + 
     theme_bw() + ylim(48,56) + xlim(-18,1) + ggtitle("Plaice stocks")
  
  ## Sole
  
  p5 <- ggplot() +
    geom_polygon(aes(x=long,y=lat,group=group), fill= "purple", data = ICESareas[ICESareas$Area_Full %in%  c("27.7.g","27.7.f"),]) +
    geom_polygon(aes(x=long,y=lat,group=group), fill= "green", data = ICESareas[ICESareas$Area_Full %in%  c("27.7.e"),]) +
    geom_polygon(aes(x=long,y=lat,group=group), fill= "blue", data = ICESareas[ICESareas$Area_Full %in%  c("27.7.h","27.7.j", "27.7.j.1","27.7.j.2","27.7.k","27.7.k.1","27.7.k.2"),]) +
    geom_polygon(aes(x=long,y=lat,group=group), fill= "grey", data = ICESareas[ICESareas$Area_Full %in%  c("27.7.b", "27.7.c.1","27.7.c.2"),]) +
    geom_polygon(aes(x=long,y=lat,group=group), size=0.2,colour="black",data=ICESareas,alpha=0) + 
    annotate("text",x=-3.5,y=49.5,label="7e") + annotate("text",x=-8,y=49,label="7h") +
    annotate("text",x=-11,y=51,label="7j") + annotate("text",x=-5.5,y=50.8,label="7f") +
    annotate("text",x=-8,y=51,label="7g") + annotate("text",x=-14,y=51,label="7k") +
    annotate("text",x=-11,y=53.5,label="7b") + annotate("text",x=-14,y=53.5,label="7c") + 
    theme_bw() + ylim(48,56) + xlim(-18,2) + ggtitle("Sole stocks")
  
  
  ## Hake
  
  areas <- unique(ICESareas$Area_Full)
  three <- "27.7.3.a"
  four  <- grep("27.4", areas, value = T)
  five  <- grep("27.5", areas, value = T)
  six  <- grep("27.6", areas, value = T)
  seven <- grep("27.7", areas, value = T)
  eight <- c("27.8.a", "27.8.b", "27.8.d")
  
  areas <- c(three, four, five, six, seven, eight)
  
  p6 <- ggplot() +
    geom_polygon(aes(x=long,y=lat,group=group), fill= "purple", data = ICESareas[ICESareas$Area_Full %in%  areas,]) +
    geom_polygon(aes(x=long,y=lat,group=group),fill="grey",size=0.2,colour="black",data=ICESareas,alpha=0) + 
    annotate("text",x=-3.5,y=49.5,label="7e") + annotate("text",x=-8,y=49,label="7h") +
    annotate("text",x=-11,y=51,label="7j") + annotate("text",x=-5.5,y=50.8,label="7f") +
    annotate("text",x=-8,y=51,label="7g") + annotate("text",x=-14,y=51,label="7k") +
    annotate("text",x=-11,y=53.5,label="7b") + annotate("text",x=-14,y=53.5,label="7c") + 
    theme_bw() + ylim(42,62) + xlim(-18,10) + ggtitle("Hake stocks")
  
  p7 <- ggplot() +
    geom_polygon(aes(x=long,y=lat,group=group), fill= "green", 
                 data = ICESareas[ICESareas$Area_Full %in%  c("27.7.b","27.7.c","27.7.c.1","27.7.c.2","27.7.d","27.7.f",
                                                              "27.7.e","27.7.g","27.7.h","27.7.j", "27.7.j.1","27.7.j.2","27.7.k","27.7.k.1","27.7.k.2",
                                                              "27.8.a","27.8.b","27.8.d"),]) +
    geom_polygon(aes(x=long,y=lat,group=group), fill= "green", alpha = 0.5, 
                 data = ICESareas[ICESareas$Area_Full %in%  c("27.7.a"),]) +
    geom_polygon(aes(x=long,y=lat,group=group),fill="grey",size=0.2,colour="black",data=ICESareas,alpha=0) + 
    annotate("text",x=-3.5,y=49.5,label="7e") + annotate("text",x=-8,y=49,label="7h") +
    annotate("text",x=-11,y=51,label="7j") + annotate("text",x=-5.5,y=50.8,label="7f") +
    annotate("text",x=-8,y=51,label="7g") + annotate("text",x=-14,y=51,label="7k") +
    annotate("text",x=-11,y=53.5,label="7b") + annotate("text",x=-14,y=53.5,label="7c") + 
    theme_bw() + ylim(42,56) + xlim(-18,2) + ggtitle("Megrim stocks")
  
  
  p8 <- ggplot() +
    geom_polygon(aes(x=long,y=lat,group=group), fill= "red", data = ICESareas[ICESareas$Area_Full %in%  c("27.7.a","27.7.b","27.7.c","27.7.c.1","27.7.c.2","27.7.d",
                                                                                                          "27.7.f","27.7.e","27.7.g","27.7.h","27.7.j", 
                                                                                                          "27.7.j.1","27.7.j.2","27.7.k","27.7.k.1","27.7.k.2",
                                                                                                          "27.8.a","27.8.b","27.8.d"),]) +
    geom_polygon(aes(x=long,y=lat,group=group),fill="grey",size=0.2,colour="black",data=ICESareas,alpha=0) + 
    annotate("text",x=-3.5,y=49.5,label="7e") + annotate("text",x=-8,y=49,label="7h") +
    annotate("text",x=-11,y=51,label="7j") + annotate("text",x=-5.5,y=50.8,label="7f") +
    annotate("text",x=-8,y=51,label="7g") + annotate("text",x=-14,y=51,label="7k") +
    annotate("text",x=-11,y=53.5,label="7b") + annotate("text",x=-14,y=53.5,label="7c") + 
    theme_bw() + ylim(42,56) + xlim(-18,2) + ggtitle("Anglerfish stocks")
  
  
  # Nephrops
  
  Nep <- read.delim(file = "rect_FU_MA_UPDATED_2017.txt")
  Nep <- filter(Nep, Assessment.Area %in% c("VIIA","VIIB-K"))
  Nep <- cbind(Nep, ICESrectangle2LonLat(Nep$Rectangle))
  Nep <- Nep[Nep$FU != "Outside FU",]
  
  p9 <- ggplot() +
    geom_point(aes(x = SI_LONG, y = SI_LATI, group = FU, colour = FU), data = Nep, shape = 15, size = 4) + 
    geom_polygon(aes(x=long,y=lat,group=group),fill="grey",size=0.2,colour="black",data=ICESareas,alpha=0) + 
    annotate("text",x=-3.5,y=49.5,label="7e") + annotate("text",x=-8,y=49,label="7h") +
    annotate("text",x=-11,y=51,label="7j") + annotate("text",x=-5.5,y=50.8,label="7f") +
    annotate("text",x=-8,y=51,label="7g") + annotate("text",x=-14,y=51,label="7k") +
    annotate("text",x=-11,y=53.5,label="7b") + annotate("text",x=-14,y=53.5,label="7c") + 
    theme_bw() + ylim(48,56) + xlim(-18,2) + ggtitle("Nephrops stocks") +
    scale_shape_discrete(solid=T)
    


  
  pdf("Stock_boundaries.pdf", width = 10, height = 8)
  plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9)
  dev.off()
  