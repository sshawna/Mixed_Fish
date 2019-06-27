# Hans Gerritsen 2014

# these functions are specific for producing tac/assessment area maps
# they these shapefiles, or they will fall over:

# ices <- readShapePoly('F:\\mapping\\shapefiles\\ices_div',proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
# coast <- readShapePoly('F:\\mapping\\shapefiles\\world\\continent',proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))


# add in extra points along the lines for projection
polyfun<-function(poly){
  if(sum(poly[1,]==poly[nrow(poly),])<2) poly <- rbind(poly,poly[1,])
  out <- NULL
  for(j in 2:nrow(poly)){
    i <- j-1
    l <- sqrt(diff(poly[c(i,j),1])^2+diff(poly[c(i,j),2])^2)
    out <- rbind(out
      ,data.frame(x=seq(poly[i,1],poly[j,1],length.out=l)
        ,y=seq(poly[i,2],poly[j,2],length.out=l))
    )
  }
  return(out)
}

applypolyfun <- function(x) {
  for(i in 1:length(x@polygons)){
    for(j in 1:length(x@polygons[[i]]@Polygons)){
      poly <- x@polygons[[i]]@Polygons[[j]]@coords
      poly1 <- polyfun(poly)
      k <- !duplicated(poly1)
      poly1 <- poly1[k,]
      poly1 <- rbind(poly1,poly1[1,])
      x@polygons[[i]]@Polygons[[j]] <- Polygon(poly1)
    }
  }
  return(x)
}


plotmap <- function(stock,stockarea,tacarea) {

  #  lab <- gCentroid(ices,T)

#  ices1 <- applypolyfun(ices)

  if(all(!is.na(stockarea))){
    # merge divisions
    a <- ifelse(ices$IcesArea%in%stockarea,'AssessmentArea',NA)
    assess <- gUnaryUnion(ices,a)
    assess1 <- applypolyfun(assess)
  } 

    
  if(all(!is.na(tacarea))){
    if(is.list(tacarea)) {
      a <- ifelse(ices$IcesArea%in%tacarea[[1]],1,NA)
      for(i in 1:length(tacarea))
        a <- ifelse(ices$IcesArea%in%tacarea[[i]],i,a)
      } else a <- ifelse(ices$IcesArea%in%tacarea,'TacArea',NA)
    tac <- gUnaryUnion(ices,a)
    tac1 <- applypolyfun(tac)
  }

  if(all(is.na(stockarea)))   midlon <- mean(range(tac1@bbox[1,]))
  if(all(is.na(tacarea)))   midlon <- mean(range(assess1@bbox[1,]))
  if(all(!is.na(stockarea)) & !all(is.na(tacarea))) midlon <- mean(range(c(assess1@bbox[1,],tac1@bbox[1,])))
  
  projection<-CRS(paste0("+proj=laea +lat_0=90 +lon_0=",midlon," +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
    
#  projection<-CRS("+proj=ortho +lat_0=53 +lon_0=-6 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs ")
  
  
  coast.proj<-spTransform(subset(coast,CONTINENT%in%c('North America','Europe','Africa','Asia')),projection)
#  ices1.proj<-spTransform(ices1,projection)
#  lab.proj<-spTransform(lab,projection)
  l5.proj <- spTransform(l5,projection)
  l6.proj <- spTransform(l6,projection)
  l7.proj <- spTransform(l7,projection)

  p1 <- Polygon(cbind(c(-10.5,-5.5,-5.5,-10.5,-10.5)
    ,c(51.5,51.5,55.3,55.3,51.5)))
  p2 <- Polygons(list(p1),'IRL')
  p3 <- SpatialPolygons(list(p2),proj4string=ices@proj4string)  
  irl.proj<-spTransform(p3,projection)
  
  if(all(!is.na(stockarea)))  assess1.proj<-spTransform(assess1,projection)
  if(all(!is.na(tacarea)))  tac1.proj<-spTransform(tac1,projection)
  
  if(all(is.na(stockarea))){
    xlim <- range(c(tac1.proj@bbox[1,],irl.proj@bbox[1,]))
    ylim <- range(c(tac1.proj@bbox[2,],irl.proj@bbox[2,]))
  }
    
  if(all(is.na(tacarea))){
    xlim <- range(c(assess1.proj@bbox[1,],irl.proj@bbox[1,]))
    ylim <- range(c(assess1.proj@bbox[2,],irl.proj@bbox[2,]))
  }
    
  if(all(!is.na(stockarea)) & all(!is.na(tacarea))) {
    xlim <- range(c(assess1.proj@bbox[1,],tac1.proj@bbox[1,],irl.proj@bbox[1,]))
    ylim <- range(c(assess1.proj@bbox[2,],tac1.proj@bbox[2,],irl.proj@bbox[2,]))
  }
  
  if(diff(xlim) < diff(ylim)) xlim <- xlim + c(-0.5,0.5)*(diff(ylim)-diff(xlim))
  if(diff(xlim) > diff(ylim)) ylim <- ylim + c(-0.5,0.5)*(diff(xlim)-diff(ylim))
  
  png(paste0(stock,'.png'),3.5,3.5+0.8,'cm',7,'white',1200)
#postscript(paste0(stock,'.eps'),width=3.5,height=3.5+0.8,paper='special')
  
    par(mar=c(3,.1,.1,.1))
    plot.new()
    plot.window(xlim=xlim,ylim=ylim,asp=1)
  #  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],col = 'lightcyan')
    if(all(!is.na(tacarea))) plot(tac1.proj,col='pink',lwd=0.5,add=T)
#    plot(ices1.proj,border='grey',lwd=0.5,add=T)

if(diff(xlim) > 4e6) plot(l5.proj,col='darkgrey',lwd=0.5,add=T) else
if(diff(xlim) > 1e6)  plot(l6.proj,col='darkgrey',lwd=0.5,add=T) else
  plot(l7.proj,col='darkgrey',lwd=0.5,add=T)

    if(all(!is.na(tacarea))) plot(tac1.proj,border='red',lwd=1,add=T)
    if(all(!is.na(stockarea))) plot(assess1.proj,col='blue',density=40,lwd=0.4,add=T)
    if(all(!is.na(stockarea))) plot(assess1.proj,border='blue',lwd=0.5,add=T)
    plot(coast.proj,add=T,col='green4',border=NA)
#    coords <- lab.proj@coords
#    text(coords,lab=ices@data$IcesArea)
    box(lwd=0.5)
    par(new=T,mar=rep(0,4),lwd=0.5)
    plot.new()
    legend('bottom',c('TAC/Management area','Assessment area'),fill=c('pink','blue'),border=c('red','blue'),density=c(NA,40),ncol=1,bty='n')
  dev.off()  
  
  assess_area <- deparse(substitute(stockarea))
  assess_area <- gsub(' ','',assess_area)
  assess_area <- gsub('c[:(:]','',assess_area)
  assess_area <- gsub('[:):]','',assess_area)
  
  tac_area <- deparse(substitute(tacarea))
  tac_area <- gsub(' ','',tac_area)
  tac_area <- gsub('list[:(:]','',tac_area)
  tac_area <- gsub('[:):],',' and ',tac_area)
  tac_area <- gsub('c[:(:]','',tac_area)
  tac_area <- gsub('[:):]','',tac_area)
  
#  if(is.list(tacarea)) tac_area <- paste(lapply(tacarea,function(x) paste(x,collapse=',')),collapse=' and ') else
#    tac_area <- paste(tacarea,collapse=',')
    
  out0 <- data.frame(stock,assess_area,tac_area)
  assign('out',rbind(out,out0),.GlobalEnv)      
}
