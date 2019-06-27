library(sp)
library(rgdal)
library(maptools)
library(dplyr)
ices2 <- writeSpatialShape(ices, "ices")
ices2 <-readOGR("H:/TCM mapping/maps","ices2")
plot(ices2)
View(ices2)

map<-readOGR("C:/MAPS","33SEE250GC_SIR") 
small.map<-subset(world, LON>-43.41 | LON < -43.1 & LAT>- 23.05 | LAT< -22.79)
writeOGR(small.map, ".", "filename", driver="ESRI Shapefile") 
#stocks
#cod-7e-k <- subset(ices,stockarea=c(VIIek,VIIaGad2),tacarea=c(VIIbc,VIIek,VIII,IX,X,CECAF_1.1_Eu))
#cod-7e-k <-writeOGR(cod-7e-k, dsn="cod-7e-k.geojson", layer=)
  
'cod-7e-k',stockarea=c(VIIe,VIIf,VIIg,VIIh,VIIj,VIIk,VIIaGad2),tacarea=c(VIIb,VIIc,VIIe,VIIf,VIIg,VIIh,VIIj,VIIk,VIII,IX,X,CECAF_1.1_Eu))
'had-7b-k',c(VIIb,VIIc,VIIe,VIIf,VIIg,VIIh,VIIj,VIIk,VIIaGad2),c(VIIbk,VIII,IX,X,CECAF_1.1_Eu)) 
'whg-7b-k',c(VIIb,VIIc,VIIe,VIIf,VIIg,VIIh,VIIj,VIIk,VIIaGad2),c(VIIbk))
'ang-78',c(VIIa,VIIb,VIIc,VIId,VIIe,VIIf,VIIg,VIIh,VIIj,VIIk,VIIIabd),list(VII,VIIIabde))
'mgw-78',c(VIIbk,VIIIabd),list(VII,VIIIabde))
'sol-celt',VIIfg,VIIfg)
'sol-7h-k',VIIhk,VIIhk)
'sol-7bc',c(VIIb,VIIc),c(VIIb,VIIc))
'her-irls',c(VIIaS,VIIgk),c(VIIaS,VIIgk))
'hke-nrtn',c(IV,VI,VII,IIIa,VIIIabde)
        ,list(c(IIIa,IIIb_Eu,IIIc_Eu,IIId_Eu),c(IIa_Eu,IV_Eu),c(VI,VII,Vb_EuInt,XII_Int,XIV_Int),c(VIIIabde)))
'sol-scow',VI,c(VI,Vb_EuInt,XII_Int,XIV_Int)) # no assessment area
# IIIa and IV; Union waters of IIa, IIIb, IIIc and Subdivisions 22-32
# VI; Union and international waters of Vb, XII and XIV
plotmap('meg-ivvi',c(IVa,VIa),list(c(IIa_Eu,IV_Eu),c(Vb_EuInt,VI,XII_Int,XIV_Int)))
# Union and international waters of Vb; VI; international waters of XII and XIV 
plotmap('meg-rock',VIb,c(Vb_EuInt,VI,XII_Int,XIV_Int))
# Union waters of IIa and IV
# VI; Union and international waters of international waters of XII and XIV
plotmap('ang-ivvi',c(IV,VI,IIIa), list(c(IIa_Eu,IV_Eu),c(VI,XII_Int,XIV_Int),c(IV_Norway)))
# VI; Union and international waters of Vb
I <- c(I_Int_N,I_Norway,I_Russia)
IIa <- c(IIa_Eu,IIa_Faroe,IIa_Iceland,IIa_Int,IIa_Greenland,IIa_Norway)
IIb <- c(IIb_Int,IIb_Int_N,IIb_Greenland,IIb_Norway)
IIIa <- c(IIIa_Eu,IIIa_Norway)
IIIb <- IIIb_Eu
IIIc <- IIIc_Eu
IIId <- c(IIId_Eu,IIId_Russia)
IVa <- c(IVa_Eu,IVa_Faroe,IVa_Norway)
IVb <- c(IVb_Eu,IVb_Norway)
IVc <- IVc_Eu
Va <- c(Va_Faroe,Va_Iceland,Va_Greenland)
Vb1 <- c(Vb1_Eu_E,Vb1_Eu_W,Vb1_Faroe,Vb1_Iceland,Vb1_Int_W)
Vb2 <- c(Vb2_Eu,Vb2_Faroe)
VIa <- c(VIa_Eu,VIa_Faroe)
VIb <- c(VIb_Eu,VIb_Int)
VIIa <- VIIa_Eu
VIIb <- VIIb_Eu
VIIc <- c(VIIc_Eu,VIIc_Int)
VIId <- VIId_Eu
VIIe <- VIIe_Eu
VIIf <- VIIf_Eu
VIIg <- VIIg_Eu
VIIh <- VIIh_Eu
VIIj <- c(VIIj_Eu,VIIj_Int)
VIIk <- c(VIIk_Eu,VIIk_Int)
VIIIa <- VIIIa_Eu
VIIIb <- VIIIb_Eu
VIIIc <- VIIIc_Eu
VIIId <- c(VIIId_Eu,VIIId_Int)
VIIIe <- c(VIIIe_Eu,VIIIe_Int)
IXa <- IXa_Eu
IXb <- c(IXb_Eu,IXb_Int)
X <- c(X_Eu,X_Int)
XII <- c(XII_Iceland,XII_Int,XII_Greenland)
XIVa <- c(XIVa_Iceland,XIVa_Int_N,XIVa_Greenland,XIVa_Norway)
XIVb <- c(XIVb_Iceland,XIVb_Int,XIVb_Greenland)
CECAF_Eu <- c(CECAF_Eu,CECAF_1.1_Eu)

II <- c(IIa,IIb)
III <- c(IIIa,IIIb,IIIc,IIId)
IV <- c(IVa,IVb,IVc)
V <- c(Va,Vb1,Vb2)
VI <- c(VIa,VIb)
VII <- c(VIIa,VIIb,VIIc,VIId,VIIe,VIIf,VIIg,VIIh,VIIj,VIIk)
VIII <- c(VIIIa,VIIIb,VIIIc,VIIId,VIIIe)
IX <- c(IXa,IXb)
XIV <- c(XIVa,XIVb)
IVbc <- c(IVb,IVc)
Vb <- c(Vb1,Vb2)
VIIac <- c(VIIa,VIIb,VIIc)
VIIbc <- c(VIIb,VIIc)
VIIbk <- c(VIIb,VIIc,VIId,VIIe,VIIf,VIIg,VIIh,VIIj,VIIk) #incl 7d
VIIfg <- c(VIIf,VIIg)
VIIfk <- c(VIIf,VIIg,VIIh,VIIj,VIIk)
VIIek <- c(VIIe,VIIf,VIIg,VIIh,VIIj,VIIk)
VIIhk <- c(VIIh,VIIj,VIIk)
VIIgk <- c(VIIg,VIIh,VIIj,VIIk)
VIIIabd <- c(VIIIa,VIIIb,VIIId)
VIIIabde <- c(VIIIa,VIIIb,VIIId,VIIIe)

II_Eu <- c(IIa_Eu)
II_Norway <- c(IIa_Norway,IIb_Norway)
II_Int <- c(IIa_Int,IIb_Int,IIb_Int_N)
III_Eu <- c(IIIa_Eu,IIIb_Eu,IIIc_Eu)
IV_Eu <- c(IVa_Eu,IVb_Eu,IVc_Eu)
IV_Norway <- c(IVa_Norway,IVb_Norway)
Vb_EuInt <- c(Vb1_Eu_E,Vb1_Eu_W,Vb1_Int_W,Vb2_Eu)
Vb_EuIntW <- c(Vb1_Eu_W,Vb1_Int_W)
Vb_EuIntE <- c(Vb1_Eu_E,Vb2_Eu)
VIa_EuInt <- VIa_Eu
VIb_EuInt <- c(VIb_Eu,VIb_Int)
VI_EuInt <- c(VIa_EuInt,VIb_EuInt)
XIV_Int <- c(XIVa_Int_N,XIVb_Int)

#TAC areas
