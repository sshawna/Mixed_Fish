Advice <- read.csv("C:/Informatics Projects/Mixed Fish Git/Mixed_Fish/data/Adviceallyears.csv")
SSadvice2015.6<-filter(Advice,Advice.Year==2015|Advice.Year==2016,Sc=="Single Species Advice")$val
SSadvice2015.6N<-rep(SSadvice2015.6,each=7)
SSadvice2017<-filter(Advice,Advice.Year==2017,Sc=="Single Species Advice")$val
SSadvice2017N<-rep(SSadvice2017,each=9)
SSadvice2018<-filter(Advice,Advice.Year==2018,Sc=="Single Species Advice")$val
SSadvice2018N<-rep(SSadvice2018,each=10)
SS<-c(SSadvice2015.6N,SSadvice2017N,SSadvice2018N)
length(SS)
Advice$SS<-SS
Advice$Diff<-Advice$val-Advice$SS
Advice$Ratio<-(Advice$val)/(Advice$SS)
###Ratio not available for Cod Advice 2018 : Catch and F for all scenarios due to the zero SS Advice
###Those Ratio values set to NA
Advice$Ratio[c(which(is.nan(Advice$Ratio)),which(Advice$Ratio==Inf))]<-NA

