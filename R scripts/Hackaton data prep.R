
rm(list=ls())

###read in  catch data and choose only required variables: 
### Year,stock,metier, Country, catch

dataForHackathon<-read.csv("data/Hackathon/dataForHackathonIdea.csv")
#dataForHackathon<-dataForHackathon[c("year","stock","metier2","Country","CatchKG")]
###and sum catch by year , country and metier

dataHnew<-aggregate(dataForHackathon$CatchKG,list(dataForHackathon$year,dataForHackathon$Country,dataForHackathon$stock,
                                                  dataForHackathon$metier2),FUN=sum)

names(dataHnew)<-c("Year","Country","Stock","Metier","CatchKG")
#dataHnew[which(dataHnew$CatchKG==0),]

library(tidyr)
data_wide <- spread(dataHnew, Stock, CatchKG)
write.csv(data_wide,"data/Hackathon/ImplicationOfCatch.csv")
dat<-filter(Advice,var=="Catch",Stock=="COD",Advice.Year==2015)
p1<-nPlot(val ~ Stock, group =  'Sc', data = dat, type = 'multiBarChart')



output$Ass2015 <- renderChart({
  dat<-filter(Assessment,var==input$Forecastfilter,FishStock==input$ForecastStockfilter,Advice.Year==2015)
  p1<-nPlot(value ~ Year, group =  'FishStock', data = dat, type = 'lineWithFocusChart')
  p1$chart(color=rev(col))
  p1$addParams(width=600,dom = 'Ass2015')
  return(p1)
})
