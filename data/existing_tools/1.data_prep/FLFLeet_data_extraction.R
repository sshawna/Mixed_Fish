#extract the relevant data from FLFleet and put that in the data folder
#Clara
#24/05/2017
#19/10/2017 - Paul

library(FLCore); library(FLFleet)
library(dplyr)
# source("../../functions/FLFcube_FLCore_R31.r")  #slot.fleet needed
#source(file.path(""..","..","functions/FLFcube_FLCore_R31.r"))  #slot.fleet needed
source(file.path("functions/FLFcube_FLCore_R31.r"))  #slot.fleet needed


Area <- "North_Sea" # choose which area 

# create a folder for the combined records, if it doesn't exist
# if(!any(grepl(Area, list.dirs(file.path("..","1.data_prep","fleet objects"))))) {
#   dir.create(file.path("..","1.data_prep","fleet objects",Area))
#   print("Need to input FLFLeet object in correct folder, which has been created for your convenience :)")
# }

# create a folder for the combined records, if it doesn't exist
if(!any(grepl(Area, list.dirs(file.path("data_explorer","1.data_prep","fleet objects"))))) {
  dir.create(file.path("data_explorer","1.data_prep","fleet objects",Area))
  print("Need to input FLFLeet object in correct folder, which has been created for your convenience :)")
}


#get FLFleet object on data
#load(list.files(file.path("..","1.data_prep","fleet objects", Area), full.names = TRUE))
load(list.files(file.path("data_explorer","1.data_prep","fleet objects", Area), full.names = TRUE))

#catchability

catchability <- slot.fleet(fleets, "catch.q")
catchability$country <- substr(catchability$fleet,1,2)
catchability$logq <- log(catchability$catch.q)
names(catchability)[which(names(catchability)=="qname")] <- "stock"

# if(!any(grepl(Area, list.dirs(file.path("..","4.catchability app","data",Area) )))) {
# dir.create(file.path("..","data_explorer","4.catchability app/data",Area)) 
# }
# save(catchability,file=file.path("..","4.catchability app","data",Area,"catchability.Rdata"))

if(!any(grepl(Area, list.dirs(file.path("data_explorer","4.catchability app","data",Area) )))) {
  dir.create(file.path("data_explorer","data_explorer","4.catchability app/data",Area)) 
}
save(catchability,file=file.path("data_explorer","4.catchability app","data",Area,"catchability.Rdata"))

#partial F

effshare <- slot.fleet(fleets, "effshare")
partF <- merge(catchability,effshare)

eff <- lapply(fleets,function(x) as.data.frame(effort(x)))
eff <- cbind(eval(parse(text=paste0('rbind(eff[[', paste(seq(length(eff)),
                                                 collapse=']] ,eff[['), ']])'))),fleet=rep(names(eff),each=max(sapply(eff,nrow))))

#or easier to use:
#eff <- lapply(names(fleets),function(x) cbind(fleet=x, as.data.frame(effort(fleets[[x]]))))
#eff <- Reduce(rbind, eff)

names(eff)[which(names(eff)=="data")] <- "effort"

partF <- merge(partF,eff)
partF$partF <- partF$effort*partF$effshare*partF$catch.q
# if(!any(grepl(Area, list.dirs(file.path("..", "5.partial F app","data",Area))))) {
# dir.create(file.path("..","5.partial F app","data",Area))
# }
# save(partF,file=file.path("..","5.partial F app","data",Area,"partF.Rdata"))

if(!any(grepl(Area, list.dirs(file.path("data_explorer", "5.partial F app","data",Area))))) {
  dir.create(file.path("data_explorer","5.partial F app","data",Area))
}
save(partF,file=file.path("data_explorer","5.partial F app","data",Area,"partF.Rdata"))

rm(effshare)

##  Landing share
landshare <- slot.fleet(fleets, "landings")

# aggregate by fleet
landshare <- landshare %>% group_by(year, qname, fleet) %>% summarise(landings = sum(landings))

# add total landings summed across fleets
fleetland <- landshare %>% group_by(year,fleet) %>% summarise(fleet_landings = sum(landings))
landshare$fleet_landings <- fleetland$fleet_landings[match(paste0(landshare$year, landshare$fleet),
                                                           paste0(fleetland$year, fleetland$fleet))]

# add total landings of stock
stockland <- landshare %>% group_by(year,qname) %>% summarise(stock_landings = sum(landings))
landshare$stock_landings <- stockland$stock_landings[match(paste0(landshare$year, landshare$qname),
                                                           paste0(stockland$year, stockland$qname))]

# calculate the proportions
landshare$catchcomp <- landshare$landings / landshare$fleet_landings 
landshare$relstab <- landshare$landings / landshare$stock_landings 

quotashare <- landshare

# if(!any(grepl(Area, list.dirs(file.path("..","6.quota share app","data" , Area))))) {
# dir.create(file.path("..","6.quota share app","data",Area))
# }
# save(quotashare,file=file.path("..","6.quota share app","data",Area,"QuotaShare.Rdata"))

if(!any(grepl(Area, list.dirs(file.path("data_explorer","6.quota share app","data" , Area))))) {
  dir.create(file.path("data_explorer","6.quota share app","data",Area))
}
save(quotashare,file=file.path("data_explorer","6.quota share app","data",Area,"QuotaShare.Rdata"))

