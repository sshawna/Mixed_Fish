
#observeEvent (
# input$Area_selector,
#if(input$Area_selector == "North_Sea") {
#  load(file.path('..','WGMIXFISH','METH-2018-master','data_explorer','5.partial F app','data','North_Sea','partF.Rdata'))
#}else if(input$Area_selector == "Celtic_Sea") {
# load(file.path('..','WGMIXFISH','METH-2018-master','data_explorer','5.partial F app','data','Celtic_Sea','partF.Rdata'))
#}
# )

#load(file.path('..','WGMIXFISH','METH-2018-master','data_explorer','5.partial F app','data','North_Sea','partF.Rdata'))
#NS.env <-  environment()
#load(file.path('..','WGMIXFISH','METH-2018-master','data_explorer','5.partial F app','data','North_Sea','partF.Rdata'), envir = NS.env)
#CS.env <- environment()
#load(file.path('..','WGMIXFISH','METH-2018-master','data_explorer','5.partial F app','data','Celtic_Sea','partF.Rdata'), envir = CS.env)

LoadToEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  return(env)
}

NS.env <- LoadToEnvironment(file.path('..','WGMIXFISH','METH-2018-master','data_explorer','5.partial F app','data','North_Sea','partF.Rdata'))
CS.env <- LoadToEnvironment(file.path('..','WGMIXFISH','METH-2018-master','data_explorer','5.partial F app','data','Celtic_Sea','partF.Rdata'))

ls.str(NS.env)
ls.str(CS.env)

saveRDS(NS.env$partF, file = "NSpartF.rds")
NSpartF <- readRDS("NSpartF.rds")
saveRDS(CS.env$partF, file = "CSpartF.rds")
CSpartF <- readRDS("CSpartF.rds")

NS.env <- LoadToEnvironment(file.path('..','WGMIXFISH','METH-2018-master','data_explorer','4.catchability app/data','North_Sea','catchability.Rdata'))
CS.env <- LoadToEnvironment(file.path('..','WGMIXFISH','METH-2018-master','data_explorer','4.catchability app/data','Celtic_Sea','catchability.Rdata'))

ls.str(NS.env)
ls.str(CS.env)

saveRDS(NS.env$catchability, file = "NScatchability.rds")
NScatchability <- readRDS("NScatchability.rds")
saveRDS(CS.env$catchability, file = "CScatchability.rds")
CScatchability <- readRDS("CScatchability.rds")

##########################################################
NS.env <- LoadToEnvironment(file.path('C:','github','METH-2018-master','data_explorer','6.quota share app/data','North_Sea','QuotaShare.Rdata'))
CS.env <- LoadToEnvironment(file.path('C:','github','METH-2018-master','data_explorer','6.quota share app/data','Celtic_Sea','QuotaShare.Rdata'))

ls.str(NS.env)

saveRDS(NS.env$quotashare,"data/existing_tools/6.quota_share_app/data/North_Sea/NSquotashare.rds")
saveRDS(NS.env$quotashare,"data/existing_tools/6.quota_share_app/data/North_Sea/NSquotashare1.rds")

NSquotashare <-  readRDS("data/existing_tools/6.quota_share_app/data/North_Sea/NSquotashare.rds")
NSquotashare <- as.data.frame(NSquotashare)
colnames(NSquotashare)[2]<-"stock"
saveRDS(NSquotashare,"data/existing_tools/6.quota_share_app/data/North_Sea/NSquotashare.rds")

NSquotashare <-  readRDS("data/existing_tools/6.quota_share_app/data/North_Sea/NSquotashare1.rds")
NSquotashare <- as.data.frame(NSquotashare)
NSquotashare [,c("relstab","catchcomp","landings")] <- round(NSquotashare [,c("relstab","catchcomp","landings")],2)
colnames(NSquotashare)[c(2,7:8)] <- c("stock","proportion_fleet_landings", "stock_landings_share")
saveRDS(NSquotashare,"data/existing_tools/6.quota_share_app/data/North_Sea/NSquotashare1.rds")


ls.str(CS.env)

saveRDS(CS.env$quotashare,"data/existing_tools/6.quota_share_app/data/Celtic_Sea/CSquotashare.rds")
saveRDS(CS.env$quotashare,"data/existing_tools/6.quota_share_app/data/Celtic_Sea/CSquotashare1.rds")

CSquotashare <-  readRDS("data/existing_tools/6.quota_share_app/data/Celtic_Sea/CSquotashare.rds")
CSquotashare <- as.data.frame(CSquotashare)
colnames(CSquotashare)[2]<-"stock"
saveRDS(CSquotashare,"data/existing_tools/6.quota_share_app/data/Celtic_Sea/CSquotashare.rds")

CSquotashare <-  readRDS("data/existing_tools/6.quota_share_app/data/Celtic_Sea/CSquotashare1.rds")
CSquotashare <- as.data.frame(CSquotashare)
CSquotashare [,c("relstab","catchcomp","landings")] <- round(CSquotashare [,c("relstab","catchcomp","landings")],2)
colnames(CSquotashare)[c(2,7:8)] <- c("stock","proportion_fleet_landings", "stock_landings_share")
saveRDS(CSquotashare,"data/existing_tools/6.quota_share_app/data/Celtic_Sea/CSquotashare1.rds")
