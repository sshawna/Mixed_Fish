
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