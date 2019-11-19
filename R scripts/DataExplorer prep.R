
########Data prep######
library(dplyr)
library(doBy)




##############Landings########################################################
##############################################################################
##############################################################################
# 00_Setup ####

rm(list = ls())
gc()

#library(tidyverse)
library(ggplot2)
library(icesTAF)
library(dplyr)
library(tidyr)

# 01 _ Notes #### 
# This script is to clean the final year of the "old accessions" data 

# 02 _ Read in data ####
accessions_landings <- read.csv("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/bootstrap/data/wgmixfish_accessions/accessions_catch.csv") 
# Adding Irish data for 2017
IRL_dat <- read.csv("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/bootstrap/data/wgmixfish_accessions/NEP_format_resubmitted_2018 WGMIXFISH-ADVICE IE metier_catch WGCSE.csv")
accessions_landings_IRL_2017 <- IRL_dat %>% select(Country,Year,Quarter,IntercatchMetierTag,VesselLengthCategory, FDFVessel,Area,Species,Landings,Value)
names(accessions_landings_IRL_2017) <- c("Country","Year","Quarter","Metier","Vessel_length", "FDF", "Area","Species","Landings","Value")
accessions_landings_IRL_2017$Discards <- NaN
accessions_landings_IRL_2017$file_name <- NaN

accessions_landings<- rbind(accessions_landings, accessions_landings_IRL_2017)


# 03 _ Clean Area and Species ####
#read in table of areas and species to change 
area_spp_fix <- read.csv("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/bootstrap/data/supporting_files/area_recoding.csv")
names(area_spp_fix) <- c("Area" ,"ICES_mix_correct", "ICES_FU","species_mix_FU" )
new_accession_landings <- left_join(accessions_landings,area_spp_fix, by = "Area" )

#make columes for cleaning
new_accession_landings$Area_keep <- new_accession_landings$ICES_mix_correct
new_accession_landings$Species<- as.character(new_accession_landings$Species)
new_accession_landings$species_mix_FU <- as.character(new_accession_landings$species_mix_FU)
new_accession_landings$Species_keep <- ifelse(new_accession_landings$Species=="NEP", new_accession_landings$species_mix_FU,new_accession_landings$Species)

#final usable dataset
accessions_landings <- new_accession_landings%>% select( Country, Year, Quarter, Metier, Vessel_length, FDF, Area_keep, Species_keep, Landings, Value)
names(accessions_landings) <-  c("Country", "Year", "Quarter", "Metier", "Vessel_length", "FDF", "Area", "Species", "Landings", "Value")


#2019 fix
# so many values with a zero
accessions_landings <- accessions_landings[-which(accessions_landings$Landings== 0),]


# 04 _ Subset for Celtic Sea####
accessions_landings$Area <- as.character(accessions_landings$Area)
accessions_landings<- accessions_landings[accessions_landings$Area %in% c(
                                                                         "27.7" ,  "27.7.b" , 
                                                                           "27.7.c" ,  "27.7.d" ,  "27.7.e"  , "27.7.f", 
                                                                           "27.7.g" ,  "27.7.h"  , "27.7.j" ,  "27.7.k" ),]

#accessions_landings<- accessions_landings[substr(accessions_landings$Area, 1,4) %in% c("27.3", "27.4", "27.6", "27.7", "27.8"),]
accessions_landings<-droplevels(accessions_landings)
# 05 _ Clean Country Names ####
accessions_landings$Country[accessions_landings$Country == "nld"] <- "NLD"
accessions_landings$Country[accessions_landings$Country %in% c("ES-AZTI", "ES" )] <- "ESP"
#accessions_landings$Country[accessions_landings$Country == "Poland"] <- "POL"
accessions_landings$Country[accessions_landings$Country %in% c("GG","IM","JE","UKE")] <- "UKE"
accessions_landings$Country[accessions_landings$Country == "DE"] <- "DEU"


accessions_landings$Country<-droplevels(accessions_landings$Country)
levels(accessions_landings$Country)
levels(accessions_landings$Country)<-c("BEL", "DEU", "DNK", "EST" , "ESP", "FRA" , "IRL" , "LTU" ,"NLD", "POL", "SE" , "UKE", "UKN" ,"UKS")


accessions_landings$Species<-as.factor(accessions_landings$Species)
# 06 _ Grouped Species Assumptions #### LP to write text around this. not used in 2019
 #accessions_landings$Landings[accessions_landings$Species=="LDB"]<-accessions_landings$Landings[accessions_landings$Species=="LDB"]*(1-0.052) #I dont think this is doing twhat we think it is!!!!
# accessions_landings$Landings[accessions_landings$Species=="LEZ"]<-accessions_landings$Landings[accessions_landings$Species=="LEZ"]*(1-0.052)
# accessions_landings$Landings[accessions_landings$Species=="MEG"]<-accessions_landings$Landings[accessions_landings$Species=="MEG"]*(1-0.052)
accessions_landings$Species[accessions_landings$Species %in% c("LEZ","LDB")]<- "MEG" # what are teh implications of doing this after the above caluclation
accessions_landings$Species[accessions_landings$Species %in% c("ANK","ANF", "MNZ")]<- "MON"
# 

# 06 _ Clean Vessel Length ####
accessions_landings$Vessel_length[(accessions_landings$Vessel_length %in% c("24<40","o24t40m","24_<40", "VL2440"))]<-"24<40m"
accessions_landings$Vessel_length[(accessions_landings$Vessel_length %in% c( "VL1012", "10_<12", "12_<18" ,"18_<24", "10<24", "VL1824"))] <-"10<24m"
accessions_landings$Vessel_length[(accessions_landings$Vessel_length %in% c("VL40XX", ">=40"))]<-">=40m"
accessions_landings$Vessel_length[(accessions_landings$Vessel_length=="<10")]<-"<10m"
accessions_landings$Vessel_length[(accessions_landings$Vessel_length %in% c("-9", ">=10m", "ALL")) ]<-"all"
accessions_landings$Vessel_length[(accessions_landings$Vessel_length %in% c(NA, "")) ]<-"all" # CM 2019: I am not sure about this line, need to quantify the impact and converage of this assumption.

# 07 _ Clean Metier Naming ####
# Here for the "old accessions" datacall we just get rid of messy metier level 6 and replace it with metier level 4

#read in table of areas and species to change 
metier_fix <- read.csv("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/bootstrap/data/supporting_files/metier_recoding.csv")
names(metier_fix) <- c("Metier" ,"level_4")
new_accession_landings <- left_join(accessions_landings,metier_fix, by = "Metier" )

#make columes for cleaning
new_accession_landings$Metier_keep <- new_accession_landings$level_4

#final usable dataset
accessions_landings <- new_accession_landings%>% select( Country, Year, Quarter, Metier_keep, Vessel_length, Area, Species, Landings, Value)
names(accessions_landings) <-  c("Country", "Year", "Quarter", "Metier", "Vessel_length", "Area", "Species", "Landings", "Value")

accessions_landings <- accessions_landings %>% group_by(Country, Year, Metier, Vessel_length,
                                                        Area, Species) %>% dplyr::summarise("Landings" = sum(Landings, na.rm = TRUE), 
                                                                                            "Value" = sum(Value, na.rm=TRUE))

#####
#accessions_landings <- read.csv("data/clean_accessions_landingsN.csv") 
#subset to keep TAC species except NEP 
set1 <- accessions_landings[accessions_landings$Species %in% c("MON", "COD", "HAD", "HKE", "MEG", "PLE", "POL", "SOL", "WHG"),]
set1$Species<-droplevels(set1$Species)

#add FU variable with NA

set1$FU<-rep(NA,dim(set1)[1])

#### subset and split NEP names into Species and FU
set2<-accessions_landings[which(grepl("NEP",accessions_landings$Species)==TRUE),]
set2$Species<-droplevels(set2$Species)
set2$FU <- NA
set2$FU <- ifelse(grepl("11", set2$Species), "11", set2$FU)
set2$FU <- ifelse(grepl("12", set2$Species), "12", set2$FU)
set2$FU <- ifelse(grepl("13", set2$Species), "13", set2$FU)
set2$FU <- ifelse(grepl("14", set2$Species), "14", set2$FU)
set2$FU <- ifelse(grepl("15", set2$Species), "15", set2$FU)
set2$FU <- ifelse(grepl("16", set2$Species), "16", set2$FU)
set2$FU <- ifelse(grepl("17", set2$Species), "17", set2$FU)
set2$FU <- ifelse(grepl("19", set2$Species), "19", set2$FU)
set2$FU <- ifelse(grepl("2021", set2$Species), "20_21", set2$FU)
set2$FU <- ifelse(grepl("22", set2$Species), "22", set2$FU)
set2$FU <- ifelse(grepl("23.24", set2$Species), "23_24", set2$FU)
set2$FU <- ifelse(grepl("3.4", set2$Species), "3_4", set2$FU)
set2$FU <- ifelse(grepl("32", set2$Species), "32", set2$FU)
set2$FU <- ifelse(grepl("33", set2$Species), "33", set2$FU)
set2$FU <- ifelse(grepl("34", set2$Species), "34", set2$FU)
set2$FU <- ifelse(grepl("5", set2$Species), "5", set2$FU)
set2$FU <- ifelse(grepl("6", set2$Species), "6", set2$FU)
set2$FU <- ifelse(grepl("7", set2$Species), "7", set2$FU)
set2$FU <- ifelse(grepl("8", set2$Species), "8", set2$FU)
set2$FU <- ifelse(grepl("9", set2$Species), "9", set2$FU)
set2$FU <- ifelse(grepl("NEP.OUT", set2$Species), "outFU", set2$FU)

set2$Species<-rep("NEP",dim(set2)[1])

# Combine all TAC species and write.csv

accessions_landings<-rbind(set1,set2)
accessions_landings$FU<-as.factor(accessions_landings$FU)
#fix for 2018 data submission
#accessions_landings$Landings[accessions_landings$Country %in% c("LTU", "DEU","ESP")]<-accessions_landings$Landings[accessions_landings$Country %in% c( "LTU","DEU","ESP")]/1000

accessions_landings$Landings[accessions_landings$Country == "NLD" & accessions_landings$Year == 2018 & accessions_landings$Landings >0] <- 
  accessions_landings$Landings[accessions_landings$Country == "NLD" & accessions_landings$Year == 2018 & accessions_landings$Landings >0] /1000

accessions_landings$Landings[accessions_landings$Country == "ESP" & accessions_landings$Year == 2018 & accessions_landings$Landings >0] <- 
  accessions_landings$Landings[accessions_landings$Country == "ESP" & accessions_landings$Year == 2018 & accessions_landings$Landings >0] /1000
accessions_landings$Landings[accessions_landings$Country == "DEU" & accessions_landings$Year == 2018 & accessions_landings$Landings >0]
#Irish fix
accessions_landings$Metier[ !accessions_landings$Species == "OTH" & accessions_landings$Country == "IRL" & 
                              accessions_landings$Vessel_length %in% c("10<24m", "24<40m" ,">=40m") &
                              accessions_landings$Metier == "MIS_MIS"] <- "OTB_DEF"



#French fix
accessions_landings$Metier[ !accessions_landings$Species == "OTH" & accessions_landings$Country == "FRA" & 
                              accessions_landings$Vessel_length %in% c("10<24m", "24<40m", ">=40m") &
                              accessions_landings$Metier == "MIS_MIS"] <- "OTB_DEF"

write.csv(accessions_landings,"data/clean_accessions_landingsN.csv",row.names=F)



##############Effort########################################################
##############################################################################
###########################################################################

# 02 _ Read in data ####
accessions_effort <- read.csv("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/bootstrap/data/wgmixfish_accessions/accessions_effort.csv") 
accessions_effort$FU<-accessions_effort$Area
# 03 _ Clean Area ####
#read in table of areas and species to change 


area_fix<- read.csv("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/bootstrap/data/supporting_files/area_recoding.csv")
names(area_fix) <- c("Area" ,"ICES_mix_correct", "ICES_FU","species_mix_FU" )
#get rid of duplicates
area_fix_new <- distinct(area_fix)
area_fix$Area <- as.character(area_fix$Area)

accessions_effort$Area<- as.character(accessions_effort$Area)

new_accession_effort <- left_join(accessions_effort,area_fix, by = "Area" )

#make columes for cleaning
new_accession_effort$Area_keep <- new_accession_effort$ICES_mix_correct

#final usable dataset
accessions_effort <- new_accession_effort%>% select( Country, Year, Quarter, Metier, Vessel_length, FDF, Area_keep, kw_days,  
                                                     Days_at_sea, No_vessels,FU )
names(accessions_effort) <-  c("Country", "Year", "Quarter", "Metier", "Vessel_length", "FDF", "Area", "kw_days",  
                               "Days_at_sea", "No_vessels","FU")

# 04 _ Subset for Celtic Sea####
accessions_effort$Area <- as.character(accessions_effort$Area)
accessions_effort<- accessions_effort[accessions_effort$Area %in% c(
  "27.7" ,  "27.7.b" , 
  "27.7.c" ,  "27.7.d" ,  "27.7.e"  , "27.7.f", 
  "27.7.g" ,  "27.7.h"  , "27.7.j" ,  "27.7.k" ),]

# 05 _ Clean Country Names ####

accessions_effort$Country<-droplevels(accessions_effort$Country )

accessions_effort$Country[accessions_effort$Country %in% c("ES-AZTI", "ES" )] <- "ESP"
accessions_effort$Country[accessions_effort$Country == "FR"] <- "FRA"
accessions_effort$Country[accessions_effort$Country %in% c("GG","IM","JE","UKE")] <- "UKE"
accessions_effort$Country<-droplevels(accessions_effort$Country )
levels(accessions_effort$Country)<-c("BEL","DEU","DKK","ESP","FRA","IRL","NLD","UKE","UKN","UKS")
summary(accessions_effort$Country )
# 06 _ Clean Vessel Length ####
accessions_effort$Vessel_length[(accessions_effort$Vessel_length %in% c("24<40","o24t40m","24_<40", "VL2440", "24-40", "24<40 m"))]<-"24<40m"
accessions_effort$Vessel_length[(accessions_effort$Vessel_length %in% c( "VL1012", "10_<12", "12_<18" ,"18_<24", "10<24",
                                                                         "VL1824"))] <-"10<24m"
accessions_effort$Vessel_length[(accessions_effort$Vessel_length %in% c("VL40XX", ">=40", "o40"))]<-">=40m"
accessions_effort$Vessel_length[(accessions_effort$Vessel_length %in% c("u10", "<10"))]<-"<10m"
accessions_effort$Vessel_length[(accessions_effort$Vessel_length %in% c(".", "-9", ">=10m", "ALL", "#N/A")) ]<-"all"
accessions_effort$Vessel_length[is.na(accessions_effort$Vessel_length) ]<-"all"  
accessions_effort$Vessel_length[(accessions_effort$Vessel_length %in% c("")) ]<-"all"



# 07 _ Clean Metier Naming ####
# Here for the "old accessions" datacall we just get rid of messy metier level 6 and replace it with metier level 4
#read in table of areas and species to change 
metier_fix <- read.csv("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/bootstrap/data/supporting_files/metier_recoding.csv")
names(metier_fix) <- c("Metier" ,"level_4")
new_accessions_effort <- left_join(accessions_effort,metier_fix, by = "Metier" )

#make columes for cleaning
new_accessions_effort$Metier_keep <- new_accessions_effort$level_4

#final usable dataset
# FDF removed as not used in Celtic Sea 
accessions_effort <- new_accessions_effort%>% select( Country, Year, Quarter, Metier_keep, Vessel_length,
                                                      Area, kw_days, Days_at_sea, No_vessels,FU)
names(accessions_effort) <-  c("Country", "Year", "Quarter", "Metier", "Vessel_length",  "Area",   "kw_days",      
                               "Days_at_sea"  , "No_vessels","FU")


accessions_effort<- accessions_effort %>% group_by(Country, Year, Metier, Vessel_length,
                                                   Area,FU) %>% dplyr::summarise("kw_days" = sum(kw_days, na.rm = TRUE), 
                                                                              "Days_at_sea" = sum(Days_at_sea,na.rm=TRUE), 
                                                                              "No_vessels" = sum(No_vessels, na.rm=TRUE) )

accessions_effort$FU<-droplevels(accessions_effort$FU)

levels(accessions_effort$FU)
#[1] "27.7"       "27.7.b"     "27.7.c"     "27.7.c.2"   "27.7.d"     "27.7.e"     "27.7.f"     "27.7.g"     "27.7.h"    
#[10] "27.7.j"     "27.7.j.2"   "27.7.k"     "27.7.k.2"   "27.7.outFU" "FU.16"      "FU.19"      "FU.20-21"   "FU.22"     
#[19] "VIIb"       "VIIbc"      "VIId"       "VIIe"       "VIIfg"      "VIIg"       "VIIh"       "VIIj"
levels(accessions_effort$FU)<-c(rep(NA,13),"outFU",16,19,"20_21",22,rep(NA,8))


write.csv(accessions_effort,"data/clean_accessions_effort.csv",row.names=F)
#######################################################################################################################
#######################################################################################################################
##############################report_06_Figure 6.4.25.3_advice_sheet_plots#######################################
source("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/bootstrap/software/functions/FLFcube_FLCore_R31.R")
source("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/bootstrap/software/functions/remove_validity_FLFleet.R")
source("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/bootstrap/software/functions/funcs.R")
load("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/results/02_Making_Fleets_Celtic_Sea_2019tier1LO_KW.RData")
df <- slot.fleet(fleets, "landings")

# metier without area 
df$metier<-substr(df$metier,1,7)

colnames(df)[colnames(df)=="qname"] <-"stock"

levels(df$stock)[levels(df$stock) == "cod.27.7e-k"]<-"COD"
levels(df$stock)[levels(df$stock) == "had.27.7b-k"]<-"HAD"
levels(df$stock)[levels(df$stock) == "whg.27.7b-ce-k"]<-"WHG"

# Calculate catch
#df$C<-df$L+df$D

# aggregate it all up
df<-aggregate(df["landings"],by=list(Year=df$year,Metier = df$metier,Stock = df$stock),sum, na.rm = T)



# reorder the Stocks and Metiers in the dataframe to something "sensible"
df$Stock<-factor(df$Stock,levels = c("COD","HAD","WHG"))



# order the dataframe
df<-df[(order(df$Year,df$Metier,df$Stock)),]
df<-filter(df,Year==2018)

MandData <- read.csv("data/ICESadvice/MandData.csv")
names(df)<-names(MandData)
MandData<-rbind(MandData,df)
write.csv(MandData,"data/ICESadvice/MandData.csv",row.names=F)
###################################################################################
#########################################################################
#####################Data for  Tools#########################


library(ggplot2); library(FLCore); library(FLFleet)
library(FLash)
options(scipen=10000)
#C:\github\WGMIXFISH2-master\Celtic_Sea\results
#2019_CelticSea_MixedFisheriesAdvice-master
source("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/bootstrap/software/functions/FLFcube_FLCore_R31.R")

#load(file.path("C:/github/WGMIXFISH2-master/Celtic_Sea/results/01_Reproduce_the_advice_Celtic_Sea_2018_LO.Rdata"))

#load(file.path("C:/github/WGMIXFISH2-master/Celtic_Sea/fleets/02_Making FLFleet v1_0_KW.RData")) 

load(file.path("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/results/01_Reproduce_the_advice_Celtic_Sea_2019tier1LO.Rdata"))

load(file.path("C:/github/2019_CelticSea_MixedFisheriesAdvice-master/results/02_Making_Fleets_Celtic_Sea_2019tier1LO_KW.RData"))


########### Update catchability data ###########
catchability2018 <-  readRDS(
    "data/existing_tools/4.catchability_app/data/CScatchability.rds")


catchability <- slot.fleet(fleets, "catch.q")
catchability$country <- substr(catchability$fleet,1,3)
catchability$logq <- log(catchability$catch.q)
names(catchability)[which(names(catchability)=="qname")] <- "stock"
# rename ["cod.27.7e-k"    "had.27.7b-k"    "whg.27.7b-ce-k"] to "COD-CS" "HAD-CS" "WHG-CS"
levels(catchability$stock)<-levels(catchability2018$stock)

#add advice year
catchability2018$AdviceYear<-rep(2018,dim(catchability2018)[1])
catchability$AdviceYear<-rep(2019,dim(catchability)[1])
#Combine both years
CScatchability<-rbind(catchability2018,catchability)
CScatchability$country<-as.factor(CScatchability$country)
CScatchability$fleet<-as.factor(CScatchability$fleet)
levels(CScatchability$country)

#"BE"  "BEL" "EN"  "FR"  "FRA" "IE"  "IRL" "OT"  "OTH"
levels(CScatchability$country)<-c("BEL" , "BEL" ,"EN"  ,"FRA" , "FRA" ,"IRL" , "IRL", "OTH",  "OTH")

levels(CScatchability$fleet)
#[1] "BE_Beam_24<40m"    "BEL_Beam_24<40m"   "EN_Beam_24<40m"    "EN_Otter_10<24m"   "EN_Static_all"     "FR_Otter_10<24m"  
#[7] "FR_Otter_24<40m"   "FR_Otter_all"      "FRA_Otter_10<24m"  "FRA_Otter_24<40m"  "IE_Beam_24<40m"    "IE_Otter_10<24m"  
#[13] "IE_Otter_24<40m"   "IRL_Beam_10<24m"   "IRL_Beam_24<40m"   "IRL_Otter_10<24m"  "IRL_Otter_24<40m"  "IRL_Static_10<24m"
#[19] "OTH_OTH"
levels(CScatchability$fleet)<-c("BEL_Beam_24<40m" ,   "BEL_Beam_24<40m"  , "EN_Beam_24<40m"  ,  "EN_Otter_10<24m" ,  "EN_Static_all" ,    "FRA_Otter_10<24m",  
                               "FRA_Otter_24<40m"  , "FRA_Otter_all"  ,    "FRA_Otter_10<24m" , "FRA_Otter_24<40m" , "IRL_Beam_24<40m",    "IRL_Otter_10<24m",  
                                 "IRL_Otter_24<40m"  , "IRL_Beam_10<24m" ,  "IRL_Beam_24<40m" ,  "IRL_Otter_10<24m",  "IRL_Otter_24<40m" , "IRL_Static_10<24m",
                                 "OTH_OTH")

saveRDS(CScatchability,"data/existing_tools/4.catchability_app/data/CScatchability.rds")

########### Update partial F  data ###########
library(ggplot2); library(FLCore); library(FLFleet)
library(FLash)
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
partF$AdviceYear<-rep(2019,dim(partF)[1])
#####READ IN 2018 DATA

partF2018 <-  readRDS(
  "data/existing_tools/3.Effort_app/data/CSpartF.rds")
partF2018$AdviceYear<-rep(2018,dim(partF2018)[1])
names(partF)
names(partF2018)
levels(partF$stock)<-levels(partF2018$stock)
#Combine both years
CSpartF<-rbind(partF2018,partF)
CSpartF$country<-as.factor(CSpartF$country)
CSpartF$fleet<-as.factor(CSpartF$fleet)
levels(CSpartF$country)

#"BE"  "BEL" "EN"  "FR"  "FRA" "IE"  "IRL" "OT"  "OTH"
levels(CSpartF$country)<-c("BEL" , "BEL" ,"EN"  ,"FRA" , "FRA" ,"IRL" , "IRL", "OTH",  "OTH")

levels(CSpartF$fleet)
#[1] "BE_Beam_24<40m"    "BEL_Beam_24<40m"   "EN_Beam_24<40m"    "EN_Otter_10<24m"   "EN_Static_all"     "FR_Otter_10<24m"  
#[7] "FR_Otter_24<40m"   "FR_Otter_all"      "FRA_Otter_10<24m"  "FRA_Otter_24<40m"  "IE_Beam_24<40m"    "IE_Otter_10<24m"  
#[13] "IE_Otter_24<40m"   "IRL_Beam_10<24m"   "IRL_Beam_24<40m"   "IRL_Otter_10<24m"  "IRL_Otter_24<40m"  "IRL_Static_10<24m"
#[19] "OTH_OTH"
levels(CSpartF$fleet)<-c("BEL_Beam_24<40m" ,   "BEL_Beam_24<40m"  , "EN_Beam_24<40m"  ,  "EN_Otter_10<24m" ,  "EN_Static_all" ,    "FRA_Otter_10<24m",  
                                "FRA_Otter_24<40m"  , "FRA_Otter_all"  ,    "FRA_Otter_10<24m" , "FRA_Otter_24<40m" , "IRL_Beam_24<40m",    "IRL_Otter_10<24m",  
                                "IRL_Otter_24<40m"  , "IRL_Beam_10<24m" ,  "IRL_Beam_24<40m" ,  "IRL_Otter_10<24m",  "IRL_Otter_24<40m" , "IRL_Static_10<24m",
                                "OTH_OTH")

saveRDS(CSpartF,"data/existing_tools/3.Effort_app/data/CSpartF.rds")


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


quotashare1 <- quotashare
quotashare1[,c("relstab","catchcomp","landings")] <- round(quotashare1[,c("relstab","catchcomp","landings")],2)
colnames(quotashare1)[7:8] <- c("proportion_fleet_landings", "stock_landings_share")

names(quotashare)[which(names(quotashare)=="qname")] <- "stock"
names(quotashare1)[which(names(quotashare1)=="qname")] <- "stock"
levels(quotashare$stock)<-levels(quotashare2018$stock)
levels(quotashare1$stock)<-levels(quotashare_1_2018$stock)

quotashare$AdviceYear<-rep(2019,dim(quotashare)[1])
quotashare<-as.data.frame(quotashare)
quotashare1$AdviceYear<-rep(2019,dim(quotashare1)[1])
quotashare1<-as.data.frame(quotashare1)
quotashare2018$AdviceYear<-rep(2018,dim(quotashare2018)[1])
quotashare_1_2018$AdviceYear<-rep(2018,dim(quotashare_1_2018)[1])

#Combine both years for quotashare
CSquotashare<-rbind(quotashare2018,quotashare)

CSquotashare$fleet<-as.factor(CSquotashare$fleet)


levels(CSquotashare$fleet)
#[1] "BE_Beam_24<40m"    "BEL_Beam_24<40m"   "EN_Beam_24<40m"    "EN_Otter_10<24m"   "EN_Static_all"     "FR_Otter_10<24m"  
#[7] "FR_Otter_24<40m"   "FR_Otter_all"      "FRA_Otter_10<24m"  "FRA_Otter_24<40m"  "IE_Beam_24<40m"    "IE_Otter_10<24m"  
#[13] "IE_Otter_24<40m"   "IRL_Beam_10<24m"   "IRL_Beam_24<40m"   "IRL_Otter_10<24m"  "IRL_Otter_24<40m"  "IRL_Static_10<24m"
#[19] "OTH_OTH"
levels(CSquotashare$fleet)<-c("BEL_Beam_24<40m" ,   "BEL_Beam_24<40m"  , "EN_Beam_24<40m"  ,  "EN_Otter_10<24m" ,  "EN_Static_all" ,    "FRA_Otter_10<24m",  
                         "FRA_Otter_24<40m"  , "FRA_Otter_all"  ,    "FRA_Otter_10<24m" , "FRA_Otter_24<40m" , "IRL_Beam_24<40m",    "IRL_Otter_10<24m",  
                         "IRL_Otter_24<40m"  , "IRL_Beam_10<24m" ,  "IRL_Beam_24<40m" ,  "IRL_Otter_10<24m",  "IRL_Otter_24<40m" , "IRL_Static_10<24m",
                         "OTH_OTH")

saveRDS(CSquotashare,"data/existing_tools/6.quota_share_app/data/CSquotashare.rds")

#Combine both years for quotashare1
CSquotashare1<-rbind(quotashare_1_2018,quotashare1)

CSquotashare1$fleet<-as.factor(CSquotashare1$fleet)


levels(CSquotashare1$fleet)
#[1] "BE_Beam_24<40m"    "BEL_Beam_24<40m"   "EN_Beam_24<40m"    "EN_Otter_10<24m"   "EN_Static_all"     "FR_Otter_10<24m"  
#[7] "FR_Otter_24<40m"   "FR_Otter_all"      "FRA_Otter_10<24m"  "FRA_Otter_24<40m"  "IE_Beam_24<40m"    "IE_Otter_10<24m"  
#[13] "IE_Otter_24<40m"   "IRL_Beam_10<24m"   "IRL_Beam_24<40m"   "IRL_Otter_10<24m"  "IRL_Otter_24<40m"  "IRL_Static_10<24m"
#[19] "OTH_OTH"
levels(CSquotashare1$fleet)<-c("BEL_Beam_24<40m" ,   "BEL_Beam_24<40m"  , "EN_Beam_24<40m"  ,  "EN_Otter_10<24m" ,  "EN_Static_all" ,    "FRA_Otter_10<24m",  
                              "FRA_Otter_24<40m"  , "FRA_Otter_all"  ,    "FRA_Otter_10<24m" , "FRA_Otter_24<40m" , "IRL_Beam_24<40m",    "IRL_Otter_10<24m",  
                              "IRL_Otter_24<40m"  , "IRL_Beam_10<24m" ,  "IRL_Beam_24<40m" ,  "IRL_Otter_10<24m",  "IRL_Otter_24<40m" , "IRL_Static_10<24m",
                              "OTH_OTH")

saveRDS(CSquotashare1,"data/existing_tools/6.quota_share_app/data/CSquotashare1.rds")

