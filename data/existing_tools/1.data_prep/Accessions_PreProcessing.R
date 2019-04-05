#############################################################
## WGMIXFISH server-side data pre-processing and filtering ##
## for data exploration ##
## Paul Dolder
## 18/05/2017
#############################################################

rm(list= ls())

## libraries
library(readr) # efficient read-in of data files
library(dplyr) # data manipulation

data_loc <- file.path('..', '..','..','data','accessions') # accessions data location


################
## Accessions ##
################

# Get list of accessions files
accession_files <- list.files(data_loc)
accession_catch  <- grep('catch', accession_files, ignore.case = TRUE, value = T)
accession_effort <- grep('effort', accession_files, ignore.case = TRUE, value = T)

# Remove duplicated Northern Irish data - already in UKE files
accession_catch  <- grep('UKN', accession_catch, ignore.case = TRUE, value = T, invert = T)
accession_effort <- grep('UKN', accession_effort, ignore.case = TRUE, value = T, invert = T)

# create a folder for the combined records, if it doesn't exist
if(!any(grepl('Combined_accession_data', list.dirs(file.path('..'))))) {
  dir.create(file.path('.','Combined_accession_data'))
}

##############################################
### Read in the data files, with a few checks
##############################################

## An error catch if the column doesn't exist 
# (e.g. discards in some cases)
combine_data <- function(ori, temp, col_n) {
  out <- tryCatch(
    {
      c(ori,c(temp[,col_n]))
    },
    error=function(cond) {
      message(cond)
      return(c(ori, rep(NA,nrow(temp))))
    },
    warning = function(cond) {
      message(cond)
      return(c(ori, rep(NA,nrow(temp))))
    },
    finally ={
      message(paste(temp[,2][1]))
          }
  )
  return(out)
}


##################################
## Catch data files
# For combining the country subs
##################################
ID <- character(); Country <- character(); Year <- character(); Quarter <- character(); Metier <- character(); Vessel_length <- character()
FDF <- character(); Area <- character(); Species <- character(); Landings <- numeric(); Value <- numeric(); Discards <- numeric()

################
# comma delim ##
################
##############################
## Loop through the data files

for (i in accession_catch) {
  print(i)
temp <- read_delim(file.path(data_loc,i), delim = ',', 
                   col_types = list("Landings" = col_number(),
                                    "Value" = col_number(),
                                    "Discards" = col_number())) %>% as.data.frame()

# semi-colon delim
if(ncol(temp) == 1) {
  temp <- read_delim(file.path(data_loc,i), delim = ';', 
                     col_types = list("Landings" = col_number(),
                                      "Value" = col_number(),
                                      "Discards" = col_number()))  %>% as.data.frame()
}

if(grepl(c('FRA'), i)) {
  if(!grepl("2018",i)) {
  temp <- read_delim(file.path(data_loc,i), delim = ',', 
                     col_types = list("Landings" = col_number(),
                                      "Value" = col_number(),
                                      "Discards" = col_number())) %>%
    as.data.frame()
  
  }
  

if(temp[1,3] != 2017)   {
temp <- temp[,-1] # empty first column in 2017, not in 2018
}

}

if(grepl('DK', i,ignore.case = TRUE)) {
  temp <- read_delim(file.path(data_loc,i), delim = ',') %>% as.data.frame() # missing ID column
  temp <- cbind(ID = 1:nrow(temp), temp)
  
}

## LTU vessel length and metier tag are wrong way round
if(grepl('LTU',i)) {
  met <- temp[,6]
  temp[,6] <- temp[,5]
  temp[,5] <- met
  
}

## Portugese missing first column
if(grepl('2018', i,ignore.case = TRUE) & grepl('PRT', i,ignore.case = TRUE))   {
  temp <- temp[,-1] # empty first column in 2017, not in 2018
}



# In principle, csv file should be as follows:
ID            <- combine_data(ID, temp,1)
Country       <- combine_data(Country, temp,2)
Year          <- combine_data(Year, temp,3)
Quarter       <- combine_data(Quarter, temp,4)
Metier        <- combine_data(Metier, temp,5)
Vessel_length <- combine_data(Vessel_length, temp,6)
FDF           <- combine_data(FDF, temp,7)
Area          <- combine_data(Area, temp,8)
Species       <- combine_data(Species, temp,9)
Landings      <- combine_data(Landings, temp,10)
Value         <- combine_data(Value, temp,11) 
Discards      <- combine_data(Discards, temp, 12)

}

Ca <- data.frame(ID=ID, Country = Country, Year = Year, Quarter = Quarter, Metier = Metier,
                 Vessel_length = Vessel_length, FDF = FDF, Area = Area, Species = Species, Landings = Landings, 
                 Value = Value, Discards = Discards)


Ca$Landings <- as.numeric(as.character(Ca$Landings))
Ca$Value <- as.numeric(as.character(Ca$Value))
Ca$Discards <- as.numeric(as.character(Ca$Discards))

levels(Ca$Country)[levels(Ca$Country) == "FR"] <- "FRA"

save(Ca, file = file.path('.', 'Combined_accession_data','Accessions_Catch.RData'))
############################################

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##############

##################################
## Effort data files
# For combining the country subs
##################################
ID <- character(); Country <- character(); Year <- character(); Quarter <- character(); Metier <- character(); Vessel_length <- character()
FDF <- character(); Area <- character(); kw_days <- numeric(); Days_at_sea <- numeric(); No_vessels <- numeric()

################
# comma delim ##
################
##############################
## Loop through the data files

for (i in accession_effort) {
  print(i)
  temp <- read_delim(file.path(data_loc ,i), delim = ',') %>% as.data.frame()
  
  # semi-colon delim
  if(ncol(temp) == 1) {
    temp <- read_delim(file.path(data_loc,i), delim = ';')  %>% as.data.frame()
  }
  
  if(grepl('FRA', i)) {
    temp <- read_delim(file.path(data_loc,i), delim = ',') %>% as.data.frame()
    
  }
  
  ## Portugese missing first column
  if(grepl('2018', i,ignore.case = TRUE) & grepl('PRT', i,ignore.case = TRUE))   {
    temp <- temp[,-1] # empty first column in 2017, not in 2018
  }
  
  if(grepl('PRT', i) & grepl('2017', i,ignore.case = TRUE)) {
       temp <- cbind(temp[,1:7], Area = '27.9.a', temp[,8:10]) # missing area
    
  }
  
   # In principle, csv file should be as follows:
  ID            <- combine_data(ID, temp,1)
  Country       <- combine_data(Country, temp,2)
  Year          <- combine_data(Year, temp,3)
  Quarter       <- combine_data(Quarter, temp,4)
  Metier        <- combine_data(Metier, temp,5)
  Vessel_length <- combine_data(Vessel_length, temp,6)
  FDF           <- combine_data(FDF, temp,7)
  Area          <- combine_data(Area, temp,8)
  kw_days       <- combine_data(kw_days, temp,9)
  Days_at_sea   <- combine_data(Days_at_sea, temp,10)
  No_vessels    <- combine_data(No_vessels, temp,11) 
  
  }

Ef <- data.frame(ID=ID, Country = Country, Year = Year, Quarter = Quarter, Metier = Metier,
                 Vessel_length = Vessel_length, FDF = FDF, Area = Area, kw_days = kw_days, Days_at_sea = Days_at_sea,
                 No_vessels = No_vessels)

Ef$kw_days <- as.numeric(as.character(Ef$kw_days))
Ef$Days_at_sea    <- as.numeric(as.character(Ef$Days_at_sea))
Ef$No_vessels <- as.numeric(as.character(Ef$No_vessels))

levels(Ef$Country)[levels(Ef$Country) == "FR"] <- "FRA"

save(Ef, file = "data_explorer/1.data_prep/Combined_accession_data/Accessions_Effort.RData")

############################################