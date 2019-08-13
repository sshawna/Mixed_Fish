library(shiny)
library(shinythemes)
library(shinyalert)
library(shinyBS)
library(ggplot2)
library(RColorBrewer)
library(ggiraph)
library(ggthemes)
library(plotly)
library(shinycssloaders)
library(tidyverse)
library(viridis)
library(DT)
library(dplyr)
library(kableExtra)
library(reshape)
library(shinyWidgets)
library(googlesheets)
library(reshape2)
library(psych)
library(grid)
library(devtools)
library(Rcpp)
#install_github('ramnathv/rCharts', force= TRUE)
library(rCharts)
library(rjson)
library(leaflet)
library(rgdal)

#library(vmstools)
options(scipen=999)
YEAR<-2018
# DT table option
opt<-list( 
  dom = "Blfrtip"
  , buttons =  list(
    extend = "collection"
    , buttons = c("csv", "excel")
    , text = "Download"
  )  # end of buttons customization
  
  # customize the length menu
  , lengthMenu = list( c(10, 20, -1) # declare values
                       , c(10, 20, "All") # declare titles
  ) # end of lengthMenu customization
  , pageLength = 10
  
  
) # end of option

data_fish <-  read.csv(file="data/Hackathon/Data.csv") 
MetierDes<-read.csv("data/ICESadvice/MetierDescription.csv")
CelticEcoSpecies <- read.csv("data/CaCS.csv")
test <- aggregate(CelticEcoSpecies$Landings, by = list(CelticEcoSpecies$Country
                                                       , CelticEcoSpecies$Year, CelticEcoSpecies$lvl4, CelticEcoSpecies$Species), FUN = "sum")
names(test) <- c("Country", "Year", "Metier", "Species", "Landings")
testArea <- aggregate(CelticEcoSpecies$Landings, by = list(CelticEcoSpecies$Country
                                                       , CelticEcoSpecies$Year, CelticEcoSpecies$lvl4, CelticEcoSpecies$Species,CelticEcoSpecies$Area), FUN = "sum")
names(testArea) <- c("Country", "Year", "Metier", "Species","Area" ,"Landings")
testL2 <- as.data.frame(CelticEcoSpecies %>% group_by(Country, Year, lvl4, Species, Area,MeshSize)
                        %>% summarise(Weight_in_tonnes = sum(Landings), Value_in_Euros = sum(Value)))
testL2$vpKG <- testL2$Value_in_Euros / (testL2$Weight_in_tonnes * 1000)
names(testL2) <- c("Country", "Year", "Metier", "Species", "Area","Mesh Size", "Landings", "Value_in_Euros", "Price_per_KG")
testL2$Landings <- round(testL2$Landings, 3)
testL2$Value_in_Euros <- round(testL2$Value_in_Euros, 3)
testL2$Price_per_KG <- round(testL2$Price_per_KG, 3)
priceL2 <- aggregate(testL2$Price_per_KG, by = list(testL2$Year, testL2$Metier, testL2$Species), FUN = "mean")
names(priceL2) <- c("Year", "Metier", "Species", "Price_per_KG")
CelticCE <- read.csv("data/EfCS.csv")
CelticCE<- CelticCE[-1]
CelticCE$Vessel_length <- factor(CelticCE$Vessel_length, levels = c("<10", "10<24", "24<40", ">=40", "all"))
testE <- aggregate(CelticCE$kw_days,
                   by = list(CelticCE$Country, CelticCE$Year, CelticCE$lvl4, CelticCE$Vessel_length), FUN = "sum")
names(testE) <- c("Country", "Year", "Metier", "Vessel_length", "KW_Day")
testE2 <- aggregate(CelticCE$kw_days, by = list(CelticCE$Country, 
                                                CelticCE$Year, CelticCE$lvl4, CelticCE$Vessel_length, CelticCE$Area), FUN = "sum")
names(testE2) <- c("Country", "Year", "Metier", "Vessel_length", "Area", "KW_Day")
sp <- c("Cod","Haddock","Whiting","Plaice", "Sole", "Hake", "Megrim", "Anglerfish", "Nephrops")
species <- c("Cod","Haddock","Whiting", "Sole", "Hake", "Megrim", "Anglerfish/Monkfish")
DataMethods<-read.csv("data/ICESadvice/DataMethods.csv")
Ldist<-readRDS("data/existing_tools/6.quota_share_app/data/Celtic_Sea/CSquotashare1.rds")
ManagementArea<-read.csv("data/ICESadvice/ManagementArea.csv")
ReferencePoints<-read.csv("data/BRef.csv")
MandData<-read.csv("data/ICESadvice/MandData.csv")
MandDataPie<-aggregate(MandData$Landings,by=list(MandData$Year,MandData$Stock),FUN=sum)
links<-read.csv("data/ICESadvice/Links.csv")
names(MandDataPie)<-c("Year","Stock","Landings")
Adecision<-read.csv("data/ICESadvice/AdviceDecision.csv")
Assessment<- read.csv("data/ICESadvice/Assessmentallyears.csv")
Advice <- read.csv("data/ICESadvice/Adviceallyears.csv")

col<-brewer.pal(n=3,"Accent")
########################################## Server ##################################################

server <- function(input, output, session) {
  
  ###########Introduction##########################
  observeEvent(input$FishGear, {
    output$I_selections <- renderUI({
      if (input$FishGear == "Who is Fishing") {
        fluidRow(column(width = 3, div(
          style = "display: inline-block;vertical-align:top; width: 175px;",
          selectInput("CountryInt", "", choices = c(
            "Select Country", "Belgium", "Denmark", "Faroe Islands", "France",
            "Germany", "Ireland", "Lithuania", "Nederlands", "Norway",
            "Spain", "United Kingdom"
          )), class = "btn-link"
        )))
      }
      else if (input$FishGear == "Description of the Fisheries") {
        fluidRow(column(width = 3, div(
          style = "display: inline-block;vertical-align:top; width: 525px;",
          selectInput("GearInt", "", choices = c(
            "Select Gear", "Otter trawl", "Nephrops-directed otter trawlers",
            "Finfish-directed otter trawlers and seiners", "Deep-water trawl fisheries",
            "Beam-trawl fisheries", "Gillnet fisheries", "Longline and line fisheries",
            "Pelagic trawls", "Other fisheries"
          )), class = "btn-link"
        )))
      }
    })
  })
  
  output$fishing <- renderUI({
    if (input$FishGear == "Who is Fishing" & input$CountryInt == "Select Country") {
      img(
        src = "images/boat.jpg", height = "400px",
        width = "500px", style = "padding-top: 7px; padding-bottom: 5px; 
        padding-right: 20px;"
      )
    }
    else if (input$FishGear == "Who is Fishing" & input$CountryInt == "Belgium") {
      div(style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;", "The Belgian fleet consists of about 33 active vessels of which about 21 fish in the Irish Sea. The majority (89%) of the vessels are > 24 m, while the remainder of the vessels are between 18 and 24 m. The Belgian fleet uses beam trawls and otter 
          trawls for rays, plaice, sole, and anglerfish. Since 2016, there is has been no targeted fisheries for sole in Division 7.a.")
    }
    else if (input$FishGear == "Who is Fishing" & input$CountryInt == "Denmark") {
      div(style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;", "Eight Danish vessels fish in 
          this ecoregion, targeting blue whiting with pelagic trawls.")
    }
    else if (input$FishGear == "Who is Fishing" & input$CountryInt == "Faroe Islands") {
      div(style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;", "Up to ten vessels 
          from the Faroe Islands operate in this ecoregion, targeting blue whiting with pelagic trawls.")
    }
    else if (input$FishGear == "Who is Fishing" & input$CountryInt == "Germany") {
      div(style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;", "About ten 
          German vessels fish in the ecoregion. This includes vessels that mainly target 
          anglerfish and hake with gillnets and longline, and about three large 
          freezer-trawlers that target mackerel.")
    }
    else if (input$FishGear == "Who is Fishing" & input$CountryInt == "Ireland") {
      div(style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;", "The Irish fishing fleet 
          is very diverse with around 1500 < 10 m and 500 >= 10 m active vessels. Small vessels (< 10 m) operate inshore, typically targeting shellfish 
          with pots or demersal fish with nets. On the shoreline, there is widespread hand gathering of periwinkles. The vessels >= 10 m target a wide 
          variety of species using several types of gear. Vessels in the 12 - 25 m length range target Nephrops using trawls on several grounds around
          Ireland and on the Porcupine Bank. Both inshore and offshore mixed demersal fisheries use trawls and seine nets to target gadoids and benthic
          species. Vessels using gillnets target hake offshore and pollack, monkfish, and cod in inshore areas. Ten beam trawlers target benthic species 
          such as megrim, anglerfish, flatfish, and rays. There are dredge fisheries for razor clams and scallops in inshore and offshore areas. 
          About 100 vessels are engaged in aquaculture related activities, including dredging for seed mussels, and mussel and oyster dredging. 
          Seventeen large (>=30 m) pelagic fishing vessels operate across the whole of the area. Vessels using pelagic trawls target mackerel, 
          horse mackerel, blue whiting, boarfish, and sprat. 
          Pelagic trawling for albacore tuna occurs in the ecoregion when the species quota has not been exhausted in the Bay of Biscay.")
    }
    else if (input$FishGear == "Who is Fishing" & input$CountryInt == "Lithuania") {
      div(style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;", "Two 
          large Lithuanian freezer trawlers target pelagic species in this ecoregion.")
    }
    else if (input$FishGear == "Who is Fishing" & input$CountryInt == "Norway") {
      div(style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;", "About 60 Norwegian vessels operate in this ecoregion. Pelagic trawlers 
          mainly target blue whiting, but also other pelagic species. 
          There is also a demersal longline fishery that mainly targets ling and blue ling.")
    }
    else if (input$FishGear == "Who is Fishing" & input$CountryInt == "Spain") {
      div(style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;", "The Spanish fleet comprises 67 vessels >24 m that operate mainly in Subarea 7 (the Porcupine and Great Sole banks) and, to a lesser degree, in Subarea 6 (west of Scotland). All of these vessels target demersal species: 
          set longlines targeting hake (44 vessels), bottom otter trawl targeting megrim, anglerfish, 
          and hake (21 vessels), and set gillnet targeting hake (2 vessels).")
    }
    
    
    else if (input$FishGear == "Who is Fishing" & input$CountryInt == "France") {
      div(style = " border-radius: 25px;background-color:#e7e1ef ;color:#525252;padding: 15px 45px;", "The French offshore fishery in the Celtic Sea (divisions 7.g and 7.h) is mostly composed 
          of bottom trawlers (18 - 35 m, around 350 vessels) targeting gadoids, Nephrops or anglerfish, megrim, and rays, with less
          than ten vessels using Danish seine. In the west of Scotland (Division 6.a) around ten bottom trawlers target both saithe 
          and deep-sea fish (at depths less than 800 m) and fewer smaller vessels target hake using longlines or nets. 
          Finally, two large pelagic trawlers target herring and mackerel,
          and one is also involved in the blue whiting fishery.")
    }
    else if (input$FishGear == "Who is Fishing" & input$CountryInt == "Nederlands") {
      div(style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;", "Around 10 - 15 large
          Dutch pelagic freezer - trawlers operate in this ecoregion, mainly 
          targeting horse mackerel and mackerel")
    }
    else if (input$FishGear == "Who is Fishing" & input$CountryInt == "United Kingdom") {
      div(style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;", HTML(
        "<b>Scotland </b  <br/>  <br/> Most fishing activity by Scottish vessels (754 boats in 2015) occurs in Subarea 6. 
        Around 62 demersal trawlers (mostly >10 m) fish for mixed gadoids and benthic species such as anglerfish and megrim.
        A small number of boats target haddock at Rockall. In inshore areas, a fleet of 164 trawlers fish mainly for 
        Nephrops - 34 of these boats are under 10 m. Pot or creel fishing is carried out by almost 400 vessels. Over 300 of these boats
        are under 10 m and target either Nephrops or lobsters and various crab species. Around 60 larger vessels (> 10 m) fish for crustaceans
        (mainly brown crab) in more offshore areas to the far north and west of Scotland. Scallop fishing is carried out by around 50 dredgers
        (mostly > 10 m) and by hand gathering (diving). Limited amounts of inshore longlining and gillnetting are also carried out. About 20
        large pelagic trawlers fish in the northern parts of the Celtic Seas ecoregion.
        In the Irish Sea, the main Scottish activity is dredging for scallops around the Isle of Man,
        performed by around 50 boats (mainly > 10 m). Pot fishing occurs along the Solway 
        Firth coast (22 vessels), and about 12 trawlers take part in the Irish Sea Nephrops fishery. 
        Trawling for Nephrops also occurs at the Porcupine Bank and in the Celtic Sea (divisions 7.c and 7.k).
        Mixed - fish trawling, longlining, and gillnetting occurs in the Celtic Sea and western English Channel (Division 7.e).
        Some boats also dredge for scallops in the western English Channel. <br/> <br/>
        <b>Nothern Ireland </b  <br/> <br/>
        The Northern Irish fleet consists of around 130 >=10 m and 180 < 10 m vessels.
        The fleet predominantly operates within divisions 7.a and 6.a. A small number of vessels target Nephrops or pelagic species in other 
        parts of the ecoregion. Within the Irish Sea, demersal trawling for Nephrops dominates the fishing effort.
        Vessels operating inshore typically target shellfish with pots, or by dredging (for king scallops) in divisions 6.a and 7.a. 
        Both trawl nets and dredge gear are used to catch queen scallops in the Irish Sea and north of Rathlin Island in Division 6.a.
        A small number of vessels trawl for haddock, hake, and (historically) cod. At present (2018), there is no permitted commercial 
        targeted fishery for cod. A pelagic and gillnet herring fishery operates in late summer early autumn in the pre- and post-spawning period.
        The gillnet fishery occurs on the western Irish Sea coastline whilst two large pelagic trawlers target herring aggregations 
        in the northern English Channel and around the Isle of Man. <br/> <br/>
        <b> England and Wales</b  <br/><br/> The largest sector in terms of vessel numbers are the potting fleets targeting non-quota 
        stocks such as crabs, lobsters (mainly in divisions 7.e, 7.f, and 7.g), and whelks in Division 7.a. The majority of these vessels
        are under 10 m in length (~ 600 vessels from a total potting sector of  ~ 700)
        although more than 50 percents are polyvalent (vessels using multiple gears).
        Vessels employing otter trawls (~ 300 vessels, around half of which are < 10 m) are mostly found in Division 7.e, 
        with additional activity in divisions 7.a and 7.f; they take a mixture of demersal 
        stocks although some target whitefish and elasmobranchs. The Nephrops fleet in Division 7.a 
        comprises around 15 vessels in the 10 - 15 m sector, with < 10 vessels under 10 m. This sector employs otter trawls that use selective gear
        to reduce whitefish bycatch. Beam - trawling activity (~ 60 vessels) is dominated by vessels longer than 15 m (~ 45 vessels), taking a mixture of flatfish and anglerfish with evidence of an increasing targeted fishery for cuttlefish in Division 7.e. Dredge fisheries, predominantly for king scallops, operate in divisions 7.e and 7.a. 
        Dredging activity occurs across all vessel sizes although there is proportionally less activity by < 15 m vessels in Division 7.a. <br/> <br/>
        <b> Isle of Man </b <br/> <br/> The main fisheries undertaken in the Isle of Man's territorial sea (12 nautical miles) are for king scallop,
        queen scallop, crab, lobster, and whelk ."
      ))
    }
    else if (input$FishGear == "Description of the Fisheries" & input$GearInt == "Select Gear") {
      img(
        src = "images/gear.jpg", height = "400px",
        width = "500px", style = "padding-top: 7px; padding-bottom: 5px; 
        padding-right: 20px;"
      )
    }
    
    else if (input$FishGear == "Description of the Fisheries" & input$GearInt == "Otter trawl") {
      div(
        style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;",
        HTML("Otter trawl is the main gear by effort used in demersal fisheries in the Celtic Sea ecoregion .
             The species caught depends on the area, depth-range habitat,
             and season fished as well as on the cod-end mesh size, but in all cases 
             the catches consist of a mixture of different species.")
        )
    }
    else if (input$FishGear == "Description of the Fisheries" & input$GearInt == "Nephrops-directed otter trawlers") {
      div(
        style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;",
        HTML("Nephrops is an important target species on discrete muddy grounds within the ecoregion. Vessels typically,
             although not exclusively, use twin- or quad-rig trawls with 80 mm cod-ends. 
             A small wanted bycatch of fish species includes cod, haddock, plaice, anglerfish,
             and to a lesser extent sole. The use of selective gears (grids, square mesh, and separator panels)
             to reduce unwanted fish bycatch has increased over time, 
             but significant discarding issues still exist on some grounds. Mixed fisheries target
             both Nephrops and finfish in the Celtic Sea using a larger mesh size (100 mm or more).")
        )
    }
    else if (input$FishGear == "Description of the Fisheries" & input$GearInt == "Finfish-directed otter trawlers and seiners") {
      div(
        style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;",
        HTML("Fish are targeted with both small (80-99 mm) and larger (> 99 mm) mesh sizes in different parts of the ecoregion,
             depending on regulation and target assemblage. Smaller mesh otter trawls and seiners are typically used to target
             a broad mixture of species, including gadoids, flatfish, and other benthic species. 
             These fisheries primarily occur within the Celtic Sea, along the slope west of Ireland and Scotland,
             and in the western English Channel. Large-mesh otter trawlers (typically 100 mm or 120 mm)
             tend to target gadoids, anglerfish, or rays.")
        )
    }
    else if (input$FishGear == "Description of the Fisheries" & input$GearInt == "Deep-water trawl fisheries") {
      div(
        style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;",
        HTML("Until 2016, deep-water trawl fisheries were conducted in ICES subareas 6 and 7, principally by France,
             with some Spanish, Irish, and Scottish participation. Trawling deeper than 800 m has been banned since December 2016. 
             This mixed deep-water trawl fishery mainly targeted roundnose grenadier, black scabbardfish, and blue ling, 
             with a bycatch mainly of smoothheads and deep-water sharks on the continental slope and offshore banks of subareas 6 and 7.")
        )
    }
    else if (input$FishGear == "Description of the Fisheries" & input$GearInt == "Beam-trawl fisheries") {
      div(
        style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;",
        HTML("Beam trawlers operate on sandy grounds in the Irish and Celtic seas and in the western English Channel. 
             The majority of the vessels use meshes in the range of 80-89 mm, and come from Belgium, the UK, and Ireland.
             In the Irish Sea, the vessels primarily target plaice and sole (although the sole fishery 
             has declined significantly in the last decade). There is also a fishery for ray species in the southern Irish Sea.
             In the Celtic Sea, the beam-trawl fishery occurs on grounds where sole, anglerfish, cuttlefish, 
             and megrim are abundant and the seabed is suitable for beam trawling. The fishery has bycatches of 
             anglerfish, cod, haddock, and whiting. In the western English Channel (Division 7.e)
             beam trawling, using 80-90 mm mesh, mainly targets sole and cuttlefish.")
        )
    }
    else if (input$FishGear == "Description of the Fisheries" & input$GearInt == "Gillnet fisheries") {
      div(
        style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;",
        HTML("The main gillnet fishery, (mainly with 120 mm mesh size) in this ecoregion targets hake along the continental slope.
             Spanish, French, UK, and Irish vessels are involved in the fishery, which typically operates at depths of 150-600 m.
             In the shallower Celtic Sea, where mesh sizes used are 120-219 mm, target species include anglerfish, flatfish, and gadoids.
             A large number of inshore gillnetters (< 12 m) are also active in the Celtic Sea ecoregion. 
             The target species and gears used tend to vary spatially and temporally. In the first quarter, the primary target
             of inshore gillnetters operating in divisions 7.g and southern 7.a is cod. Fisheries around the Irish coast
             seasonally target anglerfish, flatfish, pollack, and dogfish.
             Prior to 2006, UK, French, German, and Spanish gillnetters operated in deep waters of subareas 6 and 7 targeting hake, 
             monkfish, and deep-water sharks. This fishery stopped or seriously reduced from 2006, following EU regulation of deep-water
             gillnetting at depths below 600 m.")
        )
    }
    else if (input$FishGear == "Description of the Fisheries" & input$GearInt == "Longline and line fisheries") {
      div(
        style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;",
        HTML("Spanish-, French-, and UK-registered longliners target hake along the continental slope with bycatches of ling,
             blue ling, and other deep-water species. An English hand-line fleet operates inshore around the coast of Cornwall
             in divisions 7.e-f targeting mackerel, in an area where other fishing methods for this species are not permitted.")
        )
    }
    else if (input$FishGear == "Description of the Fisheries" & input$GearInt == "Pelagic trawls") {
      div(
        style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;",
        HTML("<b>Blue whiting </b <br/>  <br/> 
             The main fisheries target spawning and post-spawning fish west of Ireland and of Scotland.
             The fishery extends into Faroese and international waters west of the Porcupine Bank. Most of the catch (~90%)
             is taken in the first half of the year. The multi-national fleet targeting blue whiting mostly consists of large
             pelagic trawlers. Blue whiting is mainly used for fishmeal.<br/>  <br/> 
             
             <b>Herring</b <br/>  <br/> 
             The herring fishery occurs in four main parts of the ecoregion.<br/><br/>
             The fishery in Division 6.a North is conducted by single and pair RSW trawlers and by single-trawl freezer trawlers. Prior to 2006, 
             there was a fairly even distribution of effort, both temporally and spatially. The UK and Ireland are the main exploiters, but vessels
             registered to the Netherlands, Germany, and France also participate in the fishery.<br/><br/>
             In divisions 6.a South and 7.b-c, the fishery is conducted entirely by RSW pelagic trawlers and dry-hold vessels,
             both inshore and offshore on the northwestern Irish coast. In recent years, only Ireland has exploited herring in this area.
             The fishery is concentrated in quarters one and four.<br/><br/>
             The herring Division 7.a North fishery has not changed in recent years. 
             UK pelagic trawlers take the majority of catches in quarters three and four.<br/><br/>
             The main herring fishery in divisions 7.a South and 7.g-k takes place on coastal spawning grounds, 
             and on offshore feeding grounds south of Ireland. Ireland, the Netherlands, and Germany exploit this 
             fishery using two types of vessels, larger boats with RSW storage and smaller dry-hold vessels.
             <b> Boarfish</b <br/>  <br/>
             The fishery operates from September to March. Catches are generally free from bycatch
             from September to February. From March onwards a bycatch of mackerel can be found
             in the catches and the fishery generally ceases at this time. Information on the bycatch
             of other species in the boarfish fishery is sparse, bycatch numbers are thought to be minimal. 
             The fishery uses pelagic trawl nets with mesh sizes 32-54 mm.")
        )
    }
    else if (input$FishGear == "Description of the Fisheries" & input$GearInt == "Other fisheries") {
      div(
        style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;",
        HTML("Sprat fisheries often develop in the south Minch and in Irish inshore waters 
             during autumn and winter. In addition, a number of fisheries exist throughout the 
             ecoregion for stocks where ICES does not provide routine assessments or
             advice. For example, dredging for shellfish includes scallops, razor clams, cockles, clams,
             and oysters. There are also important pot and trap fisheries for crabs, lobsters, and whelks.")
        )
    }
    })
  output$MetInt <- function() {
    if(input$Area1=="West of Scotland (Division 6.a) and Rockall (Division 6.b)"){
    text_tbl <- filter(MetierDes,Area=="West of Scotland (Division 6.a) and Rockall (Division 6.b)")
    text_tbl[-1] %>%
      knitr::kable("html") %>%
      kable_styling(full_width = F, font_size = 12)%>%
      column_spec(c(1:3), background = "black", include_thead = TRUE, border_right = T) %>%
      column_spec(1,width = "25em", bold = T, border_right = T, underline = T) %>%
      column_spec(2, width = "25em", bold = T, border_right = T, underline = T) %>%
      column_spec(3, width = "15em", bold = T) %>%
      row_spec(c(1, 3, 5, 7,9,11), background = "lightyellow", color = "#525252") %>%
      row_spec(c(2, 4, 6, 8,10), background = "lightgrey")  
    }else if(input$Area1=="West of Ireland (divisions 7.b–c) and Celtic Sea slope (divisions 7.k–j)"){
      text_tbl <- filter(MetierDes,Area=="West of Ireland (divisions 7.b–c) and Celtic Sea slope (divisions 7.k–j)")
      text_tbl[-1] %>%
        knitr::kable("html") %>%
        kable_styling(full_width = F, font_size = 12)%>%
        column_spec(c(1:3), background = "black", include_thead = TRUE, border_right = T) %>%
        column_spec(1,width = "25em", bold = T, border_right = T, underline = T) %>%
        column_spec(2, width = "25em", bold = T, border_right = T, underline = T) %>%
        column_spec(3, width = "15em", bold = T) %>%
        row_spec(c(1, 3, 5, 7,9), background = "lightyellow", color = "#525252") %>%
        row_spec(c(2, 4, 6, 8), background = "lightgrey")  
    }
    else if(input$Area1=="Irish Sea (Division 7.a)"){
      text_tbl <- filter(MetierDes,Area=="Irish Sea (Division 7.a)")
      text_tbl[-1] %>%
        knitr::kable("html") %>%
        kable_styling(full_width = F, font_size = 12)%>%
        column_spec(c(1:3), background = "black", include_thead = TRUE, border_right = T) %>%
        column_spec(1,width = "25em", bold = T, border_right = T, underline = T) %>%
        column_spec(2, width = "25em", bold = T, border_right = T, underline = T) %>%
        column_spec(3, width = "15em", bold = T) %>%
        row_spec(c(1, 3, 5, 7), background = "lightyellow", color = "#525252") %>%
        row_spec(c(2, 4, 6, 8), background = "lightgrey") 
    }}
  
  output$Metierdesc<- renderUI({
    if(input$Area1 =="Select Area") {
      img(src = "images/net.jpg", height = "400px",
        width = "500px", style = "padding-top: 7px; padding-bottom: 5px; 
        padding-right: 20px;"
      )
    }
    else{tableOutput("MetInt")
      
    }
  })
    
  
  
 
  
  
  
  
  #########About botton#######
  observeEvent(input$about, {
    shinyalert(
      title = "Mixed Fisheries",
      text = "Vizualization Tool for Mixed Fisheries Landings and Effort in Celtic Seas Ecoregion. 
      <br> Try changing the filters on the panel to compare different <b>Metier</b> and <b>Species</b> by <b>years</b> .",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#addd8e",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  
  ###########Landings##########################
  ###############Page1#########################
  observeEvent(input$info1, {
    shinyalert(text = "Vizualization of Landings Proportion in Celtic Seas Ecoregion. 
               <br> The filter elements on the plot  will be animated upon mouse over. By clicking the mouse accociated Landings proportion will be summarized in KGs for <b>Metier</b> and <b>Species</b> by <b>years</b> .",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE,
               type = "info",
               showConfirmButton = TRUE,
               showCancelButton = FALSE,
               confirmButtonText = "OK",
               confirmButtonCol = "#addd8e",
               timer = 0,
               imageUrl = "",
               animation = TRUE
    )
  })  
  
  observeEvent(input$name, {
    output$L_selections <- renderUI({
      if (input$Country == "All" & input$name == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL", label = "Select Metier", levels(test$Metier), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "BEL" & input$name == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL", label = "Select Metier", levels(droplevels(filter(test, Country == "BEL")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "DE" & input$name == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL", label = "Select Metier", levels(droplevels(filter(test, Country == "DE")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "ES" & input$name == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL", label = "Select Metier", levels(droplevels(filter(test, Country == "ES")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "FRA" & input$name == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL", label = "Select Metier", levels(droplevels(filter(test, Country == "FRA")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "GG" & input$name == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL", label = "Select Metier", levels(droplevels(filter(test, Country == "GG")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "IE" & input$name == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL", label = "Select Metier", levels(droplevels(filter(test, Country == "IE")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "JE" & input$name == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL", label = "Select Metier", levels(droplevels(filter(test, Country == "JE")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "NLD" & input$name == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL", label = "Select Metier", levels(droplevels(filter(test, Country == "NLD")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "UK" & input$name == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL", label = "Select Metier", levels(droplevels(filter(test, Country == "UK")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "All" & input$name == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL1", label = "Select Species", levels(test$Species), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "BEL" & input$name == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL1", label = "Select Species", levels(droplevels(filter(test, Country == "BEL")$Species)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "DE" & input$name == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL1", label = "Select Species", levels(droplevels(filter(test, Country == "DE")$Species)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "ES" & input$name == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL1", label = "Select Species", levels(droplevels(filter(test, Country == "ES")$Species)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "FRA" & input$name == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL1", label = "Select Species", levels(droplevels(filter(test, Country == "FRA")$Species)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "GG" & input$name == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL1", label = "Select Species", levels(droplevels(filter(test, Country == "GG")$Species)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "IE" & input$name == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL1", label = "Select Species", levels(droplevels(filter(test, Country == "IE")$Species)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "JE" & input$name == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL1", label = "Select Species", levels(droplevels(filter(test, Country == "JE")$Species)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "NLD" & input$name == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL1", label = "Select Species", levels(droplevels(filter(test, Country == "NLD")$Species)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$Country == "UK" & input$name == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieL1", label = "Select Species", levels(droplevels(filter(test, Country == "UK")$Species)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryear1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
    })
  })
  
  f <- reactive({
    filter(test, Country == input$Country)
  })
  output$plotL1 <-
    renderPlotly({
      if (input$name == 1) {
        if(input$Country!="All"){
        p <- ggplot(f(), aes(Year, Landings, fill = Species)) +
          geom_bar(stat = "identity", position = "fill") +
          ggtitle("The proportion of each landed species by level 5 metier.") +
          ylab("") +
          xlab("Year") + scale_x_continuous(breaks = test$Year) +
          theme(
            legend.position = "bottom", legend.text = element_text(size = 6),
            strip.background = element_blank(), axis.text.x = element_text(
              angle = 90,
              hjust = 1
            ), axis.text = element_text(size = 6), panel.spacing.x = unit(0.05, "lines"), panel.spacing.y = unit(0.5, "lines")
          )
        p <- p + facet_wrap(. ~ Metier)
        p <- p + guides(fill = guide_legend(nrow = 2, byrow = T))
        ggplotly(p) %>%
          layout(legend = list(orientation = "h", x = 0.3, y = -0.2, bgcolor = "grey", xanchor = "center"))
        }
        else{  p <- ggplot(test, aes(Year, Landings, fill = Species)) +
          geom_bar(stat = "identity", position = "fill") +
          ggtitle("The proportion of each landed species by level 5 metier.") +
          ylab("") +
          xlab("Year") + scale_x_continuous(breaks = test$Year) +
          theme(
            legend.position = "bottom", legend.text = element_text(size = 6),
            strip.background = element_blank(), axis.text.x = element_text(
              angle = 90,
              hjust = 1
            ), axis.text = element_text(size = 6), panel.spacing.x = unit(0.05, "lines"), panel.spacing.y = unit(0.5, "lines")
          )
        p <- p + facet_wrap(. ~ Metier)
        p <- p + guides(fill = guide_legend(nrow = 2, byrow = T))
        ggplotly(p) %>%
          layout(legend = list(orientation = "h", x = 0.3, y = -0.2, bgcolor = "grey", xanchor = "center"))}
      }
      else if (input$name == 2) {
        if(input$Country!="All"){
        p <- ggplot(f(), aes(Year, Landings, fill = Metier)) +
          geom_bar(stat = "identity", position = "fill") +
          ggtitle("The proportion of each landed species by level 5 métier.") +
          ylab("") +
          xlab("Year") + scale_x_continuous(breaks = test$Year) +
          theme(
            legend.position = "bottom", legend.text = element_text(size = 6), axis.text = element_text(size = 6),
            panel.spacing.x = unit(0.05, "lines"), panel.spacing.y = unit(0.5, "lines"),
            strip.background = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)
          )
        p <- p + facet_wrap(. ~ Species)
        ggplotly(p) %>%
          layout(xaxis = list(hoverformat = ".2f"), legend = list(
            orientation = "h",
            x = 0.3, y = -0.2, bgcolor = "grey", xanchor = "center"
          )) %>%
          style(legendgroup = NULL)
        }
        else{ p <- ggplot(test, aes(Year, Landings, fill = Metier)) +
          geom_bar(stat = "identity", position = "fill") +
          ggtitle("The proportion of each landed species by level 5 métier.") +
          ylab("") +
          xlab("Year") + scale_x_continuous(breaks = test$Year) +
          theme(
            legend.position = "bottom", legend.text = element_text(size = 6), axis.text = element_text(size = 6),
            panel.spacing.x = unit(0.05, "lines"), panel.spacing.y = unit(0.5, "lines"),
            strip.background = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)
          )
        p <- p + facet_wrap(. ~ Species)
        ggplotly(p) %>%
          layout(xaxis = list(hoverformat = ".2f"), legend = list(
            orientation = "h",
            x = 0.3, y = -0.2, bgcolor = "grey", xanchor = "center"
          )) %>%
          style(legendgroup = NULL)}}
    })
  
  f1 <- reactive({
    if(input$Country!="All"){
    filter(test, Country == input$Country, Year == input$pieslideryear & Metier == input$pieL)}
    else{filter(test, Year == input$pieslideryear & Metier == input$pieL)}
  })
  f2 <- reactive({
    if(input$Country!="All"){
    filter(test, Country == input$Country, Year == input$pieslideryear1 & Species == input$pieL1)}
    else{filter(test,Year == input$pieslideryear1 & Species == input$pieL1)}
  })
  output$pie1Plot <-
    renderPlotly({
      if (input$name == 1) {
        plot_ly() %>%
          add_pie(data = f1(), labels = ~Species, values = ~Landings) %>%
          layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            legend = list(list(x = 0.35, y = 0.5))
          )
        }
      else if (input$name == 2) {
        plot_ly() %>%
          add_pie(data = f2(), labels = ~Metier, values = ~Landings) %>%
          layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            legend = list(list(x = 0.35, y = 0.5))
          )
      }
    })
  
  output$pieUI <- renderUI({
    if (input$name == 1) {
      if (dim(f1())[1] == 0) {
        h4(paste("No data available for", input$Country, ":", input$pieL, "in", input$pieslideryear, sep = " "))
      }
      else {
        plotlyOutput("pie1Plot")
      }
    }
    else if (input$name == 2) {
      if (dim(f2())[1] == 0) {
        h4(paste("No data available for", input$Country, ":", input$pieL1, "in", input$pieslideryear1, sep = " "))
      }
      else {
        plotlyOutput("pie1Plot")
      }
    }
  })
  
  ###############Page2#########################
  observeEvent(input$info2, {
    shinyalert(text = "Vizualization of Total Landings (KGs) in Celtic Seas Ecoregion. 
               <br> On the left hand side  is the total landings of species based on the selected <b>Metier</b> and <b>Year</b>.
               <br> On the right hand side  is the total landings of metier based on the selected <b>Species</b> and <b>Year</b>.
               The filter elements on the plots  will be animated upon mouse over and summarized in a table by clicking on the selection .",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE,
               type = "info",
               showConfirmButton = TRUE,
               showCancelButton = FALSE,
               confirmButtonText = "OK",
               confirmButtonCol = "#addd8e",
               timer = 0,
               imageUrl = "",
               animation = TRUE
    )
  })
  
  selected_page21 <- reactive({
    input$plotL21_selected
  })
  
  test1 <- reactive({
    filter(testL2, Year == input$set2 & Metier == input$set1)
  })
  price1 <- reactive({
    filter(priceL2, Year == input$set2 & Metier == input$set1)
  })
  
  output$plotL21 <- renderggiraph({
    if (input$Landings1 == "Weight in tonnes") {
      gg <- ggplot(test1(), aes(x = Species, y = Landings, fill = Species)) +
        geom_bar_interactive(
          stat = "identity",
          aes(data_id = test1()$Species, tooltip = test1()$Species)
        ) +
        theme_grey(base_size = 16) + ylab("Total Landings in tonnes") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          strip.background = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)
        )
      x <- girafe(code = print(gg), width_svg = 12, height_svg = 10)
      x <- girafe_options(
        x, opts_selection(
          type = "multiple", css = "fill:#FF3333;stroke:black;"
        ),
        opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;")
      )
      x
    }
    else if (input$Landings1 == "Value in Euros") {
      gg <- ggplot(test1(), aes(x = Species, y = Value_in_Euros, fill = Species)) +
        geom_bar_interactive(
          stat = "identity",
          aes(data_id = test1()$Species, tooltip = test1()$Species)
        ) +
        theme_grey(base_size = 16) + ylab("Value in Euros") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          strip.background = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)
        )
      x <- girafe(code = print(gg), width_svg = 12, height_svg = 10)
      x <- girafe_options(
        x, opts_selection(
          type = "multiple", css = "fill:#FF3333;stroke:black;"
        ),
        opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;")
      )
      x
    }
    else if (input$Landings1 == "Price per KG") {
      gg <- ggplot(price1(), aes(x = Species, y = Price_per_KG, fill = Species)) +
        geom_bar_interactive(
          stat = "identity",
          aes(data_id = price1()$Species, tooltip = price1()$Species)
        ) +
        theme_grey(base_size = 16) + ylab("Average Price per KG") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          strip.background = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)
        )
      x <- girafe(code = print(gg), width_svg = 12, height_svg = 10)
      x <- girafe_options(
        x, opts_selection(
          type = "multiple", css = "fill:#FF3333;stroke:black;"
        ),
        opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;")
      )
      x
    }
  })
  
  observeEvent(input$reset1, {
    if (input$Landings1 == "Weight in tonnes") {
      session$sendCustomMessage(type = "plotL21_set", message = character(0))
    }
    else if (input$Landings1 == "Value in Euros") {
      session$sendCustomMessage(type = "plotL21_set", message = character(0))
    }
    else if (input$Landings1 == "Price per KG") {
      session$sendCustomMessage(type = "plotL21_set", message = character(0))
    }
  })
  
  output$tabplotL21 <- renderDataTable({
    if (input$Landings1 == "Weight in tonnes") {
      out <- test1()[test1()$Species %in% selected_page21(), ][-c(2, 3, 8, 9)]
      if (nrow(out) < 1) {
        return(NULL)
      }
      row.names(out) <- NULL
      colnames(out) <- c("Country", "Species", "Area","Mesh Size", "Landings in tonnes")
      datatable(out,  extensions = 'Buttons'
                , options = opt)
    }
    else if (input$Landings1 == "Value in Euros") {
      out <- test1()[test1()$Species %in% selected_page21(), ][-c(2, 3, 7, 9)]
      if (nrow(out) < 1) {
        return(NULL)
      }
      row.names(out) <- NULL
      colnames(out) <- c("Country", "Species", "Area", "Mesh Size","Value in Euros")
      datatable(out, extensions = 'Buttons'
                , options = opt)
    }
    else if (input$Landings1 == "Price per KG") {
      out <- test1()[test1()$Species %in% selected_page21(), ][-c(2, 3, 7, 8)]
      if (nrow(out) < 1) {
        return(NULL)
      }
      row.names(out) <- NULL
      colnames(out) <- c("Country", "Species", "Area", "Mesh Size","Price per KG")
      datatable(out, extensions = 'Buttons'
                , options = opt)
    }
  })
  
 
  output$LbySpec <- renderUI({
    if (dim(test1())[1] == 0) {
      h3(paste("No data available for ", input$set1, "in", input$set2, sep = " "))
    }
    else {
      list(
        column(
          width = 7,
          ggiraph::ggiraphOutput("plotL21", width = "100%")
        ),
        column(
          width = 3, h4("Selected Species"),
          dataTableOutput("tabplotL21"),
          actionButton("reset1", label = "Reset selection")
        )
      )
    }
  })
  
  selected_pageL22 <- reactive({
    input$plotL22_selected
  })
  
  test2 <- reactive({
    filter(testL2, Year == input$set4 & Species == input$set3)
  })
  price2 <- reactive({
    filter(priceL2, Year == input$set4 & Species == input$set3)
  })
  output$plotL22 <- renderggiraph({
    if (input$Landings2 == "Weight in tonnes") {
      gg <- ggplot(test2(), aes(x = Metier, y = Landings, fill = Metier)) +
        geom_bar_interactive(
          stat = "identity",
          aes(data_id = test2()$Metier, tooltip = test2()$Metier)
        ) +
        theme_grey(base_size = 16) + ylab("Total Landings in tonnes") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          strip.background = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)
        )
      x <- girafe(code = print(gg), width_svg = 12, height_svg = 10)
      x <- girafe_options(
        x, opts_selection(
          type = "multiple", css = "fill:#FF3333;stroke:black;"
        ),
        opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;")
      )
      x
    }
    else if (input$Landings2 == "Value in Euros") {
      gg <- ggplot(test2(), aes(x = Metier, y = Value_in_Euros, fill = Metier)) +
        geom_bar_interactive(
          stat = "identity",
          aes(data_id = test2()$Metier, tooltip = test2()$Metier)
        ) +
        theme_grey(base_size = 16) + ylab("Value in Euros") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          strip.background = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)
        )
      x <- girafe(code = print(gg), width_svg = 12, height_svg = 10)
      x <- girafe_options(
        x, opts_selection(
          type = "multiple", css = "fill:#FF3333;stroke:black;"
        ),
        opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;")
      )
      x
    }
    else if (input$Landings2 == "Price per KG") {
      gg <- ggplot(price2(), aes(x = Metier, y = Price_per_KG, fill = Metier)) +
        geom_bar_interactive(
          stat = "identity",
          aes(data_id = price2()$Metier, tooltip = price2()$Metier)
        ) +
        theme_grey(base_size = 16) + ylab("Average Price per KG") +
        viridis::scale_fill_viridis(discrete = TRUE) +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          strip.background = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)
        )
      x <- girafe(code = print(gg), width_svg = 12, height_svg = 10)
      x <- girafe_options(
        x, opts_selection(
          type = "multiple", css = "fill:#FF3333;stroke:black;"
        ),
        opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;")
      )
      x
    }
  })
  
  observeEvent(input$reset2, {
    if (input$Landings2 == "Weight in tonnes") {
      session$sendCustomMessage(type = "plotL22_set", message = character(0))
    }
    else if (input$Landings2 == "Value in Euros") {
      session$sendCustomMessage(type = "plotL22_set", message = character(0))
    }
    else if (input$Landings2 == "Price per KG") {
      session$sendCustomMessage(type = "plotL22_set", message = character(0))
    }
  })
  
  output$tabplotL22 <- renderDataTable({
    if (input$Landings2 == "Weight in tonnes") {
      out <- test2()[test2()$Metier %in% selected_pageL22(), ][-c(2, 4, 8, 9)]
      if (nrow(out) < 1) {
        return(NULL)
      }
      row.names(out) <- NULL
      colnames(out) <- c("Country", "Metier", "Area","Mesh Size", "Landings in tonnes")
      datatable(out, extensions = 'Buttons'
                , options = opt) # end of option)
    }
    else if (input$Landings2 == "Value in Euros") {
      out <- test2()[test2()$Metier %in% selected_pageL22(), ][-c(2, 4, 7, 9)]
      if (nrow(out) < 1) {
        return(NULL)
      }
      row.names(out) <- NULL
      colnames(out) <- c("Country", "Metier", "Area","Mesh Size", "Value in Euros")
      datatable(out, extensions = 'Buttons'
                , options = opt) # end of option)
    }
    else if (input$Landings2 == "Price per KG") {
      out <- test2()[test2()$Metier %in% selected_pageL22(), ][-c(2, 4, 7, 8)]
      if (nrow(out) < 1) {
        return(NULL)
      }
      row.names(out) <- NULL
      colnames(out) <- c("Country", "Metier", "Area","Mesh Size" ,"Price per KG")
      datatable(out, extensions = 'Buttons'
                , options = opt# end of option
      )
    }
  })
  
  output$LbyMet <- renderUI({
    if (dim(test2())[1] == 0) {
      h3(paste("No data available for", input$set3, "in", input$set4, sep = " "))
    }
    else {
      list(column(width = 7, ggiraph::ggiraphOutput("plotL22", width = "100%")), column(
        width = 3, h4("Selected Metier"),
        dataTableOutput("tabplotL22"),
        actionButton("reset2", label = "Reset selection")
      ))
    }
  })
  
  
  
  output$Lpage2 <- renderUI({
    if (input$LP2tabset == "Selection 1") {
      uiOutput("LbySpec")
    }
    
    else if (input$LP2tabset == "Selection 2") {
      uiOutput("LbyMet")
    }
  })
  
  ###############Page3#########################
  output$tableL <- DT::renderDataTable(DT::datatable({
    L <- CelticEcoSpecies[-c(1,2,3)]
    if (input$LCountry != "All") {
      L <- filter(L, Country %in% input$LCountry)
    }
    if (input$LYear != "All") {
      L <- filter(L, Year %in% input$LYear)
    }
    if (input$LMetier != "All") {
      L <- filter(L, lvl4 %in% input$LMetier)
    }
    if (input$LSpecies != "All") {
      L <- filter(L, Species %in% input$LSpecies)
    }
    if (input$LArea != "All") {
      L <- filter(L, Area %in% input$LArea)
    }
    L
  }, extensions = 'Buttons'
  , options = opt)) # end of option))
  
  
  
  
  
  
  ##########Efforts##############
  ###############Page1#################
  observeEvent(input$info3, {
    shinyalert(text = "Vizualization of Effort Proportion in Celtic Seas Ecoregion. 
               <br> The filter elements on the plot  will be animated upon mouse over. By clicking the mouse accociated Effort proportion will be summarized in KW_days for <b>Metier</b> and <b>Vessel Length</b> by <b>years</b> .",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE,
               type = "info",
               showConfirmButton = TRUE,
               showCancelButton = FALSE,
               confirmButtonText = "OK",
               confirmButtonCol = "#addd8e",
               timer = 0,
               imageUrl = "",
               animation = TRUE
    )
  })
  
  observeEvent(input$nameE, {
    output$E_selections <- renderUI({
      if (input$CountryE == "All" & input$nameE == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE", label = "Select Metier", levels(testE$Metier), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "BEL" & input$nameE == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE", label = "Select Metier", levels(droplevels(filter(testE, Country == "BEL")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "DE" & input$nameE == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE", label = "Select Metier", levels(droplevels(filter(testE, Country == "DE")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "ES" & input$nameE == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE", label = "Select Metier", levels(droplevels(filter(testE, Country == "ES")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "FRA" & input$nameE == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE", label = "Select Metier", levels(droplevels(filter(testE, Country == "FRA")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "GG" & input$nameE == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE", label = "Select Metier", levels(droplevels(filter(testE, Country == "GG")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "IE" & input$nameE == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE", label = "Select Metier", levels(droplevels(filter(testE, Country == "IE")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      
      else if (input$CountryE == "IM" & input$nameE == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE", label = "Select Metier", levels(droplevels(filter(testE, Country == "IM")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "JE" & input$nameE == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE", label = "Select Metier", levels(droplevels(filter(testE, Country == "JE")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "NLD" & input$nameE == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE", label = "Select Metier", levels(droplevels(filter(testE, Country == "NLD")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "UK" & input$nameE == 1) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE", label = "Select Metier", levels(droplevels(filter(testE, Country == "UK")$Metier)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "All" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Vessel length", levels(testE$Vessel_length), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "BEL" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Vessel length", levels(droplevels(filter(testE, Country == "BEL")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "DE" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Vessel length", levels(droplevels(filter(testE, Country == "DE")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "ES" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Vessel length", levels(droplevels(filter(testE, Country == "ES")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "FRA" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Vessel length", levels(droplevels(filter(testE, Country == "FRA")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "GG" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Vessel length", levels(droplevels(filter(testE, Country == "GG")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "IE" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Vessel length", levels(droplevels(filter(testE, Country == "IE")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "IM" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Vessel length", levels(droplevels(filter(testE, Country == "IM")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "JE" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Vessel length", levels(droplevels(filter(testE, Country == "JE")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "NLD" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Vessel length", levels(droplevels(filter(testE, Country == "NLD")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "UK" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Vessel length", levels(droplevels(filter(testE, Country == "UK")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
    })
  })
  
  fE <- reactive({
    filter(testE, Country == input$CountryE)
  })
  
  output$plotE1 <-
    renderPlotly({
      if (input$nameE == 1) {
        if(input$CountryE!="All"){
        p <- ggplot(fE(), aes(Year, KW_Day, fill = Vessel_length)) +
          geom_bar(stat = "identity", position = "fill") +
          ggtitle("The proportion of Vessel type Effort by level 5 métier.") +
          # ylab("The proportion of total Effort") +
          xlab("Year") + scale_x_continuous(breaks = testE$Year) +
          theme(
            legend.position = "bottom", legend.text = element_text(size = 6),
            strip.background = element_blank(), axis.text.x = element_text(
              angle = 90,
              hjust = 1
            ), axis.text = element_text(size = 6), panel.spacing.x = unit(0.05, "lines"), panel.spacing.y = unit(0.5, "lines")
          )
        p <- p + facet_wrap(. ~ Metier)
        p <- p + guides(fill = guide_legend(nrow = 2, byrow = T))
        ggplotly(p) %>%
          layout(legend = list(orientation = "h", x = 0.3, y = -0.2, bgcolor = "grey", xanchor = "center"))
        }
        else{p <- ggplot(testE, aes(Year, KW_Day, fill = Vessel_length)) +
          geom_bar(stat = "identity", position = "fill") +
          ggtitle("The proportion of Vessel type Effort by level 5 métier.") +
          # ylab("The proportion of total Effort") +
          xlab("Year") + scale_x_continuous(breaks = testE$Year) +
          theme(
            legend.position = "bottom", legend.text = element_text(size = 6),
            strip.background = element_blank(), axis.text.x = element_text(
              angle = 90,
              hjust = 1
            ), axis.text = element_text(size = 6), panel.spacing.x = unit(0.05, "lines"), panel.spacing.y = unit(0.5, "lines")
          )
        p <- p + facet_wrap(. ~ Metier)
        p <- p + guides(fill = guide_legend(nrow = 2, byrow = T))
        ggplotly(p) %>%
          layout(legend = list(orientation = "h", x = 0.3, y = -0.2, bgcolor = "grey", xanchor = "center"))}}
      else if (input$nameE == 2) {
        if(input$CountryE!="All"){
        p <- ggplot(fE(), aes(Year, KW_Day, fill = Metier)) +
          geom_bar(stat = "identity", position = "fill") +
          ggtitle("The proportion of each level 5 métier  Effort by Vessel Length.") +
          ylab("The proportion of total Effort") +
          xlab("Year") + scale_x_continuous(breaks = testE$Year) +
          theme(
            legend.position = "bottom", legend.text = element_text(size = 6), axis.text = element_text(size = 6), panel.spacing.x = unit(0.05, "lines"), panel.spacing.y = unit(0.5, "lines"),
            strip.background = element_blank(), axis.text.x = element_text(
              angle = 90,
              hjust = 1
            )
          )
        p <- p + facet_wrap(. ~ Vessel_length)
        ggplotly(p) %>%
          layout(xaxis = list(hoverformat = ".2f"), legend = list(orientation = "h", x = 0.3, y = -0.2, bgcolor = "grey", xanchor = "center")) %>%
          style(legendgroup = NULL)
        }
        else{ p <- ggplot(testE, aes(Year, KW_Day, fill = Metier)) +
          geom_bar(stat = "identity", position = "fill") +
          ggtitle("The proportion of each level 5 métier  Effort by Vessel Length.") +
          ylab("The proportion of total Effort") +
          xlab("Year") + scale_x_continuous(breaks = testE$Year) +
          theme(
            legend.position = "bottom", legend.text = element_text(size = 6), axis.text = element_text(size = 6), panel.spacing.x = unit(0.05, "lines"), panel.spacing.y = unit(0.5, "lines"),
            strip.background = element_blank(), axis.text.x = element_text(
              angle = 90,
              hjust = 1
            )
          )
        p <- p + facet_wrap(. ~ Vessel_length)
        ggplotly(p) %>%
          layout(xaxis = list(hoverformat = ".2f"), legend = list(orientation = "h", x = 0.3, y = -0.2, bgcolor = "grey", xanchor = "center")) %>%
          style(legendgroup = NULL)}}
    })
  
  f3 <- reactive({
    if(input$CountryE!="All"){
    filter(testE2, Country == input$CountryE, Year == input$pieslideryearE & Metier == input$pieE)}
    else{filter(testE2, Year == input$pieslideryearE & Metier == input$pieE)}
  })
  f4 <- reactive({
    if(input$CountryE!="All"){
    filter(testE2, Country == input$CountryE, Year == input$pieslideryearE1 & Vessel_length == input$pieE1)}
    else{ filter(testE2, Year == input$pieslideryearE1 & Vessel_length == input$pieE1)}
  })
  
  
  output$pieE1Plot <-
    renderPlotly({
      if (input$nameE == 1) {
        plot_ly() %>%
          add_pie(data = f3(), labels = ~Vessel_length, values = ~KW_Day) %>%
          layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            legend = list(list(x = 0.35, y = 0.5))
          )
      }
      else if (input$nameE == 2) {
        plot_ly() %>%
          add_pie(data = f4(), labels = ~Metier, values = ~KW_Day) %>%
          layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            legend = list(list(x = 0.35, y = 0.5))
          )
      }
    })
  
  
  
  output$pieEUI <- renderUI({
    if (input$nameE == 1) {
      if (dim(f3())[1] == 0) {
        h4(paste("No data available for",input$CountryE, ":",input$pieE, "in", input$pieslideryearE, sep = " "))
      }
      else {
        plotlyOutput("pieE1Plot")
      }
    }
    else if (input$nameE == 2) {
      if (dim(f4())[1] == 0) {
        h4(paste("No data available for",input$CountryE, ":", input$pieE1, "in", input$pieslideryearE1, sep = " "))
      }
      else {
        plotlyOutput("pieE1Plot")
      }
    }
  })
  
  
  ###############Page2#################
  observeEvent(input$info4, {
    shinyalert(text = "Vizualization of Total Effort ( KW_days) in Celtic Seas Ecoregion. 
               <br> On the left hand side  is the total effort of different vessel length  based on the selected <b>Metier</b> and <b>Year</b>.
               <br> On the right hand side  is the total effort of metier based on the selected <b>Vessel Length</b> and <b>Year</b>.
               The filter elements on the plots  will be animated upon mouse over and summarized in a table by clicking on the selection .",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE,
               type = "info",
               showConfirmButton = TRUE,
               showCancelButton = FALSE,
               confirmButtonText = "OK",
               confirmButtonCol = "#addd8e",
               timer = 0,
               imageUrl = "",
               animation = TRUE
    )
  })
  
  selected_pageE21 <- reactive({
    input$plotE21_selected
  })
  
  
  
  testE11 <- reactive({
    filter(testE2, Year == input$setE2 & Metier == input$setE1)
  })
  
  output$plotE21 <- renderggiraph({
    gg <- ggplot(testE11(), aes(x = Vessel_length, y = KW_Day, fill = Vessel_length)) +
      geom_bar_interactive(
        stat = "identity",
        aes(data_id = testE11()$Vessel_length, tooltip = testE11()$Vessel_length)
      ) +
      theme_grey(base_size = 16) + ylab("Total Effort in KW_day") +
      viridis::scale_fill_viridis(discrete = TRUE) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        strip.background = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)
      )
    x <- girafe(code = print(gg), width_svg = 12, height_svg = 10)
    x <- girafe_options(
      x, opts_selection(
        type = "multiple", css = "fill:#FF3333;stroke:black;"
      ),
      opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;")
    )
    x
  })
  
  observeEvent(input$resetE1, {
    session$sendCustomMessage(type = "plotE21_set", message = character(0))
  })
  
  output$tabplotE21 <- renderDataTable({
    out <- testE11()[testE11()$Vessel_length %in% selected_pageE21(), ][-c(2, 3)]
    if (nrow(out) < 1) {
      return(NULL)
    }
    row.names(out) <- NULL
    datatable(out, extensions = 'Buttons'
              , options = opt)
  })
  
  
  
  output$EbyLength <- renderUI({
    if (dim(testE11())[1] == 0) {
      h3(paste("No data available for ", input$setE1, "in", input$setE2, sep = " "))
    }
    else {
      list(
        column(
          width = 7,
          ggiraph::ggiraphOutput("plotE21")
        ),
        column(
          width = 3,
          h4("Selected Vessel length"),
          dataTableOutput("tabplotE21"),
          actionButton("resetE1", label = "Reset selection")
        )
      )
    }
  })
  
  selected_pageE22 <- reactive({
    input$plotE22_selected
  })
  
  testE12 <- reactive({
    filter(testE2, Year == input$setE4 & Vessel_length == input$setE3)
  })
  
  output$plotE22 <- renderggiraph({
    gg <- ggplot(testE12(), aes(x = Metier, y = KW_Day, fill = Metier)) +
      geom_bar_interactive(
        stat = "identity",
        aes(data_id = testE12()$Metier, tooltip = testE12()$Metier)
      ) +
      theme_grey(base_size = 16) + ylab("Total Effort in KW_day") +
      viridis::scale_fill_viridis(discrete = TRUE) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        strip.background = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)
      )
    x <- girafe(code = print(gg), width_svg = 12, height_svg = 10)
    x <- girafe_options(
      x, opts_selection(
        type = "multiple", css = "fill:#FF3333;stroke:black;"
      ),
      opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;")
    )
    x
  })
  
  observeEvent(input$resetE2, {
    session$sendCustomMessage(type = "plotE22_set", message = character(0))
  })
  
  output$ tabplotE22 <- renderDataTable({
    out <- testE12()[testE12()$Metier %in% selected_pageE22(), ][-c(2, 4)]
    if (nrow(out) < 1) {
      return(NULL)
    }
    row.names(out) <- NULL
    datatable(out, extensions = 'Buttons'
              , options = opt)
  })
  
  
  
  
  
  output$EbyMet <- renderUI({
    if (dim(testE12())[1] == 0) {
      h3(paste("No data available for", input$setE3, "in", input$setE4, sep = " "))
    }
    else {
      list(column(
        width = 7,
        ggiraph::ggiraphOutput("plotE22")
      ), column(
        width = 3,
        h4("Selected Metier"),
        dataTableOutput("tabplotE22"),
        actionButton("resetE2", label = "Reset selection")
      ))
    }
  })
  
  output$Epage2 <- renderUI({
    if (input$EP2tabset == "Selection 1") {
      uiOutput("EbyLength")
    }
    
    else if (input$EP2tabset == "Selection 2") {
      uiOutput("EbyMet")
    }
  })
  
  
  ###############Page3#################
  output$tableE <- DT::renderDataTable(DT::datatable({
    E <- CelticCE
    if (input$ECountry != "All") {
      E <- filter(E, Country %in% input$ECountry)
    }
    if (input$EYear != "All") {
      E <- filter(E, Year %in% input$EYear)
    }
    if (input$EMetier != "All") {
      E <- filter(E, lvl4 %in% input$EMetier)
    }
    if (input$EVessel != "All") {
      E <- filter(E, Vessel_length %in% input$EVessel)
    }
    if (input$EArea != "All") {
      E <- filter(E, Area %in% input$EArea)
    }
    E
  }, extensions = 'Buttons'
  , options =opt ))
  
  ###########Existing tools##########################
  ############## 3. Effort app #######################
  partF <- reactive(
      readRDS("data/existing_tools/5.partial_F_app/data/Celtic_Sea/CSpartF.rds"))
  output$fleet.yearfilter <- renderUI({
    selectInput("year1","Year:",c("All",sort(unique(as.character(partF()$year)),decreasing=T))
    )
  })
  output$fleet.countryfilter <- renderUI({
    selectInput("country","Country",c("All",sort(unique(as.character(partF()$country)))),selected="ALL")
  })
  
  output$time.countryfilter <- renderUI({
    selectInput("country1","Country:",c(sort(unique(as.character(partF()$country)))),selected="BE"
    )})
  
  output$efftable <- DT::renderDataTable(DT::datatable({
    #aggregate across stocks (take mean)
    data <- reactive(partF()[,c("year","country", "fleet", "metier","effort","effshare")])
    data <- aggregate(list(effort =data()$effort, effshare = data()$effshare),
                      list(year=data()$year,country = data()$country, fleet=data()$fleet,metier=data()$metier), mean)
    if (input$year1 != "All") {
      data <- data[data$year == input$year1,]
    }
    if (input$country != "All") {
      data <- data[data$country == input$country,]
    }
    data[,c("effort","effshare")] <- round(data[,c("effort","effshare")],2) 
    data
  }, extensions = 'Buttons',options = opt)) 
  
  output$plotEffTS <- renderPlotly({
    dataplot1 <- partF()
    dataplot1$effmet <- dataplot1$effort*dataplot1$effshare
  
      dataplot1  <- dataplot1[dataplot1$country %in% input$country1,]
  
    p<-ggplot(dataplot1, aes(x = year, y = effmet)) +
            geom_point(aes(colour = metier)) + geom_line(aes(colour = metier)) +
            facet_wrap(~fleet,ncol=1,scales="free_y") +ylab("Effort")+xlab("Year")+
            theme_bw()
     p <- ggplotly(p)
     p %>% layout(hovermode = "compare",margin = list(l = 275, b =75))
  })                  
  
##############   Implications of Catch decreases app (Hackaton) #######################
  ################Hackathon  Work#################################
  output$plot <- renderPlot({
    # Transform data in a tidy format (long format)
    TotalWhiting=sum(data_fish$Whiting, na.rm=TRUE)
    Change=TotalWhiting*(abs(input$whitingslider)/100)  
    ChangePerFleet <- Change/dim(data_fish[data_fish$Whiting>0 & !is.na(data_fish$Whiting),])[1]
    data_fish$Whiting_indicator=data_fish$Whiting-ChangePerFleet
    data_fish$Whiting_indicator2=c()
    for(i in 1:length(data_fish$Whiting_indicator)){
      if(is.na(data_fish$Whiting_indicator[i])){
        data_fish$Whiting_indicator2[i]="black"
      }else if(data_fish$Whiting_indicator[i]<0){
        data_fish$Whiting_indicator2[i]="red"
      }else{
        data_fish$Whiting_indicator2[i]="black"
      }
    }
    
    data_fish$Whiting_indicator2<-factor(data_fish$Whiting_indicator2) 
    data_fish$Whiting_changed <- data_fish$Whiting*(100+input$whitingslider)/100  
    for(i in 1:dim(data_fish)){
      data_fish$total[i] <- sum(data_fish$Cod[i], data_fish$Haddock[i], data_fish$Whiting_changed[i], na.rm=TRUE)
    }
    data_fish$Percentage.Cod <-  data_fish$Cod/data_fish$total
    data_fish$Percentage.Haddock <-  data_fish$Haddock/data_fish$total
    data_fish$Percentage.Whiting <-  data_fish$Whiting_changed/data_fish$total
    data_fish1 <- subset(data_fish, select = c(1,2,4,7,10,13))
    data_fish1 <- filter(data_fish1, Country!= "UK (Channel Island Guernsey)" & Country!= "UK (Channel Island Jersey)")
    data_fish1$Country=factor(data_fish1$Country)
    
    data <- gather(data_fish1,key = "Species", value="CatchKG", -c(1,2,6)) 
    
    #ChangeinF=(input$whitingslider-0.52)/0.52
    
    # Set a number of 'empty bar' to add at the end of each group (Country)
    empty_bar=2
    nObsType=nlevels(as.factor(data$Species))
    to_add = data.frame( matrix(NA, empty_bar*nlevels(data$Country)*nObsType, ncol(data)) )
    colnames(to_add) = colnames(data)
    to_add$Country=rep(levels(data$Country), each=empty_bar*nObsType )
    data=rbind(data, to_add)
    data=data %>% arrange(Country, Fleet)
    data$id=rep( seq(1, nrow(data)/nObsType) , each=nObsType)
    
    
    # Get the name and the y position of each label
    
    #for(i in 1:dim(data)[1]){
    # data$indicator[i]=which[data_fish$Country=="Belgium" & data_fish$Fleet=="OTB_CRU" ]
    #}
    label_data= data %>% group_by(id, Fleet,Whiting_indicator2) %>% summarize(tot=sum(CatchKG,na.rm=TRUE))
    number_of_bar=nrow(label_data)
    angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust<-ifelse( angle < -90, 1, 0)
    label_data$angle<-ifelse(angle < -90, angle+180, angle)
    
    # prepare a data frame for base lines
    base_data=data %>% 
      group_by(Country) %>% 
      summarize(start=min(id), end=max(id) - empty_bar) %>% 
      rowwise() %>% 
      mutate(title=mean(c(start, end)))
    
    # prepare a data frame for grid (scales)
    grid_data = base_data
    grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
    grid_data$start = grid_data$start - 1
    grid_data=grid_data[-1,]
    
    
    ggplot(data) +      
      
      # Add the stacked bar
      geom_bar(aes(x=as.factor(id), y=CatchKG*10, fill=Species), stat="identity", alpha=0.5) +
      scale_fill_viridis(discrete=TRUE) +
      
      #Add scale lines in blank spaces
      # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
      geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = end, y = 5, xend = start, yend = 5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      
      # Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(max(data$id),3), y = c(0, 5, 10), label = c("0%", "50%", "100%") , color="grey", size=5 , angle=0, fontface="bold", hjust=0.75) +
      
      ylim(-10,max(label_data$tot, na.rm=T)+20) +
      theme_minimal() +
      theme(
        legend.position = "left",
        legend.text = element_text(size=17),
        legend.margin=margin(0,-200,625,0),
        #legend.box.margin = margin(10,10,10,10),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
      ) +
      coord_polar() +
      
      
      # Add labels on top of each bar
      geom_text(data=label_data, aes(x=id, y=tot*10, label=Fleet, hjust=hjust), color=label_data$Whiting_indicator2, fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
      
      # Add base line information
      geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
      geom_text(data=base_data, aes(x = title, y = -1, label=Country), hjust=c(0.5,1,1,0.6,0.5,0,0,0.5), 
                vjust=c(0.5,0.5,0,-1,0,0.5,1.5,1.5), colour = "black", alpha=0.8, size=4.5, fontface="bold", inherit.aes = FALSE)
    
  }, height= 670) 
  
  
  
############ 4. Catchability app ##################
  catchability <- reactive(
    readRDS("data/existing_tools/4.catchability_app/data/Celtic_Sea/CScatchability.rds"))
    
  
  

  output$table.yearfilter <- renderUI({
    selectInput("year2","Year:",c("All",sort(unique(as.character(catchability()$year)),decreasing=TRUE))
    )
  })
  output$table.stockfilter <- renderUI({
    selectInput("Stock","Stock", c("All",sort(unique(as.character(catchability()$stock))))
    )
  })
  output$plot.countryfilter <- renderUI({
    selectInput("country2","Country:",c(sort(unique(as.character(catchability()$country)))),
                selected =sort(unique(as.character(catchability()$country)))[1] 
    )
  })
  output$plot.stockfilter <- renderUI({
    selectInput("stock","Stock:",c(sort(unique(as.character(catchability()$stock)))),
                multiple=TRUE
    )
  })
  
  output$plot2.fleetfilter <- renderUI({
    selectInput("fleetP2","Fleet:",c(sort(unique(as.character(catchability()$fleet)))),
                selected =sort(unique(as.character(catchability()$fleet)))[1] 
    )
  })
  

  


  
  
  output$Catchtable <- DT::renderDataTable(DT::datatable({
    data <- catchability()[,c("year","stock","fleet", "metier","logq","country")]
    if (input$year2 != "All") {
      data <- data[data$year == input$year2,]
    }
    if (input$Stock != "All") {
      data <- data[data$stock == input$Stock,]
    }
    data[,c("logq")] <- round(data[,c("logq")], 2)
    data[,c("year","country","fleet","metier","stock","logq")]
  }, extensions = 'Buttons',options = opt))
  
  output$plotCatchability <- renderPlot({
    data <- catchability()
   
      data <- data[data$country %in% input$country2,]
     
      if (any(length(input$stock)>1 | input$stock != "All")) {
        data <- data[data$stock %in% input$stock,]
      }
  #remove OTH
    data <- data[data$metier!="OTH",]
    
   p<-ggplot(data, aes(x = year, y = logq )) +
            geom_point(aes(colour=stock)) + geom_smooth(method = loess, fullrange = FALSE,aes(colour=stock)) +
            facet_wrap(fleet ~ metier,scales="free_y") +ylab("log(Q)")+xlab("Year")+
            theme_bw() + scale_x_continuous(breaks = seq(2009,2017))
   
   p# <- ggplotly(p)
   
  # p %>% layout(tooltip="text",margin = list(l = 150, b =75))
  })
  
  
  output$plot2Catchability<-renderPlotly({
    data <- catchability()
    
    data <- data[data$fleet %in% input$fleetP2,]
   
    #remove OTH
    data <- data[data$metier!="OTH",]
    
    p<-ggplot(data, aes(x = year, y = logq )) +
      geom_point(aes(colour=metier)) + geom_line(aes(colour=metier)) +
      facet_wrap( ~ stock,scales="free_y",ncol=1) +ylab("log(Q)")+xlab("Year")+
      theme_bw() + scale_x_continuous(breaks = seq(2009,2017))
    
    p <- ggplotly(p)
    
     p %>% layout(tooltip="text",margin = list(l = 275, b =75))
  })
  
  ################### 5. Partial F app #########################
  output$PF.year.table <- renderUI({
    selectInput("year3","Year:", c("All",sort(unique(as.character(partF()$year)),decreasing=T)))
  })
  output$PF.stock.table <- renderUI({
    selectInput("Stock3","Stock",c("All",sort(unique(as.character(partF()$stock)))))
  })
 
  output$PF.country.plot1 <- renderUI({
    selectInput("country3","Country:",c(sort(unique(as.character(partF()$country)))),
                selected =sort(unique(as.character(partF()$country)))[1] )
  })
  output$PF.stock.plot1 <- renderUI({
    selectInput("stock3", "Stock:",c(sort(unique(as.character(partF()$stock)))),
                multiple=TRUE)
  })
  output$PF.year.plot2 <- renderUI({
    selectInput("year_","Year:",c("All",sort(unique(as.character(partF()$year)),
                                             decreasing=T)),selected = 'All')
  })
  output$PF.stock.plot2 <- renderUI({
    selectInput("stocks","Stocks:",c("All",sort(unique(as.character(partF()$stock)))),
                multiple=T,selected = 'All')
  })
  output$PFtable <- DT::renderDataTable(DT::datatable({
    data <- partF()[,c("year","stock","fleet", "metier","partF","country")]
    
    if (input$year3 != "All") {
      data <- data[data$year == input$year3,]
    }
    if (input$Stock3 != "All") {
      data <- data[data$stock == input$Stock3,]
    }
    data[,c("partF")] <- round(data[,c("partF")],8)
    data[,c("year","country","fleet","metier","stock","partF")]
  }, extensions = 'Buttons',options = opt))
  
  output$plotPartialF <- renderPlot({
    data <- partF()
    data <- aggregate(list(partF=data$partF),list(year=data$year,stock=data$stock,
                                                  fleet=data$fleet, metier=data$metier,country=data$country),sum)
    data <- merge(data,aggregate(data$partF,list(year=data$year,stock=data$stock),sum,na.rm=T))
    data$percent <- (data$partF/data$x)*100
  
      data <- data[data$country %in% input$country3,]
    
    
    if (any(length(input$stock3)>1 | input$stock3 != "All")) {
      data <- data[data$stock %in% input$stock3,]
    }
    
    data <- subset(data, !is.na(percent))
    
    #remove OTH
    data <- data[data$metier!="OTH",]
    
    p<-ggplot(data, aes(x = year, y = percent)) +
            geom_point(aes(colour = stock)) + geom_smooth(method = lm, fullrange = FALSE, aes(colour = stock)) +
            facet_wrap(fleet ~ metier,scales="free_y") +xlab("Year")
            theme_bw() #+ scale_x_continuous(breaks = seq(2004,2014,by=4)))
    
    p# <- ggplotly(p)
    
   # p %>% layout(margin = list( b =75))
  })
  
 

  ################### 6. Quota Share app #########################
  
  
  quotashare <- reactive(
   readRDS("data/existing_tools/6.quota_share_app/data/Celtic_Sea/CSquotashare.rds")
    )
  
  quotashare1 <- reactive(
   readRDS("data/existing_tools/6.quota_share_app/data/Celtic_Sea/CSquotashare1.rds")
    )
  

  
  output$QS.year.table <- renderUI({
    selectInput("year4","Year:", c("All",sort(unique(as.character(quotashare()$year)),decreasing=T)))
  })
  output$QS.stock.table <- renderUI({
    selectInput("Stock4","Stock",
                c("All",sort(unique(as.character(quotashare()$stock)))))
  })
  
  output$QS.fleet.plot1 <- renderUI({
    selectInput("fleet4","Fleet:",c(sort(unique(as.character(quotashare()$fleet))))
               ,selected =sort(unique(as.character(quotashare()$fleet)))[1] )
  })
  output$QS.stock.plot1 <- renderUI({ 
    selectInput("stock5","Stock:", c(sort(unique(as.character(quotashare()$stock)))),
                                                  multiple=TRUE )
  })
  
  output$QS.fleet.plot2 <- renderUI({ 
    selectInput("fleet5","Fleet:", c(sort(unique(as.character(quotashare()$fleet)))),
                multiple=TRUE,selected =sort(unique(as.character(quotashare()$fleet)))[1])
  })
  

  output$QS.stock.plot3 <- renderUI({ 
    selectInput("stock6","Stock:", c(sort(unique(as.character(quotashare1()$stock)))),
                multiple=FALSE)
  }) 
  
  output$QS.year.plot3 <- renderUI({ 
    selectInput("year6","Year",c(sort(unique(quotashare1()$year))))
  }) 
  
  output$QS.landings.plot3 <- renderUI({ 
    sliderInput("plim", "percentage of stock landings", 0, 100, 80)
  }) 
 
   
  
  output$QStable <- DT::renderDataTable(DT::datatable({
    data_tab<-quotashare1()
    if (input$year4 != "All") {
      data_tab <- data_tab[data_tab$year == input$year4,]
    }
    if (input$Stock4 != "All") {
      data_tab <- data_tab[data_tab$stock == input$Stock4,]
    }
    
    data_tab[,c("year","fleet","stock","landings","proportion_fleet_landings", "stock_landings_share")]
  }, extensions = 'Buttons',options = opt))
  
  output$plotQS <- renderPlotly({
    data<-quotashare()
    data <- aggregate(list(Share=data$relstab),list(year=data$year,stock=data$stock,
                                                    fleet=data$fleet),sum)
    
      data <- data[data$fleet %in% input$fleet4,]
      if (any(length(input$stock5)>1 | input$stock5 != "All")) {
        data <- data[data$stock %in% input$stock5,]
      }
    
    
   p<-ggplot(data, aes(x = year, y = Share)) +
            geom_point(aes(colour = stock), size = 2) + 
            geom_line(aes(colour = stock)) +
            facet_wrap(~ fleet,scales="free_y",ncol = 2) +
            theme_bw() + theme(axis.text.x = element_text(angle = - 90)) 
   
   p<- ggplotly(p)
    p %>% layout(hovermode = "compare",margin = list(l = 230, b =75))
  })
  
  output$plotQScomp <- renderPlotly({
    data<-quotashare()
    if (input$fleet5 != "All") {
      data <- data[data$fleet %in% input$fleet5,]
    }
    
   p<-ggplot(data, aes(x = year, y = catchcomp,group=fleet)) +
            geom_bar(stat = 'identity', aes(fill = stock)) +
            facet_wrap(~ fleet,scales="free_y",ncol=2) + geom_text(aes(x = year, y = 0.9, label = round(fleet_landings,0))) + 
            theme_bw() + theme(axis.text.x = element_text(angle = - 90)) + coord_flip()
   ggplotly(p)
  })
  output$plotCompLandings <- renderPlot({
    data_tab<-quotashare1()
 # for the chosen stock, which fleets are the most important
    dats <- data_tab[data_tab$year == input$year6,]
    dats <- dats[dats$stock == as.character(input$stock6),]
    dats <- dats[order(dats$stock_landings_share),]
    # select fleets contribute to XX% (input$plim) of the landings
    dats <- dats[cumsum(dats$stock_landings_share) > (1-0.01*input$plim) , ]
    dats1<- dats[c("fleet","stock_landings_share")]
    names(dats1)[2] <- "prop"
    # compute the position of each fleet on the X axis
    dats1$pos <- 0.5 * (cumsum(dats1$prop) + cumsum(c(0, dats1$prop[-length(dats1$prop)])))
    
    # select the data of the original file for the main fleets only which is what will be plotted
    dats2 <- data_tab[is.element(data_tab$fleet , dats1$fleet) & data_tab$year == input$year6,]
    dats <- merge(dats1,dats2,all.y=T)
    # put all no important stocks into a OTHer bin 
    dats$oth<-0
    dats$oth[dats$proportion_fleet_landings<0.05 & dats$stock != input$stock6]  <-1
    dats$stock <- as.character(dats$stock)
    dats$stock[dats$oth == 1] <- "OTH"
    
    
    
    g<-ggplot(dats , aes(x = pos , y = proportion_fleet_landings , width = prop , fill =  stock, col = (dats$stock==input$stock6)
                         )) + 
      geom_bar(stat="identity",position="fill" ) + 
      facet_grid(~fleet, scales = "free_x", space = "free_x") + 
      theme_minimal() +
      theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())        + 
      scale_colour_manual("legend", values = c(NA,"black"), guide = F )   +
      scale_x_continuous(name=paste("% of",input$stock6,"landings per fleet"),position = "top",breaks=dats$pos,labels = paste(round(100*dats$prop,0),"%"))  +
      theme(strip.text.x = element_text( angle = 90) , text = element_text(size=12))  + ylab("Species composition of the landings\nper fleet") # +
      #ggtitle(paste0("Main fleets contributing to ",input$plim,"% of \n",input$stock6, " landings \n\n")) + theme(plot.title = element_text(hjust = 0.5))
    
 g
   
  # p %>% layout(margin = list( l=120,b =75,t=105))
  })
  
  output$QS.Landings.page<-renderUI({
 list( paste("In the the Celtic Sea main fleets responsible for ",input$plim, "% of the ", 
             input$stock6, "landings in", input$year6, sep="  " ),plotOutput("plotCompLandings",
                                                                             width = '1200px', height = '800px'))}) 
  
  
  ############## Mapping ##########################
  # Create foundational leaflet map
  # and store it as a reactive expression
  foundational.map <- reactive({
    if(input$Species_selector == "Cod"){
      Cod_tac <- readOGR("www/Shapefiles","Cod_tac_T")
      Cod_7ek <- readOGR("www/Shapefiles","Cod_7ek_T")
      Add_tac <- readOGR("www/Shapefiles","Add_tac_T")
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>% 
        addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                    layers = "0",
                    options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                    attribution = "ICES") %>%
        setView(lng=-14,lat=52,zoom=4) %>% 
        addLegend("bottomleft",col=c('#3d771e','#91d46d','#c773bd'),
                  labels = c("Cod TAC area",  "Additional TAC areas","Cod 7ek stock"))%>%
        addPolygons(data=Cod_tac, group="TAC", stroke = FALSE,fill=TRUE,
                    fillColor = '#3d771e', fillOpacity=0.5,
                    popup=paste("<b>Full</b> ",Cod_tac$Area_Full, "<br />",
                                "<b>Area_27:</b> ",Cod_tac$Area_27, "<br />",
                                "<b>Major_FA:</b> ",Cod_tac$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Cod_tac$SubArea, "<br />",
                                "<b>Division:</b> ",Cod_tac$Division, "<br />",
                                "<b>Sub-Division:</b> ",Cod_tac$SubDivisio, "<br />")) %>%
        addPolygons(data=Add_tac, group="TAC", stroke = FALSE,fill=TRUE,
                    fillColor = '#91d46d', fillOpacity=0.8,
                    popup=paste("<b>ICES Code: </b>",Add_tac$ICESCODE, "<br />",
                                "<b>ICES Area:</b> ",Add_tac$IcesArea, "<br />")) %>%
        addPolygons(data=Cod_7ek,  group="Stocks", stroke =TRUE, 
                    fill=TRUE, fillColor = '#c773bd', fillOpacity=0.7,
                    color = "white",dashArray = "3",
                    popup=paste("<b>Stock: </b>7ek", "<br />",
                                "<b>Area: </b> ",Cod_7ek$Area_Full, "<br />",
                                "<b>Major_FA:</b> ",Cod_7ek$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Cod_7ek$SubArea, "<br />",
                                "<b>Division:</b> ",Cod_7ek$Division, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addLayersControl(baseGroups = c("Esri.OceanBasemap", "ICES Areas"),
                         overlayGroups = c("TAC","Stocks"),  
                         options = layersControlOptions(collapsed = FALSE)) 
    }else if(input$Species_selector == "Sole"){
      Sol_tac <- readOGR("www/Shapefiles","Sol_tac_T")
      Sol_7e <- readOGR("www/Shapefiles","Sol_7e_T")
      Sol_7fg <- readOGR("www/Shapefiles","Sol_7fg_T")
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>% 
        addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                    layers = "0",
                    options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                    attribution = "ICES") %>%
        setView(lng=-14,lat=52,zoom=5) %>% 
        addLegend("bottomleft",col=c('#3d771e','#c773bd','#590948'),
                  labels = c("Sole TAC area","Sole 7bk stock","Sole 8abd stock"))%>%
        addPolygons(data=Sol_tac, group="TAC", stroke = FALSE,fill=TRUE,
                    fillColor = '#3d771e', fillOpacity=0.4,
                    popup=paste("<b>Full name:</b> ",Sol_tac$Area_Full, "<br />",
                                "<b>Area_27:</b> ",Sol_tac$Area_27, "<br />",
                                "<b>Major_FA:</b> ",Sol_tac$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Sol_tac$SubArea, "<br />",
                                "<b>Division:</b> ",Sol_tac$Division, "<br />",
                                "<b>Sub-Division:</b> ",Sol_tac$SubDivisio, "<br />")) %>%
        addPolygons(data=Sol_7e,  group="Stocks", stroke =TRUE, weight=1,
                    fill=TRUE, fillColor = '#c773bd', fillOpacity=0.7,
                    color = "white",dashArray = "3",
                    popup=paste("<b>Stock: </b>7e", "<br />",
                                "<b>Area: </b> ",Sol_7e$Area_Full, "<br />",
                                "<b>Major_FA:</b> ",Sol_7e$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Sol_7e$SubArea, "<br />",
                                "<b>Division:</b> ",Sol_7e$Division, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addPolygons(data=Sol_7fg,  group="Stocks", stroke =TRUE, weight=1,
                    fill=TRUE, fillColor = '#590948', fillOpacity=0.7,
                    color = "white",dashArray = "3",
                    popup=paste("<b>Stock: </b>7fg", "<br />",
                                "<b>Area: </b> ",Sol_7fg$Area_Full, "<br />",
                                "<b>Area: </b> ",Sol_7fg$Area_Full, "<br />",
                                "<b>Major_FA:</b> ",Sol_7fg$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Sol_7fg$SubArea, "<br />",
                                "<b>Division:</b> ",Sol_7fg$Division, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addLayersControl(baseGroups = c("Esri.OceanBasemap"),
                         overlayGroups = c("Stocks","TAC"),  
                         options = layersControlOptions(collapsed = FALSE))
    }else if(input$Species_selector == "Haddock"){
      Had_tac <- readOGR("www/Shapefiles","Had_tac_T")
      Had_7bk <- readOGR("www/Shapefiles","Had_7bk_T")
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>% 
        addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                    layers = "0",
                    options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                    attribution = "ICES") %>%
        setView(lng=-14,lat=52,zoom=5) %>% 
        addLegend("bottomleft",col=c('#3d771e','#c773bd'),
                  labels = c("Haddock TAC area","Haddock 7b-k stock"))%>%
        addPolygons(data=Had_tac, group="TAC", stroke = FALSE,fill=TRUE,
                    fillColor = '#3d771e', fillOpacity=0.4,
                    popup=paste("<b>Full name:</b> ",Had_tac$Area_Full, "<br />",
                                "<b>Area_27:</b> ",Had_tac$Area_27, "<br />",
                                "<b>Major_FA:</b> ",Had_tac$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Had_tac$SubArea, "<br />",
                                "<b>Division:</b> ",Had_tac$Division, "<br />",
                                "<b>Sub-Division:</b> ",Had_tac$SubDivisio, "<br />")) %>%
        addPolygons(data=Had_7bk,  group="Stocks", stroke =TRUE, weight=1,
                    fill=TRUE, fillColor = '#c773bd', fillOpacity=0.7,
                    color = "white",dashArray = "3",
                    popup=paste("<b>ICES Code: </b>",Had_7bk$ICESCODE, "<br />",
                                "<b>ICES Name: </b> ",Had_7bk$ICESNAM, "<br />",
                                "<b>ICES Area:</b> ",Had_7bk$IcesArea, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addLayersControl(baseGroups = c("Esri.OceanBasemap"),
                         overlayGroups = c("Stocks","TAC"),  
                         options = layersControlOptions(collapsed = FALSE))
    }else if(input$Species_selector == "Whiting"){
      Whg_tac <- readOGR("www/Shapefiles","Whg_tac_T")
      Whg_7bcek <- readOGR("www/Shapefiles","Whg_7bcek_T")
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>% 
        addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                    layers = "0",
                    options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                    attribution = "ICES") %>%
        setView(lng=-14,lat=52,zoom=5) %>% 
        addLegend("bottomleft",col=c('#3d771e','#590948'),#'#c773bd'
                  labels = c("Whiting TAC area","Whiting 7bcek stock"))%>%
        addPolygons(data=Whg_tac, group="TAC", stroke = FALSE,fill=TRUE,
                    fillColor = '#3d771e', fillOpacity=0.4,
                    popup=paste("<b>Full name:</b> ",Whg_tac$Area_Full, "<br />",
                                "<b>Area_27:</b> ",Whg_tac$Area_27, "<br />",
                                "<b>Major_FA:</b> ",Whg_tac$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Whg_tac$SubArea, "<br />",
                                "<b>Division:</b> ",Whg_tac$Division, "<br />",
                                "<b>Sub-Division:</b> ",Whg_tac$SubDivisio, "<br />")) %>%
        addPolygons(data=Whg_7bcek,  group="Stocks", stroke =TRUE, weight=1,
                    fill=TRUE, fillColor = '#590948', fillOpacity=0.9,
                    color = "white",dashArray = "3",
                    popup=paste("<b>ICES Code: </b>",Whg_7bcek$ICESCODE, "<br />",
                                "<b>ICES Name: </b> ",Whg_7bcek$ICESNAM, "<br />",
                                "<b>ICES Area:</b> ",Whg_7bcek$IcesArea, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addLayersControl(baseGroups = c("Esri.OceanBasemap"),
                         overlayGroups = c("Stocks","TAC"),  
                         options = layersControlOptions(collapsed = FALSE))
    }else if(input$Species_selector == "Hake"){
      Hke_tac <- readOGR("www/Shapefiles","Hke_tac_T")
      Hke_3a46 <- readOGR("www/Shapefiles","Hke_3a46_T")
      Hke_7 <- readOGR("www/Shapefiles","Hke_7_T")
      Hke_8abd <- readOGR("www/Shapefiles","Hke_8abd_T")
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>% 
        addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                    layers = "0",
                    options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                    attribution = "ICES") %>%
        setView(lng=-14,lat=52,zoom=5) %>% 
        addLegend("bottomleft",col=c('#3d771e','#c773bd','#590948','#CB8B84'),
                  labels = c("Hake TAC area","Hake 3a46 stock", "Hake 7 stock","Hake 8abd stock"))%>%
        addPolygons(data=Hke_tac, group="TAC", stroke = FALSE,fill=TRUE,
                    fillColor = '#3d771e', fillOpacity=0.4,
                    popup=paste("<b>ICES Code: </b>",Hke_tac$ICESCODE, "<br />",
                                "<b>ICES Name: </b> ",Hke_tac$ICESNAM, "<br />",
                                "<b>ICES Area:</b> ",Hke_tac$IcesArea, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addPolygons(data=Hke_3a46,  group="Stocks", stroke =TRUE, weight=1,
                    fill=TRUE, fillColor = '#c773bd', fillOpacity=0.5,
                    color = "white",dashArray = "3",
                    popup=paste("<b>Stock: </b>3a46", "<br />",
                                "<b>Area: </b> ",Hke_3a46$Area_Full, "<br />",
                                "<b>Major_FA:</b> ",Hke_3a46$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Hke_3a46$SubArea, "<br />",
                                "<b>Division:</b> ",Hke_3a46$Division, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addPolygons(data=Hke_7,  group="Stocks", stroke =TRUE, weight=1,
                    fill=TRUE, fillColor = '#590948', fillOpacity=0.5,
                    color = "white",dashArray = "3",
                    popup=paste("<b>Stock: </b>7", "<br />",
                                "<b>Area: </b> ",Hke_7$Area_Full, "<br />",
                                "<b>Area: </b> ",Hke_7$Area_Full, "<br />",
                                "<b>Major_FA:</b> ",Hke_7$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Hke_7$SubArea, "<br />",
                                "<b>Division:</b> ",Hke_7$Division, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addPolygons(data=Hke_8abd,  group="Stocks", stroke =TRUE, weight=1,
                    fill=TRUE, fillColor = '#CB8B84', fillOpacity=0.3,
                    color = "white",dashArray = "3",
                    popup=paste("<b>Stock: </b>8abd", "<br />",
                                "<b>Area: </b> ",Hke_8abd$Area_Full, "<br />",
                                "<b>Area: </b> ",Hke_8abd$Area_Full, "<br />",
                                "<b>Major_FA:</b> ",Hke_8abd$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Hke_8abd$SubArea, "<br />",
                                "<b>Division:</b> ",Hke_8abd$Division, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addLayersControl(baseGroups = c("Esri.OceanBasemap"),
                         overlayGroups = c("Stocks","TAC"),  
                         options = layersControlOptions(collapsed = FALSE))
    }else if(input$Species_selector == "Megrim"){
      Meg_tac <- readOGR("www/Shapefiles","Meg_tac_T")
      Meg_7bk <- readOGR("www/Shapefiles","Meg_7bk_T")
      Meg_8abd <- readOGR("www/Shapefiles","Meg_8abd_T")
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>% 
        addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                    layers = "0",
                    options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                    attribution = "ICES") %>%
        setView(lng=-14,lat=52,zoom=5) %>% 
        addLegend("bottomleft",col=c('#3d771e','#c773bd','#590948'),
                  labels = c("Megrim TAC area", "Megrim 7bk stock","Megrim 8abd stock"))%>%
        addPolygons(data=Meg_tac, group="TAC", stroke = FALSE,fill=TRUE,
                    fillColor = '#3d771e', fillOpacity=0.4,
                    popup=paste("<b>Full name:</b> ",Meg_tac$Area_Full, "<br />",
                                "<b>Area_27:</b> ",Meg_tac$Area_27, "<br />",
                                "<b>Major_FA:</b> ",Meg_tac$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Meg_tac$SubArea, "<br />",
                                "<b>Division:</b> ",Meg_tac$Division, "<br />",
                                "<b>Sub-Division:</b> ",Meg_tac$SubDivisio, "<br />")) %>%
        addPolygons(data=Meg_7bk,  group="Stocks", stroke =TRUE, weight=1,
                    fill=TRUE, fillColor = '#c773bd', fillOpacity=0.7,
                    color = "white",dashArray = "3",
                    popup=paste("<b>Stock: </b>7bk", "<br />",
                                "<b>Area: </b> ",Meg_7bk$Area_Full, "<br />",
                                "<b>Major_FA:</b> ",Meg_7bk$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Meg_7bk$SubArea, "<br />",
                                "<b>Division:</b> ",Meg_7bk$Division, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addPolygons(data=Meg_8abd,  group="Stocks", stroke =TRUE, weight=1,
                    fill=TRUE, fillColor = '#590948', fillOpacity=0.7,
                    color = "white",dashArray = "3",
                    popup=paste("<b>Stock: </b>8abd", "<br />",
                                "<b>Area: </b> ",Meg_8abd$Area_Full, "<br />",
                                "<b>Area: </b> ",Meg_8abd$Area_Full, "<br />",
                                "<b>Major_FA:</b> ",Meg_8abd$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Meg_8abd$SubArea, "<br />",
                                "<b>Division:</b> ",Meg_8abd$Division, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addLayersControl(baseGroups = c("Esri.OceanBasemap"),
                         overlayGroups = c("Stocks","TAC"),  
                         options = layersControlOptions(collapsed = FALSE))
    }else if(input$Species_selector == "Anglerfish/Monkfish"){
      Mon_tac <- readOGR("www/Shapefiles","Mon_tac_T")
      Mon_7bk <- readOGR("www/Shapefiles","Mon_7bk_T")
      Mon_8abd <- readOGR("www/Shapefiles","Mon_8abd_T")
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>% 
        addWMSTiles("http://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                    layers = "0",
                    options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"),
                    attribution = "ICES") %>%
        setView(lng=-14,lat=52,zoom=5) %>% 
        addLegend("bottomleft",col=c('#3d771e','#c773bd','#590948'),
                  labels = c("Monk/Angler TAC area", "Monk/Angler 7bk stock","Monk/Angler 8abd stock"))%>%
        addPolygons(data=Mon_tac, group="TAC", stroke = FALSE,fill=TRUE,
                    fillColor = '#3d771e', fillOpacity=0.4,
                    popup=paste("<b>Full name:</b> ",Mon_tac$Area_Full, "<br />",
                                "<b>Area_27:</b> ",Mon_tac$Area_27, "<br />",
                                "<b>Major_FA:</b> ",Mon_tac$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Mon_tac$SubArea, "<br />",
                                "<b>Division:</b> ",Mon_tac$Division, "<br />",
                                "<b>Sub-Division:</b> ",Mon_tac$SubDivisio, "<br />")) %>%
        addPolygons(data=Mon_7bk,  group="Stocks", stroke =TRUE, weight=1,
                    fill=TRUE, fillColor = '#c773bd', fillOpacity=0.7,
                    color = "white",dashArray = "3",
                    popup=paste("<b>Stock: </b>7bk", "<br />",
                                "<b>Area: </b> ",Mon_7bk$Area_Full, "<br />",
                                "<b>Major_FA:</b> ",Mon_7bk$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Mon_7bk$SubArea, "<br />",
                                "<b>Division:</b> ",Mon_7bk$Division, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addPolygons(data=Mon_8abd,  group="Stocks", stroke =TRUE, weight=1,
                    fill=TRUE, fillColor = '#590948', fillOpacity=0.7,
                    color = "white",dashArray = "3",
                    popup=paste("<b>Stock: </b>8abd", "<br />",
                                "<b>Area: </b> ",Mon_8abd$Area_Full, "<br />",
                                "<b>Area: </b> ",Mon_8abd$Area_Full, "<br />",
                                "<b>Major_FA:</b> ",Mon_8abd$Major_FA, "<br />",
                                "<b>Sub-Area:</b> ",Mon_8abd$SubArea, "<br />",
                                "<b>Division:</b> ",Mon_8abd$Division, "<br />"),
                    highlight = highlightOptions(weight = 5,
                                                 bringToFront = TRUE)) %>%
        addLayersControl(baseGroups = c("Esri.OceanBasemap"),
                         overlayGroups = c("Stocks","TAC"),  
                         options = layersControlOptions(collapsed = FALSE))
    }
  }) # end of foundational.map()
  
  
  # render foundational leaflet map
  output$map <- leaflet::renderLeaflet({
    #if(input$Species_selector == "Please select"){
    # leaflet() %>%
    #  addProviderTiles(providers$Esri.OceanBasemap) %>% 
    #  addPolylines(color = "grey",data= div, group = "ICES Sub-Areas", weight = 3)%>%
    #  addPolylines(color = "darkgrey",data= cont, group = "ICES Sub-Areas", weight = 3)#%>%
    
    #}else if(input$Species_selector != "Please select"){
    # call reactive map
    foundational.map()
  }
  )
  

  

  
  ########################################################################################################################
  ################################Stock Advice######################################################
  
  output$ICES.Summary<- renderText({
    t<-as.numeric(input$ICESAyear)
    a<-"
    Mixed-fisheries scenarios are based on the central assumption that the 
    fishing patterns and catchability of a fleet in"
    b<-paste("The term “fleet’s stock share” or “stock share”
    is used to describe the share of the fishing opportunities for 
    each particular fleet, calculated based on the single-stock advice for",t+1,
    "and the historical proportion of the stock landings taken by the fleet.")
    
    if(input$ICESAyear!=2018){
paste(h5(a, t,"and",t+1,"are the same as those in",t-1, "(similar to procedures in single-stock forecasts,
     where growth and selectivity are assumed constant). ", b))}
      else{paste(h5(a,t,"and",t+1,"are the same as the average of", t-3,"–",t-1,".", b))}})
  
  output$ICES.SC <- function() {
    
    t<-as.numeric(input$ICESAyear)
    text_tbl <- data.frame(
      Scenarios = c(
        "Maximum", "Minimum", "Haddock MSY approach", "Whiting MSY approach", "Status quo effort","Cod MSY approach"
      ),
      Explanation = c(
        " For each fleet, fishing stops when all stocks have been caught up to the fleet’s stock shares.
        This option causes overfishing of the single-stock advice possibilities of most stocks.",
        "For each fleet, fishing stops when the catch for any one of the stocks meets the fleet’s stock share. This option is the most precautionary option, 
        causing underutilization of the single-stock advice possibilities of other stocks.",
        "All fleets set their effort corresponding to that required to catch their haddock stock share, regardless of other catches. ",
        "All fleets set their effort corresponding to that required to catch their whiting stock share, regardless of other catches.",
        paste("The effort of each fleet is set equal to the effort in the most recently recorded year",t-1),
        "All fleets set their effort corresponding to that required to catch their whiting stock share, regardless of other catches."
      )
    )
    if(input$ICESAyear==2015){
      S<-filter(Adecision,Year==2015)
      text_tbl<-cbind(text_tbl,S[-c(1,2)])
      names(text_tbl)[3]<-"Advice Suggestion"
      
      text_tbl %>%
        knitr::kable("html") %>%
        kable_styling(full_width = F, font_size = 13) %>%
        column_spec(c(1:3), background = "black", include_thead = TRUE, border_right = T) %>%
        column_spec(1, bold = T, border_right = T, underline = T) %>%
        column_spec(2, width = "30em", bold = T) %>%
        column_spec(3, width = "30em", bold = T)%>%
        row_spec(c(1, 3, 5), background = "lightgrey") %>%
        row_spec(c(2, 4,6), background = "lightyellow", color = "#525252")
    }
    else if(input$ICESAyear==2016){
      S<-filter(Adecision,Year==2016)
      text_tbl<-cbind(text_tbl,S[-c(1,2)])
      names(text_tbl)[3]<-"Advice Suggestion"
      text_tbl %>%
      knitr::kable("html") %>%
      kable_styling(full_width = F, font_size = 13) %>%
      column_spec(c(1:3), background = "black", include_thead = TRUE, border_right = T) %>%
      column_spec(1, bold = T, border_right = T, underline = T) %>%
      column_spec(2, width = "30em", bold = T) %>%
      column_spec(3, width = "30em", bold = T)%>%
      row_spec(c(1, 3, 5), background = "lightgrey") %>%
      row_spec(c(2, 4,6), background = "lightyellow", color = "#525252")
    }
    else if(input$ICESAyear==2017){
      S<-filter(Adecision,Year==2017)
      add<-data.frame(Scenarios=c("Value","Range"),Explanation =c("A simple scenario accounting for the economic importance of each stock for
                       each fleet. The effort by fleet is equal to the average of the efforts required to catch the fleet’s stock shares
                      of each of the stocks, weighted by the historical catch value of that stock. This option causes overfishing of some stocks
                      and underutilization of others.","This  scenario  searches  for  the  minimum sum of differences between 
                      potential catches by stock under the minimum and the maximum scenarios within the FMSY ranges. "))
      text_tbl<-rbind(text_tbl,add)
      text_tbl<-cbind(text_tbl,S[-c(1,2)])
      names(text_tbl)[3]<-"Advice Suggestion"
      text_tbl %>%
        knitr::kable("html") %>%
        kable_styling(full_width = F, font_size = 13) %>%
        column_spec(c(1:3), background = "black", include_thead = TRUE, border_right = T) %>%
        column_spec(1, bold = T, border_right = T, underline = T) %>%
        column_spec(2, width = "30em", bold = T) %>%
        column_spec(3, width = "30em", bold = T)%>%
        row_spec(c(1, 3, 5,7), background = "lightgrey") %>%
        row_spec(c(2, 4,6,8), background = "lightyellow", color = "#525252")
      
    }
    else if(input$ICESAyear==2018){
      S<-filter(Adecision,Year==2018)
      add<-data.frame(Scenarios=c("Value","Cod FMSY","Range"),Explanation =c("A simple scenario accounting for the economic importance of each stock for
                      each fleet. The effort by fleet is equal to the average of the efforts required to catch the fleet’s stock shares
                      of each of the stocks, weighted by the historical catch value of that stock. This option causes overfishing of some stocks
                      and underutilization of others.","All fleets set their effort corresponding to that required to catch their cod stock share, 
                      where the cod TAC is set according to reduced FMSY (F = 0.12, FMSY × (SSB(2019) / MSY Btrigger)),
                      regardless of other catches.","This  scenario  searches  for  the  minimum sum of differences between 
                      potential catches by stock under the minimum and the maximum scenarios within the FMSY ranges."))
      text_tbl<-rbind(text_tbl,add)
      text_tbl<-cbind(text_tbl,S[-c(1,2)])
      names(text_tbl)[3]<-"Advice Suggestion"
      text_tbl %>%
        knitr::kable("html") %>%
        kable_styling(full_width = F, font_size = 13) %>%
        column_spec(c(1:3), background = "black", include_thead = TRUE, border_right = T) %>%
        column_spec(1, bold = T, border_right = T, underline = T) %>%
        column_spec(2, width = "30em", bold = T) %>%
        column_spec(3, width = "30em", bold = T)%>%
        row_spec(c(1, 3, 5,7,9), background = "lightgrey") %>%
        row_spec(c(2, 4,6,8), background = "lightyellow", color = "#525252")
    }}
    
   output$MethodData <- renderText({ 
     DataMethods=filter(DataMethods, Year==input$ICESAyear)
     paste(DataMethods[1,2])
      
    })
   
   output$MethodData2 <- renderText({ 
     DataMethods=filter(DataMethods, Year==input$ICESAyear)
     paste(DataMethods[1,3])
     
   })
   
   output$ManagementArea<-function() {
     text_tbl<-ManagementArea
     text_tbl %>%
     knitr::kable("html") %>%
     kable_styling(full_width = F, font_size = 13)%>%
     column_spec(c(1:3), background = "black", include_thead = TRUE, border_right = T) %>%
       column_spec(1, bold = T, border_right = T, underline = T) %>%
       column_spec(2, width = "3em", bold = T, border_right = T, underline = T) %>%
       column_spec(3, width = "60em", bold = T) %>%
       row_spec(c(1, 3), background = "lightgrey") %>%
       row_spec(c(2), background = "lightyellow", color = "#525252")}
   
  ref <- reactive({
     filter(ReferencePoints, Year==input$ICESAyear)
   })
  
   output$table.ref<-function() {
     text_tbl<-ref()
     if(text_tbl$Year!=2018){
     text_tbl<- text_tbl[-1]
     text_tbl %>%
       knitr::kable("html")%>%
       kable_styling(full_width = F, font_size = 13)%>%
       column_spec(c(1:4), background = "black", include_thead = TRUE, border_right = T) %>%
       column_spec(1, bold = T, border_right = T, underline = T) %>%
       column_spec(2, bold = T, border_right = T) %>%
       column_spec(3, bold = T) %>%
       column_spec(4, bold = T) %>%
       row_spec(c(1, 3,5,7), background = "lightgrey") %>%
       row_spec(c(2,4,6,8), background = "lightyellow", color = "#525252")}
     else{ text_tbl<- text_tbl[-1]
     text_tbl %>%
       knitr::kable("html")%>%
       kable_styling(full_width = F, font_size = 13)%>%
       column_spec(c(1:4), background = "black", include_thead = TRUE, border_right = T) %>%
       column_spec(1, bold = T, border_right = T, underline = T) %>%
       column_spec(2, bold = T, border_right = T, underline = T) %>%
       column_spec(3, bold = T) %>%
       column_spec(4, bold = T) %>%
       row_spec(c(1, 3,5,7,9,11), background = "lightgrey") %>%
       row_spec(c(2,4,6,8,10,12), background = "lightyellow", color = "#525252")}
     
   }
   
   output$pieChart <- renderChart({
     year=as.numeric(input$ICESAyear)-1
     t<- subset(MandDataPie, Year == year)
     t$PERCENT = round((t$Landings/sum(t$Landings)) * 100,2)
     p1 <- nPlot(x = "Stock", y = "Landings", data = t, type = "pieChart")
     p1$chart(tooltipContent = "#! function(key, y, e, graph){return '<h3>'
              + key + '</h3>' + '<p>'+ 'Landings: ' + y + ' tones' + '<br>' + ' % of Landings: ' + e.point.PERCENT} !#" )
     p1$set(width = 300, height = 500)
     p1$chart(color = rev(col),showLegend = FALSE)
     p1$addParams(height = 300, dom = 'pieChart ')
     return(p1)
})
   
 
   
   output$LandingBars <-renderText({
     t<-as.numeric(input$ICESAyear)
     paste("Landings distribution in ", t-1, "of species by métier used by mixed-fisheries model. ")
     
     })
   output$BarChart <- renderChart({
     year=as.numeric(input$ICESAyear)-1
     d<- subset(MandData, Year == year) 
     d$Landings<-d$Landings/1000
     d1 <- dPlot(
       x ="Metier",
       y = "Landings",
       groups = "Stock",
       data = d,
       type = "bar"
     )
     d1$xAxis(orderRule = "Metier")
     d1$legend(
       x = 60,
       y = 10,
       width = 700,
       height = 20,
       horizontalAlign = "right"
     )
     d1$defaultColors(col)
     d1$addParams(height = 400,width=900, dom = 'BarChart ')
     
     if(input$barChoice==1){
       d1$yAxis (
         type= "addMeasureAxis"
         , outputFormat = "0.5f")}
     else if(input$barChoice==2){d1$yAxis (
       type= "addPctAxis"
     )}
     else if(input$barChoice==3){
       d1 <- dPlot(
         x =c("Metier","Stock"),
         y = "Landings",
         groups = "Stock",
         data = d,
         type = "bar"
       )
       d1$xAxis(orderRule = "Metier")
       
       d1$legend(
         x = 60,
         y = 10,
         width = 700,
         height = 20,
         horizontalAlign = "right"
       )
       d1$defaultColors(col)
       d1$yAxis (
         type= "addMeasureAxis"
         , outputFormat = "0.5f")
       d1$addParams(height = 400 ,width=900,dom = 'BarChart ')
       
     }
 
     
     return(d1)
   })
   
   output$NoteBars <-renderText({
     t<-as.numeric(input$ICESAyear)
     paste("Note: The “other” (OTH) displayed here is a mixed category consisting of 
           (i) landings without corresponding effort and (ii) 
           landings of any combination of fleet and métier with landings < 1% of
           any of the considered stocks in  ", t-1,"." )
     
   })
   
   ###Forecast page
   output$Ass2015 <- renderChart({
     dat<-filter(Assessment,var==input$Forecastfilter,Advice.Year==2015)
     p1<-nPlot(value ~ Year, group =  'FishStock', data = dat, type = 'lineWithFocusChart')
     p1$chart(color=rev(col))
     p1$addParams(width=500,dom = 'Ass2015')
     return(p1)
   })
   
   output$Adv2015 <- renderChart({
    if(input$Forecastfilter2==1){
   dat<-filter(Advice,var==input$Forecastfilter,Advice.Year==2015)
   p1<-nPlot(val ~ Stock, group =  'Sc', data = dat, type = 'multiBarChart')
   p1$chart(showControls = FALSE,color = brewer.pal(n=10,"Set3"),margin = list(left = 75,right = 50))
   p1$yAxis(axisLabel = 
              if(input$Forecastfilter=="SSB")
                {paste("Predicted SSB 2017")}
              else{paste("Predicted",input$Forecastfilter,"2016")})
   p1$addParams(width=600,dom = 'Adv2015')
   return(p1)}
     else if(input$Forecastfilter2==2){
       dat<-filter(Advice,var==input$Forecastfilter,Sc!="Single Species Advice",Advice.Year==2015)
       p1<-nPlot(Ratio ~ Stock, group =  'Sc', data = dat, type = 'multiBarChart')
       p1$chart(showControls = FALSE,color = brewer.pal(n=10,"Set3"),margin = list(left = 75,right = 50))
       p1$yAxis(axisLabel = 
                  if(input$Forecastfilter=="SSB")
                  {paste("Predicted SSB 2017 relative to the Single Species Advice")}
                else{paste("Predicted",input$Forecastfilter,"2016 relative to the Single Species Advice")})
       p1$addParams(width=600,dom = 'Adv2015')
       return(p1)
     }
     else if(input$Forecastfilter2==3){dat<-filter(Advice,var==input$Forecastfilter,Sc!="Single Species Advice",Advice.Year==2015)
     p1<-nPlot(Diff ~ Stock, group =  'Sc', data = dat, type = 'multiBarChart')
     p1$chart(showControls = FALSE,color = brewer.pal(n=10,"Set3"),margin = list(left = 75,right = 50))
     p1$yAxis(axisLabel = 
                if(input$Forecastfilter=="SSB")
                {paste("Difference between predicted SSB 2017 and Single Spicies Advice")}
              else{paste("Difference between predicted",input$Forecastfilter,"2016 and Single Spicies Advice")})
     p1$addParams(width=600,dom = 'Adv2015')
     return(p1)}
   })
   
 output$Forecast2015<-renderUI({
  list(fluidRow(column(5,showOutput("Ass2015", "nvd3")),
 column(5,showOutput("Adv2015", "nvd3"))))
 })
 
 output$Ass2016 <- renderChart({
   dat<-filter(Assessment,var==input$Forecastfilter,Advice.Year==2016)
   p1<-nPlot(value ~ Year, group =  'FishStock', data = dat, type = 'lineWithFocusChart')
   p1$chart(color=rev(col))
   p1$addParams(width=500,dom = 'Ass2016')
   return(p1)
 })
 
 output$Adv2016 <- renderChart({
   if(input$Forecastfilter2==1){
     dat<-filter(Advice,var==input$Forecastfilter,Advice.Year==2016)
     p1<-nPlot(val ~ Stock, group =  'Sc', data = dat, type = 'multiBarChart')
     p1$chart(showControls = FALSE,color = brewer.pal(n=10,"Set3"),margin = list(left = 75,right = 50))
     p1$yAxis(axisLabel = 
                if(input$Forecastfilter=="SSB")
                {paste("Predicted SSB 2018")}
              else{paste("Predicted",input$Forecastfilter,"2017")})
     p1$addParams(width=600,dom = 'Adv2016')
     return(p1)}
   else if(input$Forecastfilter2==2){
     dat<-filter(Advice,var==input$Forecastfilter,Sc!="Single Species Advice",Advice.Year==2016)
     p1<-nPlot(Ratio ~ Stock, group =  'Sc', data = dat, type = 'multiBarChart')
     p1$chart(showControls = FALSE,color = brewer.pal(n=10,"Set3"),margin = list(left = 75,right = 50))
     p1$yAxis(axisLabel = 
                if(input$Forecastfilter=="SSB")
                {paste("Predicted SSB 2018 relative to the Single Species Advice")}
              else{paste("Predicted",input$Forecastfilter,"2017 relative to the Single Species Advice")})
     p1$addParams(width=600,dom = 'Adv2016')
     return(p1)
   }
   else if(input$Forecastfilter2==3){dat<-filter(Advice,var==input$Forecastfilter,Sc!="Single Species Advice",Advice.Year==2016)
   p1<-nPlot(Diff ~ Stock, group =  'Sc', data = dat, type = 'multiBarChart')
   p1$chart(showControls = FALSE,color = brewer.pal(n=10,"Set3"),margin = list(left = 75,right = 50))
   p1$yAxis(axisLabel = 
              if(input$Forecastfilter=="SSB")
              {paste("Difference between predicted SSB 2018 and Single Spicies Advice")}
            else{paste("Difference between predicted",input$Forecastfilter,"2017 and Single Spicies Advice")})
   p1$addParams(width=600,dom = 'Adv2016')
   return(p1)}
 })
 
 output$Forecast2016<-renderUI({
   fluidRow(column(5,showOutput("Ass2016", "nvd3")),
            column(5,showOutput("Adv2016", "nvd3")))
 })
 
 output$Ass2017 <- renderChart({
   dat<-filter(Assessment,var==input$Forecastfilter,Advice.Year==2017)
   p1<-nPlot(value ~ Year, group =  'FishStock', data = dat, type = 'lineWithFocusChart')
   p1$chart(color=rev(col))
   p1$addParams(width=500,dom = 'Ass2017')
   return(p1)
 })
 
 output$Adv2017 <- renderChart({
   if(input$Forecastfilter2==1){
     dat<-filter(Advice,var==input$Forecastfilter,Advice.Year==2017)
     p1<-nPlot(val ~ Stock, group =  'Sc', data = dat, type = 'multiBarChart')
     p1$chart(showControls = FALSE,color = brewer.pal(n=10,"Set3"),margin = list(left = 75,right = 50))
     p1$yAxis(axisLabel = 
                if(input$Forecastfilter=="SSB")
                {paste("Predicted SSB 2019")}
              else{paste("Predicted",input$Forecastfilter,"2018")})
     p1$addParams(width=600,dom = 'Adv2017')
     return(p1)}
   else if(input$Forecastfilter2==2){
     dat<-filter(Advice,var==input$Forecastfilter,Sc!="Single Species Advice",Advice.Year==2017)
     p1<-nPlot(Ratio ~ Stock, group =  'Sc', data = dat, type = 'multiBarChart')
     p1$chart(showControls = FALSE,color = brewer.pal(n=10,"Set3"),margin = list(left = 75,right = 50))
     p1$yAxis(axisLabel = 
                if(input$Forecastfilter=="SSB")
                {paste("Predicted SSB 2019 relative to the Single Species Advice")}
              else{paste("Predicted",input$Forecastfilter,"2018 relative to the Single Species Advice")})
     p1$addParams(width=600,dom = 'Adv2017')
     return(p1)
   }
   else if(input$Forecastfilter2==3){dat<-filter(Advice,var==input$Forecastfilter,Sc!="Single Species Advice",Advice.Year==2017)
   p1<-nPlot(Diff ~ Stock, group =  'Sc', data = dat, type = 'multiBarChart')
   p1$chart(showControls = FALSE,color = brewer.pal(n=10,"Set3"),margin = list(left = 75,right = 50))
   p1$yAxis(axisLabel = 
              if(input$Forecastfilter=="SSB")
              {paste("Difference between predicted SSB 2019 and Single Spicies Advice")}
            else{paste("Difference between predicted",input$Forecastfilter,"2018 and Single Spicies Advice")})
   p1$addParams(width=600,dom = 'Adv2017')
   return(p1)}
 })
 
 output$Forecast2017<-renderUI({
   fluidRow(column(5,showOutput("Ass2017", "nvd3")),
            column(5,showOutput("Adv2017", "nvd3")))
 })
 
 output$Ass2018 <- renderChart({
   dat<-filter(Assessment,var==input$Forecastfilter,Advice.Year==2018)
   p1<-nPlot(value ~ Year, group =  'FishStock', data = dat, type = 'lineWithFocusChart')
   p1$chart(color=rev(col))
   p1$addParams(width=500,dom = 'Ass2018')
  
   return(p1)
 })
 
 output$Adv2018 <- renderChart({
   if(input$Forecastfilter2==1){
     dat<-filter(Advice,var==input$Forecastfilter,Advice.Year==2018)
     p1<-nPlot(val ~ Stock, group =  'Sc', data = dat, type = 'multiBarChart')
     p1$chart(showControls = FALSE,color = brewer.pal(n=10,"Set3"),margin = list(left = 75,right = 50))
     p1$yAxis(axisLabel = 
                if(input$Forecastfilter=="SSB")
                {paste("Predicted SSB 2020")}
              else{paste("Predicted",input$Forecastfilter,"2019")})
     p1$addParams(width=600,dom = 'Adv2018')
     return(p1)}
   else if(input$Forecastfilter2==2){
     dat<-filter(Advice,var==input$Forecastfilter,Sc!="Single Species Advice",Advice.Year==2018)
     p1<-nPlot(Ratio ~ Stock, group =  'Sc', data = dat, type = 'multiBarChart')
     p1$chart(showControls = FALSE,color = brewer.pal(n=10,"Set3"),margin = list(left = 75,right = 50))
     p1$yAxis(axisLabel = 
                if(input$Forecastfilter=="SSB")
                {paste("Predicted SSB 2020 relative to the Single Species Advice")}
              else{paste("Predicted",input$Forecastfilter,"2019 relative to the Single Species Advice")})
     p1$addParams(width=600,dom = 'Adv2018')
     return(p1)
   }
   else if(input$Forecastfilter2==3){dat<-filter(Advice,var==input$Forecastfilter,Sc!="Single Species Advice",Advice.Year==2018)
   p1<-nPlot(Diff ~ Stock, group =  'Sc', data = dat, type = 'multiBarChart')
   p1$chart(showControls = FALSE,color = brewer.pal(n=10,"Set3"),margin = list(left = 75,right = 50))
   p1$yAxis(axisLabel = 
              if(input$Forecastfilter=="SSB")
              {paste("Difference between predicted SSB 2020 and Single Spicies Advice")}
            else{paste("Difference between predicted",input$Forecastfilter,"2019 and Single Spicies Advice")})
   p1$addParams(width=600,dom = 'Adv2018')
   return(p1)}
 })
 
 output$Forecast2018<-renderUI({
   fluidRow(column(5,showOutput("Ass2018", "nvd3")),
            column(5,showOutput("Adv2018", "nvd3")))
 })
 
 output$TitletableForecast<-renderText({
   t<-as.numeric(input$ICESAyear)
   if(input$Forecastfilter=="SSB"){
   paste("SSB",t+2,"resulting from mixed fisheries scenario applied in",t ,".")}
   else{paste(input$Forecastfilter," per mixed fisheries scenario",t,"." )}
   
 })
 
 
 output$TabcondPanel<-renderUI({
  if(input$Forecastfilter=="SSB" & input$Forecastfilter2==1){fluidRow(column(4,
     prettyRadioButtons("idSSB",
     label = "", thick = T, animation = "pulse",
      choices = list("SSB > Bpa or MSY Btrigger" = 1,
                     "SSB > Blim" = 2,
                      "SSB< Blim" = 3),
       selected = character(0), inline = F
     )),column(7,dataTableOutput("SSBoptionsT")))}
   else if(input$Forecastfilter=="F"& input$Forecastfilter2==1){fluidRow(column(4,
        prettyRadioButtons("idF",
        label = "", thick = T, animation = "pulse",
        choices = list("F <= Fmsy " = 1, "F > Fmsy, < Fpa" = 2,
        "F > Fpa" = 3, "F > Flim"=4),
        selected = character(0), inline = F
        )),column(7,dataTableOutput("FoptionsT")))}
   else{dataTableOutput("Alloptions")} 
 })
 
 output$Alloptions <- DT::renderDataTable({
   if(input$Forecastfilter2==1){
   t<-filter(Advice,Advice.Year==input$ICESAyear,var==input$Forecastfilter)
   t<-t[c(1,3,5)]
   x <-  reshape(t, timevar = 'Sc', sep = '_', direction = 'wide',idvar ='Stock')
   colnames(x)<-c("Stock",as.character(unique(t$Sc)))
   datatable(x, rownames = F,
             options = list(dom = 'C<"clear">rti', pageLength = -1))}
  else if(input$Forecastfilter2==2){
     t<-filter(Advice,Advice.Year==input$ICESAyear,var==input$Forecastfilter)
     t<-na.omit(t[c(1,3,8)])
     x <-  reshape(t, timevar = 'Sc', sep = '_', direction = 'wide',idvar ='Stock')
     colnames(x)<-c("Stock",as.character(unique(t$Sc)))
     datatable(x, rownames = F,
               options = list(dom = 'C<"clear">rti', pageLength = -1))}
   else if(input$Forecastfilter2==3){
     t<-filter(Advice,Advice.Year==input$ICESAyear,var==input$Forecastfilter)
     t<-t[c(1,3,7)]
     x <-  reshape(t, timevar = 'Sc', sep = '_', direction = 'wide',idvar ='Stock')
     colnames(x)<-c("Stock",as.character(unique(t$Sc)))
     datatable(x, rownames = F,
               options = list(dom = 'C<"clear">rti', pageLength = -1))}
 })
 
 output$SSBoptionsT <- DT::renderDataTable({
   t<-filter(Advice,Advice.Year==input$ICESAyear,var=="SSB")
  t<-t[c(1,3,5)]
    x <-  reshape(t, timevar = 'Stock', sep = '_', direction = 'wide',idvar ='Sc')
     colnames(x)<-c("",as.character(unique(t$Stock)))
     datatable(x, rownames = F,
               options = list(dom = 'C<"clear">rti', pageLength = -1))
})
 
 observeEvent(input$idSSB, {
   t<-filter(Advice,Advice.Year==input$ICESAyear,var=="SSB")
   Blim<-unique(t$Blim)
   Btrigger<-unique(t$MSYBtrigger)
   Bpa<-unique(t$Bpa)
   t<-t[c(1,3,5)]
   x <-  reshape(t, timevar = 'Stock', sep = '_', direction = 'wide',idvar ='Sc')
   colnames(x)<-c("",as.character(unique(t$Stock)))
   if(input$idSSB == "1"){
     output$SSBoptionsT <- DT::renderDataTable({
 
     datatable(x, rownames = F,
               options = list(dom = 'C<"clear">rti', pageLength = -1))%>%
       formatStyle(2, color = JS(paste("value >",Bpa[1],"|| value >",Btrigger[1],"? 'red' : ''")))%>%
       formatStyle(3, color = JS(paste("value >",Bpa[2],"|| value >",Btrigger[2],"? 'red' : ''")))%>%
       formatStyle(4, color = JS(paste("value >",Bpa[3],"|| value >",Btrigger[3],"? 'red' : ''")))
    })}
   else if (input$idSSB == "2"){ output$SSBoptionsT <- DT::renderDataTable({
   datatable(x, rownames = F,
               options = list(dom = 'C<"clear">rti', pageLength = -1))%>%
       formatStyle(2, color = JS(paste("value >",Blim[1],"? 'red' : ''")))%>%
       formatStyle(3, color = JS(paste("value >",Blim[2],"? 'red' : ''")))%>%
       formatStyle(4, color = JS(paste("value >",Blim[3],"? 'red' : ''")))
   })}
   else if (input$idSSB == "3"){ output$SSBoptionsT <- DT::renderDataTable({
   datatable(x, rownames = F,
               options = list(dom = 'C<"clear">rti', pageLength = -1))%>%
       formatStyle(2, color = JS(paste("value <",Blim[1],"? 'red' : ''")))%>%
       formatStyle(3, color = JS(paste("value <",Blim[2],"? 'red' : ''")))%>%
       formatStyle(4, color = JS(paste("value <",Blim[3],"? 'red' : ''")))
   })}
   
 })
 
 output$FoptionsT <- DT::renderDataTable({
   t<-filter(Advice,Advice.Year==input$ICESAyear,var=="F")
   t<-t[c(1,3,5)]
   x <-  reshape(t, timevar = 'Stock', sep = '_', direction = 'wide',idvar ='Sc')
   colnames(x)<-c("",as.character(unique(t$Stock)))
   datatable(x, rownames = F,
             options = list(dom = 'C<"clear">rti', pageLength = -1))
 })
 observeEvent(input$idF, {
  t<-filter(Advice,Advice.Year==input$ICESAyear,var=="F")
     Fmsy<-unique(t$Fmsy)
     Fpa<-unique(t$Fpa)
     Flim<-unique(t$Flim)
     t<-t[c(1,3,5)]
     x <-  reshape(t, timevar = 'Stock', sep = '_', direction = 'wide',idvar ='Sc')
     colnames(x)<-c("",as.character(unique(t$Stock)))
  if (input$idF == "1"){ output$FoptionsT <- DT::renderDataTable({
       datatable(x, rownames = F,
                 options = list(dom = 'C<"clear">rti', pageLength = -1))%>%
         formatStyle(2, color = JS(paste("value <=",Fmsy[1],"? 'red' : ''")))%>%
         formatStyle(3, color = JS(paste("value <=",Fmsy[2],"? 'red' : ''")))%>%
         formatStyle(4, color = JS(paste("value <=",Fmsy[3],"? 'red' : ''")))
     })}
     else if(input$idF == "2"){
       output$FoptionsT <- DT::renderDataTable({
         
         datatable(x, rownames = F,
                   options = list(dom = 'C<"clear">rti', pageLength = -1))%>%
           formatStyle(2, color = JS(paste("value >",Fmsy[1],"&& value <",Fpa[1],"? 'red' : ''")))%>%
           formatStyle(3, color = JS(paste("value >",Fmsy[2],"&& value <",Fpa[2],"? 'red' : ''")))%>%
           formatStyle(4, color = JS(paste("value >",Fmsy[3],"&& value <",Fpa[3],"? 'red' : ''")))
       })}
     else if (input$idF == "3"){ output$FoptionsT <- DT::renderDataTable({
       datatable(x, rownames = F,
                 options = list(dom = 'C<"clear">rti', pageLength = -1))%>%
         formatStyle(2, color = JS(paste("value >",Fpa[1],"? 'red' : ''")))%>%
         formatStyle(3, color = JS(paste("value >",Fpa[2],"? 'red' : ''")))%>%
         formatStyle(4, color = JS(paste("value >",Fpa[3],"? 'red' : ''")))
     })}
     else if (input$idF == "4"){ output$FoptionsT <- DT::renderDataTable({
       datatable(x, rownames = F,
                 options = list(dom = 'C<"clear">rti', pageLength = -1))%>%
         formatStyle(2, color = JS(paste("value >",Flim[1],"? 'red' : ''")))%>%
         formatStyle(3, color = JS(paste("value >",Flim[2],"? 'red' : ''")))%>%
         formatStyle(4, color = JS(paste("value >",Flim[3],"? 'red' : ''")))
     })}
     
 })
 
 
 
 
 
 

 

   
  ################################# 
   
   output$ICESlinkpdf <- renderUI({
     t<-filter(links,Year==input$ICESAyear)
    url <- a("ICES Mixed-fisheries advice", href=paste(t[1,2]),target="_blank")
     tagList(url)
   })
  
  ###end of server###  
    }


########################################## ui ##################################################


ui <- fluidPage(tags$head(
  tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                  .btn:focus{ background-color:lightgrey;}
                  .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter,
                  .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,
                  .dataTables_wrapper .dataTables_paginate {color: #000000; }
                  thead { color: #808b96;}tbody {color: #000000;"))),
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"),
theme = shinytheme("spacelab"), #spacelab superhero
  titlePanel("Mixed Fisheries"),
  navlistPanel(id="mainpanel", widths=c(2,10),
               tabPanel(" Introduction",
                        value = "mp", icon = icon("home"),
                        fluidRow(column(width = 5, offset = 4, h2("Celtic Seas Ecoregion.",
                                                                  style = "font-family: 'Lobster', cursive;
                                                                  font-weight: 500; line-height: 1.1; "))), hr(),
                        fluidRow(column(width = 6, 
                        div(style = " border-radius: 25px;height:380px;background-color:#e5f5e0 ;color: black;
                            font-size:100%;",
                            tags$ul(
                              "The Celtic Seas ecoregion covers the northwestern shelf seas of the EU. 
                              It includes areas of the deeper eastern Atlantic Ocean and coastal seas that are
                              heavily influenced by oceanic inputs. The ecoregion ranges from north of Shetland 
                              to Brittany in the south. Three key areas constitute this ecoregion:",
                              p(""),
                              tags$li("Northern parts; the Malin shelf, west of Scotland, eastern Rockall Bank, 
                                      and north of Scotland (parts of Subdivision 2.a.2, 
                                      divisions 4.a and 6.a, and Subdivision 6.b.2);"),
                              p(""),
                              tags$li("the Celtic Sea and west of Ireland (Division 7.b and Subdivision 7.c.2; 
                                      parts of divisions 7.e, 7.f, 7.g, 7.h, and subdivisions 7.j.2
                                      and 7.k.2);"),
                              p(""),
                              tags$li("the Irish Sea (Division 7.a)."),
                              "In the north there are strong linkages with the North Sea, in the southeast
                              a strong linkage with the channel area, and in the south a strong link with the Bay of Biscay. 
                              The eastern part of the Rockall Bank is within the geographic scope of the ecoregion although 
                              it is separated from the western European shelf by the Rockall Trough."
                              )),br(),
                          div(
                          style = " border-radius: 25px;height:27px;width:220px;background-color:#cc4c02;font-size:
                          22px;color:lightblue;",
                          tags$ul("Mixed fisheries.")
                        ), br(),
                        
                        div(style = " border-radius: 25px;height:175px;background-color:#e5f5e0 ;color: black;
                            font-size:100%;",
                            tags$ul(
                              "Fishing operations typically catch more than one species at a time, 
                            although some fishing operations are more species selective than others.
                            For example, pelagic trawling tends to catch only one species whereas demersal 
                            trawling normally catches several species simultaneously. These operations are
                            reported to ICES at a level that is aggregated for each EU Member State by
                            key descriptors of fishing activity (hereafter called métier). The catch composition resulting from any fishing activity is
                            described as a technical interaction. "
                            ))),
                        column(
                          width = 6,img(
                            src = "images/CSEregion.png", height = "500px",
                            width = "600px" ,style='padding:0px;'
                          ),p(HTML("                                          *The Celtic Sea ecoregion highlited in yellow. "),style="white-space: pre-wrap")
                          )), useShinyalert(), br(),
                         fluidRow(
                           column(width = 5, div(
                            style = " border-radius: 25px;height:27px;background-color:#cc4c02;font-size:
                            18px;color:lightblue;",
                            tags$ul("Who is fishing and description of the fisheries.")
                          ), hr()),
                          column(width = 3,offset=3, div(
                            style = " border-radius: 25px;height:27px;background-color:#cc4c02;font-size:
                            18px;color:lightblue;",
                            tags$ul("Metier Definitions.")
                          ))
                        ),
                        fluidRow(
                                 column(width = 3, div(
                                   style = "display: inline-block;vertical-align:top; width: 225px;",
                                   selectInput("FishGear", "", choices = c("Who is Fishing", "Description of the Fisheries")), class = "btn-link"
                                 )),column(width = 6,offset=3, div(
                                   style = "display: inline-block;vertical-align:top; width: 625px;",
                                   selectInput("Area1", "", choices = c("Select Area",levels(MetierDes$Area))), class = "btn-link"
                                 )))
                        , uiOutput("I_selections"),
                        fluidRow( column(6, uiOutput("fishing")),column(
                          width = 6,uiOutput("Metierdesc")
                          ))
                        ),
              navbarMenu("Data Explorer",tabPanel("Landings: Celtic Sea",
                        value = "mi", icon = icon("fish"),
                        tabsetPanel(
                          id = "Ltabselected", type = "pills",
                          tabPanel("Page1",
                                   value = "page1",
                                   fluidRow(
                                     column(width = 5, offset = 4, h2("The Proportion of Landings.", style = "font-family: 'Lobster', cursive;
                                                                      font-weight: 500; line-height: 1.1; ")),
                                     actionButton("info1", icon("info-circle"), style = "padding-top: 7px;
                                                  padding-bottom: 5px; padding-right: 20px;", class = "btn-primary")
                                     ), hr(),
                                   fluidRow(
                                     column(width = 3, div(
                                       style = "display: inline-block;vertical-align:top; width: 225px;",
                                       selectInput("Country", "Select Country:", choices = c("All",levels(test$Country))), class = "btn-link"
                                     )),
                                     column(width = 3, div(
                                       style = "display: inline-block;vertical-align:top; width: 225px;",
                                       selectInput("name", "Select Parameter:",
                                                   choices = c("Metier by Species" = 1, "Species by Metier" = 2)
                                       ), class = "btn-link"
                                     )),
                                     uiOutput("L_selections")
                                   ),
                                   fluidRow(
                                     column(width = 8, plotlyOutput("plotL1")
                                            %>%
                                              withSpinner(color = "#0dc5c1")),
                                     column(width = 4, uiOutput("pieUI")
                                            %>%
                                              withSpinner(color = "#0dc5c1"))
                                   )),
                          tabPanel("Page2",
                                   value = "page2",
                                   fluidRow(
                                     column(width = 5, offset = 4, h2("Total Landings.", style = "font-family: 'Lobster', cursive;
                                                                      font-weight: 500; line-height: 1.1; ")),
                                     actionButton("info2", icon("info-circle"),
                                                  style = "padding-top: 7px;padding-bottom: 5px; padding-right: 20px;", class = "btn-primary"
                                     )
                                     ), hr(),
                                   fluidRow(column(3, div(
                                     style = "width:220px;",
                                     tabsetPanel(
                                       id = "LP2tabset", tabPanel(
                                         "Selection 1", selectInput("set1", label = "Select Metier", levels(test$Metier), selectize = T),
                                         selectInput("set2", label = "Select Year", c(2009:2017), selectize = T),
                                         selectInput("Landings1", label = "Landings:",
                                                     choices = c("Weight in tonnes", "Value in Euros", "Price per KG"), selectize = T)
                                       ),
                                       tabPanel(
                                         "Selection 2", selectInput("set3", label = "Select Species", levels(test$Species), selectize = T),
                                         selectInput("set4", label = "Select Year", c(2009:2017), selectize = T), 
                                         selectInput("Landings2", label = "Landings:", choices = c("Weight in tonnes", "Value in Euros", "Price per KG"), selectize = T)
                                       )
                                     ), class = "btn-link"
                                   )), column(width = 9, uiOutput("Lpage2")))
                          ),
                          tabPanel("Page3",
                                   value = "page3",
                                   fluidRow(column(width = 5, offset = 4, h2("Landings Data.", style = "font-family: 'Lobster', cursive;
                                                                             font-weight: 500; line-height: 1.1; "))), hr(),
                                   fluidRow(
                                     column(2, selectInput("LCountry", "Country:", c("All", unique(as.character(CelticEcoSpecies$Country))),
                                                           multiple = F, selected = "All"
                                     ), class = "btn-link"),
                                     column(2, selectInput("LYear", "Year:", c("All", unique(as.character(CelticEcoSpecies$Year))),
                                                           multiple = F, selected = "All"
                                     ), class = "btn-link"),
                                     column(3, selectInput("LMetier", "Metier:", c("All", unique(as.character(CelticEcoSpecies$lvl4))),
                                                           multiple = F, selected = "All"
                                     ), class = "btn-link"),
                                     column(3, selectInput("LSpecies", "Species:", c("All", unique(as.character(CelticEcoSpecies$Species))),
                                                           multiple = F, selected = "All"
                                     ), class = "btn-link"),
                                     column(2, selectInput("LArea", "Area:", c("All", unique(as.character(CelticEcoSpecies$Area))),
                                                           multiple = F, selected = "All"
                                     ), class = "btn-link")
                                   ),
                                   fluidRow(DT::dataTableOutput("tableL"))
                                   )
                          )
               )
               , tabPanel(" Effort: Celtic Sea",
                          value = "sb", icon = icon("ship"),
                          tabsetPanel(
                            id = "Etabselected", type = "pills",
                            tabPanel("Page1",
                                     value = "page1",
                                     fluidRow(
                                       column(width = 5, offset = 4, h2("The Proportion of Effort.", style = "font-family: 'Lobster', cursive;
                                                                        font-weight: 500; line-height: 1.1; ")),
                                       actionButton("info3", icon("info-circle"), style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px;", class = "btn-primary")
                                       ), hr(),
                                     fluidRow(
                                       column(width = 3, div(style = "display: inline-block;vertical-align:top; width: 225px;", selectInput("CountryE", "Select Country:", choices = c("All",levels(test$Country))), class = "btn-link")),
                                       column(width = 3, div(style = "display: inline-block;vertical-align:top; width: 225px;", selectInput("nameE", "Select Parameter:", choices = c("Metier by Vessel length" = 1, "Vessel length by Metier" = 2)), class = "btn-link")),
                                       uiOutput("E_selections")
                                     ),
                                     fluidRow(
                                       column(width = 8, plotlyOutput("plotE1")
                                              %>%
                                                withSpinner(color = "#0dc5c1")),
                                       column(width = 4, uiOutput("pieEUI")
                                              %>%
                                                withSpinner(color = "#0dc5c1"))
                                     )
                            ),
                            tabPanel("Page2",
                                     value = "page2",
                                     fluidRow(
                                       column(width = 5, offset = 4, h2("Total Effort KW_days.", style = "font-family: 'Lobster', cursive;
                                                                        font-weight: 500; line-height: 1.1; ")),
                                       actionButton("info4", icon("info-circle"), style = "padding-top: 7px;padding-bottom: 5px; 
                                                    padding-right: 20px;", class = "btn-primary")
                                       ), hr(),
                                     fluidRow(column(3, div(
                                       style = "width:220px;",
                                       tabsetPanel(
                                         id = "EP2tabset",
                                         tabPanel(
                                           "Selection 1", selectInput("setE1", label = "Select Metier", levels(testE$Metier), selectize = T),
                                           selectInput("setE2", label = "Select Year", c(2009:2017), selectize = T)
                                         ),
                                         tabPanel(
                                           "Selection 2", selectInput("setE3", label = "Select Vessel Length", levels(testE$Vessel_length), selectize = T),
                                           selectInput("setE4", label = "Select Year", c(2009:2017), selectize = T)
                                         )
                                       ), class = "btn-link"
                                     )), column(width = 9, uiOutput("Epage2")))
                                       ),
                            tabPanel("Page3",
                                     value = "page3",
                                     fluidRow(column(width = 5, offset = 4, h2("Effort Data.", style = "font-family: 'Lobster', cursive;
                                                                               font-weight: 500; line-height: 1.1; "))), hr(),
                                     fluidRow(
                                       column(2, selectInput("ECountry", "Country:", c("All", unique(as.character(CelticCE$Country))),
                                                             multiple = F, selected = "All"
                                       ), class = "btn-link"),
                                       column(2, selectInput("EYear", "Year:", c("All", unique(as.character(CelticCE$Year))),
                                                             multiple = F, selected = "All"
                                       ), class = "btn-link"),
                                       column(3, selectInput("EMetier", "Metier:", c("All", unique(as.character(CelticCE$lvl4))),
                                                             multiple = F, selected = "All"
                                       ), class = "btn-link"),
                                       column(3, selectInput("EVessel", "Vessel Length:", c("All", unique(as.character(CelticCE$Vessel_length))),
                                                             multiple = F, selected = "All"
                                       ), class = "btn-link"),
                                       column(2, selectInput("EArea", "Area:", c("All", unique(as.character(CelticCE$Area))),
                                                             multiple = F, selected = "All"
                                       ), class = "btn-link")
                                     ),
                                     # Create a new row for the table.
                                     fluidRow(DT::dataTableOutput("tableE"))
                                     )
  )
)),

tabPanel("Tools", value = "et", icon = icon("wrench"),
       selectInput(inputId = "Toolselected", label="Tool", choices=c("Effort App","Implications of catch decreases App","Catchability App","Partial F App","Quota share App"),#"Raw accessions App",
                     multiple=FALSE, selected = "Effort App"),
         #conditionalPanel("input.Toolselected=='Raw accessions App'"),
         conditionalPanel(condition = "input.Toolselected == 'Effort App'",
                          tabsetPanel(id="effortappPanel", type="pills",
                                      tabPanel("Fleet Effort Tables",
                                               fluidPage(
                                                 h2("Effort data",style = "font-family: 'Lobster', cursive;
                                                 font-weight: 500; line-height: 1.1;text-align:center "), #paste(textOutput("Area"),
                                                 fluidRow(
                                                   column(3,uiOutput("fleet.yearfilter")),
                                                   column(3,uiOutput("fleet.countryfilter"))
                                                 ),
                                                 fluidRow(
                                                   DT::dataTableOutput("efftable")
                                                   %>% withSpinner(color="#0dc5c1")
                                                 )
                                               )),
                                      tabPanel("Effort time series",
                                               fluidPage(
                                                 h2("Effort data",style = "font-family: 'Lobster', cursive;
                                                 font-weight: 500; line-height: 1.1;text-align:center "), 
                                                 fluidRow(
                                                   column(3,uiOutput("time.countryfilter")) 
                                                 ),
                                                 mainPanel(
                                                   plotlyOutput("plotEffTS", width = '1200px', height = '800px')
                                                   %>% withSpinner(color="#0dc5c1")
                                                 )
                                               ) #end of fluidPage
                                      )
                          ) #end of tabsetPanel
         ), #end of conditionalPanel
       conditionalPanel(condition = "input.Toolselected == 'Implications of catch decreases App'",
                        fluidPage( h2("Visualising the implications of catch decreases for fleets in a mixed fishery context",style = "font-family: 'Lobster', cursive;
                                                 font-weight: 500; line-height: 1.1;text-align:center "),
                                               fluidRow(
                                                 plotOutput("plot",width = 800, height = 700),
                                                 absolutePanel(id="controls",top = 150, left = 700, width = 400, height = "auto", fixed=FALSE, draggable = TRUE,
                                                               sliderInput("whitingslider", "Choose % reduction in Whiting Catch:", min = -100, max =0, value = 0, 
                                                                           step = NULL, sep = "", animate = FALSE, post  = " %")))
                                               )),#end of conditionalPanel
         conditionalPanel("input.Toolselected == 'Catchability App'", 
                          tabsetPanel(id="FleetCatchabilityPanel", type="tabs",
                                      tabPanel("Fleet Catchability Tables",
                                               fluidPage(
                                                 h2("Catchability data",style = "font-family: 'Lobster', cursive;
                                                 font-weight: 500; line-height: 1.1;text-align:center "), #paste(Area,
                                                 fluidRow(
                                                   column(3,uiOutput("table.yearfilter")),
                                                   column(3,uiOutput("table.stockfilter"))
                                                 ),
                                                 fluidRow(
                                                   DT::dataTableOutput("Catchtable")
                                                   %>% withSpinner(color="#0dc5c1")
                                                 )
                                               )),
                                      tabPanel("Catchability Time Series",
                                               fluidPage(
                                                 h2("Catchability data",style = "font-family: 'Lobster', cursive;
                                                 font-weight: 500; line-height: 1.1;text-align:center "), #paste(Area,
                                                 fluidRow(
                                                   column(3,uiOutput("plot.countryfilter")),
                                                   column(3,uiOutput("plot.stockfilter"))
                                                 ),
                                                 mainPanel(
                                                   plotOutput("plotCatchability", width = '1200px', height = '2000px')
                                                   %>% withSpinner(color="#0dc5c1")
                                                 )
                                               )
                                      ),
                                      tabPanel("Catchability Plot",
                                               fluidPage(
                                                 h2("Catchability data",style = "font-family: 'Lobster', cursive;
                                                 font-weight: 500; line-height: 1.1;text-align:center "), #paste(Area,
                                                 fluidRow(
                                                   column(3,uiOutput("plot2.fleetfilter"))
                                                 ),
                                                 mainPanel(
                                                   plotlyOutput("plot2Catchability", width = '1200px', height = '2000px')
                                                   %>% withSpinner(color="#0dc5c1")
                                                 )
                                               )
                                      )
                                      
                          )
         ),
         conditionalPanel("input.Toolselected == 'Partial F App'",
                          tabsetPanel(id="Partial F Panel", type="tabs",
                                      tabPanel("Fleet Partial F Tables",
                                               fluidPage(
                                                 h2("Partial F data",style = "font-family: 'Lobster', cursive;
                                                 font-weight: 500; line-height: 1.1;text-align:center "),#paste(Area,
                                                 fluidRow(
                                                   column(3,uiOutput("PF.year.table")),
                                                   column(3,uiOutput("PF.stock.table"))        
                                                 ),
                                                 fluidRow(
                                                   DT::dataTableOutput("PFtable")
                                                   %>% withSpinner(color="#0dc5c1")
                                                 )
                                               )),
                                      tabPanel("Partial F time series",
                                               fluidPage(
                                                 h2("Partial F data",style = "font-family: 'Lobster', cursive;
                                                 font-weight: 500; line-height: 1.1;text-align:center "),#paste(Area,)
                                                 fluidRow(
                                                   column(3,uiOutput("PF.country.plot1")
                                                   ),
                                                   column(3,uiOutput("PF.stock.plot1")
                                                   )
                                                 ),
                                                 mainPanel(
                                                   plotOutput("plotPartialF", width = '1200px', height = '800px')
                                                   %>% withSpinner(color="#0dc5c1")
                                                 )
                                               ) #end of FluidPage
                                      )
                          )#end of tabsetPanel
         ), #end of Partial F conditionalPanel
         conditionalPanel("input.Toolselected == 'Quota share App'",
                          tabsetPanel(id="Quota share Panel", type="tabs",
                                      tabPanel("Fleet Landings share Tables",
                                               fluidPage(
                                                 h2("Landings Share data",style = "font-family: 'Lobster', cursive;
                                                 font-weight: 500; line-height: 1.1;text-align:center "),
                                                 fluidRow(
                                                   column(3,uiOutput("QS.year.table")),
                                                   column(3,uiOutput("QS.stock.table"))),
                                                # Create a new row for the table.
                                                 fluidRow(
                                                   DT::dataTableOutput("QStable")%>%
                                                     withSpinner(color="#0dc5c1")
                                                 )
                                               )),
                                      
                                      tabPanel("Landings share time series",
                                               fluidPage(
                                                 h2("Landings share data",style = "font-family: 'Lobster', cursive;
                                                 font-weight: 500; line-height: 1.1;text-align:center "),
                                                fluidRow(
                                               column(3,uiOutput("QS.fleet.plot1")
                                               ),
                                               column(3,uiOutput("QS.stock.plot1")
                                               )
                                      ),
                                      mainPanel(
                                        plotlyOutput("plotQS", width = '1200px', height = '800px')
                                        %>% withSpinner(color="#0dc5c1")
                                      )
                                               
                                               ))
                                      ,#end of tabPanel
                                      
                                      tabPanel("Landings composition time series",
                                               fluidPage(
                                                 h2("Landings Composition data",style = "font-family: 'Lobster', cursive;
                                                 font-weight: 500; line-height: 1.1;text-align:center "),
                                                 fluidRow(
                                                   column(3,uiOutput("QS.fleet.plot2")
                                                   )
                                                 ),
                                                 mainPanel(
                                                   plotlyOutput("plotQScomp", width = '1200px', height = '800px')
                                                  # plotlyOutput("plotQS", width = '800px', height = '800px')
                                                   %>% withSpinner(color="#0dc5c1")
                                                 )
                                                 
                                               )),#end of tabPanel
                                      tabPanel("Main fleets catching each stock",
                                               fluidPage(
                                                 h2("Main contributing fleets per stock and their landing species composition",style = "font-family: 'Lobster', cursive;
                                                 font-weight: 500; line-height: 1.1;text-align:center "),
                                                 fluidRow(
                                                   column(3,uiOutput("QS.stock.plot3")
                                                   ),
                                                   column(3,uiOutput("QS.year.plot3")
                                                   ),
                                                   column(3,uiOutput("QS.landings.plot3")
                                                 )),
                                                 
                                                 mainPanel(
                                                   uiOutput("QS.Landings.page")
                                                 )
                                               ) #end of FluidPage
                                      )#end of tabPanel
                          )
)),
tabPanel(" Mapping", value ="sc", icon = icon("map-marked"),
         fluidRow(column(width=11,offset=1,
                         selectInput("Species_selector","Select Species", choices=c(species)),
                         leafletOutput("map",  height='880')%>%
                           withSpinner(color = "#0dc5c1"))
         ),
         br(),
         br(),
         br(),
         br(),
         br(),
         br()
),
tabPanel("ICES Advice: Celtic Sea",
         value = "sc", icon = icon("line-chart"),
         fluidRow(column(
           width = 7, offset = 2,
           h2("Mixed-fisheries advice for Divisions 7.b-c, e–k (Celtic Sea).", style = "font-family: 'Lobster', cursive;
              font-weight: 500; line-height: 1.1; ")
           )), hr(), div(
           style = "border-radius: 25px,width:120px;color:orange",
           selectInput("ICESAyear",
                       label = "Select Advice Year", choices = c(2015:YEAR),
                       selectize = T,selected = YEAR
           )
         ),br(),"Mixed-fisheries considerations combine single-species stock assessments with 
                    information on the average catch composition and fishing effort of the fleets in the Celtic Sea.
                    In the absence of specific mixed-fisheries management objectives, ICES does not advise on unique
                    mixed-fisheries catch opportunities for the individual stocks. "
                   ,br(),hr(),
                    tabsetPanel(type="pills",
                                tabPanel( "Methods and Data",
                                          htmlOutput("MethodData"),br(), h4("Advice and management area for the three gadoids species considered.",
                                                                                   style = "font-weight:bold;text-decoration: underline;color:lightblue;text-align:center"
                                          ),tableOutput("ManagementArea"),br(),
                                          fluidRow(column(width=6,h4("Landings distribution.",
                                                                     style = "font-weight:bold;color:lightblue;text-align:center;text-decoration: underline;"
                                          ),fluidRow(column(width=6,htmlOutput("MethodData2")),column(width=6,showOutput("pieChart", "nvd3")))),column(width=6,h4("Single Species Reference points.",
                                                               style = "font-weight:bold;text-decoration: underline;color:lightblue;text-align:center"
                                          ),tableOutput("table.ref"))),div( style = "font-size: 18px;font-weight:bold;color:lightblue;text-align:center;text-decoration: underline;"
                                                                            ,textOutput("LandingBars")),br(),
                                          fluidRow(column(4,radioButtons("barChoice", label = h3(""),
                                          choices = list("Landings in '000 tonnes (stacked)" = 1,
                                                         "Landings in '000 tonnes (grouped)" = 3,
                                                         "% of Landings " = 2),  selected = 1)),column(8,showOutput("BarChart", "dimple")
                                                                                                       ,br(),textOutput("NoteBars")))),
                                tabPanel( "Summary of the Advice", htmlOutput("ICES.Summary"), 
                                          h4("Mixed-fisheries scenarios considered for the Celtic Sea gadoids.",
                                          style = "font-weight:bold;color:lightblue;text-decoration: underline;text-align:center"
                                ),tableOutput("ICES.SC")),
                                tabPanel( "Forecast",fluidRow(column(4,div(
                                  style = "border-radius: 25px,width:120px;color:orange",
                                  selectInput("Forecastfilter",
                                              label = "Select Parameter", choices = c("Catch","SSB","F"),
                                              selectize = T,selected = "Catch"
                                  ))
                                )),
                                fluidRow(column(6,h4("Single Species Assessment Summary. Weights are in tonnes",
                                         style = "font-weight:bold;color:lightblue;text-decoration: underline;"
                                                          )),
                                         column(6,prettyRadioButtons("Forecastfilter2",
                                                                     label = "", thick = T, animation = "pulse",
                                                                     choices = list("Total" = 1, "Relative to the Single Species Advice" = 2,
                                                                                    "Difference between Single Species Advice" = 3),
                                                                     selected = 1, inline = T
                                         ))),
                                fluidRow(conditionalPanel(condition = "input.ICESAyear == 2015",uiOutput("Forecast2015")),
                                         conditionalPanel(condition = "input.ICESAyear == 2016",uiOutput("Forecast2016")),
                                         conditionalPanel(condition = "input.ICESAyear == 2017",uiOutput("Forecast2017")),
                                         conditionalPanel(condition = "input.ICESAyear == 2018",uiOutput("Forecast2018")))
                               ,fluidRow(div(h4(textOutput("TitletableForecast"),
                                                style = "font-weight:bold;color:lightblue;text-decoration: underline;text-align:center")),
                                         br(),uiOutput("TabcondPanel"))
                                        
                              
                                
                                )),
         hr(), h5("Link to the ICES Mixed-fisheries advice pdf:"), 
         uiOutput("ICESlinkpdf"))
),

hr(),
fluidRow(width =12,
         img(src="Logos/Niamh.png", width = "1250px", height = "100px", 
             style="display: block; margin-left: auto; margin-right: auto;margin-top:0em")
)
)

shinyApp(ui, server)
