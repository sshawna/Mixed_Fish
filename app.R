<<<<<<< HEAD
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
#library(vmstools)
options(scipen=999)

data_fish <-  read.csv(file="data/Hackathon/Data.csv") 
CelticEcoSpecies <- read.csv("data/CaCS.csv")
test <- aggregate(CelticEcoSpecies$Landings, by = list(CelticEcoSpecies$Country
                                                       , CelticEcoSpecies$Year, CelticEcoSpecies$lvl4, CelticEcoSpecies$Species), FUN = "sum")
names(test) <- c("Country", "Year", "Metier", "Species", "Landings")
testL2 <- as.data.frame(CelticEcoSpecies %>% group_by(Country, Year, lvl4, Species, Area)
                        %>% summarise(Weight_in_tonnes = sum(Landings), Value_in_Euros = sum(Value)))
testL2$vpKG <- testL2$Value_in_Euros / (testL2$Weight_in_tonnes * 1000)
names(testL2) <- c("Country", "Year", "Metier", "Species", "Area", "Landings", "Value_in_Euros", "Price_per_KG")
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
projectionsAllyears <- read.csv("data/Repr_Advice/projectionsAllyears.csv") 
levels(projectionsAllyears$Stock) <- c("COD_CS", "HAD_CS", "MON_CS", "N_HKE", "N_MEG", "SOL_7E", "SOL_7FG", "WHG_CS")
dataForstock <- projectionsAllyears%>% filter(year != 2019, year != 2020)
BRPs <- read_csv("data/BRPs.csv") 
shareW <- read.csv("data/FCube/share1.csv")
effbystL <- read.csv("data/FCube/effbystL.csv")
effbyScen <- read.csv("data/FCube/effbyScenarioW.csv")
effbyScenL <- read.csv("data/FCube/effbyScenario.csv")
effbymet<-read.csv("data/FCube/effbymet.csv")
chocked<-read.csv("data/FCube/ChockedS.csv")
FCubepage3 <- read.csv("data/FCube/FCubePage.csv")
FCubepage3Radar <- dcast(FCubepage3, sc + year + value ~ stock, value.var = "RelativeToSS")
sp <- c("Cod","Haddock","Whiting","Plaice", "Sole", "Hake", "Megrim", "Anglerfish", "Nephrops")




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
      else if (input$FishGear == "Gear Type") {
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
        width = "600px", style = "padding-top: 7px; padding-bottom: 5px; 
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
    else if (input$FishGear == "Gear Type" & input$GearInt == "Select Gear") {
      img(
        src = "images/gear.jpg", height = "400px",
        width = "600px", style = "padding-top: 7px; padding-bottom: 5px; 
        padding-right: 20px;"
      )
    }
    
    else if (input$FishGear == "Gear Type" & input$GearInt == "Otter trawl") {
      div(
        style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;",
        HTML("Otter trawl is the main gear by effort used in demersal fisheries in the Celtic Sea ecoregion .
             The species caught depends on the area, depth-range habitat,
             and season fished as well as on the cod-end mesh size, but in all cases 
             the catches consist of a mixture of different species.")
        )
    }
    else if (input$FishGear == "Gear Type" & input$GearInt == "Nephrops-directed otter trawlers") {
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
    else if (input$FishGear == "Gear Type" & input$GearInt == "Finfish-directed otter trawlers and seiners") {
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
    else if (input$FishGear == "Gear Type" & input$GearInt == "Deep-water trawl fisheries") {
      div(
        style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;",
        HTML("Until 2016, deep-water trawl fisheries were conducted in ICES subareas 6 and 7, principally by France,
             with some Spanish, Irish, and Scottish participation. Trawling deeper than 800 m has been banned since December 2016. 
             This mixed deep-water trawl fishery mainly targeted roundnose grenadier, black scabbardfish, and blue ling, 
             with a bycatch mainly of smoothheads and deep-water sharks on the continental slope and offshore banks of subareas 6 and 7.")
        )
    }
    else if (input$FishGear == "Gear Type" & input$GearInt == "Beam-trawl fisheries") {
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
    else if (input$FishGear == "Gear Type" & input$GearInt == "Gillnet fisheries") {
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
    else if (input$FishGear == "Gear Type" & input$GearInt == "Longline and line fisheries") {
      div(
        style = " border-radius: 25px;background-color:#e7e1ef ;font-style:italic;color:#525252;padding: 15px 45px;",
        HTML("Spanish-, French-, and UK-registered longliners target hake along the continental slope with bycatches of ling,
             blue ling, and other deep-water species. An English hand-line fleet operates inshore around the coast of Cornwall
             in divisions 7.e-f targeting mackerel, in an area where other fishing methods for this species are not permitted.")
        )
    }
    else if (input$FishGear == "Gear Type" & input$GearInt == "Pelagic trawls") {
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
    else if (input$FishGear == "Gear Type" & input$GearInt == "Other fisheries") {
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
      if (input$Country == "BEL" & input$name == 1) {
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
      else if (input$name == 2) {
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
    })
  
  f1 <- reactive({
    filter(test, Country == input$Country, Year == input$pieslideryear & Metier == input$pieL)
  })
  f2 <- reactive({
    filter(test, Country == input$Country, Year == input$pieslideryear1 & Species == input$pieL1)
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
      out <- test1()[test1()$Species %in% selected_page21(), ][-c(2, 3, 7, 8)]
      if (nrow(out) < 1) {
        return(NULL)
      }
      row.names(out) <- NULL
      colnames(out) <- c("Country", "Species", "Area", "Landings in tonnes")
      datatable(out,  extensions = 'Buttons'
                , options = list( 
                  paging = T,
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
                  
                  
                ))
    }
    else if (input$Landings1 == "Value in Euros") {
      out <- test1()[test1()$Species %in% selected_page21(), ][-c(2, 3, 6, 8)]
      if (nrow(out) < 1) {
        return(NULL)
      }
      row.names(out) <- NULL
      colnames(out) <- c("Country", "Species", "Area", "Value in Euros")
      datatable(out, extensions = 'Buttons'
                , options = list( 
                  paging = T,
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
                  
                  
                ))
    }
    else if (input$Landings1 == "Price per KG") {
      out <- test1()[test1()$Species %in% selected_page21(), ][-c(2, 3, 6, 7)]
      if (nrow(out) < 1) {
        return(NULL)
      }
      row.names(out) <- NULL
      colnames(out) <- c("Country", "Species", "Area", "Price per KG")
      datatable(out, extensions = 'Buttons'
                , options = list( paging = T,
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
                                  
                                  
                ))
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
      out <- test2()[test2()$Metier %in% selected_pageL22(), ][-c(2, 4, 7, 8)]
      if (nrow(out) < 1) {
        return(NULL)
      }
      row.names(out) <- NULL
      colnames(out) <- c("Country", "Metier", "Area", "Landings in tonnes")
      datatable(out, extensions = 'Buttons'
                , options = list( 
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
                  
                  
                )) # end of option)
    }
    else if (input$Landings2 == "Value in Euros") {
      out <- test2()[test2()$Metier %in% selected_pageL22(), ][-c(2, 4, 6, 8)]
      if (nrow(out) < 1) {
        return(NULL)
      }
      row.names(out) <- NULL
      colnames(out) <- c("Country", "Metier", "Area", "Value in Euros")
      datatable(out, extensions = 'Buttons'
                , options = list( 
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
                  
                  
                )) # end of option)
    }
    else if (input$Landings2 == "Price per KG") {
      out <- test2()[test2()$Metier %in% selected_pageL22(), ][-c(2, 4, 6, 7)]
      if (nrow(out) < 1) {
        return(NULL)
      }
      row.names(out) <- NULL
      colnames(out) <- c("Country", "Metier", "Area", "Price per KG")
      datatable(out, extensions = 'Buttons'
                , options = list( 
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
    L <- CelticEcoSpecies[-c(1,2)]
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
  , options = list( 
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
    
    
  ))) # end of option))
  
  
  
  
  
  
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
      if (input$CountryE == "BEL" & input$nameE == 1) {
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
      else if (input$CountryE == "BEL" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Select Metier", levels(droplevels(filter(testE, Country == "BEL")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "DE" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Select Metier", levels(droplevels(filter(testE, Country == "DE")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "ES" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Select Metier", levels(droplevels(filter(testE, Country == "ES")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "FRA" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Select Metier", levels(droplevels(filter(testE, Country == "FRA")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "GG" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Select Metier", levels(droplevels(filter(testE, Country == "GG")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "IE" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Select Metier", levels(droplevels(filter(testE, Country == "IE")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "IM" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Select Metier", levels(droplevels(filter(testE, Country == "IM")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "JE" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Select Metier", levels(droplevels(filter(testE, Country == "JE")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "NLD" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Select Metier", levels(droplevels(filter(testE, Country == "NLD")$Vessel_length)), selectize = T), class = "btn-link")),
          column(width = 2, div(style = "display: inline-block;vertical-align:top; width: 150px;", sliderInput("pieslideryearE1", "Choose Year:", min = 2009, max = 2017, value = 2017, step = NULL, sep = "", animate = TRUE), class = "btn-link"))
        )
      }
      else if (input$CountryE == "UK" & input$nameE == 2) {
        fluidRow(
          column(width = 2, offset = 2, div(style = "display: inline-block;vertical-align:top; width: 120px;", selectInput("pieE1", label = "Select Metier", levels(droplevels(filter(testE, Country == "UK")$Vessel_length)), selectize = T), class = "btn-link")),
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
      else if (input$nameE == 2) {
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
    })
  
  f3 <- reactive({
    filter(testE2, Country == input$CountryE, Year == input$pieslideryearE & Metier == input$pieE)
  })
  f4 <- reactive({
    filter(testE2, Country == input$CountryE, Year == input$pieslideryearE1 & Vessel_length == input$pieE1)
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
        h4(paste("No data available for", input$pieE, "in", input$pieslideryearE, sep = " "))
      }
      else {
        plotlyOutput("pieE1Plot")
      }
    }
    else if (input$nameE == 2) {
      if (dim(f4())[1] == 0) {
        h4(paste("No data available for", input$pieE1, "in", input$pieslideryearE1, sep = " "))
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
              , options = list( 
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
                
                
              ))
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
              , options = list( 
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
                
                
              ))
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
  , options = list( 
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
    
    
  )))
  
  ###########Existing tools##########################
  ############## 3. Effort app #######################
  partF <- reactive(
    if(input$Area_selector == "North_Sea") {
      readRDS("data/existing_tools/5.partial_F_app/data/North_Sea/partF.rds")
    }else if(input$Area_selector == "Celtic_Sea") {
      readRDS("data/existing_tools/5.partial_F_app/data/Celtic_Sea/CSpartF.rds")
    }
  )
  output$fleet.yearfilter <- renderUI({
    selectInput("year1","Year:",c("All",sort(unique(as.character(partF()$year)),decreasing=T))
    )
  })
  output$fleet.countryfilter <- renderUI({
    selectInput("country","Country",c("All",sort(unique(as.character(partF()$country)))),selected="All")
  })
  
  output$time.countryfilter <- renderUI({
    selectInput("country1","Country:",c("All",sort(unique(as.character(partF()$country)))),selected="All"
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
  })) 
  
  output$plotEffTS <- renderPlot({
    dataplot1 <- partF()
    dataplot1$effmet <- dataplot1$effort*dataplot1$effshare
    if (input$country1 != "All") {
      dataplot1  <- dataplot1[dataplot1$country %in% input$country1,]
    }
    print(ggplot(dataplot1, aes(x = year, y = effmet,group=metier)) +
            geom_point(aes(colour = factor(metier))) + geom_line(aes(group = metier)) +
            facet_wrap(~fleet,scales="free_y") +
            theme_bw())
  })                  
  
  output$plotEff_Fl <- renderPlot({
    # aggregate across stocks (take mean)
    data <- partF()[,c("year","country", "fleet", "metier","effort","effshare")]
    data <- aggregate(list(effort =data$effort, effshare = data$effshare),list(year=data$year,country = data$country, 
                                                                               fleet=data$fleet,metier=data$metier), mean)
    data <- aggregate(list(effort =data$effort),list(year=data$year,country = data$country, 
                                                     fleet=data$fleet), mean)
    if(input$Area_selector == "North_Sea"){ #to avoid distorsion in plot for these two fleets
      data[data$fleet %in% c("EN_Otter24-40","EN_FDF","EN_Otter>=40","EN_Otter<24"),"fleet"] <- "EN_Otter + FDF"
      data[data$fleet %in% c("DK_Seine","DK_Otter<24"),"fleet"] <- "DK_Otter<24 + Seine"
      data <- aggregate(list(effort =data$effort),list(year=data$year,country = data$country, 
                                                       fleet=data$fleet), sum)
    } 
    data_ori <- subset(data,year==min(data$year))
    names(data_ori)[4] <- "effort_ori" 
    data <- merge(data,data_ori[-1],all.x=T) #might need ()
    data$eff_rel <- data$effort/data$effort_ori
    
    print(ggplot(data, aes(x = year, y = eff_rel)) +
            geom_point() + geom_line() +# geom_smooth(method = lm, fullrange = FALSE, aes(colour = factor(metier))) +
            facet_wrap(~fleet) +
            geom_hline(yintercept=1,linetype="dashed") +
            theme_bw()) #+ scale_x_continuous(breaks = seq(2004,2014,by=4)))
  })     
  
  ############ 4. Catchability app ##################
  catchability <- reactive(
    if(input$Area_selector == "North_Sea") {
      readRDS("data/existing_tools/4.catchability_app/data/North_Sea/NScatchability.rds")
    }else if(input$Area_selector == "Celtic_Sea") {
      readRDS("data/existing_tools/4.catchability_app/data/Celtic_Sea/CScatchability.rds")
    }
  )
  output$table.yearfilter <- renderUI({
    selectInput("year","Year:",c("All",sort(unique(as.character(catchability()$year)),decreasing=TRUE))
    )
  })
  output$table.stockfilter <- renderUI({
    selectInput("Stock","Stock", c("All",sort(unique(as.character(catchability()$stock))))
    )
  })
  output$plot.countryfilter <- renderUI({
    selectInput("country","Country:",c("All",sort(unique(as.character(catchability()$country))))
    )
  })
  output$plot.stockfilter <- renderUI({
    selectInput("stock","Stock:",c("All",sort(unique(as.character(catchability()$stock)))),
                multiple=TRUE, selected="All"
    )
  })
  
  output$Catchtable <- DT::renderDataTable(DT::datatable({
    data <- catchability()[,c("year","stock","fleet", "metier","logq","country")]
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    if (input$Stock != "All") {
      data <- data[data$stock == input$Stock,]
    }
    data[,c("logq")] <- round(data[,c("logq")], 2)
    data[,c("year","country","fleet","metier","stock","logq")]
  }))
  
  output$plotCatchability <- renderPlot({
    data <- catchability()
    if (input$country != "All") {
      data <- data[data$country %in% input$country,]
    }
    if (input$stock != "All") {
      data <- data[data$stock %in% input$stock,]
    }
    #remove OTH
    data <- data[data$metier!="OTH",]
    print(ggplot(data, aes(x = year, y = logq,group=stock)) +
            geom_point(aes(colour=stock)) + geom_smooth(method = loess, fullrange = FALSE,aes(colour=stock)) +
            facet_wrap(fleet ~ metier,scales="free_y") +
            theme_bw())# + scale_x_continuous(breaks = seq(2004,2014,by=4)))
  })
  
  ################### 5. Partial F app #########################
  output$PF.year.table <- renderUI({
    selectInput("year3","Year:", c("All",sort(unique(as.character(partF()$year)),decreasing=T)))
  })
  output$PF.stock.table <- renderUI({
    selectInput("Stock3","Stock",c("All",sort(unique(as.character(partF()$stock)))))
  })
  output$PF.country.plot1 <- renderUI({
    selectInput("country","Country:",c("All",sort(unique(as.character(partF()$country)))),selected = 'BE')
  })
  output$PF.stock.plot1 <- renderUI({
    selectInput("stock", "Stock:",c("All",sort(unique(as.character(partF()$stock)))),
                multiple=TRUE,selected = 'All')
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
    if (input$Stock != "All") {
      data <- data[data$stock == input$Stock,]
    }
    data[,c("partF")] <- round(data[,c("partF")],8)
    data[,c("year","country","fleet","metier","stock","partF")]
  }))
  
  output$plotPartialF <- renderPlot({
    data <- partF()
    data <- aggregate(list(partF=data$partF),list(year=data$year,stock=data$stock,
                                                  fleet=data$fleet, metier=data$metier,country=data$country),sum)
    data <- merge(data,aggregate(data$partF,list(year=data$year,stock=data$stock),sum,na.rm=T))
    data$percent <- data$partF/data$x
    if (input$country != "All") {
      data <- data[data$country %in% input$country,]
    }
    
    if (any(length(input$stock)>1 | input$stock != "All")) {
      data <- data[data$stock %in% input$stock,]
    }
    
    data <- subset(data, !is.na(percent))
    
    #remove OTH
    data <- data[data$metier!="OTH",]
    
    print(ggplot(data, aes(x = year, y = percent,group=stock)) +
            geom_point(aes(colour = factor(stock))) + geom_smooth(method = lm, fullrange = FALSE, aes(colour = factor(stock))) +
            facet_wrap(fleet ~ metier,scales="free_y") +
            theme_bw() + scale_x_continuous(breaks = seq(2004,2014,by=4)))
  })
  
  output$Spiderplot <- renderPlot({
    data <- partF()
    
    if (input$year_ != "All") {
      data <- data[data$year %in% input$year_,]
    }
    
    if (input$stocks != "All") {
      data <- data[data$stock %in% input$stocks,]
    }
    #remove OTH
    data <- data[data$metier!="OTH",]
    data <- aggregate(list(partF=data$partF),list(stock=data$stock,fleet=data$fleet),sum)
    data <- merge(data,aggregate(data$partF,list(stock=data$stock),sum))
    data$percent <- data$partF/data$x
    nstock <- length(input$stocks)
    print(ggplot(data,aes(x=stock,y=percent,col=stock)) + 
            facet_wrap(~fleet,ncol=7) + coord_polar() + 
            geom_bar(stat="identity",fill=NA) + theme_bw() + 
            theme(legend.position="none") + 
            theme(strip.text.x = element_text(size = rel(0.8)),
                  axis.text.x = element_text(size = rel(0.5),
                                             angle = 360/(2*pi)*rev( pi/2 + seq( pi/nstock, 2*pi-pi/nstock, len=nstock))
                  ))
    )
  })
  
  ############## Mapping ##########################
  sp <- c("Cod","Haddock","Whiting","Plaice", "Sole", "Hake", "Megrim", "Anglerfish", "Nephrops")
  stocks <- read.csv("www/maps/stocks_SS.csv")
  #stocks_list <- list('cod-7e-k','had-7b-k','hke-nrtn','meg-ivvi','meg-rock','mgw-78','whg-7b-k')
  stocks_list <- levels(stocks$stock)
  #create dropdown to select stock
  updateSelectizeInput(session, 'Stockselector',
                       choices = list(stocks_list),
                       server = TRUE,
                       selected =1)
  
  output$Stockareas <- renderImage({
    filename <- normalizePath(file.path('www/maps/',
                                        paste(input$Species_selector, '_stocks.png', sep='')))
    # Return a list containing the filename
    list(src = filename,
         width = 690,
         height = 545)
  }, deleteFile = FALSE)
  
  output$Stockoverlap <- renderImage({
    filename <- normalizePath(file.path('www/maps/',
                                        paste(input$Stockselector, '.png', sep='') 
    ))
    # Return a list containing the filename
    list(src = filename,
         width = 470,
         height = 350)
  }, deleteFile = FALSE)
  
  ###########Scenarios##########################
  
  #####Advice 2019###
  output$table1 <- function() {
    text_tbl <- data.frame(
      Tiers = c("Tier1", "Tier2", "Tier3"),
      Stock = c("COD-CS, HAD-CS, WHG-CS ", "N-HEK, N-MEG, MON-CS", "SOL-7E, SOL-7FG"),
      Details = c(
        "These are the stocks in the original analysis, all are category 1 assessments with deterministic short term forecasts which can be performed accurately in FLR. ",
        "These were identified as the first priority demersal stocks to include, but were also the most challenging due to the range of assessment and forecasting methods. The following summarises the issues encountered:
        Northern hake:  The single stock assessment is a length-based SS3 model, where the output from the assessment is converted to an age-based approximation to allow a forecast in FLR. Similarly, to the forecasts performed for the Bay of Biscay model, we were able to forecast catches close to the single stock advice (< 2 % difference in 2018, ~ 5 % difference in 2019) but difference in SSB were very difference (~ 33 % higher in 2020).
        Megrim: The single stock assessment is an age-based Bayesian model where the median output from the assessment was used as input to deterministic forecasts in FLR. There was some difficulty reproducing close to the advice (a catch difference of 16 % in 2018) which we could not explain. This is being further investigated with the stock coordinator as there is no clear reason why a large difference should be found (a small difference from a deterministic forecast of the median assessment outputs might be expected from the median of a stochastic forecast). Also, we are required to make an assumption concerning the split of the TAC that’s belongs to each species based on the landings split, which is uncertain/unclear.
        Monkfish: The single stock assessment is a statistical catch-at-age model with forecasts undertaken in the FLR framework.  There is no problem in recreating the forecasts.
        ",
        "While not considered immediate priority stocks for inclusion they are category 1 stocks with full analytical assessments and forecasts. As the assessments are XSA with deterministic short term forecasts we could replicate them perfectly with FLR. "
      )
    )
    
    text_tbl %>%
      knitr::kable("html") %>%
      kable_styling(full_width = F) %>%
      column_spec(c(1:3), background = "black", include_thead = TRUE, border_right = T) %>%
      column_spec(1, bold = T, border_right = T, underline = T) %>%
      column_spec(2, width = "10em", bold = T, border_right = T, underline = T) %>%
      column_spec(3, width = "30em", bold = T) %>%
      row_spec(c(1, 3), background = "lightgrey") %>%
      row_spec(2, background = "lightyellow", color = "#525252")
  }
  
  dataForstock1 <- dataForstock %>% filter(type == "fbar")
  stock_year <- aggregate(data ~ year + Stock, data = dataForstock1, FUN = sum)
  stock_year_wide <- cast(stock_year, year ~ Stock, sum)
  stock_year_wide$Geometric_Mean<-rep(NA,dim(stock_year_wide)[1])
  stock_year_wide$Mean<-rep(NA,dim(stock_year_wide)[1])
  for(i in 1:dim(stock_year_wide)[1]){stock_year_wide$Geometric_Mean[i]<-geometric.mean(as.numeric(stock_year_wide[-1][i,]))
  stock_year_wide$Mean[i]<-mean(as.numeric(stock_year_wide[-1][i,]),na.rm = TRUE)}
  output$StockStatus <- renderPlotly({
    p <- plot_ly(stock_year_wide,
                 x = ~year, y = ~COD_CS,type = "scatter",mode = "lines" , name = "COD-CS",
                 text = ~ paste(year,":Year","<br> ",COD_CS, ":COD-CS"), hoverinfo = "text"
    ) %>%
      add_trace(y = ~HAD_CS, name = "HAD-CS", text = ~ paste(year,":Year","<br> ",HAD_CS, ":HAD-CS")) %>%
      add_trace(y = ~MON_CS, name = "MON-CS", text = ~ paste(year,":Year","<br> ",MON_CS, ":MON-CS")) %>%
      add_trace(y = ~N_HKE, name = "N-HKE", text = ~ paste(year,":Year","<br> ",N_HKE, ":N-HKE")) %>%
      add_trace(y = ~N_MEG, name = "N-MEG", text = ~ paste(year,":Year","<br> ",N_MEG, ":N-MEG")) %>%
      add_trace(y = ~SOL_7E, name = "SOL-7E", text = ~ paste(year,":Year","<br> ",SOL_7E, ":SOL-7E")) %>%
      add_trace(y = ~SOL_7FG, name = "SOL-7FG", text = ~ paste(year,":Year","<br> ",SOL_7FG, ":SOL-7FG")) %>%
      add_trace(y = ~WHG_CS, name = "WHG-CS", text = ~ paste(year,":Year","<br> ",WHG_CS, ":WHG-CS")) %>%
      add_trace(y = ~Geometric_Mean, name = "Geom_Mean", text = ~ paste(year,":Year","<br> ",Geometric_Mean, ":Geom_Mean"),line=list(width = 8, dash = 'dot',color="black")) %>%
      add_trace(y = ~Mean, name = "Mean", text = ~ paste(year,":Year","<br> ",Mean, ":Mean"),line=list(width = 8, dash = 'dash',color="gray")) %>%
      layout(yaxis = list(title = " Fbar"), xaxis = list(title = ""), legend = list(x = 0, y = 1.2)) %>%
      config(displayModeBar = F) %>%
      layout(height = 350)
    p
  })
  output$Advice <- renderPlotly({
    p <- plot_ly(BRPs) %>%
      add_markers(
        x = ~Stock, y = ~Flim, name = "Flim",
        text = ~ paste(Flim, ":Flim"), hoverinfo = "text", marker = list(symbol = 1, size = 12)
      ) %>%
      add_markers(x = ~Stock, y = ~Fpa, name = "Fpa", text = ~ paste(Fpa, ":Fpa"), hoverinfo = "text", marker = list(symbol = 2, size = 12)) %>%
      add_markers(x = ~Stock, y = ~Fmsy, name = "Fmsy", text = ~ paste(Fmsy, ":Fmsy"), hoverinfo = "text", marker = list(symbol = 3, size = 12)) %>%
      add_markers(x = ~Stock, y = ~Fmsy_lower, name = "Fmsy min", text = ~ paste(Fmsy_lower, ":Fmsy min"), hoverinfo = "text", marker = list(symbol = 4, size = 12)) %>%
      add_markers(x = ~Stock, y = ~Fmsy_upper, name = "Fmsy max", text = ~ paste(Fmsy_upper, ":Fmsy max"), hoverinfo = "text", marker = list(symbol = 5, size = 12)) %>%
      add_markers(x = ~Stock, y = ~Current_F, name = "Current F", text = ~ paste(Current_F, ":Current F"), hoverinfo = "text", marker = list(symbol = 17, size = 12)) %>%
      add_markers(x = ~Stock, y = ~Fadvice2019, name = "Advice 2019", text = ~ paste(Fadvice2019, ":Advice 2019"), hoverinfo = "text", marker = list(symbol = 27, size = 12)) %>%
      add_markers(x = ~Stock, y = ~Fbar_Fcube_2018, name = "FCube 2018", text = ~ paste(Fbar_Fcube_2018, ":Fcube_2018"), hoverinfo = "text", marker = list(symbol = 25, size = 12)) %>%
      add_markers(x = ~Stock, y = ~Fbar_Fcube_2019, name = "FCube 2019", text = ~ paste(Fbar_Fcube_2019, ":FCube 2019"), hoverinfo = "text", marker = list(symbol = 26, size = 12)) %>%
      layout(yaxis = list(title = "F Applied in Advice Year"), xaxis = list(title = ""), legend = list(x = 0, y = 1.10)) %>%
      config(displayModeBar = F) %>%
      layout(hovermode = "compare", height = 350)
    p
  })
  
  observeEvent(input$A2, {
    dataForstock1 <- dataForstock %>% filter(type == "ssb")
    stock_year <- aggregate(data ~ year + Stock, data = dataForstock1, FUN = sum)
    stock_year_wide <- cast(stock_year, year ~ Stock, sum)
    stock_year_wide$Geometric_Mean<-rep(NA,dim(stock_year_wide)[1])
    stock_year_wide$Mean<-rep(NA,dim(stock_year_wide)[1])
    for(i in 1:dim(stock_year_wide)[1]){stock_year_wide$Geometric_Mean[i]<-geometric.mean(as.numeric(stock_year_wide[-1][i,]))
    stock_year_wide$Mean[i]<-mean(as.numeric(stock_year_wide[-1][i,]),na.rm = TRUE)}
    output$StockStatus <- renderPlotly({
      p <- plot_ly(stock_year_wide,
                   x = ~year, y = ~COD_CS, type = "scatter",mode = "lines", name = "COD-CS",
                   text = ~ paste(year,"Year","<br> ",COD_CS, ":COD-CS"), hoverinfo = "text"
      ) %>%
        add_trace(y = ~HAD_CS, name = "HAD-CS", text = ~ paste(year,":Year","<br> ",HAD_CS, ":HAD-CS")) %>%
        add_trace(y = ~MON_CS, name = "MON-CS", text = ~ paste(year,":Year","<br> ",MON_CS, ":MON-CS")) %>%
        add_trace(y = ~N_HKE, name = "N-HKE", text = ~ paste(year,":Year","<br> ",N_HKE, ":N-HKE")) %>%
        add_trace(y = ~N_MEG, name = "N-MEG", text = ~ paste(year,":Year","<br> ",N_MEG, ":N-MEG")) %>%
        add_trace(y = ~SOL_7E, name = "SOL-7E", text = ~ paste(year,":Year","<br> ",SOL_7E, ":SOL-7E")) %>%
        add_trace(y = ~SOL_7FG, name = "SOL-7FG", text = ~ paste(year,":Year","<br> ",SOL_7FG, ":SOL-7FG")) %>%
        add_trace(y = ~WHG_CS, name = "WHG-CS", text = ~ paste(year,":Year","<br> ",WHG_CS, ":WHG-CS")) %>%
        add_trace(y = ~Geometric_Mean, name = "Geom_Mean", text = ~ paste(year,":Year","<br> ",Geometric_Mean, ":Geom_Mean"),line=list(width = 8, dash = 'dot',color="black")) %>%
        add_trace(y = ~Mean, name = "Mean", text = ~ paste(year,":Year","<br> ",Mean, ":Mean"),line=list(width = 8, dash = 'dash',color="gray")) %>%
        layout(yaxis = list(title = " SSB"), xaxis = list(title = ""), legend = list(x = 0.06, y = 0.98)) %>%
        config(displayModeBar = F) %>%
        layout(height = 350)
      p
    })
    output$Advice <- renderPlotly({
      p <- plot_ly(BRPs) %>%
        add_markers(
          x = ~Stock, y = ~Blim, name = "Blim",
          text = ~ paste(Blim, ":Blim"), hoverinfo = "text", marker = list(symbol = 1, size = 12)
        ) %>%
        add_markers(x = ~Stock, y = ~Bpa, name = "Bpa", text = ~ paste(Bpa, ":Bpa"), hoverinfo = "text", marker = list(symbol = 2, size = 12)) %>%
        add_markers(x = ~Stock, y = ~Bmsytrigger, name = "Bmsytrigger", text = ~ paste(Bmsytrigger, ":Bmsytrigger"), hoverinfo = "text", marker = list(symbol = 3, size = 12)) %>%
        add_markers(x = ~Stock, y = ~Current_SSB, name = "Current SSB", text = ~ paste(Current_SSB, ":Current SSB"), hoverinfo = "text", marker = list(symbol = 4, size = 12)) %>%
        add_markers(x = ~Stock, y = ~SSB_Advice_2019, name = "Advice 2019", text = ~ paste(SSB_Advice_2019, ":Advice 2019"), hoverinfo = "text", marker = list(symbol = 5, size = 12)) %>%
        add_markers(x = ~Stock, y = ~SSB_Advice_2020, name = "Advice 2020", text = ~ paste(SSB_Advice_2020, ":Advice 2020"), hoverinfo = "text", marker = list(symbol = 17, size = 12)) %>%
        add_markers(x = ~Stock, y = ~SSB_Fcube_2018, name = "Fcube 2018", text = ~ paste(SSB_Fcube_2018, ":Fcube 2018"), hoverinfo = "text", marker = list(symbol = 27, size = 12)) %>%
        add_markers(x = ~Stock, y = ~SSB_Fcube_2019, name = "Fcube 2019", text = ~ paste(SSB_Fcube_2019, ":Fcube 2019"), hoverinfo = "text", marker = list(symbol = 25, size = 12)) %>%
        add_markers(x = ~Stock, y = ~SSB_Fcube_2020, name = "FCube 2020", text = ~ paste(SSB_Fcube_2020, ":FCube 2020"), hoverinfo = "text", marker = list(symbol = 26, size = 12)) %>%
        layout(yaxis = list(title = "SSB in Advice Year"), xaxis = list(title = ""), legend = list(x = 0, y = 1.10)) %>%
        config(displayModeBar = F) %>%
        layout(hovermode = "compare")
      p
    })
  })
  
  
  observeEvent(input$A1, {
    dataForstock1 <- dataForstock %>% filter(type == "fbar")
    stock_year <- aggregate(data ~ year + Stock, data = dataForstock1, FUN = sum)
    stock_year_wide <- cast(stock_year, year ~ Stock, sum)
    stock_year_wide$Geometric_Mean<-rep(NA,dim(stock_year_wide)[1])
    stock_year_wide$Mean<-rep(NA,dim(stock_year_wide)[1])
    for(i in 1:dim(stock_year_wide)[1]){stock_year_wide$Geometric_Mean[i]<-geometric.mean(as.numeric(stock_year_wide[-1][i,]))
    stock_year_wide$Mean[i]<-mean(as.numeric(stock_year_wide[-1][i,]),na.rm = TRUE)}
    output$StockStatus <- renderPlotly({
      p <- plot_ly(stock_year_wide,
                   x = ~year, y = ~COD_CS,type = "scatter",mode = "lines" , name = "COD-CS",
                   text = ~ paste(year,":Year","<br> ",COD_CS, ":COD-CS"), hoverinfo = "text"
      ) %>%
        add_trace(y = ~HAD_CS, name = "HAD-CS", text = ~ paste(year,":Year","<br> ",HAD_CS, ":HAD-CS")) %>%
        add_trace(y = ~MON_CS, name = "MON-CS", text = ~ paste(year,":Year","<br> ",MON_CS, ":MON-CS")) %>%
        add_trace(y = ~N_HKE, name = "N-HKE", text = ~ paste(year,":Year","<br> ",N_HKE, ":N-HKE")) %>%
        add_trace(y = ~N_MEG, name = "N-MEG", text = ~ paste(year,":Year","<br> ",N_MEG, ":N-MEG")) %>%
        add_trace(y = ~SOL_7E, name = "SOL-7E", text = ~ paste(year,":Year","<br> ",SOL_7E, ":SOL-7E")) %>%
        add_trace(y = ~SOL_7FG, name = "SOL-7FG", text = ~ paste(year,":Year","<br> ",SOL_7FG, ":SOL-7FG")) %>%
        add_trace(y = ~WHG_CS, name = "WHG-CS", text = ~ paste(year,":Year","<br> ",WHG_CS, ":WHG-CS")) %>%
        add_trace(y = ~Geometric_Mean, name = "Geom_Mean", text = ~ paste(year,":Year","<br> ",Geometric_Mean, ":Geom_Mean"),line=list(width = 8, dash = 'dot',color="black")) %>%
        add_trace(y = ~Mean, name = "Mean", text = ~ paste(year,":Year","<br> ",Mean, ":Mean"),line=list(width = 8, dash = 'dash',color="gray")) %>%
        layout(yaxis = list(title = " Fbar"), xaxis = list(title = ""), legend = list(x = 0, y = 1.20)) %>%
        config(displayModeBar = F)
      p
    })
    output$Advice <- renderPlotly({
      p <- plot_ly(BRPs) %>%
        add_markers(
          x = ~Stock, y = ~Flim, name = "Flim",
          text = ~ paste(Flim, ":Flim"), hoverinfo = "text", marker = list(symbol = 1, size = 12)
        ) %>%
        add_markers(x = ~Stock, y = ~Fpa, name = "Fpa", text = ~ paste(Fpa, ":Fpa"), hoverinfo = "text", marker = list(symbol = 2, size = 12)) %>%
        add_markers(x = ~Stock, y = ~Fmsy, name = "Fmsy", text = ~ paste(Fmsy, ":Fmsy"), hoverinfo = "text", marker = list(symbol = 3, size = 12)) %>%
        add_markers(x = ~Stock, y = ~Fmsy_lower, name = "Fmsy min", text = ~ paste(Fmsy_lower, ":Fmsy min"), hoverinfo = "text", marker = list(symbol = 4, size = 12)) %>%
        add_markers(x = ~Stock, y = ~Fmsy_upper, name = "Fmsy max", text = ~ paste(Fmsy_upper, ":Fmsy max"), hoverinfo = "text", marker = list(symbol = 5, size = 12)) %>%
        add_markers(x = ~Stock, y = ~Current_F, name = "Current F", text = ~ paste(Current_F, ":Current F"), hoverinfo = "text", marker = list(symbol = 17, size = 12)) %>%
        add_markers(x = ~Stock, y = ~Fadvice2019, name = "Advice 2019", text = ~ paste(Fadvice2019, ":Advice 2019"), hoverinfo = "text", marker = list(symbol = 27, size = 12)) %>%
        add_markers(x = ~Stock, y = ~Fbar_Fcube_2018, name = "FCube 2018", text = ~ paste(Fbar_Fcube_2018, ":Fcube_2018"), hoverinfo = "text", marker = list(symbol = 25, size = 12)) %>%
        add_markers(x = ~Stock, y = ~Fbar_Fcube_2019, name = "FCube 2019", text = ~ paste(Fbar_Fcube_2019, ":FCube 2019"), hoverinfo = "text", marker = list(symbol = 26, size = 12)) %>%
        layout(yaxis = list(title = "F Applied in Advice Year"), xaxis = list(title = ""), legend = list(x = 0, y = 1.10)) %>%
        config(displayModeBar = F) %>%
        layout(hovermode = "compare")
      p
    })
  })
  observeEvent(input$A3, {
    dataForstock1 <- dataForstock %>% filter(type == "catch")
    stock_year <- aggregate(data ~ year + Stock, data = dataForstock1, FUN = sum)
    stock_year_wide <- cast(stock_year, year ~ Stock, sum)
    stock_year_wide$Geometric_Mean<-rep(NA,dim(stock_year_wide)[1])
    stock_year_wide$Mean<-rep(NA,dim(stock_year_wide)[1])
    for(i in 1:dim(stock_year_wide)[1]){stock_year_wide$Geometric_Mean[i]<-geometric.mean(as.numeric(stock_year_wide[-1][i,]))
    stock_year_wide$Mean[i]<-mean(as.numeric(stock_year_wide[-1][i,]),na.rm = TRUE)}
    output$StockStatus <- renderPlotly({
      p <- plot_ly(stock_year_wide,
                   x = ~year, y = ~COD_CS,type = "scatter",mode = "lines" , name = "COD-CS",
                   text = ~ paste(year,":Year","<br> ",COD_CS, ":COD-CS"), hoverinfo = "text"
      ) %>%
        add_trace(y = ~HAD_CS, name = "HAD-CS", text = ~ paste(year,":Year","<br> ",HAD_CS, ":HAD-CS")) %>%
        add_trace(y = ~MON_CS, name = "MON-CS", text = ~ paste(year,":Year","<br> ",MON_CS, ":MON-CS")) %>%
        add_trace(y = ~N_HKE, name = "N-HKE", text = ~ paste(year,":Year","<br> ",N_HKE, ":N-HKE")) %>%
        add_trace(y = ~N_MEG, name = "N-MEG", text = ~ paste(year,":Year","<br> ",N_MEG, ":N-MEG")) %>%
        add_trace(y = ~SOL_7E, name = "SOL-7E", text = ~ paste(year,":Year","<br> ",SOL_7E, ":SOL-7E")) %>%
        add_trace(y = ~SOL_7FG, name = "SOL-7FG", text = ~ paste(year,":Year","<br> ",SOL_7FG, ":SOL-7FG")) %>%
        add_trace(y = ~WHG_CS, name = "WHG-CS", text = ~ paste(year,":Year","<br> ",WHG_CS, ":WHG-CS")) %>%
        add_trace(y = ~Geometric_Mean, name = "Geom_Mean", text = ~ paste(year,":Year","<br> ",Geometric_Mean, ":Geom_Mean"),line=list(width = 8, dash = 'dot',color="black")) %>%
        add_trace(y = ~Mean, name = "Mean", text = ~ paste(year,":Year","<br> ",Mean, ":Mean"),line=list(width = 8, dash = 'dash',color="gray")) %>%
        layout(yaxis = list(title = " Catch"), xaxis = list(title = ""), legend = list(x = 0.06, y = 0.98)) %>%
        config(displayModeBar = F)
      p
    })
    
    output$Advice <- renderPlotly({
      p <- plot_ly(BRPs) %>%
        add_markers(
          x = ~Stock, y = ~Current_Catch, name = "Current Catch",
          text = ~ paste(Current_Catch, ":Current Catch"), hoverinfo = "text", marker = list(symbol = 1, size = 12)
        ) %>%
        add_markers(x = ~Stock, y = ~Catch_Advice_2019, name = "Advice 2019", text = ~ paste(Catch_Advice_2019, ":Advice 2019"), hoverinfo = "text", marker = list(symbol = 2, size = 12)) %>%
        add_markers(x = ~Stock, y = ~Catch_Fcube_2018, name = "FCube 2018", text = ~ paste(Catch_Fcube_2018, ":FCube 2018"), hoverinfo = "text", marker = list(symbol = 3, size = 12)) %>%
        add_markers(x = ~Stock, y = ~Catch_Fcube_2019, name = "FCube 2019", text = ~ paste(Catch_Fcube_2019, ":FCube 2019"), hoverinfo = "text", marker = list(symbol = 4, size = 12)) %>%
        layout(yaxis = list(title = "Catch in Advice Year"), xaxis = list(title = ""), legend = list(x = 0, y = 1.10)) %>%
        config(displayModeBar = F) %>%
        layout(hovermode = "compare")
      p
    })
  })
  
  observeEvent(input$A4, {
    dataForstock1 <- dataForstock %>% filter(type == "discards")
    stock_year <- aggregate(data ~ year + Stock, data = dataForstock1, FUN = sum)
    stock_year_wide <- cast(stock_year, year ~ Stock, sum)
    output$StockStatus <- renderPlotly({
      p <- plot_ly(stock_year_wide,
                   x = ~year, y = ~HAD_CS, type = "scatter",mode = "lines+markers", name = "HAD-CS",
                   text = ~ paste(HAD_CS, ":HAD-CS"), hoverinfo = "text"
      ) %>%
        add_trace(y = ~MON_CS, name = "MON-CS", text = ~ paste(MON_CS, ":MON-CS")) %>%
        add_trace(y = ~N_HKE, name = "N-HKE", text = ~ paste(N_HKE, ":N-HKE")) %>%
        add_trace(y = ~N_MEG, name = "N-MEG", text = ~ paste(N_MEG, ":N-MEG")) %>%
        add_trace(y = ~SOL_7E, name = "SOL-7E", text = ~ paste(SOL_7E, ":SOL-7E")) %>%
        add_trace(y = ~SOL_7FG, name = "SOL-7FG", text = ~ paste(SOL_7FG, ":SOL-7FG")) %>%
        add_trace(y = ~WHG_CS, name = "WHG-CS", text = ~ paste(WHG_CS, ":WHG-CS")) %>%
        layout(yaxis = list(title = "Discards"), xaxis = list(title = ""), legend = list(x = 0.06, y = 0.98)) %>%
        config(displayModeBar = F) 
    })
    
    output$Advice <- renderPlotly({
      p <- plot_ly(BRPs) %>%
        add_markers(
          x = ~Stock, y = ~Current_Discards, name = "Current Discards",
          text = ~ paste(Current_Discards, ":Current Discards"), hoverinfo = "text", marker = list(symbol = 1, size = 12)
        ) %>%
        add_markers(x = ~Stock, y = ~Discards_Advice_2019, name = "Advice 2019", text = ~ paste(Discards_Advice_2019, ":Advice 2019"), hoverinfo = "text", marker = list(symbol = 2, size = 12)) %>%
        layout(yaxis = list(title = "Discards in Advice Year"), xaxis = list(title = ""), legend = list(x = 0, y = 1.10)) %>%
        config(displayModeBar = F) %>%
        layout(hovermode = "compare")
      p
    })
  })
  observeEvent(input$A5, {
    dataForstock1 <- dataForstock %>% filter(type == "landings")
    stock_year <- aggregate(data ~ year + Stock, data = dataForstock1, FUN = sum)
    stock_year_wide <- cast(stock_year, year ~ Stock, sum)
    stock_year_wide$Geometric_Mean<-rep(NA,dim(stock_year_wide)[1])
    stock_year_wide$Mean<-rep(NA,dim(stock_year_wide)[1])
    for(i in 1:dim(stock_year_wide)[1]){stock_year_wide$Geometric_Mean[i]<-geometric.mean(as.numeric(stock_year_wide[-1][i,]))
    stock_year_wide$Mean[i]<-mean(as.numeric(stock_year_wide[-1][i,]),na.rm = TRUE)}
    output$StockStatus <- renderPlotly({
      p <- plot_ly(stock_year_wide,
                   x = ~year, y = ~COD_CS,type = "scatter",mode = "lines" , name = "COD-CS",
                   text = ~ paste(year,":Year","<br> ",COD_CS, ":COD-CS"), hoverinfo = "text"
      ) %>%
        add_trace(y = ~HAD_CS, name = "HAD-CS", text = ~ paste(year,":Year","<br> ",HAD_CS, ":HAD-CS")) %>%
        add_trace(y = ~MON_CS, name = "MON-CS", text = ~ paste(year,":Year","<br> ",MON_CS, ":MON-CS")) %>%
        add_trace(y = ~N_HKE, name = "N-HKE", text = ~ paste(year,":Year","<br> ",N_HKE, ":N-HKE")) %>%
        add_trace(y = ~N_MEG, name = "N-MEG", text = ~ paste(year,":Year","<br> ",N_MEG, ":N-MEG")) %>%
        add_trace(y = ~SOL_7E, name = "SOL-7E", text = ~ paste(year,":Year","<br> ",SOL_7E, ":SOL-7E")) %>%
        add_trace(y = ~SOL_7FG, name = "SOL-7FG", text = ~ paste(year,":Year","<br> ",SOL_7FG, ":SOL-7FG")) %>%
        add_trace(y = ~WHG_CS, name = "WHG-CS", text = ~ paste(year,":Year","<br> ",WHG_CS, ":WHG-CS")) %>%
        add_trace(y = ~Geometric_Mean, name = "Geom_Mean", text = ~ paste(year,":Year","<br> ",Geometric_Mean, ":Geom_Mean"),line=list(width = 8, dash = 'dot',color="black")) %>%
        add_trace(y = ~Mean, name = "Mean", text = ~ paste(year,":Year","<br> ",Mean, ":Mean"),line=list(width = 8, dash = 'dash',color="gray")) %>%
        layout(yaxis = list(title = " Landings"), xaxis = list(title = ""), legend = list(x = 0.06, y = 0.98)) %>%
        config(displayModeBar = F) 
      p
    })
    output$Advice <- renderPlotly({
      p <- plot_ly(BRPs) %>%
        add_markers(
          x = ~Stock, y = ~Current_Landings, name = "Current Landings",
          text = ~ paste(Current_Landings, ":Current Landings"), hoverinfo = "text", marker = list(symbol = 1, size = 12)
        ) %>%
        add_markers(x = ~Stock, y = ~Landings_Advice_2019, name = "Advice 2019", text = ~ paste(Landings_Advice_2019, ":Advice 2019"), hoverinfo = "text", marker = list(symbol = 2, size = 12)) %>%
        add_markers(x = ~Stock, y = ~Landings_Fcube_2018, name = "FCube 2018", text = ~ paste(Landings_Fcube_2018, ":FCube 2018"), hoverinfo = "text", marker = list(symbol = 3, size = 12)) %>%
        add_markers(x = ~Stock, y = ~Landings_Fcube_2019, name = "FCube 2019", text = ~ paste(Landings_Fcube_2019, ":FCube 2019"), hoverinfo = "text", marker = list(symbol = 4, size = 12)) %>%
        layout(yaxis = list(title = "Landings in Advice Year"), xaxis = list(title = ""), legend = list(x = 0, y = 1.10)) %>%
        config(displayModeBar = F) %>%
        layout(hovermode = "compare")
      p
    })
  })
  
  
  
  ######FCube######
  ##page1##
  output$table2 <- function() {
    text_tbl <- data.frame(
      Scenarios = c(
        "Maximum", "Minimum", "Haddock MSY approach", "Whiting MSY approach", "Status quo effort",
        "Value", "Cod Fmsy"
      ),
      Abbreviation = c("max", "min", "had-CS ", "wht-cs", "sq_E", "val", "cod-cs"),
      Explanation = c(
        " For each fleet, fishing stops when all stocks have been caught up to the fleet’s stock shares.
        This option causes overfishing of the single-stock advice possibilities of most stocks.",
        "For each fleet, fishing stops when the catch for any one of the stocks meets the fleet’s stock share. This option is the most precautionary option, 
        causing underutilization of the single-stock advice possibilities of other stocks.",
        "All fleets set their effort corresponding to that required to catch their haddock stock share, regardless of other catches. ",
        "All fleets set their effort corresponding to that required to catch their whiting stock share, regardless of other catches.",
        "The effort of each fleet is set equal to the effort in the most recently recorded year (2017) for which catch and effort data are available.",
        "A simple scenario accounting for the economic importance of each stock for each fleet. 
        The effort by fleet is equal to the average of the efforts required to catch the fleet’s stock shares of each of the stocks, weighted by the historical catch value of that stock (see example below). This option causes overfishing of some stocks and underutilization of others.",
        "All fleets set their effort corresponding to that required to catch their cod stock share, where the cod TAC is set according to reduced FMSY (F = 0.12, FMSY × (SSB(2019) / MSY Btrigger)), regardless of other catches."
      )
    )
    
    text_tbl %>%
      knitr::kable("html") %>%
      kable_styling(full_width = F, font_size = 13) %>%
      column_spec(c(1:3), background = "black", include_thead = TRUE, border_right = T) %>%
      column_spec(1, bold = T, border_right = T, underline = T) %>%
      column_spec(2, width = "3em", bold = T, border_right = T, underline = T) %>%
      column_spec(3, width = "30em", bold = T) %>%
      row_spec(c(1, 3, 5, 7), background = "lightgrey") %>%
      row_spec(c(2, 4, 6), background = "lightyellow", color = "#525252")
  }
  
  
  output$table3 <- function() {
    text_tbl <- data.frame(
      Mixed_Fisheries_Metier = c("OTB_DEF", "OTT_DEF", "OTB_CRU", "OTT_CRU", "OTM_DEF", "GNS_DEF", "GTR_DEF", "LSS_FIF", "SSC_DEF", "TBB_DEF", "MIS_MIS / OTH"),
      Gear = c("Otter trawls", "Twin otter trawls", "Otter trawls", "Twin otter trawls", "Midwater trawls", "Gillnets", "Trammelnets", "Longlines", "Scottish seines", "Beam trawls", "Other gears"),
      Target_Species = c("Demersal fish", "Demersal fish", "Crustaceans", "Crustaceans", "Demersal fish", "Demersal fish", "Demersal fish", "Finfish", "Demersal fish", "Demersal fish", "Any")
    )
    text_tbl %>%
      knitr::kable("html") %>%
      kable_styling(full_width = F, font_size = 14) %>%
      column_spec(c(1:3), background = "black", include_thead = TRUE, border_right = T) %>%
      column_spec(1, bold = T, border_right = T, underline = T) %>%
      column_spec(2, width = "15em", bold = T, border_right = T, underline = T) %>%
      column_spec(3, width = "15em", bold = T) %>%
      row_spec(c(1, 3, 5, 7, 9, 11), background = "lightyellow", color = "#525252") %>%
      row_spec(c(2, 4, 6, 8, 10), background = "lightgrey")
  }
  
  ##page2##
  radarP <- reactive({
    filter(shareW,country==input$filltype, stock == input$FCShare)
  })
  
  output$radrarS <-
    renderPlotly({
      plot_ly(
        type = "scatterpolar",
        # mode = "lines+markers",
        r= filter(radarP(),scenario=="baseline")$share,
        theta = levels(shareW$scenario)[-1],
        # fill="tozeroy",
        fill="toself",
        name = paste(input$filltype,":2017"),
        marker = list(size = 12)) %>%
        add_trace(r= filter(radarP(),scenario!="baseline")$share,
                  theta = levels(shareW$scenario)[-1],
                  fill = "toself",
                  name=paste(input$filltype,":2019"),marker = list(size = 11))%>%layout(showlegend=T) 
      
    })
  
  output$shareWtable <- function() {
    text_tbl <- radarP()[-c(1, 2)]
    names(text_tbl) <- c("Stock", "Country", "Scenario", "Share")
    text_tbl %>%
      knitr::kable("html") %>%
      kable_styling(full_width = F, font_size = 16) %>%
      #column_spec(c(1:7), bold = T, border_right = T, underline = T) %>%
      row_spec(c(1, 3, 5, 7), background = "lightyellow", color = "#525252") %>%
      row_spec(c(2, 4, 6,8), background = "lightgrey")
  }
  
  ######### Plot effort fleet by scenario######
  
  #####Circular barplot######
  
  
  output$FCEfSbarplot<-renderPlot({
    if(input$FCEfSbarPlot==1){
      t1 <- filter(effbymet,scenario == input$FCEfSbar)
      t1<-t1[c(10,11,5,8)]
      t1$efmet <-log(t1$efmet)
      wide<-spread(t1, metier, efmet)
      widet1 <- gather(wide,key = "metier", value="Effort", -c(1,2))
      data<-widet1 
      empty_bar=3
      nObsType=nlevels(as.factor(data$metier))
      to_add = data.frame( matrix(NA, empty_bar*nlevels(data$Country)*nObsType, ncol(data)) )
      colnames(to_add) = colnames(data)
      to_add$Country=rep(levels(data$Country), each=empty_bar*nObsType )
      data=rbind(data, to_add)
      data=data %>% arrange(Country, Fleet)
      data$id=rep( seq(1, nrow(data)/nObsType) , each=nObsType)
      
      label_data= data %>% group_by(id, Fleet) %>% summarize(tot=sum(Effort,na.rm=TRUE))
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
        geom_bar(aes(x=as.factor(id), y=Effort, fill=metier), stat="identity", alpha=0.5) +
        scale_fill_manual(values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99"))+
        #scale_fill_viridis(discrete=TRUE) +
        
        # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
        geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
        geom_segment(data=grid_data, aes(x = end, y = 5, xend = start, yend = 5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
        geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
        
        
        # Add text showing the value of each 100/75/50/25 lines
        annotate("text", x = rep(max(data$id),3), y = c(0,5, 10), label = c("0", "5", "10") , color="grey", size=5 , angle=0, fontface="bold", hjust=1) +
        ylim(-15,max(label_data$tot, na.rm=T)+20) +
        #theme_minimal() +
        theme(
          legend.position = "left",
          legend.text = element_text(size=15),
          #legend.margin=margin(0,-20,62,0),
          legend.box.margin = margin(10,10,10,10),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          # plot.margin = unit(rep(-7,4), "cm") 
          plot.margin = unit(c(-4,-4,-4,-4) ,"cm") 
        ) +
        coord_polar() +
        # Add labels on top of each bar
        geom_text(data=label_data, aes(x=id, y=tot+1, label=Fleet, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
        
        # Add base line information
        geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )   +
        geom_text(data=base_data, aes(x = title, y = -1, label=Country), hjust=c(0.5,1,0.5,0,0.5), 
                  vjust=c(0.5,0.5,0,0.5,1.5), colour = "black", alpha=0.8, size=4.5, fontface="bold", inherit.aes = FALSE)}
    else{
      t1 <- filter(effbymet,scenario == input$FCEfSbar)
      t1<-t1[c(10,11,5,8)]
      #t1$efmet <-log(t1$efmet)
      wide<-spread(t1, metier, efmet)
      
      
      wideP<-matrix(NA,nrow = 20,ncol=11)
      for(i in 1:20){
        wideP[i,]<-as.numeric(wide[-c(1,2)][i,]/sum(wide[-c(1,2)][i,],na.rm=T))}
      t<-cbind(wide[c(1,2)],as.data.frame(wideP))
      names(t)<-names(wide)
      
      #widet1 <- gather(wide,key = "metier", value="Effort", -c(1,2))
      widet <- gather(t,key = "metier", value="Effort", -c(1,2))
      data<-widet
      empty_bar=3
      nObsType=nlevels(as.factor(data$metier))
      to_add = data.frame( matrix(NA, empty_bar*nlevels(data$Country)*nObsType, ncol(data)) )
      colnames(to_add) = colnames(data)
      to_add$Country=rep(levels(data$Country), each=empty_bar*nObsType )
      data=rbind(data, to_add)
      data=data %>% arrange(Country, Fleet)
      data$id=rep( seq(1, nrow(data)/nObsType) , each=nObsType)
      
      label_data= data %>% group_by(id, Fleet) %>% summarize(tot=sum(Effort,na.rm=TRUE))
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
        geom_bar(aes(x=as.factor(id), y=Effort*10, fill=metier), stat="identity", alpha=0.5) +
        scale_fill_manual(values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99"))+
        #scale_fill_viridis(discrete=TRUE) +
        
        # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
        geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
        geom_segment(data=grid_data, aes(x = end, y = 5, xend = start, yend = 5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
        geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
        
        
        # Add text showing the value of each 100/75/50/25 lines
        annotate("text", x = rep(max(data$id),3), y = c(0,5, 10), label = c("0", "50%", "100%") , color="grey", size=5 , angle=0, fontface="bold", hjust=1) +
        ylim(-5,max(label_data$tot, na.rm=T)+30) +
        #theme_minimal() +
        theme(
          legend.position = "left",
          legend.text = element_text(size=15),
          #legend.margin=margin(0,-20,62,0),
          legend.box.margin = margin(10,10,10,10),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          # plot.margin = unit(rep(-7,4), "cm") 
          plot.margin = unit(c(-8,-8,-8,-8) ,"cm") 
        ) +
        coord_polar() +
        # Add labels on top of each bar
        geom_text(data=label_data, aes(x=id, y=tot*10, label=Fleet, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
        
        # Add base line information
        geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )   +
        geom_text(data=base_data, aes(x = title, y = -1, label=Country), hjust=c(0.5,1,0.5,0,0.5), 
                  vjust=c(0.5,0.5,0,0.5,1.5), colour = "black", alpha=0.8, size=4.5, fontface="bold", inherit.aes = FALSE)}
    
  })
  
  
  #######Barplot#####
  efbyscenario <- reactive({
    filter(effbymet, fleet == input$FCEscen)# & scenario == input$fillScenario)
  })
  
  output$plotFleetScenario <-
    renderPlotly({
      p<-plot_ly(efbyscenario()) %>%
        add_trace(
          x = ~scenario, y = ~efmet, color = ~metier, type = "bar", hoverinfo = "text",
          text = ~ paste(paste("Effort:", efmet),paste("Share:",effshare), metier, sep = "<br />")
        )%>%
        layout(showticklabels = TRUE, showlegend = T,barmode="stack", yaxis = list(title = "Effort ('000 kwdays)"))
      p
    })
  
  
  
  
  
  ############### Fleet by Stock###########################
  
  effbys <- reactive({
    filter(effbystL, fleet == input$FCEfS)
  })
  output$plotFleetbyStock <-
    renderPlotly({
      plot_ly(effbys()) %>%
        add_trace(
          x = ~stock, y = ~effort, color = ~Country, type = "scatter",
          mode = "lines+markers", line = list(width = 2), hoverinfo = "text",
          text = ~ paste(paste("Effort:", effort), Country, sep = "<br />"), colors = c("yellow", "red", "blue", "green", "orange", "purple")
        ) %>%
        layout(showticklabels = TRUE, hovermode = "compare", showlegend = T, yaxis = list(title = "Effort ('000 kwdays)"))
    })
  
  tabeffbys <- reactive({
    filter(chocked, Country == input$FCEfS)
  })
  output$fleetbystock <- function() {
    text_tbl <-  tabeffbys()[-c(1,7)]
    names(text_tbl)<-c('Fleet','Max','Unchoked Stock','Min','Choked Stock')
    text_tbl %>%
      knitr::kable("html") %>%
      kable_styling(full_width = F, font_size = 16)
  }
  
  ####page3
  
  
  #
  output$FCubeCircularplot1 <- renderPlot({
    t1 <- filter(FCubepage3, value == "landings")
    t1$RelativeToSS <- round(t1$RelativeToSS, 3)
    t1 <- t1[c(1, 3, 7)]
    t1 <- t1 %>% arrange(sc, RelativeToSS)
    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 3
    to_add <- data.frame(matrix(NA, empty_bar * nlevels(t1$sc), ncol(t1)))
    colnames(to_add) <- colnames(t1)
    to_add$sc <- rep(levels(t1$sc), each = empty_bar)
    t1 <- rbind(t1, to_add)
    t1 <- t1 %>% arrange(sc)
    t1$id <- seq(1, nrow(t1))
    
    # Get the name and the y position of each label
    label_data <- t1
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse(angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle + 180, angle)
    
    # prepare a data frame for base lines
    base_data <- t1 %>%
      group_by(sc) %>%
      summarize(start = min(id), end = max(id) - empty_bar) %>%
      rowwise() %>%
      mutate(title = mean(c(start, end)))
    
    # prepare a data frame for grid (scales)
    grid_data <- base_data
    grid_data$end <- grid_data$end[ c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1, ]
    
    # Make the plot
    p <- ggplot(t1, aes(x = as.factor(id), y = RelativeToSS, fill = sc)) + # Note that id is a factor. If x is numeric, there is some space between the first bar
      
      geom_bar(aes(x = as.factor(id), y = RelativeToSS, fill = sc), stat = "identity", alpha = 0.5) +ggtitle("Prediction Relative to the Single Species Advice")+
      
      # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
      geom_segment(data = grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      geom_segment(data = grid_data, aes(x = end, y = 0.5, xend = start, yend = 0.5), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      geom_segment(data = grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      geom_segment(data = grid_data, aes(x = end, y = 1.5, xend = start, yend = 1.5), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      geom_segment(data = grid_data, aes(x = end, y = 2, xend = start, yend = 2), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      # geom_segment(data=grid_data, aes(x = end, y = 2.5, xend = start, yend = 2.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      
      # Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(max(t1$id), 5), y = c(0, 0.5, 1, 1.5, 2), label = c("0", "0.5", "1", "1.5", "2"), color = "black", size = 5, angle = 0, fontface = "bold", hjust = 0.75) +
      
      geom_bar(aes(x = as.factor(id), y = RelativeToSS, fill = sc), stat = "identity", alpha = 0.5) +
      # ylim(-100,120) +
      # ylim(-10,max(label_data$tot, na.rm=T)+20) +
      theme_minimal() +
      theme(
        # legend.position = "none",
        legend.position = "left",
        # legend.text = element_text(size=17),
        legend.margin = margin(0, -50, 62, 0),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        # plot.margin = unit(c(0,0,0,0), "mm")
        plot.margin = unit(rep(-1, 4), "cm")
      ) +
      coord_polar() +
      geom_text(data = label_data, aes(x = id, y = RelativeToSS, label = stock, hjust = hjust), color = "black", fontface = "bold", alpha = 0.6, size = 4, angle = label_data$angle, inherit.aes = FALSE) +
      
      # Add base line information
      geom_segment(data = base_data, aes(x = start, y = -0.1, xend = end, yend = -0.1), colour = "black", alpha = 0.8, size = 1, inherit.aes = FALSE) +
      geom_segment(data = base_data, aes(x = start, y = 1, xend = end, yend = 1), colour = "red", alpha = 0.8, size = 0.8, inherit.aes = FALSE) +
      geom_text(
        data = base_data, aes(x = title, y = -1, label = sc), hjust = c(-0.3, -1, -2, 0.6, 2, 4, 1.3),
        vjust = c(-6.8, -4, 4, 7.5, 5.5, 0, -6), colour = "black", alpha = 0.8, size = 4, fontface = "bold", inherit.aes = FALSE
      )
    
    p
  })
  
  t3 <- filter(FCubepage3Radar, value == "landings")
  
  output$FCubeRadarplot1 <-
    renderPlotly({
      plot_ly(
        type = "scatterpolar",
        # mode = 'lines',
        # fill = "toself"
        fill="tozeroy"
      ) %>%
        add_trace(
          r = c(1, 1, 1, 1, 1, 1, 1, 1),
          theta = c(levels(t3$sc), levels(t3$sc)[1]),
          # fill="tozeroy",
          fill = "toself",
          name = "Ratio=1",
          lines = list(color = "red"),
          marker = list(size = 14, color = "red",symbol=4)
        ) %>%
        add_trace(
          r = t3$`COD-CS`,
          theta = t3$sc,
          name = "COD-CS", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t3$`HAD-CS`,
          theta = t3$sc,
          name = "HAD-CS", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t3$`MON-CS`,
          theta = t3$sc,
          name = "MON-CS", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t3$`N-HKE`,
          theta = t3$sc,
          name = "N-HKE", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t3$`N-MEG`,
          theta = t3$sc,
          name = "N-MEG", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t3$`SOL-7E`,
          theta = t3$sc,
          name = "sol-7E", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t3$`SOL-7FG`,
          theta = t3$sc,
          name = "SOL-7FG", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t3$`WHG-CS`,
          theta = t3$sc,
          name = "WHG-CS", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        layout(hovermode = "compare")
    })
  
  output$FCubeOvershootrplot1<-renderPlotly({
    t31 <- filter(FCubepage3, value == "landings",sc==input$FCOver1)
    t31 %>%plot_ly(x = ~stock, y = ~Diff, color = ~Decision ,hoverinfo = "text",
                   text = ~ paste(paste("Advice:",SSAdvice),paste( "Predicted:",data),paste(Decision,":",abs(Diff)), sep = "<br />")) %>%
      layout(showticklabels = TRUE, showlegend = T,   yaxis = list(title = ""))
  })
  
  output$FCubepage31 <- renderUI({
    if (input$PlottypeFpage == 2) {
      list(h4("Prediction Relative to the Single Species Advice.",
              style = "font-weight:bold;color:orange;text-decoration: underline;"
      ),
      plotlyOutput("FCubeRadarplot1"),br(),
      h4("Stock overshoot and undershoot by scenarios.",
         style = "font-weight:bold;color:orange;text-decoration: underline;"
      ),
      div(
        style = "border-radius: 25px,width:120px;color:orange",
        selectInput("FCOver1",
                    label = "Select Scenario", choices = levels(FCubepage3$sc),
                    selectize = T
        )),plotlyOutput("FCubeOvershootrplot1"))
    }
    else {list(
      
      h4("Prediction Relative to the Single Species Advice.",
         style = "font-weight:bold;color:orange;text-decoration: underline;"
      ),
      plotOutput("FCubeCircularplot1", width = 800, height = 700))
    }
  })
  
  
  
  output$FCubeCircularplot2 <- renderPlot({
    t1 <- filter(FCubepage3, value == "Fbar")
    t1$RelativeToSS <- round(t1$RelativeToSS, 3)
    t1 <- t1[c(1, 3, 7)]
    t1 <- t1 %>% arrange(sc, RelativeToSS)
    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 3
    to_add <- data.frame(matrix(NA, empty_bar * nlevels(t1$sc), ncol(t1)))
    colnames(to_add) <- colnames(t1)
    to_add$sc <- rep(levels(t1$sc), each = empty_bar)
    t1 <- rbind(t1, to_add)
    t1 <- t1 %>% arrange(sc)
    t1$id <- seq(1, nrow(t1))
    
    # Get the name and the y position of each label
    label_data <- t1
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse(angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle + 180, angle)
    
    # prepare a data frame for base lines
    base_data <- t1 %>%
      group_by(sc) %>%
      summarize(start = min(id), end = max(id) - empty_bar) %>%
      rowwise() %>%
      mutate(title = mean(c(start, end)))
    
    # prepare a data frame for grid (scales)
    grid_data <- base_data
    grid_data$end <- grid_data$end[ c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1, ]
    
    # Make the plot
    p <- ggplot(t1, aes(x = as.factor(id), y = RelativeToSS, fill = sc)) + # Note that id is a factor. If x is numeric, there is some space between the first bar
      
      geom_bar(aes(x = as.factor(id), y = RelativeToSS, fill = sc), stat = "identity", alpha = 0.5) +
      
      # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
      geom_segment(data = grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      geom_segment(data = grid_data, aes(x = end, y = 0.5, xend = start, yend = 0.5), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      geom_segment(data = grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      geom_segment(data = grid_data, aes(x = end, y = 1.5, xend = start, yend = 1.5), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      geom_segment(data = grid_data, aes(x = end, y = 2, xend = start, yend = 2), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      # geom_segment(data=grid_data, aes(x = end, y = 2.5, xend = start, yend = 2.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      
      # Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(max(t1$id), 5), y = c(0, 0.5, 1, 1.5, 2), label = c("0", "0.5", "1", "1.5", "2"), color = "black", size = 5, angle = 0, fontface = "bold", hjust = 0.75) +
      
      geom_bar(aes(x = as.factor(id), y = RelativeToSS, fill = sc), stat = "identity", alpha = 0.5) +
      # ylim(-100,120) +
      # ylim(-10,max(label_data$tot, na.rm=T)+20) +
      theme_minimal() +
      theme(
        # legend.position = "none",
        legend.position = "left",
        # legend.text = element_text(size=17),
        legend.margin = margin(0, -50, 62, 0),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        # plot.margin = unit(c(0,0,0,0), "mm")
        plot.margin = unit(rep(-1, 4), "cm")
      ) +
      coord_polar() +
      geom_text(data = label_data, aes(x = id, y = RelativeToSS, label = stock, hjust = hjust), color = "black", fontface = "bold", alpha = 0.6, size = 4, angle = label_data$angle, inherit.aes = FALSE) +
      
      # Add base line information
      geom_segment(data = base_data, aes(x = start, y = -0.1, xend = end, yend = -0.1), colour = "black", alpha = 0.8, size = 1, inherit.aes = FALSE) +
      geom_segment(data = base_data, aes(x = start, y = 1, xend = end, yend = 1), colour = "red", alpha = 0.8, size = 0.8, inherit.aes = FALSE) +
      geom_text(
        data = base_data, aes(x = title, y = -1, label = sc), hjust = c(-0.3, -1, -2, 0.6, 2, 4, 1.3),
        vjust = c(-6.8, -4, 4, 7.5, 5.5, 0, -6), colour = "black", alpha = 0.8, size = 4, fontface = "bold", inherit.aes = FALSE
      )
    
    p
  })
  
  t4 <- filter(FCubepage3Radar, value == "Fbar")
  
  output$FCubeRadarplot2 <-
    renderPlotly({
      plot_ly(
        type = "scatterpolar",
        # mode = 'lines',
        #fill = "toself"
        fill="tozeroy"
      ) %>%
        add_trace(
          r = c(1, 1, 1, 1, 1, 1, 1, 1),
          theta = c(levels(t4$sc), levels(t4$sc)[1]),
          # fill="tozeroy",
          fill = "toself",
          name = "Ratio=1",
          lines = list(color = "red"),
          marker = list(size = 14, color = "red",symbol=4)
        ) %>%
        add_trace(
          r = t4$`COD-CS`,
          theta = t4$sc,
          name = "COD-CS", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t4$`HAD-CS`,
          theta = t4$sc,
          name = "HAD-CS", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t4$`MON-CS`,
          theta = t4$sc,
          name = "MON-CS", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t4$`N-HKE`,
          theta = t4$sc,
          name = "N-HKE", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t4$`N-MEG`,
          theta = t4$sc,
          name = "N-MEG", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t4$`SOL-7E`,
          theta = t4$sc,
          name = "sol-7E", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t4$`SOL-7FG`,
          theta = t4$sc,
          name = "SOL-7FG", marker = list(size = 12)#, lines = list(color = "black")
        ) %>%
        add_trace(
          r = t4$`WHG-CS`,
          theta = t4$sc,
          name = "WHG-CS", marker = list(size = 12)#, lines = list(color = "black")
        )
    })
  
  output$FCubeOvershootrplot2<-renderPlotly({
    t31 <- filter(FCubepage3, value == "Fbar",sc==input$FCOver2)
    t31 %>%plot_ly(x = ~stock, y = ~Diff, color = ~Decision ,hoverinfo = "text",
                   text = ~ paste(paste("Advice:",SSAdvice),paste( "Predicted:",data),paste(Decision,":",abs(Diff)), sep = "<br />")) %>%
      layout(showticklabels = TRUE, showlegend = T,   yaxis = list(title = ""))})
  
  output$FCubepage32 <- renderUI({
    if (input$PlottypeFpage == 2) {list(
      
      h4("Prediction Relative to the Single Species Advice.",
         style = "font-weight:bold;color:orange;text-decoration: underline;"
      ),
      plotlyOutput("FCubeRadarplot2"),br(),
      h4("Stock overshoot and undershoot by scenarios.",
         style = "font-weight:bold;color:orange;text-decoration: underline;"
      ),
      div(
        style = "border-radius: 25px,width:120px;color:orange",
        selectInput("FCOver2",
                    label = "Select Scenario", choices = levels(FCubepage3$sc),
                    selectize = T
        )),plotlyOutput("FCubeOvershootrplot2"))
    }
    else {
      list(
        
        h4("Prediction Relative to the Single Species Advice.",
           style = "font-weight:bold;color:orange;text-decoration: underline;"
        ),
        plotOutput("FCubeCircularplot2", width = 800, height = 700))
    }
  })
  
  
  output$FCubeCircularplot3 <- renderPlot({
    t1 <- filter(FCubepage3, value == "ssb" & year == input$SSEyear)
    t1$RelativeToSS <- round(t1$RelativeToSS, 3)
    t1 <- t1[c(1, 3, 7)]
    t1 <- t1 %>% arrange(sc, RelativeToSS)
    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 3
    to_add <- data.frame(matrix(NA, empty_bar * nlevels(t1$sc), ncol(t1)))
    colnames(to_add) <- colnames(t1)
    to_add$sc <- rep(levels(t1$sc), each = empty_bar)
    t1 <- rbind(t1, to_add)
    t1 <- t1 %>% arrange(sc)
    t1$id <- seq(1, nrow(t1))
    
    # Get the name and the y position of each label
    label_data <- t1
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse(angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle + 180, angle)
    
    # prepare a data frame for base lines
    base_data <- t1 %>%
      group_by(sc) %>%
      summarize(start = min(id), end = max(id) - empty_bar) %>%
      rowwise() %>%
      mutate(title = mean(c(start, end)))
    
    # prepare a data frame for grid (scales)
    grid_data <- base_data
    grid_data$end <- grid_data$end[ c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1, ]
    
    # Make the plot
    p <- ggplot(t1, aes(x = as.factor(id), y = RelativeToSS, fill = sc)) + # Note that id is a factor. If x is numeric, there is some space between the first bar
      
      geom_bar(aes(x = as.factor(id), y = RelativeToSS, fill = sc), stat = "identity", alpha = 0.5) +
      
      # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
      geom_segment(data = grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      geom_segment(data = grid_data, aes(x = end, y = 0.5, xend = start, yend = 0.5), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      geom_segment(data = grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      geom_segment(data = grid_data, aes(x = end, y = 1.5, xend = start, yend = 1.5), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      geom_segment(data = grid_data, aes(x = end, y = 2, xend = start, yend = 2), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
      # geom_segment(data=grid_data, aes(x = end, y = 2.5, xend = start, yend = 2.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      
      # Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(max(t1$id), 5), y = c(0, 0.5, 1, 1.5, 2), label = c("0", "0.5", "1", "1.5", "2"), color = "black", size = 5, angle = 0, fontface = "bold", hjust = 0.75) +
      
      geom_bar(aes(x = as.factor(id), y = RelativeToSS, fill = sc), stat = "identity", alpha = 0.5) +
      # ylim(-100,120) +
      # ylim(-10,max(label_data$tot, na.rm=T)+20) +
      theme_minimal() +
      theme(
        # legend.position = "none",
        legend.position = "left",
        # legend.text = element_text(size=17),
        legend.margin = margin(0, -50, 62, 0),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        # plot.margin = unit(c(0,0,0,0), "mm")
        plot.margin = unit(rep(-1, 4), "cm")
      ) +
      coord_polar() +
      geom_text(data = label_data, aes(x = id, y = RelativeToSS, label = stock, hjust = hjust), color = "black", fontface = "bold", alpha = 0.6, size = 4, angle = label_data$angle, inherit.aes = FALSE) +
      
      # Add base line information
      geom_segment(data = base_data, aes(x = start, y = -0.1, xend = end, yend = -0.1), colour = "black", alpha = 0.8, size = 1, inherit.aes = FALSE) +
      geom_segment(data = base_data, aes(x = start, y = 1, xend = end, yend = 1), colour = "red", alpha = 0.8, size = 0.8, inherit.aes = FALSE) +
      geom_text(
        data = base_data, aes(x = title, y = -1, label = sc), hjust = c(-0.3, -1, -2, 0.6, 2, 4, 1.3),
        vjust = c(-6.8, -4, 4, 7.5, 5.5, 0, -6), colour = "black", alpha = 0.8, size = 4, fontface = "bold", inherit.aes = FALSE
      )
    
    p
  })
  
  
  
  
  output$FCubeRadarplot3 <-
    
    renderPlotly({
      if (input$SSEyear == 2019) {
        t5 <- filter(FCubepage3Radar, value == "ssb" & year == 2019)
        plot_ly(
          type = "scatterpolar",
          # mode = 'lines',
          #fill = "toself"
          fill="tozeroy"
        ) %>%
          add_trace(
            r = c(1, 1, 1, 1, 1, 1, 1, 1),
            theta = c(levels(t4$sc), levels(t4$sc)[1]),
            # fill="tozeroy",
            fill = "toself",
            name = "Ratio=1",
            lines = list(color = "red"),
            marker = list(size = 14, color = "red",symbol=4)
          ) %>%
          add_trace(
            r = t5$`COD-CS`,
            theta = t5$sc,
            name = "COD-CS", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`HAD-CS`,
            theta = t5$sc,
            name = "HAD-CS", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`MON-CS`,
            theta = t5$sc,
            name = "MON-CS", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`N-HKE`,
            theta = t5$sc,
            name = "N-HKE", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`N-MEG`,
            theta = t5$sc,
            name = "N-MEG", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`SOL-7E`,
            theta = t5$sc,
            name = "sol-7E", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`SOL-7FG`,
            theta = t5$sc,
            name = "SOL-7FG", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`WHG-CS`,
            theta = t5$sc,
            name = "WHG-CS", marker = list(size = 12)#, lines = list(color = "black")
          )
      }
      else if (input$SSEyear == 2020) {
        t5 <- filter(FCubepage3Radar, value == "ssb" & year == 2020)
        plot_ly(
          type = "scatterpolar",
          # mode = 'lines',
          #fill = "toself"
          fill="tozeroy"
        ) %>%
          add_trace(
            r = c(1, 1, 1, 1, 1, 1, 1, 1),
            theta = c(levels(t4$sc), levels(t4$sc)[1]),
            # fill="tozeroy",
            fill = "toself",
            name = "Ratio=1",
            lines = list(color = "red"),
            marker = list(size = 14, color = "red",symbol=4)
          ) %>%
          add_trace(
            r = t5$`COD-CS`,
            theta = t5$sc,
            name = "COD-CS", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`HAD-CS`,
            theta = t5$sc,
            name = "HAD-CS", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`MON-CS`,
            theta = t5$sc,
            name = "MON-CS", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`N-HKE`,
            theta = t5$sc,
            name = "N-HKE", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`N-MEG`,
            theta = t5$sc,
            name = "N-MEG", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`SOL-7E`,
            theta = t5$sc,
            name = "sol-7E", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`SOL-7FG`,
            theta = t5$sc,
            name = "SOL-7FG", marker = list(size = 12)#, lines = list(color = "black")
          ) %>%
          add_trace(
            r = t5$`WHG-CS`,
            theta = t5$sc,
            name = "WHG-CS", marker = list(size = 12)#, lines = list(color = "black")
          )
      }
    })
  
  output$FCubeOvershootrplot3<-renderPlotly({
    if (input$SSEyear == 2019) {
      t31 <- filter(FCubepage3, value == "ssb",year==2019,sc==input$FCOver3)
      t31 %>%plot_ly(x = ~stock, y = ~Diff, color = ~Decision ,hoverinfo = "text",
                     text = ~ paste(paste("Advice:",SSAdvice),paste( "Predicted:",data),paste(Decision,":",abs(Diff)), sep = "<br />")) %>%
        layout(showticklabels = TRUE, showlegend = T,   yaxis = list(title = ""))}
    else if (input$SSEyear == 2020) {
      t31 <- filter(FCubepage3, value == "ssb",year==2020,sc==input$FCOver3)
      t31 %>%plot_ly(x = ~stock, y = ~Diff, color = ~Decision ,hoverinfo = "text",
                     text = ~ paste(paste("Advice:",SSAdvice),paste( "Predicted:",data),paste(Decision,":",abs(Diff)), sep = "<br />")) %>%
        layout(showticklabels = TRUE, showlegend = T,   yaxis = list(title = ""))}
    
    
  })
  
  
  output$FCubepage33 <- renderUI({
    
    if (input$PlottypeFpage == 2) {list(
      
      h4("Prediction Relative to the Single Species Advice.",
         style = "font-weight:bold;color:orange;text-decoration: underline;"
      ),
      plotlyOutput("FCubeRadarplot3"),br(),
      h4("Stock overshoot and undershoot by scenarios.",
         style = "font-weight:bold;color:orange;text-decoration: underline;"
      ),
      div(
        style = "border-radius: 25px,width:120px;color:orange",
        selectInput("FCOver3",
                    label = "Select Scenario", choices = levels(FCubepage3$sc),
                    selectize = T
        )),plotlyOutput("FCubeOvershootrplot3"))
    }
    else {list(h4("Prediction Relative to the Single Species Advice.",
                  style = "font-weight:bold;color:orange;text-decoration: underline;"
    ),
    plotOutput("FCubeCircularplot3", width = 800, height = 700))
    }
  })
  
  
  ###end of server###  
    }


########################################## ui ##################################################


ui <- fluidPage(tags$head(
  tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                  .btn:focus{ background-color:lightgrey;}
                  .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter,
                  .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,
                  .dataTables_wrapper .dataTables_paginate {color: #ffffff; }
                  thead { color: #ffffff;}tbody {color: #000000;"))),
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"),
  theme = shinytheme("superhero"), #spacelab
  titlePanel("Mixed Fisheries"),
  navlistPanel(id="mainpanel", widths=c(2,10), 
               tabPanel(" Introduction",
                        value = "mp", icon = icon("home"),
                        fluidRow(column(width = 5, offset = 4, h2("Celtic Seas Ecoregion.",
                                                                  style = "font-family: 'Lobster', cursive;
                                                                  font-weight: 500; line-height: 1.1; "))), hr(),
                        div(style = " border-radius: 25px;height:200px;background-color:#e5f5e0 ;color: black",
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
                              )), useShinyalert(), br(),
                        fluidRow(
                          column(width = 6, div(
                            style = " border-radius: 25px;height:27px;background-color:#cc4c02;font-size:
                            22px;color:lightblue;",
                            tags$ul("Celtic Seas Ecoregion Map.")
                          )),
                          column(width = 3, div(
                            style = " border-radius: 25px;height:27px;background-color:#cc4c02;font-size:
                            22px;color:lightblue;",
                            tags$ul("Explore.")
                          ), hr())
                        ),
                        fluidRow(column(width = 6, h5("* showing major cities, ports, and ICES areas.")),
                                 column(width = 3, div(
                                   style = "display: inline-block;vertical-align:top; width: 225px;",
                                   selectInput("FishGear", "", choices = c("Who is Fishing", "Gear Type")), class = "btn-link"
                                 )), uiOutput("I_selections")),
                        fluidRow(column(
                          width = 6,img(
                            src = "images/Celtic_Seas_map.png", height = "400px",
                            width = "600px", style = "padding-top: 7px; padding-bottom: 5px; 
                            padding-right: 20px;"
                          )
                          ), column(6, uiOutput("fishing")))
                        ),
               
               tabPanel(" Hackathon Work", value = "hw", icon = icon("folder-open"),
                        h3("Visualising the implications of catch decreases for fleets in a mixed fishery context"),
                        plotOutput("plot"),
                        absolutePanel(id="controls",top = 80, left = 700, width = 400, height = "auto", fixed=FALSE, draggable = TRUE,
                                      sliderInput("whitingslider", "Choose % reduction in Whiting Catch:", min = -100, max =0, value = 0, 
                                                  step = NULL, sep = "", animate = FALSE, post  = " %"))),
               tabPanel("Landings: Celtic Sea",
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
                                       selectInput("Country", "Select Country:", choices = levels(test$Country)), class = "btn-link"
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
                                   )
                                     ),
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
                                       column(width = 3, div(style = "display: inline-block;vertical-align:top; width: 225px;", selectInput("CountryE", "Select Country:", choices = levels(test$Country)), class = "btn-link")),
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
),

tabPanel(" Existing Tools", value = "et", icon = icon("wrench"),
         selectInput("Area_selector","Select Area", choices=c("North_Sea", "Celtic_Sea"), selected = "North_Sea"),
         selectInput(inputId = "Toolselected", label="Tool", choices=c("Raw accessions App","Effort App","Catchability App","Partial F App","Quota share App"),
                     multiple=FALSE, selected = "Catchability App"),
         #conditionalPanel("input.Toolselected=='Raw accessions App'"),
         conditionalPanel(condition = "input.Toolselected == 'Effort App'",
                          tabsetPanel(id="effortappPanel", type="pills",
                                      tabPanel("Fleet Effort Tables",
                                               fluidPage(
                                                 titlePanel("Effort data"), #paste(textOutput("Area"),
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
                                                 titlePanel("Effort data"), #paste(Area,"Effort data")),
                                                 fluidRow(
                                                   column(3,uiOutput("time.countryfilter")) 
                                                 ),
                                                 mainPanel(
                                                   plotOutput("plotEffTS", width = '800px', height = '800px')
                                                   %>% withSpinner(color="#0dc5c1")
                                                 )
                                               ) #end of fluidPage
                                      ),#end of tabPanel
                                      tabPanel("Total effort by fleet",
                                               fluidPage(
                                                 titlePanel(paste("Effort by fleet relative to first data year")), #paste(Area,"Effort by fleet relative to first data year")),
                                                 mainPanel(
                                                   plotOutput("plotEff_Fl", width = '800px', height = '800px')
                                                   %>% withSpinner(color="#0dc5c1")
                                                 )
                                               ) #end of FluidPage
                                      ) #end of tabPanel
                          ) #end of tabsetPanel
         ), #end of conditionalPanel
         conditionalPanel("input.Toolselected == 'Catchability App'", 
                          tabsetPanel(id="FleetCatchabilityPanel", type="tabs",
                                      tabPanel("Fleet Catchability Tables",
                                               fluidPage(
                                                 titlePanel("Catchability data"), #paste(Area,
                                                 fluidRow(
                                                   column(3,uiOutput("table.yearfilter")),
                                                   column(3,uiOutput("table.stockfilter"))
                                                 ),
                                                 fluidRow(
                                                   DT::dataTableOutput("Catchtable")
                                                   %>% withSpinner(color="#0dc5c1")
                                                 )
                                               )),
                                      tabPanel("Catchability Plots",
                                               fluidPage(
                                                 titlePanel("Catchability data"), #paste(Area,
                                                 fluidRow(
                                                   column(3,uiOutput("plot.countryfilter")),
                                                   column(3,uiOutput("plot.stockfilter"))
                                                 ),
                                                 mainPanel(
                                                   plotOutput("plotCatchability", width = '800px', height = '800px')
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
                                                 titlePanel("Partial F data"),#paste(Area,
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
                                                 titlePanel("Partial F data"),#paste(Area,)
                                                 fluidRow(
                                                   column(3,uiOutput("PF.country.plot1")
                                                   ),
                                                   column(3,uiOutput("PF.stock.plot1")
                                                   )
                                                 ),
                                                 mainPanel(
                                                   plotOutput("plotPartialF", width = '800px', height = '800px')
                                                   %>% withSpinner(color="#0dc5c1")
                                                 )
                                               ) #end of FluidPage
                                      ),#end of tabPanel
                                      tabPanel("Partial F spider chart",
                                               fluidPage(
                                                 titlePanel("Partial F data"), #paste(Area,)
                                                 fluidRow(
                                                   column(3,uiOutput("PF.year.plot2")
                                                   ),
                                                   column(3,uiOutput("PF.stock.plot2")
                                                   )
                                                 ),
                                                 mainPanel(
                                                   plotOutput("Spiderplot", width = '800px', height = '800px')
                                                   %>% withSpinner(color="#0dc5c1")
                                                 )
                                               ) #end of FluidPage
                                      )#end of tabPanel
                          )#end of tabsetPanel
         ), #end of Partial F conditionalPanel
         conditionalPanel("input.Toolselected == 'Quota share App'")
),
tabPanel(" Mapping", value ="sc", icon = icon("map-marked"),
         fluidRow(column(width=5, selectInput("Species_selector","Select Species", choices=c(sp)),
                         plotOutput('Stockareas')),
                  column(width=5,offset=1,
                         selectizeInput(inputId = "Stockselector", label="Select Stock",
                                        choices=NULL, multiple=FALSE),
                         plotOutput('Stockoverlap'))
         ),
         br(),
         br(),
         br(),
         br(),
         br(),
         br()
),
tabPanel("Scenarios: Celtic Sea",
         value = "sc", icon = icon("line-chart"),
         tabsetPanel(
           id = "Mtabselected", type = "pills",
           tabPanel("Advice 2019",
                    value = "page1", fluidRow(column(
                      width = 5, offset = 3,
                      h2("Reproduce the Single Species Advice.", style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1; ")
                      )), hr(),
                    fluidRow(
                      column(width = 5, offset = 1, h4("Description of Stocks Grouping into Tiers.",
                                                       style = "font-weight:bold;color:orange;text-decoration: underline;"
                      )),
                      column(
                        width = 6, h4("Summary of Species Stock Status.",
                                      style = "text-align:center;font-weight:bold;color:orange;text-decoration: underline;"
                        ), actionButton("A1", "Fbar", style = "border-radius: 10px;padding-top: 7px; padding-bottom: 5px; padding-right: 20px;color: black;", class = "btn-primary"),
                        actionButton("A2", "SSB", style = "border-radius: 10px;padding-top: 7px; padding-bottom: 5px; padding-right: 20px;color: black;", class = "btn-primary"),
                        actionButton("A3", "Catch", style = "border-radius: 10px;padding-top: 7px; padding-bottom: 5px; padding-right: 20px;color: black;", class = "btn-primary"),
                        actionButton("A4", "Discards", style = "border-radius: 10px;padding-top: 7px; padding-bottom: 5px; padding-right: 20px;color: black;", class = "btn-primary"),
                        actionButton("A5", "Landings", style = "border-radius: 10px;padding-top: 7px; padding-bottom: 5px; padding-right: 20px;color: black;", class = "btn-primary")
                      )
                    ),
                    fluidRow(
                      column(width = 5, tableOutput("table1") %>% withSpinner(color = "#0dc5c1"), img(
                        src = "images/wave.jpeg", height = "170px",
                        width = "550px", style = "align:center;padding-top: 7px; padding-bottom: 5px; 
                        padding-right: 20px;"
                      )), column(
                        width = 7, plotlyOutput("StockStatus") %>%
                          withSpinner(color = "#0dc5c1"),
                        h4("Biological Reference Points and Advice.", style = " text-align:center;font-weight:bold;color:orange;text-decoration: underline;"), plotlyOutput("Advice") %>%
                          withSpinner(color = "#0dc5c1")
                      )
                    )
                    ),
           tabPanel("FCube",
                    value = "page2",
                    fluidRow(column(
                      width = 5, offset = 4,
                      h2("FCube Forecast.", style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1; ")
                      )), hr(),
                    tabsetPanel(
                      id = "FCubepages", type = "pills",
                      tabPanel(
                        "Page 1", fluidRow(
                          column(width = 5, h4("Mixed-fisheries scenarios considered for the Celtic Sea gadoids.",
                                               style = "font-weight:bold;color:orange;text-decoration: underline;"
                          )),
                          column(width = 6, h4("Métier categories used in the mixed-fisheries analysis.",
                                               style = "margin-bottom=125px;text-align:center;font-weight:bold;color:orange;text-decoration: underline;"
                          ))
                        ),
                        fluidRow(
                          column(width = 5, tableOutput("table2")),
                          column(width = 7, tableOutput("table3"), img(
                            src = "images/wave.jpeg", height = "200px",
                            width = "800px", style = "align:center;padding-top: 7px; padding-bottom: 5px; 
                            padding-right: 20px;"
                          ))
                          )
                        ),
                      tabPanel(
                        "Page 2", fluidRow(
                          column(6, prettyRadioButtons("EfFilter",
                                                       label = h3(""), thick = T, animation = "pulse",
                                                       choices = list("RELATIVE SHARE OF SPECIES’ LANDINGS" = 1, "FLEET BY SCENARIO" = 2, "FLEET BY STOCK" = 3),
                                                       selected = 1, inline = T
                          )),
                          column(
                            3, conditionalPanel(
                              condition = "input.EfFilter == 1",
                              div(
                                style = "border-radius: 25px,width:120px;color:orange",
                                selectInput("FCShare",
                                            label = "Select Stock", choices = levels(shareW$stock),
                                            selectize = T
                                )
                              )
                            ),
                            conditionalPanel(
                              condition = "input.EfFilter == 2"
                            ),
                            conditionalPanel(
                              condition = "input.EfFilter == 3",
                              div(
                                style = "border-radius: 25px,width:120px;color:orange",
                                selectInput("FCEfS", label = "Select Fleet", levels(effbystL$fleet), selectize = T)
                              )
                            )
                          ),
                          column(
                            3, conditionalPanel(
                              condition = "input.EfFilter == 1",
                              div(
                                style = "border-radius: 25px,width:120px;color:orange",
                                selectInput("filltype", label = "Select Country", choices = c("BE", "EN","FR","IE","OT","SP"), selectize = T)
                              )
                            ),
                            conditionalPanel(
                            condition = "input.EfFilter == 2")
                            ,
                            conditionalPanel(condition = "input.EfFilter == 3")
                          )
                        ),
                        fluidRow(
                        conditionalPanel(
                            condition = "input.EfFilter == 1",
                            column(width = 5, h5("Relative share of species' landings by country in 2019 scenarios compared to the 2017 (baseline)", style = "margin-bottom=125px;text-align:center;
                                                 font-weight:bold;color:orange;text-decoration: underline;"), plotlyOutput("radrarS")), column(width = 6, tableOutput("shareWtable"))
                            ),conditionalPanel(
                              condition = "input.EfFilter == 2",
                              column(width = 12, h5("Effort by fleet and metier  for 2017(baseline) and estimates for various scenarios in 2019.", style = "margin-bottom=125px;text-align:center;
                                                    font-weight:bold;color:orange;text-decoration: underline;"),br(),
                                     fluidRow(column(3, div(
                                       style = "border-radius: 25px,width:120px;color:orange",
                                       selectInput("FCEfSbarPlot", label = "Select Scale", choices = c("Log Scale"=1,"Percentages"=2), selectize = T)
                                     ) ),
                                     column(3, div(
                                       style = "border-radius: 25px,width:120px;color:orange",
                                       selectInput("FCEfSbar", label = "Select Scenario", levels(effbymet$scenario), selectize = T))))
                                     ,br(),plotOutput("FCEfSbarplot",height = 800),
                                     div(
                                       style = "border-radius: 25px,width:120px;color:orange",
                                       selectInput("FCEscen",
                                                   label = "Select Flteet", choices = levels(effbymet$fleet),
                                                   selectize = T
                                       )
                                       
                                     ), plotlyOutput("plotFleetScenario", height = 600))
                              #,
                              #column(width = 4, tableOutput("EscenWtable"))
                              ),
                          conditionalPanel(condition = "input.EfFilter == 3", column(width = 7, h5("Estimates of Effort by fleet corresponding to the individual quota share by fish stock in 2019", style = "margin-bottom=125px;text-align:center;
                                                                                                   font-weight:bold;color:orange;text-decoration: underline;"), plotlyOutput("plotFleetbyStock", height = 600)),
                           column(width = 5,  div(style = " border-radius: 5px;background-color:grey ;",
                           tableOutput("fleetbystock")))
                           )
                      )), tabPanel("Page 3", fluidRow(
                        column(
                          3, prettyRadioButtons("FCubeFilter",
                                                label = "", thick = T, animation = "pulse",
                                                choices = list("LANDINGS" = 1, "FBAR" = 2, "SSB" = 3),
                                                selected = 1, inline = F
                          ),
                          div(
                            style = "border-radius: 25px,width:120px;color:orange",
                            selectInput("PlottypeFpage",
                                        label = "Select Plot", choices = c("Radar" = 2, "Circular Barchart" = 1),
                                        selectize = T
                            )
                          ),
                          conditionalPanel(condition = "input.FCubeFilter == 1"),
                          conditionalPanel(condition = "input.FCubeFilter == 2"),
                          conditionalPanel(
                            condition = "input.FCubeFilter == 3",
                            div(
                              style = "border-radius: 25px,width:120px;color:orange",
                              selectInput("SSEyear",
                                          label = "Select Year", choices = c(2019, 2020),
                                          selectize = T
                              )
                            )
                          )
                        ),
                        column(
                          9,
                          conditionalPanel(condition = "input.FCubeFilter == 1", uiOutput("FCubepage31")) # plotOutput("FCubeCircularplot1",width = 800 , height = 700))
                          ,
                          conditionalPanel(condition = "input.FCubeFilter == 2", uiOutput("FCubepage32")) # plotOutput("FCubeCircularplot2",width = 800 , height = 700))
                          ,
                          conditionalPanel(condition = "input.FCubeFilter == 3", uiOutput("FCubepage33")) # plotOutput("FCubeCircularplot3",width = 800 , height = 700))
                        )
                      ))
                      
                      )
                      )
      )
    )
),

hr(),
fluidRow(width =12,
         img(src="Logos/Niamh.png", width = "1250px", height = "100px", 
             style="display: block; margin-left: auto; margin-right: auto;margin-top:0em")
)
)

shinyApp(ui, server)
=======
library(shiny)
library(tidyverse)
library(viridis)

setwd("P:\\Hackathon\\Data prep")
data_fish <-  read.csv(file="Data.csv")
data_fish <- data_fish %>%
  mutate(Country =  
           recode(Country,
                           'UK(Northern Ireland)'="N.Ireland",
                           'UK(Scotland)'="Scotland",
                           'UK (England)'="England"
                           )
  )

ui <- fluidPage(
  
  titlePanel("Visualising the implications of catch decreases for fleets in a mixed fishery context"),
    mainPanel(width=12,
      plotOutput("plot"), #width = "800px" , height = "900px"),
      style = "margin-top:-10em"
    ),
    absolutePanel(id="controls", 
                top = 80, left = 980, width = 400, height = "auto", fixed=FALSE,draggable = TRUE,
                sliderInput("whitingslider", "Choose % reduction in Whiting Catch:", min = -100, max =0, value = 0, 
                            step = NULL, sep = "", animate = FALSE, post  = " %")
    )
  )



server <- function(input, output) {
  #output$data=reactive({})
  

  output$plot <- renderPlot({
    # Transform data in a tidy format (long format)
    TotalWhiting=sum(data_fish$Whiting, na.rm=TRUE)
    Change=TotalWhiting*(abs(input$whitingslider)/100)  
    #Change=TotalWhiting*(abs(20)/100)
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
    
  }, bg="transparent", height= 1000)
}

shinyApp(ui=ui,server=server)
>>>>>>> 74ca1156c53247cac153633245e236a0f55e5315
