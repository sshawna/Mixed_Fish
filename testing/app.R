
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

options(scipen=999)

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





########################################## Server ##################################################

server <- function(input, output, session) {
 
  
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
  
  
  output$FCEfSbarplot1<-renderPlot({
    if(input$FCEfSbarPlot==1){
      t1 <- filter(effbymet,scenario == input$FCEfSbar1)
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
        ylim(-10,max(label_data$tot, na.rm=T)+1) +
        theme_minimal() +
        theme(
          legend.position = "left",
          legend.text = element_text(size=10),
          legend.margin=margin(0,-200,300,0),
          #legend.box.margin = margin(10,10,10,10),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          #plot.margin =  margin(0,0,0,0)
          plot.margin = unit(c(-4,-6,0,-1) ,"cm") 
        ) +
        coord_polar() +
        # Add labels on top of each bar
        geom_text(data=label_data, aes(x=id, y=tot+1, label=Fleet, hjust=hjust), color="black", fontface="bold", size=4, angle= label_data$angle, inherit.aes = FALSE ) +
        
        # Add base line information
        geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )   +
        geom_text(data=base_data, aes(x = title, y = -1, label=Country), hjust=c(0.5,1,0.5,0,0.5), 
                  vjust=c(0.5,0.5,0,0.5,1.5), colour = "black", alpha=0.8, size=4.5, fontface="bold", inherit.aes = FALSE)}
    else{
      t1 <- filter(effbymet,scenario == input$FCEfSbar1)
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
        ylim(-5,max(label_data$tot, na.rm=T)+20) +
        theme_minimal() +
        theme(
          legend.position = "left",
          legend.text = element_text(size=10),
          legend.margin=margin(0,-200,425,0),
          legend.box.margin = margin(10,10,10,10),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(c(-4,-6,-1,-1), "cm")
        ) +
        coord_polar() +
        # Add labels on top of each bar
        geom_text(data=label_data, aes(x=id, y=tot*10, label=Fleet, hjust=hjust), color="black", fontface="bold", size=4, angle= label_data$angle, inherit.aes = FALSE ) +
        
        # Add base line information
        geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )   +
        geom_text(data=base_data, aes(x = title, y = -1, label=Country), hjust=c(0.5,1,0.5,0,0.5), 
                  vjust=c(0.5,0.5,0,0.5,1.5), colour = "black", alpha=0.8, size=4.5, fontface="bold", inherit.aes = FALSE)}
    
  })
  
  output$FCEfSbarplot2<-renderPlot({
    if(input$FCEfSbarPlot==1){
      t1 <- filter(effbymet,scenario == input$FCEfSbar2)
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
        ylim(-10,max(label_data$tot, na.rm=T)+1) +
        theme_minimal() +
        theme(
          legend.position = "left",
          legend.text = element_text(size=10),
          legend.margin=margin(0,-200,300,0),
          #legend.box.margin = margin(10,10,10,10),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          #plot.margin =  margin(0,0,0,0)
          plot.margin = unit(c(-4,-6,0,-1) ,"cm") 
        ) +
        coord_polar() +
        # Add labels on top of each bar
        geom_text(data=label_data, aes(x=id, y=tot+1, label=Fleet, hjust=hjust), color="black", fontface="bold", size=4, angle= label_data$angle, inherit.aes = FALSE ) +
        
        # Add base line information
        geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )   +
        geom_text(data=base_data, aes(x = title, y = -1, label=Country), hjust=c(0.5,1,0.5,0,0.5), 
                  vjust=c(0.5,0.5,0,0.5,1.5), colour = "black", alpha=0.8, size=4.5, fontface="bold", inherit.aes = FALSE)}
    else{
      t1 <- filter(effbymet,scenario == input$FCEfSbar2)
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
        ylim(-5,max(label_data$tot, na.rm=T)+20) +
        theme_minimal() +
        theme(
          legend.position = "left",
          legend.text = element_text(size=10),
          legend.margin=margin(0,-200,425,0),
          legend.box.margin = margin(10,10,10,10),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(c(-4,-6,-1,-1), "cm")
        ) +
        coord_polar() +
        # Add labels on top of each bar
        geom_text(data=label_data, aes(x=id, y=tot*10, label=Fleet, hjust=hjust), color="black", fontface="bold", size=4, angle= label_data$angle, inherit.aes = FALSE ) +
        
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
  
  #t3 <- filter(FCubepage3,stock==input$FCfilterpage31,value == "landings")
  
  output$FCubeRadarplot1 <-
    
    renderPlotly({
      t3 <- filter(FCubepage3,stock==input$FCfilterpage31,value == "landings")
      plot_ly(
        type = "scatterpolar",
         mode = 'lines',
         fill = "toself"
        #fill="tozeroy"
      ) %>%
        add_trace(
          r = c(1, 1, 1, 1, 1, 1, 1),
          theta = c("max", "min","cod-cs","had-cs","whg-cs","sq_E","val"),
          # fill="tozeroy",
          fill = "toself",
          name = "Ratio=1",
          lines = list(color = "red"),
          marker = list(size = 14, color = "red",symbol=4)) %>%
        add_trace(r= t3$RelativeToSS,
                  theta = c("max", "min","cod-cs","had-cs","whg-cs","sq_E","val")
                  ,
                  fill = "toself",
                  name=paste(input$FCfilterpage31),marker = list(size = 11))%>%layout(showlegend=T) 
     
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
      div(
        style = "border-radius: 25px,width:120px;color:orange",
        selectInput("FCfilterpage31",
                    label = "Select Stock", choices = levels(FCubepage3$stock),
                    selectize = T
        )),
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
  
 # t4 <- filter(FCubepage3Radar, value == "Fbar")
  
  output$FCubeRadarplot2 <-
    renderPlotly({
      t3 <- filter(FCubepage3,stock==input$FCfilterpage32,value == "Fbar")
      plot_ly(
        type = "scatterpolar",
        mode = 'lines',
        fill = "toself"
        #fill="tozeroy"
      ) %>%
        add_trace(
          r = c(1, 1, 1, 1, 1, 1, 1),
          theta = c("max", "min","cod-cs","had-cs","whg-cs","sq_E","val"),
          # fill="tozeroy",
          fill = "toself",
          name = "Ratio=1",
          lines = list(color = "red"),
          marker = list(size = 14, color = "red",symbol=4)) %>%
        add_trace(r= t3$RelativeToSS,
                  theta = c("max", "min","cod-cs","had-cs","whg-cs","sq_E","val")
                  ,
                  fill = "toself",
                  name=paste(input$FCfilterpage32),marker = list(size = 11))%>%layout(showlegend=T) 
      
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
      div(
        style = "border-radius: 25px,width:120px;color:orange",
        selectInput("FCfilterpage32",
                    label = "Select Stock", choices = levels(FCubepage3$stock),
                    selectize = T
        )),
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
        
        t3 <- filter(FCubepage3,stock==input$FCfilterpage33,value == "ssb"& year == 2019)
        plot_ly(
          type = "scatterpolar",
          mode = 'lines',
          fill = "toself"
          #fill="tozeroy"
        ) %>%
          add_trace(
            r = c(1, 1, 1, 1, 1, 1, 1),
            theta = c("max", "min","cod-cs","had-cs","whg-cs","sq_E","val"),
            # fill="tozeroy",
            fill = "toself",
            name = "Ratio=1",
            lines = list(color = "red"),
            marker = list(size = 14, color = "red",symbol=4)) %>%
          add_trace(r= t3$RelativeToSS,
                    theta = c("max", "min","cod-cs","had-cs","whg-cs","sq_E","val")
                    ,
                    fill = "toself",
                    name=paste(input$FCfilterpage33),marker = list(size = 11))%>%layout(showlegend=T) 
        
      }
      else if (input$SSEyear == 2020) {
        
        t3 <- filter(FCubepage3,stock==input$FCfilterpage33,value == "ssb"& year == 2020)
        plot_ly(
          type = "scatterpolar",
          mode = 'lines',
          fill = "toself"
          #fill="tozeroy"
        ) %>%
          add_trace(
            r = c(1, 1, 1, 1, 1, 1, 1),
            theta = c("max", "min","cod-cs","had-cs","whg-cs","sq_E","val"),
            # fill="tozeroy",
            fill = "toself",
            name = "Ratio=1",
            lines = list(color = "red"),
            marker = list(size = 14, color = "red",symbol=4)) %>%
          add_trace(r= t3$RelativeToSS,
                    theta = c("max", "min","cod-cs","had-cs","whg-cs","sq_E","val")
                    ,
                    fill = "toself",
                    name=paste(input$FCfilterpage33),marker = list(size = 11))%>%layout(showlegend=T) 
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
      div(
        style = "border-radius: 25px,width:120px;color:orange",
        selectInput("FCfilterpage33",
                    label = "Select Stock", choices = levels(FCubepage3$stock),
                    selectize = T
        )),
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
  theme = shinytheme("spacelab"), #spacelab
  titlePanel("Mixed Fisheries"),
  navlistPanel(id="mainpanel", widths=c(2,10), 
   
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
                            ), conditionalPanel(
                              condition = "input.EfFilter == 2",
                              column(width = 12, h5("Effort by fleet and metier  for 2017(baseline) and estimates for various scenarios in 2019.", style = "margin-bottom=125px;text-align:center;
                                                    font-weight:bold;color:orange;text-decoration: underline;"),br(),
                                     fluidRow(column(4,offset=4,div(
                                       style = "border-radius: 25px,width:120px;color:orange",
                                       selectInput("FCEfSbarPlot", label = "Select Scale", choices = c("Log Scale"=1,"Percentages"=2), selectize = T)
                                     ) )),
                                     fluidRow(column(6, div(
                                       style = "border-radius: 25px,width:120px;color:orange",
                                       selectInput("FCEfSbar1", label = "Select Scenario", levels(effbymet$scenario), selectize = T))
                                       ,plotOutput("FCEfSbarplot1",height = 750)),
                                       column(6, div(
                                         style = "border-radius: 25px,width:120px;color:orange",
                                         selectInput("FCEfSbar2", label = "Select Scenario", levels(effbymet$scenario), selectize = T))
                                         ,plotOutput("FCEfSbarplot2",height=750))),
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
