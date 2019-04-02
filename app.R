library(shiny)
library(shinythemes)
library(shinyalert)
library(ggplot2)
library(RColorBrewer)
library(ggiraph)
library(ggthemes)
library(plotly)
library(shinycssloaders)
library(tidyverse)
library(viridis)
options(scipen=999)

data_fish <-  read.csv(file="Hackathon/Data.csv") 
CelticEcoSpecies<-read.csv("data/CelticEcoSpecies.csv")
test<-aggregate(CelticEcoSpecies$OfficialLanW,by=list(CelticEcoSpecies$Year,CelticEcoSpecies$FishActEUivl5,CelticEcoSpecies$Species),FUN="sum")
names(test)<-c("Year","Metier_lvl5","Species","OfficialLanW")

CelticCE<-read.csv("data/CelticCE.csv")
testE<-aggregate(CelticCE$Kw.days,by=list(CelticCE$Year,CelticCE$FishActEUivl5,CelticCE$VesselLen),FUN="sum")
names(testE)<-c("Year","Metier_lvl5","VesselLen","KWperDay")

ui <- fluidPage(
  theme = shinytheme("spacelab"), #superhero
  titlePanel("Mixed Fisheries"),
  navlistPanel(id="mainpanel", widths=c(2,10), 
               tabPanel(" Introduction", value = "mp", icon = icon("home")),
               tabPanel(" Hackathon Work", value = "hw", icon = icon("folder-open"),
                        h3("Visualising the implications of catch decreases for fleets in a mixed fishery context"),
                        plotOutput("plot"),
                        sliderInput("whitingslider", "Choose % reduction in Whiting Catch:", min = -100, max =0, value = 0, 
                                                  step = NULL, sep = "", animate = FALSE, post  = " %")
               ),
               tabPanel(" Landings",value = "mi", icon = icon("fish"), 
                        tabsetPanel(id="Ltabselected", type="pills",
                                    tabPanel("Page1", value="page1",  
                                             fluidRow(
                                               column(width=12,offset=7,div(
                                                 style="display: inline-block;vertical-align:top; width: 200px;",
                                                 selectInput("set1", label = "Select Metier",
                                                             c("None",levels(test$Metier_lvl5)),selectize = T))
                                                 )),
                                             fluidRow(
                                               column(width = 7,ggiraph::ggiraphOutput("plotL1")
                                                      %>% withSpinner(color="#0dc5c1")),
                                               column(width = 5, plotlyOutput("plotL1param",height = "400px")
                                                      %>% withSpinner(color="#0dc5c1")))
                                             ),
                                    tabPanel("Page2", value="page2"  ,  
                                             fluidRow(
                                               column(width=12,offset=7,div(
                                                 style="display: inline-block;vertical-align:top; width: 200px;",
                                                 selectInput("set2", label = "Select Species",
                                                             c("None",levels(test$Species)),selectize = T))
                                                 )),
                                             fluidRow(
                                               column(width = 7,ggiraph::ggiraphOutput("plotL2")
                                                      %>% withSpinner(color="#0dc5c1")),
                                               column(width = 5, plotlyOutput("plotL2param",height = "400px")
                                                      %>% withSpinner(color="#0dc5c1"))
                                             )))),
               
               tabPanel(" Effort", value = "sb", icon = icon("ship"),
                        fluidRow(
                          column(width=12,offset=7,div(
                            style="display: inline-block;vertical-align:top; width: 200px;",
                            selectInput("set3", label = "Select Metier",
                                        c("None",levels(testE$Metier_lvl5)),selectize = T))
                          )),
                        fluidRow(
                          column(width = 7,ggiraph::ggiraphOutput("plotE1")
                                 %>% withSpinner(color="#0dc5c1")),
                          column(width = 5, plotlyOutput("plotE1param",height = "400px")
                                 %>% withSpinner(color="#0dc5c1")))
               ),
               tabPanel(" Existing Tools", value = "et", icon = icon("wrench"),
                        useShinyalert(),  
                        tags$li(class = "dropdown",
                                actionButton("about", "About"),
                                style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px;")),
               tabPanel(" Schenarios", value ="sc", icon = icon("line-chart"))
  ),
  hr(),
  fluidRow(width =12,
           img(src="Logos/Niamh.png", width = "1250px", height = "100px", 
               style="display: block; margin-left: auto; margin-right: auto;margin-top:0em")
  )
)

############################################################################################

server <- function(input, output, session) {
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
    
  }, bg="transparent", height= 1000)
  ##################################### End of Hackathon ################################################ 
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
  
  ###########Landings plots ##########################
  ###############Page1####################################
  
  output$plotL1 <-renderggiraph({
    compplot <- ggplot(test, aes(Year,OfficialLanW, fill=Species)) + 
      geom_bar_interactive(stat = "identity", position = "fill",aes(tooltip = Species, data_id = Species)) +
      ylab("The proportion of total Landings") +
      xlab("Year") + scale_x_continuous(breaks=test$Year) +
      ggtitle("The proportion of the species landed by each unique level 5 métier") +
      facet_wrap(~ Metier_lvl5) +
      theme_grey(base_size = 16) + 
      viridis::scale_fill_viridis(discrete = TRUE) +
      theme(legend.position="bottom", 
            legend.text=element_text(size=10),
            strip.background = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
    x <- girafe(code = print(compplot), width_svg = 13, height_svg = 9)
    x <- girafe_options(x, opts_selection(
      type = "single", css = "fill:#FF3333;stroke:black;"),
      opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
    x
  })

  
  a<-levels(test$Metier_lvl5)
  plotlist1 = list()
  for(i in 1:length(a)){
    test1<-filter(test,Metier_lvl5==a[i])
    compplot <- ggplot(test1, aes(Year,OfficialLanW, fill=Species)) + 
      geom_bar(stat = "identity") + 
      #geom_bar_interactive(stat = "identity",aes(tooltip = Species)) +
      ylab("Total Landings") +
      xlab("Year") + ggtitle(a[i]) + 
      scale_x_continuous(breaks=test1$Year) +
      #geom_bar(stat = "identity") +
      #facet_wrap(~ FishActEUivl5) +
      theme_grey(base_size = 16) + 
      viridis::scale_fill_viridis(discrete = TRUE) +
      theme(legend.text=element_text(size=10),
            strip.background = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
    plotlist1[[i]]<-ggplotly(compplot)
  }
  
  output$plotL1param=renderPlotly({
    if(input$set1=="None"){
    }else if(input$set1==a[1]){
      plotlist1[[1]]
    }else if(input$set1==a[2]){
      plotlist1[[2]]
    }else if(input$set1==a[3]){
      plotlist1[[3]]
    }else if(input$set1==a[4]){
      plotlist1[[4]]
    }else if(input$set1==a[5]){
      plotlist1[[5]] 
    }else if(input$set1==a[6]){
      plotlist1[[6]]
    }else if(input$set1==a[7]){
      plotlist1[[7]]
    }else if(input$set1==a[8]){
      plotlist1[[8]]
    }else if(input$set1==a[9]){
      plotlist1[[9]]
    }else if(input$set1==a[10]){
      plotlist1[[10]]
    }else if(input$set1==a[11]){
      plotlist1[[11]]
    }else if(input$set1==a[12]){
      plotlist1[[12]]
    }else if(input$set1==a[13]){
      plotlist1[[13]]
    }else if(input$set1==a[14]){
      plotlist1[[14]]
    }else if(input$set1==a[15]){
      plotlist1[[15]]}
  })
  
  #########################Page 2#############################
  
  output$plotL2 <-renderggiraph({
    compplot <- ggplot(test, aes(Year,OfficialLanW, fill=Metier_lvl5)) + 
      #geom_bar(stat = "identity", aes(fill=fillwhite)) 
      geom_bar_interactive(stat = "identity", position = "fill",aes(tooltip = Metier_lvl5, data_id = Metier_lvl5)) +
      ylab("The proportion of total Landings") +
      xlab("Year") + scale_x_continuous(breaks=test$Year) +
      ggtitle("The proportion of each level 5 métier  landings by the species") +
      facet_wrap(~ Species) +
      theme_grey(base_size = 16) + 
      viridis::scale_fill_viridis(discrete = TRUE) +
      theme(legend.position="bottom", 
            legend.text=element_text(size=10),
            strip.background = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
    x <- girafe(code = print(compplot), width_svg = 13, height_svg = 9)
    x <- girafe_options(x, opts_selection(
      type = "single", css = "fill:#FF3333;stroke:black;"),
      opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
    x
  })
  
  a1<-levels(test$Species)
  plotlist2 = list()
  for(i in 1:length(a1)){
    test1<-filter(test,Species==a1[i])
    compplot <- ggplot(test1, aes(Year,OfficialLanW, fill=Metier_lvl5)) + 
      geom_bar(stat = "identity") + 
      #geom_bar_interactive(stat = "identity",aes(tooltip = Species)) +
      ylab("Total Landings") +
      xlab("Year") + ggtitle(a1[i]) + 
      scale_x_continuous(breaks=test1$Year) +
      #geom_bar(stat = "identity") +
      #facet_wrap(~ FishActEUivl5) +
      theme_grey(base_size = 16) + 
      viridis::scale_fill_viridis(discrete = TRUE) +
      theme(legend.text=element_text(size=10),
            strip.background = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
    plotlist2[[i]]<-ggplotly(compplot)
  }
  
  output$plotL2param=renderPlotly({
    if(input$set2=="None"){
    }else if(input$set2==a1[1]){
      plotlist2[[1]]
    }else if(input$set2==a1[2]){
      plotlist2[[2]]
    }else if(input$set2==a1[3]){
      plotlist2[[3]]
    }else if(input$set2==a1[4]){
      plotlist2[[4]]
    }else if(input$set2==a1[5]){
      plotlist2[[5]] 
    }else if(input$set2==a1[6]){
      plotlist1[[6]]
    }else if(input$set2==a1[7]){
      plotlist2[[7]]
    }else if(input$set1==a1[8]){
      plotlist2[[8]]
    }else if(input$set2==a1[9]){
      plotlist2[[9]]
    }else if(input$set2==a1[10]){
      plotlist2[[10]]
    }else if(input$set2==a1[11]){
      plotlist2[[11]]
    }else if(input$set2==a1[12]){
      plotlist2[[12]]}
    })
  
  ##########Efforts page##############
  ###############Page1#################

  output$plotE1 <-renderggiraph({
    compplot <- ggplot(testE, aes(Year,KWperDay, fill=VesselLen)) + 
      #geom_bar(stat = "identity", aes(fill=fillwhite)) 
      geom_bar_interactive(stat = "identity", position = "fill",
                           aes(tooltip = VesselLen, data_id = VesselLen)) +
      ylab("The proportion of KW per day") +
      xlab("Year") + scale_x_continuous(breaks=test$Year) +
      # ggtitle("The proportion of the species landed by each unique level 5 métier") +
      #geom_bar(stat = "identity") +
      facet_wrap(~ Metier_lvl5) +
      theme_grey(base_size = 16) + 
      viridis::scale_fill_viridis(discrete = TRUE) +
      theme(legend.position="bottom",
            legend.text=element_text(size=10),
            strip.background = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
    x <- girafe(code = print(compplot), width_svg = 13, height_svg = 9)
    x <- girafe_options(x, opts_selection(
      type = "single", css = "fill:#FF3333;stroke:black;"),
      opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
    x
  })
  
  a<-levels(testE$Metier_lvl5)
  plotlist3 = list()
  for(i in 1:length(a)){
    testE1<-filter(testE,Metier_lvl5==a[i])
    compplot <- ggplot(testE1, aes(Year,KWperDay, fill=VesselLen)) + 
      geom_bar(stat = "identity")+ 
      #geom_bar_interactive(stat = "identity",aes(tooltip = Species))+
      ylab("Total KW Per Day") +
      xlab("Year") + ggtitle(a[i])+ scale_x_continuous(breaks=test$Year) +
      #geom_bar(stat = "identity") +
      #facet_wrap(~ FishActEUivl5) +
      theme_grey(base_size = 16) + 
      viridis::scale_fill_viridis(discrete = TRUE) +
      theme(legend.position="NONE", 
            legend.text=element_text(size=10),
            strip.background = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
    plotlist3[[i]]<-ggplotly(compplot)
  }
  
  output$plotE1param=renderPlotly({
    if(input$set3=="None"){
    }else if(input$set3==a[1]){
      plotlist3[[1]]
    }else if(input$set3==a[2]){
      plotlist3[[2]]
    }else if(input$set3==a[3]){
      plotlist3[[3]]
    }else if(input$set3==a[4]){
      plotlist3[[4]]
    }else if(input$set3==a[5]){
      plotlist3[[5]] 
    }else if(input$set3==a[6]){
      plotlist3[[6]]
    }else if(input$set3==a[7]){
      plotlist3[[7]]
    }else if(input$set3==a[8]){
      plotlist3[[8]]
    }else if(input$set1==a[9]){
      plotlist1[[9]]
    }else if(input$set1==a[10]){
      plotlist3[[10]]
    }else if(input$set3==a[11]){
      plotlist3[[11]]
    }else if(input$set3==a[12]){
      plotlist3[[12]]
    }else if(input$set3==a[13]){
      plotlist3[[13]]
    }else if(input$set3==a[14]){
      plotlist3[[14]]
    }else if(input$set3==a[15]){
      plotlist3[[15]]}
    })
}

shinyApp(ui, server)
