library(shiny)
library(shinythemes)
library(shinyalert)
library(ggplot2)
library(RColorBrewer)
library(ggiraph)
library(ggthemes)
library(plotly)
library(shinycssloaders)
options(scipen=999)

CelticEcoSpecies<-read.csv("data/CelticEcoSpecies.csv")
test<-aggregate(CelticEcoSpecies$OfficialLanW,by=list(CelticEcoSpecies$Year,CelticEcoSpecies$FishActEUivl5,CelticEcoSpecies$Species),FUN="sum")
names(test)<-c("Year","Metier_lvl5","Species","OfficialLanW")

CelticCE<-read.csv("data/CelticCE.csv")
testE<-aggregate(CelticCE$Kw.days,by=list(CelticCE$Year,CelticCE$FishActEUivl5,CelticCE$VesselLen),FUN="sum")
names(testE)<-c("Year","Metier_lvl5","VesselLen","KWperDay")

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Mixed Fisheries"),
  navlistPanel(id="mainpanel", widths=c(2,10), 
               tabPanel(" Introduction", value = "mp", icon = icon("home"),
                        h3("Content goes here"),
                        useShinyalert(),  
                        tags$li(class = "dropdown",
                                actionButton("about", "About"),
                                style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px;")
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
               tabPanel(" Existing Tools", value = "et", icon = icon("wrench")),
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
      viridis::scale_fill_viridis(discrete = TRUE)+
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
<<<<<<< HEAD
    }else if(input$set1==a1[8]){
=======
    }
    else if(input$set2==a1[8]){
      
>>>>>>> refs/remotes/origin/master
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
      xlab("Year") + ggtitle(a[i])+ scale_x_continuous(breaks=test$Year)+
      
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
<<<<<<< HEAD
    }else if(input$set1==a[9]){
      plotlist1[[9]]
    }else if(input$set1==a[10]){
      plotlist3[[10]]
    }else if(input$set3==a[11]){
=======
    }
    else if(input$set3==a[9]){
      plotlist1[[9]]
    }else if(input$set3==a[10]){
      
      plotlist3[[10]]}
    else if(input$set3==a[11]){
>>>>>>> refs/remotes/origin/master
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
