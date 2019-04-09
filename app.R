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
options(scipen=999)

data_fish <-  read.csv(file="data/Hackathon/Data.csv") 
CelticEcoSpecies<-read.csv("data/CelticEcoSpecies.csv")
test<-aggregate(CelticEcoSpecies$OfficialLanW,by=list(CelticEcoSpecies$Year,CelticEcoSpecies$FishActEUivl5,CelticEcoSpecies$Species),FUN="sum")
names(test)<-c("Year","Metier_lvl5","Species","OfficialLanW")

CelticCE<-read.csv("data/CelticCE.csv")
testE<-aggregate(CelticCE$Kw.days,by=list(CelticCE$Year,CelticCE$FishActEUivl5,CelticCE$VesselLen),FUN="sum")
names(testE)<-c("Year","Metier_lvl5","VesselLen","KW_Day")

ui <- fluidPage(

  theme = shinytheme("superhero"), #spacelab
  titlePanel("Mixed Fisheries"),
  navlistPanel(id="mainpanel", widths=c(2,10), 
               tabPanel(" Introduction", value = "mp", icon = icon("home"),
                        useShinyalert(),  
                        tags$li(class = "dropdown",
                                actionButton("about", "About"),
                                style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px;")),

               tabPanel(" Hackathon Work", value = "hw", icon = icon("folder-open"),
                        h3("Visualising the implications of catch decreases for fleets in a mixed fishery context"),
                        plotOutput("plot"),
                        absolutePanel(id="controls",top = 80, left = 700, width = 400, height = "auto", fixed=FALSE, draggable = TRUE,
                        sliderInput("whitingslider", "Choose % reduction in Whiting Catch:", min = -100, max =0, value = 0, 
                                                  step = NULL, sep = "", animate = FALSE, post  = " %"))),
               tabPanel(" Landings",value = "mi", icon = icon("fish"), 
                        tabsetPanel(id="Ltabselected", type="pills",
                                    tabPanel("Page1", value="page1", 
                                             fluidRow(
                                               column(width=5,offset = 4 ,h4( "The Proportion of Landings:")),
                                               actionButton("info1", icon("info-circle"), style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px;")),hr(),
                                            absolutePanel(id="set",top = 85, left = 450, width = 700, height = "auto", 
                                                             fixed=FALSE, draggable = TRUE,
                                                              selectInput("name", "Select Parameter:",
                                                              choices = c("Metier by Species"=1,"Species by Metier"=2))), br(),br(),
                                               
                                               fluidRow(
                                               column(width = 7,ggiraph::ggiraphOutput("plotL1")
                                                      %>% withSpinner(color="#0dc5c1")) ,
                                               column(width = 4,h5("Landings in kg by selection:"),
                                               tableOutput("tabplotL1")%>% withSpinner(color="#0dc5c1"))
                                               #column(width = 5, plotlyOutput("plotL1param",height = "400px")
                                                     # %>% withSpinner(color="#0dc5c1")))
                                             )),
                                    tabPanel("Page2", value="page2"  , 
                                             fluidRow(
                                               column(width=5,offset = 4 ,h4( "Total Landings in kilograms:")),actionButton("info2", icon("info-circle"), style = "padding-top: 7px;
                                                                   padding-bottom: 5px; padding-right: 20px;")),hr(),
                                             fluidRow(
                                               column(width=3,div(style="display: inline-block;vertical-align:top; width: 125px;",selectInput("set1", label = "Select Metier",levels(test$Metier_lvl5),selectize = T))
                                               ), column(width=3,div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("set2", label = "Select Year",c(2009:2017),selectize = T))
                                               ),column(width=3,div(style="display: inline-block;vertical-align:top; width: 125px;",selectInput("set3", label = "Select Species",levels(test$Species),selectize = T))
                                               ), column(width=3,div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("set4", label = "Select Year",c(2009:2017),selectize = T))
                                               )),
                                             fluidRow(
                                             column(width=6,uiOutput("LbySpec")),
                                             column(width=6,uiOutput("LbyMet")))),
                                             tabPanel("Page3", value="page3"  , 
tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter,
.dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,
.dataTables_wrapper .dataTables_paginate {color: #ffffff; }thead { color: #ffffff;}tbody {color: #000000; } "
                )),h3( "Landings Data:"),fluidRow(column(3,
                                                        selectInput("LYear", "Year:", c("All",unique(as.character(CelticEcoSpecies$Year))),
                                                                    multiple = F, selected = 'All')), column(3,
                                                        selectInput("LMetier","Metier:",c("All",unique(as.character(CelticEcoSpecies$FishActEUivl5))),
                                                                    multiple = F, selected = 'All')),column(3,
                                                        selectInput("LSpecies","Species:",c("All",unique(as.character(CelticEcoSpecies$Species))),
                                                                    multiple = F, selected = 'All')),column(3,
                                                        selectInput("LArea", "Area:",c("All",unique(as.character(CelticEcoSpecies$Area))),
                                                                    multiple = F, selected = 'All'))),
                                           # Create a new row for the table.
                                               fluidRow(
                                                 DT::dataTableOutput("tableL")
                                               )))),
               
               tabPanel(" Effort", value = "sb", icon = icon("ship"),
                        tabsetPanel(id="Etabselected", type="pills",
                                    tabPanel("Page1", value="page1", 
                                             fluidRow(
                                               column(width=5,offset = 4 ,h4( "The Proportion of Effort:")),
                                               actionButton("info3", icon("info-circle"), style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px;")),hr(),
                                             absolutePanel(id="setE",top = 85, left = 450, width = 700, height = "auto", 
                                                           fixed=FALSE, draggable = TRUE,
                                                           selectInput("nameE", "Select Parameter:",
                                                                       choices = c("Metier by Vessel length "=1,"Vessel length by Metier"=2))), br(),br(),
                                             
                                             fluidRow(
                                               column(width = 7,ggiraph::ggiraphOutput("plotE1")
                                                      %>% withSpinner(color="#0dc5c1")) ,
                                               column(width = 4,h5("Effort in KW_days by selection:"),
                                                      tableOutput("tabplotE1")%>% withSpinner(color="#0dc5c1"))
                                               #column(width = 5, plotlyOutput("plotL1param",height = "400px")
                                               # %>% withSpinner(color="#0dc5c1")))
                                             )),
                                    tabPanel("Page2", value="page2"  , 
                                             fluidRow(
                                               column(width=5,offset = 4 ,h4( "Total Effort KW_days:")),actionButton("info4", icon("info-circle"), style = "padding-top: 7px;
                                                                                                                     padding-bottom: 5px; padding-right: 20px;")),hr(),
                                             fluidRow(
                                               column(width=3,div(style="display: inline-block;vertical-align:top; width: 125px;",selectInput("setE1", label = "Select Metier",levels(testE$Metier_lvl5),selectize = T))
                                               ), column(width=3,div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("setE2", label = "Select Year",c(2009:2017),selectize = T))
                                               ),column(width=3,div(style="display: inline-block;vertical-align:top; width: 125px;",selectInput("setE3", label = "Select Vessel Length",levels(testE$VesselLen),selectize = T))
                                               ), column(width=3,div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("setE4", label = "Select Year",c(2009:2017),selectize = T))
                                               )),
                                             fluidRow(
                                               column(width=6,uiOutput("EbyLength")),
                                               column(width=6,uiOutput("EbyMet")))),
                                    tabPanel("Page3", value="page3"  , 
                                             tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter,
                                                             .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,
                                                             .dataTables_wrapper .dataTables_paginate {color: #ffffff; }thead { color: #ffffff;}tbody {color: #000000; } "
                                             )),h3( "Effort Data:"),fluidRow(column(3,
                                                                                    selectInput("EYear", "Year:", c("All",unique(as.character(CelticCE$Year))),
                                                                                                multiple = F, selected = 'All')), column(3,
                                                                                                                                         selectInput("EMetier","Metier:",c("All",unique(as.character(CelticCE$FishActEUivl5))),
                                                                                                                                                     multiple = F, selected = 'All')),column(3,
                                                                                                                                                                                             selectInput("EVessel","Vessel Length:",c("All",unique(as.character(CelticCE$VesselLen))),
                                                                                                                                                                                                         multiple = F, selected = 'All')),column(3,
                                                                                                                                                                                                                                                 selectInput("EArea", "Area:",c("All",unique(as.character(CelticCE$Area))),
                                                                                                                                                                                                                                                             multiple = F, selected = 'All'))),
                                             # Create a new row for the table.
                                             fluidRow(
                                               DT::dataTableOutput("tableE")
                                             )))),

               tabPanel(" Existing Tools", value = "et", icon = icon("wrench"),
                        tabsetPanel(id="Tpanelselected", type="pills",
                                    tabPanel("Raw accessions App", value="page1"),
                                    tabPanel("Effort App", value="page2"),
                                    tabPanel("Catchability App", value="page3"),
                                    tabPanel("Partial F App", value="page4"),
                                    tabPanel("Quota share App", value="page5")
                                    )),
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
    
  }, height= 670)
  ##################################### End of Hackathon ################################################ 
 
  
  
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
  
  ###########Information botton Landing Page 1 #########
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
  
  ###########Information botton Landing Page 2 #########
  
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
  
  ###########Information botton Effort Page 1 #########
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
  
  ###########Information botton Effort Page 2 #########
  
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
  
  
  
  ###########Landings##########################
  ###############Page1#########################
  
selected_page1 <- reactive({
    input$plotL1_selected
  })
  
output$plotL1 <-renderggiraph({
    if(input$name== 1){
    compplot <- ggplot(test, aes(Year,OfficialLanW, fill=Species)) + 
    geom_bar_interactive(stat = "identity", position = "fill",aes(tooltip = Species, data_id = Species)) +
    ggtitle("The proportion of each landed species by level 5 métier.")+
    ylab("The proportion of total Landings") +
    xlab("Year") + scale_x_continuous(breaks=test$Year) +
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
    x}
    else if(input$name==2){ compplot <- ggplot(test, aes(Year,OfficialLanW, fill=Metier_lvl5)) + 
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
    x}
  })
  
output$tabplotL1 <- renderTable({
    if(input$name== 1){
    out <- test[test$Species %in%selected_page1 (), ][-3]
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    colnames(out)<-c("Year","Metier","Landings in kg")
    out}
    else if(input$name== 2)
    { out <- test[test$Metier_lvl5%in%selected_page1 (), ][-2]
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    colnames(out)<-c("Year","Species","Landings in kg")
    out}
  })
 
 

  ###########Landings##########################
  ###############Page2#########################
selected_page21<- reactive({
    input$plotL21_selected
  })
  
testL2<-aggregate(CelticEcoSpecies$OfficialLanW,by=list(CelticEcoSpecies$Year,CelticEcoSpecies$FishActEUivl5,CelticEcoSpecies$Species,CelticEcoSpecies$Area),FUN="sum")
names(testL2)<-c("Year","Metier_lvl5","Species","Area","OfficialLanW")
  
test1<-reactive({filter(testL2,Year==input$set2 & Metier_lvl5==input$set1)})
  
 output$plotL21 <- renderggiraph({
     gg <- ggplot(test1(), aes(x = Species, y = OfficialLanW, fill = Species )) +
        geom_bar_interactive(stat = "identity",
        aes( data_id = test1()$Species, tooltip = test1()$Species)) +
        theme_grey(base_size = 16) + ylab("Total Landings in kg")+
        viridis::scale_fill_viridis(discrete = TRUE)+
        theme(legend.position="bottom", 
              legend.text=element_text(size=10),
              strip.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))
      x <- girafe(code = print(gg), width_svg = 6, height_svg = 8)
      x <- girafe_options(x, opts_selection(
        type = "multiple", css = "fill:#FF3333;stroke:black;"),
        opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
      x
  })
  
observeEvent(input$reset1, {
    session$sendCustomMessage(type = 'plotL21_set', message = character(0))
  })
  
output$tabplotL21 <- renderTable({
    out <- test1()[test1()$Species %in% selected_page21(), ][-c(1,2)]
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    colnames(out)<-c("Species","Area","Landings in kg")
    out
  })
  
output$LbySpec<-renderUI({ if(dim(test1())[1]==0){h3(paste("No data available for ", input$set1,"in",input$set2, sep=" "))} 
   else{list(column(width = 7,
    ggiraph::ggiraphOutput("plotL21")),
       column(width = 3,
       h4("Selected Species"),
       tableOutput("tabplotL21"),
       actionButton("reset1", label = "Reset selection")
       ))}})
  
selected_pageL22 <- reactive({
    input$plotL22_selected
  })

test2<-reactive({filter(testL2,Year==input$set4 & Species==input$set3)}) 
  
output$plotL22 <- renderggiraph({
  gg <- ggplot(test2(), aes(x = Metier_lvl5, y = OfficialLanW, fill = Metier_lvl5)) +
  geom_bar_interactive(stat = "identity",
  aes( data_id = test2()$Metier_lvl5, tooltip = test2()$Metier_lvl5)) +
      theme_grey(base_size = 16) + ylab("Total Landings in kg")+
      viridis::scale_fill_viridis(discrete = TRUE)+
      theme(legend.position="bottom", 
            legend.text=element_text(size=10),
            strip.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))
    x <- girafe(code = print(gg), width_svg = 6, height_svg = 8)
    x <- girafe_options(x, opts_selection(
      type = "multiple", css = "fill:#FF3333;stroke:black;"),
      opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
    x
  })
  
observeEvent(input$reset2, {
    session$sendCustomMessage(type = 'plotL22_set', message = character(0))
  })
  
output$tabplotL22<- renderTable({
    out <- test2()[test2()$Metier_lvl5 %in% selected_pageL22(), ][-c(1,3)]
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    colnames(out)<-c("Metier","Area","Landings in kg")
    out})
  
output$LbyMet<-renderUI({ if(dim(test2())[1]==0){h3(paste("No data available for", input$set3,"in",input$set4, sep=" "))} 
    else{list(column(width = 7, ggiraph::ggiraphOutput("plotL22")
    ), column(width = 3,
              h4("Selected Metier"),
              tableOutput("tabplotL22"),
              actionButton("reset2", label = "Reset selection") ))}})
  
###########Landings##########################
###############Page3#########################

  # Filter data based on selections

  output$tableL <- DT::renderDataTable(DT::datatable({
    L <- CelticEcoSpecies[-c(1,2,4,9,10,12:15,20,21,23,24)]
    if (input$LYear != "All") {
    L <- filter(L, Year %in% input$LYear)
    }
    if (input$LMetier != "All") {
     L <- filter(L, FishActEUivl5 %in% input$LMetier)
    }
    if (input$LSpecies != "All") {
    L <- filter(L, Species %in% input$LSpecies)
    }
    if (input$LArea != "All") {
    L <- filter(L, Area %in% input$LArea)
    }
    L}))
  
  
  
##########Efforts page##############
###############Page1#################
selected_pageE1 <- reactive({
  input$plotE1_selected
})

output$plotE1 <-renderggiraph({
  if(input$nameE== 1){
    compplot <- ggplot(testE, aes(Year,KW_Day, fill=VesselLen)) + 
      geom_bar_interactive(stat = "identity", position = "fill",aes(tooltip = VesselLen, data_id = VesselLen)) +
      ggtitle("The proportion of Vessel type Effort by level 5 métier.")+
      ylab("The proportion of total Effort") +
      xlab("Year") + scale_x_continuous(breaks=testE$Year) +
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
    x}
  else if(input$nameE==2){ compplot <- ggplot(testE, aes(Year,KW_Day, fill=Metier_lvl5)) + 
    #geom_bar(stat = "identity", aes(fill=fillwhite)) 
    geom_bar_interactive(stat = "identity", position = "fill",aes(tooltip = Metier_lvl5, data_id = Metier_lvl5)) +
    ylab("The proportion of total Effort") +
    xlab("Year") + scale_x_continuous(breaks=testE$Year) +
    ggtitle("The proportion of each level 5 métier  Effort by Vessel Length") +
    facet_wrap(~ VesselLen) +
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
  x}
})

output$tabplotE1 <- renderTable({
  if(input$nameE== 1){
    out <- testE[testE$VesselLen %in%selected_pageE1 (), ][-3]
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    colnames(out)<-c("Year","Metier","Effort in KW_day")
    out}
  else if(input$nameE== 2)
  { out <- testE[testE$Metier_lvl5%in%selected_pageE1 (), ][-2]
  if( nrow(out) < 1 ) return(NULL)
  row.names(out) <- NULL
  colnames(out)<-c("Year","Vessel Length","Effort in KW_day")
  out}
})



##########Efforts page##############
###############Page2#################

selected_pageE21<- reactive({
  input$plotE21_selected
})

testE1<-aggregate(CelticCE$Kw.days,by=list(CelticCE$Year,CelticCE$FishActEUivl5,CelticCE$VesselLen,CelticCE$Area),FUN="sum")
                  names(testE1)<-c("Year","Metier_lvl5","VesselLen","Area","KW_days")
                  
                  testE11<-reactive({filter(testE1,Year==input$setE2 & Metier_lvl5==input$setE1)})
                  
                  output$plotE21 <- renderggiraph({
                    gg <- ggplot(testE11(), aes(x = VesselLen, y = KW_days, fill = VesselLen )) +
                      geom_bar_interactive(stat = "identity",
                                           aes( data_id = testE11()$VesselLen, tooltip = testE11()$VesselLen)) +
                      theme_grey(base_size = 16) + ylab("Total Effort in KW_day")+
                      viridis::scale_fill_viridis(discrete = TRUE)+
                      theme(legend.position="bottom", 
                            legend.text=element_text(size=10),
                            strip.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))
                    x <- girafe(code = print(gg), width_svg = 6, height_svg = 8)
                    x <- girafe_options(x, opts_selection(
                      type = "multiple", css = "fill:#FF3333;stroke:black;"),
                      opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
                    x
                  })
                  
                  observeEvent(input$resetE1, {
                    session$sendCustomMessage(type = 'plotE21_set', message = character(0))
                  })
                  
                  output$tabplotE21 <- renderTable({
                    out <- testE11()[testE11()$VesselLen %in% selected_pageE21(), ][-c(1,2)]
                    if( nrow(out) < 1 ) return(NULL)
                    row.names(out) <- NULL
                    out
                  })
                  
                  output$EbyLength<-renderUI({ if(dim(testE11())[1]==0){h3(paste("No data available for ", input$setE1,"in",input$setE2, sep=" "))} 
                    else{list(column(width = 7,
                                     ggiraph::ggiraphOutput("plotE21")),
                              column(width = 3,
                                     h4("Selected Vessel length"),
                                     tableOutput("tabplotE21"),
                                     actionButton("resetE1", label = "Reset selection")
                              ))}})
                  
                  selected_pageE22 <- reactive({
                    input$plotE22_selected
                  })
                  
                  testE12<-reactive({filter(testE1,Year==input$setE4 & VesselLen ==input$setE3)}) 
                  
                  output$plotE22 <- renderggiraph({
                    gg <- ggplot(testE12(), aes(x = Metier_lvl5, y = KW_days, fill = Metier_lvl5)) +
                      geom_bar_interactive(stat = "identity",
                                           aes( data_id = testE12()$Metier_lvl5, tooltip = testE12()$Metier_lvl5)) +
                      theme_grey(base_size = 16) + ylab("Total Effort in KW_day")+
                      viridis::scale_fill_viridis(discrete = TRUE)+
                      theme(legend.position="bottom", 
                            legend.text=element_text(size=10),
                            strip.background = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1))
                    x <- girafe(code = print(gg), width_svg = 6, height_svg = 8)
                    x <- girafe_options(x, opts_selection(
                      type = "multiple", css = "fill:#FF3333;stroke:black;"),
                      opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"))
                    x
                  })
                  
                  observeEvent(input$resetE2, {
                    session$sendCustomMessage(type = 'plotE22_set', message = character(0))
                  })
                  
                  output$ tabplotE22<- renderTable({
                    out <- testE12()[testE12()$Metier_lvl5 %in% selected_pageE22(), ][-c(1,3)]
                    if( nrow(out) < 1 ) return(NULL)
                    row.names(out) <- NULL
                    out
                  })
                  
                  
                  
                  output$EbyMet<-renderUI({ if(dim(testE12())[1]==0){h3(paste("No data available for", input$setE3,"in",input$setE4, sep=" "))} 
                    else{list(column(width = 7,
                                     ggiraph::ggiraphOutput("plotE22")
                    ), column(width = 3,
                              h4("Selected Metier"),
                              tableOutput("tabplotE22"),
                              actionButton("resetE2", label = "Reset selection")
                    ))}})



###########Effort##########################
###############Page3#########################

# Filter data based on selections

output$tableE <- DT::renderDataTable(DT::datatable({
  E <-CelticCE[-c(1,2,3,8:10,16,19)]
  if (input$EYear != "All") {
    E <- filter(E, Year %in% input$EYear)
  }
  if (input$EMetier != "All") {
    E <- filter(E, FishActEUivl5 %in% input$EMetier)
  }
  if (input$EVessel != "All") {
    E <- filter(E, VesselLen %in% input$EVessel)
  }
  if (input$EArea != "All") {
    E <- filter(E, Area %in% input$EArea)
  }
  E}))

}

shinyApp(ui, server)
