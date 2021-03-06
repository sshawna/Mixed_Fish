library(shiny)
library(tidyverse)
library(viridis)

#setwd("P:\\Hackathon\\Data prep")
data_fish <-  read.csv(file="Data_2.csv")
data_fish<-data_fish[-which(is.na(data_fish$Whiting)),]
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
    
    #data_fish$Whiting_changed <- data_fish$Whiting*(100+input$whitingslider)/100  
    data_fish$Whiting_changed  =c()
    for(i in 1:length(data_fish$Whiting_indicator)){
       if(data_fish$Whiting_indicator[i]<=0){
        data_fish$Whiting_changed[i]=0
      }else if(data_fish$Whiting_indicator[i]>0){
        data_fish$Whiting_changed[i]=data_fish$Whiting_indicator[i]
      }
    }

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
        plot.margin = unit(rep(-2,4), "cm") 
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