



shinyServer(function(input, output, session) {

  ##################################################By Species######################################################################
  selected_species <- reactive({
    input$plot1_selected
  })
  
 test1<-reactive({filter(test,Year==input$set2 & Metier_lvl5==input$set1)})
  
 output$plot1 <- renderggiraph({
   if(dim(test1())[1]==0){}
   else if(input$set2=="Choose"&input$set1=="Choose")
   {}
   else if(input$set1=="Choose"){}else if(input$set2=="Choose"){}
   else{ 
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
    x}
  })
  
  observeEvent(input$reset1, {
    session$sendCustomMessage(type = 'plot1_set', message = character(0))
  })
  
  output$datatab1 <- renderTable({
    out <- test1()[test1()$Species %in% selected_species(), ][-c(1,2)]
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    out
  })
  ##################################################By Metier######################################################################
 
  
   selected_metier <- reactive({
    input$plot2_selected
  })
  
  
  test2<-reactive({filter(test,Year==input$set4 & Species==input$set3)}) 
  
  output$plot2 <- renderggiraph({
    
    if(dim(test2())[1]==0){}
    else if(input$set3=="Choose"&input$set4=="Choose")
    {}
    else if(input$set3=="Choose"){}else if(input$set4=="Choose"){}
   else{ gg <- ggplot(test2(), aes(x = Metier_lvl5, y = OfficialLanW, fill = Metier_lvl5)) +
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
    x}
  })
  
  observeEvent(input$reset2, {
    session$sendCustomMessage(type = 'plot2_set', message = character(0))
  })
  
  output$datatab2 <- renderTable({
    out <- test2()[test2()$Metier_lvl5 %in% selected_metier(), ][-c(1,3)]
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    out
  })
 
})