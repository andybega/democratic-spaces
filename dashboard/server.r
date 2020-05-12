
function(input, output, session) {  

# Builds the Space, index, and cutpoint text
  observeEvent(c(input$space), {
    space_var_name <- input$space    
    SpaceDescript_name <- switch(space_var_name, 
                                v2xcs_ccsi = "Associational Space",
                                v2x_pubcorr = "Economic Space",
                                v2x_veracc_osp = "Electoral Space",
                                v2x_horacc_osp = "Governing Space",
                                v2xcl_rol = "Individual Space",
                                v2x_freexp_altinf = "Informational Space")
    
    SpaceDescript_text <- switch(space_var_name, 
                                 v2xcs_ccsi = "The degree of CSO autonomy from the state and citizens' ability to freely and actively pursue their political and civic goals.",
                                 v2x_pubcorr = "The extent to which public sector employees grant favors in exchange for bribes (or other material inducements), and how often they steal, embezzle, or misappropriate public funds or other state resources for personal or family use.",
                                 v2x_veracc_osp = "The ability of the population to hold their government accountable through elections and political parties.",
                                 v2x_horacc_osp = "The degree to which the legislative and judicial branches can hold the executive branch accountable as well as legislative and judical oversight over the bureaucracy and security services.",
                                 v2xcl_rol = "The extent to which the laws are transparent and rigorously enforced and public administration impartial, and the extent to which citizens enjoy access to justice, secure property rights, freedom from forced labor, freedom of movement, physical integrity rights, and freedom of religion.",
                                 v2x_freexp_altinf = "The degree of media censorship, harassment of journalists, media bias, media self-censorship, whether the media is critical and pluralistic, as well as the freedom of discussion and academic and cultural expression.")
    
    SpaceDescript_index <- switch(space_var_name, 
                                 v2xcs_ccsi = "Core Civil Society (ranges from 0 to 1)",
                                 v2x_pubcorr = "Public Corruption (ranges from 0 to 1)",
                                 v2x_veracc_osp = "Vertical Accountability (ranges from 0 to 1)",
                                 v2x_horacc_osp = "Horizontal Accountability (ranges from 0 to 1)",
                                 v2xcl_rol = "Equality Before the Law and Individual Liberty (ranges from 0 to 1)",
                                 v2x_freexp_altinf = "Freedom of Expression and Alternative Sources of Information (ranges from 0 to 1)")
    
    SpaceDescript_thres <- switch(space_var_name, 
                                 v2xcs_ccsi = "Year-to-year change of +/- 0.05",
                                 v2x_pubcorr = "Year-to-year change of +/- 0.03",
                                 v2x_veracc_osp = "Year-to-year change of +/- 0.08",
                                 v2x_horacc_osp = "Year-to-year change of +/- 0.06",
                                 v2xcl_rol = "Year-to-year change of +/- 0.04",
                                 v2x_freexp_altinf = "Year-to-change year of +/- 0.05")
    
    output$SpaceDescript_vedm <- renderText({"V-Dem Index"})
    output$SpaceDescript_thres2 <- renderText({"Opening/Closing event threshold"})
    output$SpaceDescript_index <- renderText({as.character(SpaceDescript_index)})
    output$SpaceDescript_name <- renderText({as.character(SpaceDescript_name)})
    output$SpaceDescript_thres <- renderText({as.character(SpaceDescript_thres)})
    output$SpaceDescript_text <- renderText({as.character(SpaceDescript_text)})
  })
  
  ## Builds the top 20 regional barchart
  
  output$hcbarplot <-  renderHighchart({
    if(input$region > 0){
      if(input$direction == "Closing"){topNriskFun(prob1_dat %>% filter(outcome == input$space, region == input$region) %>% arrange(down_rank), space = input$space, region = as.numeric(input$region), direction = "down")}
      else{topNriskFun(prob1_dat %>% filter(outcome == input$space, region == input$region) %>% arrange(up_rank), region = as.numeric(input$region), space = input$space, direction = "up")}
    }
    else{
      if(input$direction == "Closing"){topNriskFun(prob1_dat %>% filter(outcome == input$space) %>% arrange(down_rank), region = as.numeric(input$region), space = input$space, direction = "down")}
      else{topNriskFun(prob1_dat %>% filter(outcome == input$space) %>% arrange(up_rank), region = as.numeric(input$region), space = input$space, direction = "up")}
    }
  })
  
  ## Builds initial map, links to barchart 
  output$map1 <- renderLeaflet({
    cols <- c("#FDEFE6", "#FAD0B4", "#F8B183", "#F59252", "#F37321", "#D0D0D1")
    color_data <- GW_shp_file_new@data$map_color_up_v2xcs_ccsi
    color_data <- ifelse(is.na(color_data), "#D0D0D1", color_data)
    leaflet(GW_shp_file_new, options = list(preferCanvas = TRUE, minZoom = 1, maxZoom = 4, zoomControl = FALSE)) %>% 
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
      setView(lng = 12, lat = 35, zoom = 1)%>%
      setMaxBounds(lng1 = -260, lat1 = -55.90223, lng2 = 360, lat2 = 83.11387)%>%
      addPolygons(weight = 0.5, smoothFactor = 0.5, color = "#444444",
                  layerId= ~country_name,
                  fillOpacity = 1,
                  fillColor = ~color_data,
                  fill = T,
                  highlightOptions = highlightOptions(color = "#28576B", weight = 1.5, bringToFront = TRUE)) %>%
      addLegend(position = "bottomleft", 
                title = "Estimate 2010-21", 
                layerId = "legend",
                opacity = 1,
                labels = c("< 5%", "< 15%", "< 25%", "< 35%", ">= 35%", "No Data"), 
                colors = cols) 
  })
  
  #update map based on change in space input
  observeEvent(c(input$space), {
    if(input$direction == "Opening"){
    color_var <- paste0("map_color_up_", input$space)
    show <- "Fill_up"
    hide <- "Fill_down"
    cols <- c("#BFDFED" ,"#8FC7E0", "#5FB0D3" ,"#2F99C6", "#0082BA", "#D0D0D1")}
    else{color_var <- paste0("map_color_down_", input$space)
    cols <- c("#FDEFE6", "#FAD0B4", "#F8B183", "#F59252", "#F37321", "#D0D0D1")
    show <- "Fill_down"
    hide <- "Fill_up"}
    color_data <- GW_shp_file_new@data[, color_var] 
    color_data <- ifelse(is.na(color_data), "#D0D0D1", color_data)
    leafletProxy("map1", data = GW_shp_file_new, session) %>%
      removeShape(layerId = "color_data") %>%
      removeControl("legend") %>%
      addPolygons(weight = 0.5, smoothFactor = 0.5, color = "#444444",
                  layerId= ~country_name,
                  fillOpacity = 1,
                  fillColor = ~color_data,
                  fill = T,
                  highlightOptions = highlightOptions(color = "#28576B", weight = 1.5, bringToFront = TRUE))%>%
      addLegend(position = "bottomleft", 
                title = "Estimate 2020-21", 
                layerId = "legend",
                opacity = 1,
                labels = c("< 5%", "< 15%", "< 25%", "< 35%", ">= 35%", "No Data"), 
                colors = cols)
  
  })
  
  #update map based on change in direction input
  observeEvent(c(input$direction), {
    if(input$direction == "Opening"){
      color_var <- paste0("map_color_up_", input$space)
      show <- "Fill_up"
      hide <- "Fill_down"
      cols <- c("#BFDFED" ,"#8FC7E0", "#5FB0D3" ,"#2F99C6", "#0082BA", "#D0D0D1")}
    else{color_var <- paste0("map_color_down_", input$space)
    cols <- c("#FDEFE6", "#FAD0B4", "#F8B183", "#F59252", "#F37321", "#D0D0D1")
    show <- "Fill_down"
    hide <- "Fill_up"}
    color_data <- GW_shp_file_new@data[, color_var] 
    color_data <- ifelse(is.na(color_data), "#D0D0D1", color_data)
    leafletProxy("map1", data = GW_shp_file_new, session) %>%
      removeShape(layerId = "color_data") %>%
      removeControl("legend") %>%
      addPolygons(weight = 0.5, smoothFactor = 0.5, color = "#444444",
                  layerId= ~country_name,
                  fillOpacity = 1,
                  fillColor = ~color_data,
                  fill = T,
                  highlightOptions = highlightOptions(color = "#28576B", weight = 1.5, bringToFront = TRUE))%>%
      addLegend(position = "bottomleft", 
                title = "Estimate 2020-21", 
                layerId = "legend",
                opacity = 1,
                labels = c("< 5%", "< 15%", "< 25%", "< 35%", ">= 35%", "No Data"), 
                colors = cols)
  })
  
  observeEvent(c(input$map1_shape_click), {
    updateSelectInput(session, "countrySelect", choices = countryNamesText, selected = "")
  })
  
  observeEvent(c(input$map1_shape_click), {
    country_name <- as.character(input$map1_shape_click$id)
    updateSelectInput(session, "countrySelect", choices = countryNamesText, selected = country_name)
  }) 
  
  observeEvent(c(input$canvasClicked), {
    updateSelectInput(session, "countrySelect", choices = countryNamesText, selected = "")
  })
  
  observeEvent(c(input$canvasClicked), {
    
    canvasClick <- NULL
    canvasClick <- as.numeric(input$canvasClicked[2]) + 1
    
    if(input$region > 0){
      if(input$direction == "Opening"){
        bar_plot_dat <- prob1_dat %>% filter(outcome == input$space, region == input$region, direction == input$direction) %>% arrange(up_rank)}
      else{bar_plot_dat <- prob1_dat %>% filter(outcome == input$space, region == input$region, direction == input$direction) %>% arrange(down_rank)}
      country_name <- as.character(bar_plot_dat$country_name[seq(1, 68, 1)[canvasClick]])}
    
    else{if(input$direction == "Opening"){
        bar_plot_dat <- prob1_dat %>% filter(outcome == input$space, direction == input$direction) %>% arrange(up_rank)}
      else{bar_plot_dat <- prob1_dat %>% filter(outcome == input$space, direction == input$direction) %>% arrange(down_rank)}
      country_name <- as.character(bar_plot_dat$country_name[seq(1, 68, 1)[canvasClick]])
      }
    updateSelectInput(session, "countrySelect", choices = countryNamesText, selected = country_name)
  })
  
  
  observeEvent(c(input$space), {
    updateCheckboxGroupInput(session, "checkGroup", inline = T,
      choiceNames = list(
        tags$span("Associational", style = paste("color:", v2xcs_ccsi_color, "; font-weight: bold; font-size: 80%;", sep = "")),
        tags$span("Economic", style = paste("color:", v2x_pubcorr_color, "; font-weight: bold; font-size: 80%;", sep = "")),
        tags$span("Electoral", style = paste("color:", v2x_veracc_osp_color, "; font-weight: bold; font-size: 80%;", sep = "")),
        tags$span("Governing", style = paste("color:", v2x_horacc_osp_color, "; font-weight: bold; font-size: 80%;", sep = "")),
        tags$span("Individual", style = paste("color:", v2xcl_rol_color, "; font-weight: bold; font-size: 80%;", sep = "")),
        tags$span("Informational", style = paste("color:", v2x_freexp_altinf_color, "; font-weight: bold; font-size: 80%;", sep = ""))),
      choiceValues = c(
        "v2xcs_ccsi",
        "v2x_pubcorr",
        "v2x_veracc_osp",
        "v2x_horacc_osp",
        "v2xcl_rol",
        "v2x_freexp_altinf"),
      selected = input$space)
  })
  
  #center the map based on regional selection
  observeEvent(c(input$region, input$direction, input$space), {
    if(input$region > 0 & input$region != 5){
      region_lat <- switch(input$region, 
                           `1` = 44.75, 
                           `2` = -14.84,
                           `3` = 29.59,
                           `4` = -6.57,
                           `6` = 13.71)
      region_long <- switch(input$region, 
                           `1` = 41.71, 
                           `2` = -75.43,
                           `3` = 20.42,
                           `4` = 23.25,
                           `6` = 100.42)
      leafletProxy("map1")%>%
        clearPopups()%>%
        setView(lng = region_long, lat = region_lat, zoom = 3) ## input$map1_zoom
    }
    
    if(input$region == 0){
      leafletProxy("map1")%>%
        clearPopups()%>%
        setView(lng = 12, lat = 35, zoom = 1)
    }
    if(input$region == 5){
      leafletProxy("map1")%>%
        clearPopups()%>%
        setView(lng = -40, lat = 35, zoom = 2)
    }
  })
  
  #update country-level risk and time series plots based on country selection
    observeEvent(c(input$countrySelect), {
    country_name <- input$countrySelect
    
    dat0 <- GW_shp_file_data[GW_shp_file_data$country_name == country_name, ]
    center_lon <- dat0%>%
          pull(center_lon)%>%
          as.matrix(.)
    center_lat <- dat0%>%
          pull(center_lat)%>%
          as.matrix(.)
    if(input$direction == "Closing"){
    popUp_text <- dat0 %>%
          pull(!!sym(paste0("popUp_text_down_", input$space)))
     }
     else{
       popUp_text <- dat0 %>%
         pull(!!sym(paste0("popUp_text_up_", input$space)))
     }
    leafletProxy("map1")%>%
    clearPopups()%>%
    addPopups(lng = center_lon, lat = center_lat, popup = popUp_text)%>%
    setView(lng = center_lon, lat = center_lat, zoom = 3)
    if(country_name != ""){
      dat1 <- prob1_dat[prob1_dat$country_name == country_name, ]
      output$riskPlot <-  renderHighchart({riskPlotFun(dat1)})
    }

    if(country_name == ""){
      output$riskPlot <-  renderHighchart({blankRiskPlotFun()})
      output$TimeSeriesPlot <-  renderHighchart({blankTimeSeriesFun()})
    }
  }) 
  observeEvent(c(input$countrySelect, input$checkGroup), {
    country_name <- input$countrySelect
    clickedSelected <- input$checkGroup
    #plotCIs <- input$plotCIs
    if(country_name != ""){
      dat_new <- country_characteristic_dat[country_characteristic_dat$country_name == country_name, ]
      output$TimeSeriesPlot <-  renderHighchart({timeSeriesPlotFun(dat_new %>% filter(year >= 2010, year <= 2019), clickedSelected)})
    }
  }) 
  
  #update all the text based on space input
  observeEvent(c(input$space), {
    space_var_name4 <- input$space
    SpaceDescript_name2 <- switch(space_var_name4,
                                  v2xcs_ccsi = "Associational Space",
                                  v2x_pubcorr = "Economic Space",
                                  v2x_veracc_osp = "Electoral Space",
                                  v2x_horacc_osp = "Governing Space",
                                  v2xcl_rol = "Individual Space",
                                  v2x_freexp_altinf = "Informational Space")
    
    SpaceDescript_text2 <- switch(space_var_name4,
                                  v2xcs_ccsi = "The degree of CSO autonomy from the state and citizens' ability to freely and actively pursue their political and civic goals.",
                                  v2x_pubcorr = "The extent to which public sector employees grant favors in exchange for bribes (or other material inducements), and how often they steal, embezzle, or misappropriate public funds or other state resources for personal or family use.",
                                  v2x_veracc_osp = "The ability of the population to hold their government accountable through elections and political parties.",
                                  v2x_horacc_osp = "The degree to which the legislative and judicial branches can hold the executive branch accountable as well as legislative and judical oversight over the bureaucracy and security services.",
                                  v2xcl_rol = "The extent to which the laws are transparent and rigorously enforced and public administration impartial, and the extent to which citizens enjoy access to justice, secure property rights, freedom from forced labor, freedom of movement, physical integrity rights, and freedom of religion.",
                                  v2x_freexp_altinf = "The degree of media censorship, harassment of journalists, media bias, media self-censorship, whether the media is critical and pluralistic, as well as the freedom of discussion and academic and cultural expression.")
    
    SpaceDescript_index2 <- switch(space_var_name4,
                                   v2xcs_ccsi = "Core Civil Society",
                                   v2x_pubcorr = "Public Corruption",
                                   v2x_veracc_osp = "Vertical Accountability",
                                   v2x_horacc_osp = "Horizontal Accountability",
                                   v2xcl_rol = "Equality Before the Law and Individual Liberty",
                                   v2x_freexp_altinf = "Freedom of Expression and Alternative Sources of Information")
    
    SpaceDescript_thres4 <- switch(space_var_name4,
                                   v2xcs_ccsi = "Year-to-year change of +/- 0.05",
                                   v2x_pubcorr = "Year-to-year change of +/- 0.03",
                                   v2x_veracc_osp = "Year-to-year change of +/- 0.08",
                                   v2x_horacc_osp = "Year-to-year change of +/- 0.06",
                                   v2xcl_rol = "Year-to-year change of +/- 0.04",
                                   v2x_freexp_altinf = "Year-to-change year of +/- 0.05")
    
    output$SpaceDescript_vedm2 <- renderText({"V-Dem Index"})
    output$SpaceDescript_thres3 <- renderText({"Opening/Closing event threshold"})
    output$SpaceDescript_index2 <- renderText({as.character(SpaceDescript_index2)})
    output$SpaceDescript_name2 <- renderText({as.character(SpaceDescript_name2)})
    output$SpaceDescript_thres4 <- renderText({as.character(SpaceDescript_thres4)})
    output$SpaceDescript_text2 <- renderText({as.character(SpaceDescript_text2)})
    })
    
    observeEvent(c(input$canvasClicked1), {
     click_name <- input$canvasClicked1[3]

        space_var_name4 <- switch(click_name,
                                  Associational = "v2xcs_ccsi",
                                  Economic = "v2x_pubcorr",
                                  Electoral = "v2x_veracc_osp",
                                  Governing = "v2x_horacc_osp",
                                  Individual = "v2xcl_rol",
                                  Informational = "v2x_freexp_altinf")
        
        checkGroup_selected <- input$checkGroup

      if(!(space_var_name4 %in% checkGroup_selected)){
        checkGroup_selected_update <- c(space_var_name4, checkGroup_selected)
      } else checkGroup_selected_update <- checkGroup_selected[!(checkGroup_selected %in% space_var_name4)]

        updateCheckboxGroupInput(session, "checkGroup", inline = T,
                                 choiceNames = list(
                                   tags$span("Associational", style = paste("color:", v2xcs_ccsi_color, "; font-weight: bold; font-size: 80%;", sep = "")),
                                   tags$span("Economic", style = paste("color:", v2x_pubcorr_color, "; font-weight: bold; font-size: 80%;", sep = "")),
                                   tags$span("Electoral", style = paste("color:", v2x_veracc_osp_color, "; font-weight: bold; font-size: 80%;", sep = "")),
                                   tags$span("Governing", style = paste("color:", v2x_horacc_osp_color, "; font-weight: bold; font-size: 80%;", sep = "")),
                                   tags$span("Individual", style = paste("color:", v2xcl_rol_color, "; font-weight: bold; font-size: 80%;", sep = "")),
                                   tags$span("Informational", style = paste("color:", v2x_freexp_altinf_color, "; font-weight: bold; font-size: 80%;", sep = ""))),
                                 choiceValues = c(
                                   "v2xcs_ccsi",
                                   "v2x_pubcorr",
                                   "v2x_veracc_osp",
                                   "v2x_horacc_osp",
                                   "v2xcl_rol",
                                   "v2x_freexp_altinf"),
                                 selected = checkGroup_selected_update)
        
        SpaceDescript_name2 <- switch(space_var_name4,
                                      v2xcs_ccsi = "Associational Space",
                                      v2x_pubcorr = "Economic Space",
                                      v2x_veracc_osp = "Electoral Space",
                                      v2x_horacc_osp = "Governing Space",
                                      v2xcl_rol = "Individual Space",
                                      v2x_freexp_altinf = "Informational Space")
        
        SpaceDescript_text2 <- switch(space_var_name4,
                                      v2xcs_ccsi = "The degree of CSO autonomy from the state and citizens' ability to freely and actively pursue their political and civic goals.",
                                      v2x_pubcorr = "The extent to which public sector employees grant favors in exchange for bribes (or other material inducements), and how often they steal, embezzle, or misappropriate public funds or other state resources for personal or family use.",
                                      v2x_veracc_osp = "The ability of the population to hold their government accountable through elections and political parties.",
                                      v2x_horacc_osp = "The degree to which the legislative and judicial branches can hold the executive branch accountable as well as legislative and judical oversight over the bureaucracy and security services.",
                                      v2xcl_rol = "The extent to which the laws are transparent and rigorously enforced and public administration impartial, and the extent to which citizens enjoy access to justice, secure property rights, freedom from forced labor, freedom of movement, physical integrity rights, and freedom of religion.",
                                      v2x_freexp_altinf = "The degree of media censorship, harassment of journalists, media bias, media self-censorship, whether the media is critical and pluralistic, as well as the freedom of discussion and academic and cultural expression.")
        
        SpaceDescript_index2 <- switch(space_var_name4,
                                       v2xcs_ccsi = "Core Civil Society",
                                       v2x_pubcorr = "Public Corruption",
                                       v2x_veracc_osp = "Vertical Accountability",
                                       v2x_horacc_osp = "Horizontal Accountability",
                                       v2xcl_rol = "Equality Before the Law and Individual Liberty",
                                       v2x_freexp_altinf = "Freedom of Expression and Alternative Sources of Information")
        
        SpaceDescript_thres4 <- switch(space_var_name4,
                                       v2xcs_ccsi = "Year-to-year change of +/- 0.05",
                                       v2x_pubcorr = "Year-to-year change of +/- 0.03",
                                       v2x_veracc_osp = "Year-to-year change of +/- 0.08",
                                       v2x_horacc_osp = "Year-to-year change of +/- 0.06",
                                       v2xcl_rol = "Year-to-year change of +/- 0.04",
                                       v2x_freexp_altinf = "Year-to-change year of +/- 0.05")
        
        output$SpaceDescript_vedm2 <- renderText({"V-Dem Index"})
        output$SpaceDescript_thres3 <- renderText({"Opening/Closing event threshold"})
        output$SpaceDescript_index2 <- renderText({as.character(SpaceDescript_index2)})
        output$SpaceDescript_name2 <- renderText({as.character(SpaceDescript_name2)})
        output$SpaceDescript_thres4 <- renderText({as.character(SpaceDescript_thres4)})
        output$SpaceDescript_text2 <- renderText({as.character(SpaceDescript_text2)})
      })
    

    table_data <- eventReactive(input$update, ignoreNULL = FALSE, { #, ignoreInit = TRUE
      
      space_pattern <- paste(input$space2, collapse = "|")
      region_pattern <- paste(input$region2, collapse = "|")
      suppressWarnings(dat <- table_dat %>%
        filter(str_detect(key_word, space_pattern)) %>%
        filter(str_detect(key_word, region_pattern)) %>%
        select(-c(outcome, region, Region, key_word)) %>% 
        arrange(Country, Space) %>% 
        DT::datatable(container = data_table_format, rownames = FALSE, 
                      options = list(sDom  = '<"top">lrt<"bottom">ip', 
                        language = list(search = 'Filter:'),
                        columnDefs = list(list(className = 'dt-center', targets = c(2:7)))
                      ),
                      class = "display"))%>%
        DT::formatStyle(columns = c(1:8), color = "#002649") %>% 
        DT::formatPercentage(columns = c(3, 6), digits = 0)
    })
    
    output$tableprint <- DT::renderDataTable({
      table_data()
      
    })
    
    download_data <- eventReactive(input$update, ignoreNULL = FALSE, { #, ignoreInit = TRUE
      
      space_pattern <- paste(input$space2, collapse = "|")
      region_pattern <- paste(input$region2, collapse = "|")
      suppressWarnings(dat <- table_dat %>%
        filter(str_detect(key_word, space_pattern)) %>%
        filter(str_detect(key_word, region_pattern)) %>%
        select(-c(outcome, region, Region, key_word)) %>% 
        arrange(Country, Space))
        
    })
    output$downloadData <- downloadHandler(
      filename = "DemSpaceForecasts-2020-2021.csv",
      content = function(file) {
        write.csv(download_data(), file, row.names = FALSE)
      }
    )
    # observeEvent(c(input$update), ignoreInit = TRUE, {#, ignoreNULL = T
    #   output$test <- renderPrint({input$update})
    #   output$test2 <- renderPrint({input$region2})
    # })
}