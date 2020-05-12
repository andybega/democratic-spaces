
function(input, output, session) {
  output$map1 <- renderLeaflet({
    leaflet(GW_shp_file_new, options = list(minZoom = 1, maxZoom = 4))%>% #
      addPolygons(weight = 0.5, smoothFactor = 0.5, color = "#444444", 
                  layerId=~country_name,
                  opacity = 1.0,  
                  fillOpacity = 1,
                  fillColor = ~map_color_prob,
                  highlightOptions = highlightOptions(color = "#28576B", weight = 1.5, bringToFront = TRUE))%>%
      setView(lng = 12, lat = 35, zoom = 2)%>%
      setMaxBounds(lng1 = -260, lat1 = -55.90223, lng2 = 360, lat2 = 83.11387)%>%
      addLegend(position = "bottomleft", 
        title = "Estimated Risk 2019-20", 
        opacity = 1,
        labels = c("< 5%", "< 10%", "< 20%", "< 25%", ">= 25%", "Closed Autocracy"), 
        colors = c("#fef0d9","#fdcc8a","#fc8d59", "#e34a33", "#b30000", "#D0D0D1"))
  })
  # country_name <- NULL
  observeEvent(c(input$map1_shape_click), {
    country_name <- as.character(input$map1_shape_click$id)
    updateSelectInput(session, "countrySelect", choices = countryNamesText, selected = country_name)
  }) 
  observeEvent(c(input$canvasClicked), {
    country_name <- as.character(bar_plot_dat$country_name[seq(1, 34, 1)[as.numeric(input$canvasClicked[2]) + 1]])
    updateSelectInput(session, "countrySelect", choices = countryNamesText, selected = country_name)
  })
  observeEvent(c(input$countrySelect), {
    country_name <- input$countrySelect
    dat0 <- GW_shp_file_data[GW_shp_file_data$country_name == country_name, ]
    center_lon <- dat0%>%
          pull(center_lon)%>%
          as.matrix(.)
    center_lat <- dat0%>%
          pull(center_lat)%>%
          as.matrix(.)
    popUp_text <- dat0%>%
          pull(popUp_text)

    proxy <- leafletProxy("map1")%>%
    clearPopups()%>%
    addPopups(lng = center_lon, lat = center_lat, popup = popUp_text)%>%
    setView(lng = center_lon, lat = center_lat, zoom = input$map1_zoom)
    if(country_name != ""){
      output$dataTable <- renderTable({extended_row_dat[extended_row_dat$country_name == country_name, c("year", "Extended RoW Classification")]}, width = "auto")
      output$ROWclass <- renderText({paste("Current Extended RoW Classification: ", extended_row_dat[extended_row_dat$country_name == country_name & extended_row_dat$year == 2018, c("Extended RoW Classification")], sep ="")})
      dat1 <- prob1_dat[prob1_dat$country_name == country_name, ]
      output$riskPlot <-  renderHighchart({riskPlotFun(dat1)})
    }

    if(country_name == ""){
      output$riskPlot <-  renderHighchart({blankRiskPlotFun()})
      output$TimeSeriesPlot <-  renderHighchart({blankTimeSeriesFun()})
    }
  }) 
  observeEvent(c(input$countrySelect, input$checkGroup, input$plotCIs), {
    country_name <- input$countrySelect
    clickedSelected <- input$checkGroup
    plotCIs <- input$plotCIs
    if(country_name != ""){
      dat_new <- country_characteristic_dat[country_characteristic_dat$country_name == country_name, ]
      output$TimeSeriesPlot <-  renderHighchart({timeSeriesPlotFun(dat_new, clickedSelected, plotCIs)})
    }
  }) 
  output$hcbarplot <-  renderHighchart({topNriskFun(bar_plot_dat, N)})
}