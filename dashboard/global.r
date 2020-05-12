

library(tidyverse)
library(leaflet)
library(shiny)
library(highcharter)
library(shinyWidgets)
library(shinyBS)
library(here)

# setwd(here::here("dashboard/"))

# packs <- c("tidyverse", "rio", "leaflet", "shiny", "highcharter", "shinyWidgets", "shinyBS")
# # install.packages(packs, dependencies = TRUE)
# lapply(packs, library, character.only = TRUE)

## Load and transform some data
GW_shp_file_new <- readRDS("Data/map_dat.rds") 
GW_shp_file_data <- data.frame(GW_shp_file_new@data, stringsAsFactors = FALSE) %>%
  na.omit(.)
#GW_shp_file_data <- GW_shp_file_new@data


country_characteristic_dat <- readRDS("Data/country_characteristic_dat.rds")
countryNamesText <- c("", sort(unique(as.character(country_characteristic_dat$country_name))))

rank_data_down <- readRDS("Data/rank_data_down.rds")
rank_data_up <- readRDS("Data/rank_data_up.rds")
prob1_dat <- readRDS("Data/prob1_dat.rds") 

table_dat <- prob1_dat %>% 
  select(-c(thres, index_name, popUp_text_up, popUp_text_down, colors, change_2019, down_rank, up_rank)) %>% 
  pivot_wider(names_from = direction, values_from = value) %>% 
  group_by(outcome) %>% 
  mutate(
         ORank = rank(Opening, ties.method = "max"),
         ORank = max(ORank) - ORank + 1,
         OCat = ntile(Opening, 5),
         OCat = factor(OCat, labels = c("Lowest", "Low", "Medium", "High", "Highest")),
         CRank = rank(Closing, ties.method = "max"),
         CRank = max(CRank) - CRank + 1,
         CCat = ntile(Closing, 5),
         CCat = factor(CCat, labels = c("Lowest", "Low", "Medium", "High", "Highest"))) %>%
  rename(Country = country_name, 
         Space = names) %>%
  ungroup() %>% 
  select(Country, Space, Opening, ORank, OCat, Closing, CRank, CCat, outcome, region) %>% 
  mutate(Region = case_when(region == 1 ~ "E. Europe and Central Asia",
                                 region == 2 ~ "Latin America and the Caribbean",
                                 region == 3 ~ "Middle East and N. Africa",
                                 region == 4 ~ "Sub-Saharan Africa",
                                 region == 5 ~ "W. Europe and N. America*",
                                 region == 6 ~ "Asia and Pacific"), 
         key_word = paste("Global", Space, Region, Country, sep = ",")) %>% 
  rename(`Opening Rank` = ORank,
         `Opening Cat` = OCat,
         `Closing Rank` = CRank, 
         `Closing Cat` = CCat) %>% 
  arrange(Country)
 
 data_table_format <- htmltools::withTags(table(
    class = 'display',
    thead(
    tr(
      th(colspan = 2, style = 'text-align: center; border: 0; color:#A51E36; font-size: 150%;', '2020-2021 Forecasts'),
      th(colspan = 3, style = 'text-align: center; color:#002649;', 'Opening'),
      th(colspan = 3, style = 'text-align: center; color:#002649;', 'Closing')
    ),
    tr(
      th(style = 'text-align: center; color:#002649;', 'Country'),
      th(style = 'text-align: center; color:#002649;', 'Space'),
      th(style = 'text-align: center; color:#002649;', 'Estimate'),
      th(style = 'text-align: center; color:#002649;', 'Ranking'),
      th(style = 'text-align: center; color:#002649;', 'Category'),
      th(style = 'text-align: center; color:#002649;', 'Estimate'),
      th(style = 'text-align: center; color:#002649;', 'Ranking'),
      th(style = 'text-align: center; color:#002649;', 'Category')
    )
    )
  ))

## Set colors
ts_colors <- RColorBrewer::brewer.pal(7, "Set1")
plotsFontSize <- "13px"
v2xcs_ccsi_color <- ts_colors[1]  #"#AA4643"
v2x_pubcorr_color <- ts_colors[2] #"#BC449E"
v2x_veracc_osp_color <- ts_colors[3] #"#4572A7"
v2x_horacc_osp_color <- ts_colors[4]#"#3D96AE"
v2xcl_rol_color <- ts_colors[5] #"#80699B"
v2x_freexp_altinf_color <- ts_colors[7] #"#89A54E"

#use rank data
topNriskFun <- function(dat, region, space, direction){
  canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category, Math.random()]);}")
  
  region_text <- case_when(region == 0 ~ "Global", 
    region == 1 ~ "E. Europe and Central Asia",
    region == 2 ~ "Latin America and the Caribbean",
    region == 3 ~ "Middle East and N. Africa",
    region == 4 ~ "Sub-Saharan Africa",
    region == 5 ~ "W. Europe and N. America*",
    region == 6 ~ "Asia and Pacific")
  
  space_text <- case_when(space == "v2xcs_ccsi" ~ "Associational",
    space == "v2x_pubcorr" ~ "Economic",
    space == "v2x_veracc_osp" ~ "Electoral",
    space == "v2x_horacc_osp" ~ "Governing",
    space == "v2xcl_rol" ~ "Individual",
    space == "v2x_freexp_altinf" ~ "Informational")
  
  direction_text <- case_when(direction == "up" ~ "Opening Event",
                              direction == "down" ~ "Closing Event")
  
  plot_title <- paste0("Top 20 ", direction_text, " Estimates for the ", space_text, " Space")
  plot_subtitle <- paste0(region_text, ", 2020-2021")
  
  if(direction == "up"){
    dat <- dat %>%
      filter(country_name %in% dat$country_name[seq(1,60,3)]) %>%
      arrange(up_rank, desc(direction)) %>% 
      mutate(direction = factor(direction, levels = c("Opening", "Neither", "Closing")))
    names_ <- c("Opening", "Stable", "Closing") 
    }
    else{
      dat <- dat %>%
        filter(country_name %in% dat$country_name[seq(1,60,3)]) %>%
        arrange(down_rank, direction) %>% 
      mutate(direction = factor(direction, levels = c("Closing", "Neither", "Opening")))
      names_ <- c("Closing", "Stable", "Opening") 
    } 
  
  dat %>%
    hchart(type = "bar", hcaes(x = country_name, 
                               y = value * 100, 
                               group = direction, 
                               color = colors), #, pointWidth = 9, pointPadding = 0.1, marginRight = 10
           name = names_)%>% #
    hc_plotOptions(bar = list(grouping = "true")) %>%
    #hc_tooltip(formatter = JS("function(){return false;}"))%>%
    hc_xAxis(title = "", 
           labels = list(style = list(color = "#002649",
                            fontSize = "9pt", 
                            fontWeight = "bold")))%>%
    hc_title(text = plot_title, 
             align = "left",
             style = list(color = "#002649",
               fontSize = "9pt",
               fontWeight = "bold")) %>% 
    hc_subtitle(text = plot_subtitle, 
             align = "left",
             style = list(color = "#002649",
                          fontSize = "9pt",
                          fontWeight = "bold")) %>% 
    hc_yAxis(min = 0, max = 100,
           title = list(text = "Estimated Probabilities (%)",
                         style = list(color = "#002649",
                            fontSize = "9pt", 
                            fontWeight = "bold")), 
           labels = list(style = list(color = "#002649",
                            fontSize = "9pt", 
                            fontWeight = "bold")),
           opposite = TRUE)%>%
    hc_tooltip(pointFormat = '{point.series.name}  {point.y:.0f}%') %>%
    hc_plotOptions(series = list(events = list(click = canvasClickFunction))) %>%
    hc_legend(enabled = F) %>% 
    hc_exporting(enabled = TRUE, 
                 buttons = list(contextButton = 
                            list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG", "downloadCSV"))))
}


#use prob1_dat
riskPlotFun <- function(dat){
  
  canvasClickFunction1 <- JS("function(event) {Shiny.onInputChange('canvasClicked1', [this.name, event.point.category, Math.random()]);}")
  country_name <- unique(dat$country_name)
  plot_title <- paste0("Estimates by space for ", country_name, ", 2020-2021")
  
  Plot1 <- dat %>% 
    arrange(names) %>% 
    mutate(direction = factor(direction, levels = c("Opening", "Neither", "Closing"))) %>%
    hchart(type = "bar", hcaes(x = names, 
                               y = 100*value, 
                               group = direction, 
                               color = colors), 
           name = c("Opening", "Stable", "Closing"), pointWidth = 11, pointPadding = 0.1, marginRight = 10)%>%
    # hc_plotOptions(bar = list(stacking = "normal")) %>%
    # hc_plotOptions(bar = list(grouping = "true")) %>%
    hc_xAxis(title = list(text = ""), 
             labels = list(style = list(color = "#002649",
                                        fontSize = "9pt", 
                                        fontWeight = "bold")),
            categories = c("Associational", "Economic", "Electoral", "Governing", "Individual", "Informational"))%>%
    hc_yAxis(min = 0, max = 100, title = list(text = "Estimated probabilities (%)",
                                              style = list(color = "#002649",
                                                           fontSize = "9pt", 
                                                           fontWeight = "bold")), 
             labels = list(style = list(color = "#002649",
                                        fontSize = "9pt", 
                                        fontWeight = "bold")))%>% 
    hc_title(text = plot_title, 
             align = "left",
             style = list(color = "#002649",
                          fontSize = "9pt",
                          fontWeight = "bold")) %>%
    hc_tooltip(pointFormat = '{point.series.name} {point.y:.0f}%') %>%
    hc_plotOptions(series = list(events = list(click = canvasClickFunction1))) %>%
    # hc_add_event_point(event = "click") %>% 
    hc_legend(enabled = F) %>% 
    hc_exporting(enabled = TRUE, 
                 buttons = list(contextButton = 
                                  list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG", "downloadCSV"))))
  }

timeSeriesPlotFun <- function(dat, to_plot, CIs = F){
  blank_dat <- data.frame(year = c(2010:2019), Value = NA)
  country_name <- unique(dat$country_name)
  plot_title <- paste0("V-Dem index scores for ", country_name, ", 2010-2019")
  
  PlotHC <- blank_dat%>%
    hchart(type = "line", hcaes(x = year, y = Value), name = "blank")%>%
          hc_yAxis(min = 0, max = 1,
             title = list(text = "",
                          style = list(color = "#002649",
                                       fontSize = "9pt", 
                                       fontWeight = "bold")), 
             labels = list(
               style = list(color = "#002649",
                            fontSize = "9pt", 
                            fontWeight = "bold")))%>%
          hc_xAxis(data = blank_dat$year, tickInterval = 1,
             title = list(text = "",
                          style = list(color = "#002649",
                                       fontSize = "9pt", 
                                       fontWeight = "bold")),
             labels = list(style = list(color = "#002649",
                                        fontSize = "9pt", 
                                        fontWeight = "bold"), rotation = "-45"))%>%
      hc_plotOptions(series = list(marker = list( enabled = FALSE, radius = 1.2, symbol = "circle"), states = list(hover = list (enabled = TRUE, radius = 3))))%>%
      hc_tooltip(shared = TRUE, crosshairs = TRUE) %>% 
    hc_title(text = plot_title,
             margin = 20, align = "center",
             style = list(color = "#002649",
                          fontSize = "9pt", 
                          fontWeight = "bold")) #%>% 
    # hc_exporting(enabled = TRUE, 
    #              buttons = list(contextButton = 
    #                               list(menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG", "downloadCSV"))))
  
  if("v2xcs_ccsi" %in% to_plot){
    PlotHC <- PlotHC%>%
      hc_add_series(data = dat, type = "line", hcaes(x = year, y = v2xcs_ccsi),
          name = "<b>Asociational Space </b><br> <span style='font-size: 85%'> Core Civil Society Index", color = v2xcs_ccsi_color, id = "p2")
      if(CIs){
      PlotHC <- PlotHC%>%
        hc_add_series(data = dat, type = "arearange", hcaes(x = year, low = v2xcs_ccsi_codelow, high = v2xcs_ccsi_codehigh),
          name = "Asociational CI", fillOpacity = 0.15, lineWidth = 0, color = v2xcs_ccsi_color, linkedTo = "p2") 
      }
  } 
  if(!("v2xcs_ccsi" %in% to_plot)){
    PlotHC <- PlotHC%>%
        hc_rm_series(name = c("<b>Asociational Space </b><br> <span style='font-size: 85%'> Core Civil Society Index", "Asociational CIs"))
  } 
  
  if("v2x_pubcorr" %in% to_plot){
    PlotHC <- PlotHC%>%
      hc_add_series(data = dat, type = "line", hcaes(x = year, y = v2x_pubcorr),
                    name = "<b>Economic Space </b><br> <span style='font-size: 85%'> Public Corruption Index", color = v2x_pubcorr_color, id = "p7")
    if(CIs){
      PlotHC <- PlotHC%>%
        hc_add_series(data = dat, type = "arearange", hcaes(x = year, low = v2x_pubcorr_codelow, high = v2x_pubcorr_codehigh),
                      name = "Economic CI", fillOpacity = 0.15, lineWidth = 0, color = v2x_pubcorr_color, linkedTo = "p7") 
    }
  } 
  if(!("v2x_pubcorr" %in% to_plot)){
    PlotHC <- PlotHC%>%
      hc_rm_series(name = c("<b>Economic Space </b><br> <span style='font-size: 85%'> Public Corruption Index", "Economic CIs"))
  } 
  
  if("v2x_veracc_osp" %in% to_plot){
    PlotHC <- PlotHC%>%
      hc_add_series(data = dat, type = "line", hcaes(x = year, y = v2x_veracc_osp),
                    name = "<b>Electoral Space </b><br> <span style='font-size: 85%'> Vertical Accountability Index", color = v2x_veracc_osp_color, id = "p8")
    if(CIs){
      PlotHC <- PlotHC%>%
        hc_add_series(data = dat, type = "arearange", hcaes(x = year, low = v2x_veracc_osp_codelow, high = v2x_veracc_osp_codehigh),
                      name = "Electoral CI", fillOpacity = 0.15, lineWidth = 0, color = v2x_veracc_osp_color, linkedTo = "p8") 
    }
  } 
  if(!("v2x_veracc_osp" %in% to_plot)){
    PlotHC <- PlotHC%>%
      hc_rm_series(name = c("<b>Electoral Space </b><br> <span style='font-size: 85%'> Vertical Accountability Index", "Electoral CIs"))
  } 
  
  if("v2x_horacc_osp" %in% to_plot){
    PlotHC <- PlotHC%>%
      hc_add_series(data = dat, type = "line", hcaes(x = year, y = v2x_horacc_osp),
                    name = "<b>Governing Space </b><br> <span style='font-size: 85%'> Horizontal Accountability Index", color = v2x_horacc_osp_color, id = "p9")
    if(CIs){
      PlotHC <- PlotHC%>%
        hc_add_series(data = dat, type = "arearange", hcaes(x = year, low = v2x_horacc_osp_codelow, high = v2x_horacc_osp_codehigh),
                      name = "Governing CI", fillOpacity = 0.15, lineWidth = 0, color = v2x_horacc_osp_color, linkedTo = "p9") 
    }
  } 
  if(!("v2x_horacc_osp" %in% to_plot)){
    PlotHC <- PlotHC%>%
      hc_rm_series(name = c("<b>Governing Space </b><br> <span style='font-size: 85%'> Horizontal Accountability Index", "Governing CIs"))
  } 
  
  if("v2xcl_rol" %in% to_plot){
    PlotHC <- PlotHC%>%
      hc_add_series(data = dat, type = "line", hcaes(x = year, y = v2xcl_rol),
          name = "<b>Individual Space </b><br> <span style='font-size: 85%'> Equality Before the Law &amp; Ind Liberty Index", color = v2xcl_rol_color, id = "p6")
      if(CIs){
      PlotHC <- PlotHC%>%
        hc_add_series(data = dat, type = "arearange", hcaes(x = year, low = v2xcl_rol_codelow, high = v2xcl_rol_codehigh),
          name = "Individual CI", fillOpacity = 0.15, lineWidth = 0, color = v2xcl_rol_color, linkedTo = "p6") 
      }
  } 
  if(!("v2xcl_rol" %in% to_plot)){
    PlotHC <- PlotHC%>%
        hc_rm_series(name = c("<b>Individual Space </b><br> <span style='font-size: 85%'> Equality Before the Law &amp; Ind Liberty Index", "Individual CIs"))
  } 
 
  if("v2x_freexp_altinf" %in% to_plot){
    PlotHC <- PlotHC%>%
      hc_add_series(data = dat, type = "line", hcaes(x = year, y = v2x_freexp_altinf),
          name = "<b>Informatonal Space </b><br> <span style='font-size: 85%'> Freedom of Expression &amp; Alt Info Index", color = v2x_freexp_altinf_color, id = "p4")
      if(CIs){
      PlotHC <- PlotHC%>%
        hc_add_series(data = dat, type = "arearange", hcaes(x = year, low = v2x_freexp_altinf_codelow, high = v2x_freexp_altinf_codehigh),
          name = "Informatonal CI", fillOpacity = 0.15, lineWidth = 0, color = v2x_freexp_altinf_color, linkedTo = "p4") 
      }
  } 
  if(!("v2x_freexp_altinf" %in% to_plot)){
    PlotHC <- PlotHC%>%
        hc_rm_series(name = c("<b>Informatonal Space </b><br> <span style='font-size: 85%'> Freedom of Expression &amp; Alt Info Index", "Informatonal CIs"))
  } 
  PlotHC
}


blankRiskPlotFun <- function(){
  blank_dat <- data.frame(outcomes = c("Associational", "Economic", "Electoral", "Governing", "Individual", "Informational"), prob_1 = 0)
  plot_title <- "Estimates by space for [select country], 2020-2021"
  blank_dat%>%
    hchart(type = "bar", hcaes(x = outcomes, y = prob_1), name = "Estimated probabilities (%)", pointPadding = -0.1)%>%
    hc_xAxis(title = list(text = "", style = list(color = "#002649",
                                                  fontSize = "9pt", 
                                                  fontWeight = "bold")), 
             labels = list(style = list(color = "#002649",
                                        fontSize = "9pt", 
                                        fontWeight = "bold")),
             categories = list("Associational", "Economic", "Electoral", "Governing", "Individual", "Informational"))%>%
    hc_yAxis(min = 0, max = 100, title = list(text = "Estimated probabilities (%)",
                                              style = list(color = "#002649",
                                                           fontSize = "9pt", 
                                                           fontWeight = "bold")), 
             labels = list(style = list(color = "#002649",
                                        fontSize = "9pt", 
                                        fontWeight = "bold"))) %>% 
    hc_title(text = plot_title, 
             align = "left",
             style = list(color = "#002649",
                          fontSize = "9pt",
                          fontWeight = "bold"))
}

blankTimeSeriesFun <- function(){
  blank_dat <- data.frame(year = c(2010:2019), Value = NA)
  plot_title <- "V-Dem index scores for [select country], 2010-2019"
  
  blank_dat%>%
    hchart(type = "line", hcaes(x = year, y = Value), name = "blank")%>%
    hc_yAxis(min = 0, max = 1,
             title = list(text = "",
                          style = list(color = "#002649",
                                       fontSize = "9pt", 
                                       fontWeight = "bold")), 
             labels = list(style = list(color = "#002649",
                                        fontSize = "9pt", 
                                        fontWeight = "bold")))%>%
    hc_xAxis(data = blank_dat$year, tickInterval = 1,
             title = list(text = "",
                          style = list(color = "#002649",
                                       fontSize = "9pt", 
                                       fontWeight = "bold")),
             labels = list(style = list(color = "#002649",
                                        fontSize = "9pt", 
                                        fontWeight = "bold" 
             ), rotation = "-45"))%>%
    hc_plotOptions(series = list(marker = list( enabled = FALSE, radius = 1.2, symbol = "circle"), states = list(hover = list (enabled = TRUE, radius = 3))))%>%
    hc_tooltip(shared = TRUE, crosshairs = TRUE) %>% 
    hc_title(text = plot_title,
             margin = 20, align = "center",
             style = list(color = "#002649",
                          fontSize = "9pt", 
                          fontWeight = "bold"))
}

