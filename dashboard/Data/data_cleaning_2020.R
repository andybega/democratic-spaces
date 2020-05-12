#
#   This script cleans/processes various data sets from create-data/ and
#   modelrunner/.
#
#   Laura Maxwell & Rick Morgan
#
#   Input:
#     - fcasts-rf-2020.csv: forecasts created in March 2020;
#       from modelrunnner/output/fcasts-rf.csv (file has just been renamed)
#     - dv_data_1968_on.csv: from create-data/scripts/0-split-raw-vdem.R
#
#   Output:
#     - map_dat.rds
#     - country_characteristic_dat.RDS
#     - prob1_dat.rds
#     - rank_data_up.rds
#     - rank_data_down.rds
#

library(tidyverse)
library(magrittr)
library(sf)
library(rmapshaper)
library(rgeos)
library(rgdal)
library(cshapes)
library(here)

setwd(here::here("dashboard/Data/"))

iri_dat <- read.csv("fcasts-rf-2020.csv", stringsAsFactors = F)
current_forecast <- iri_dat %>%
  filter(from_year == 2019)
outcomes <- unique(iri_dat$outcome)

dvs <- read_csv("dv_data_1968_on.csv") %>% 
  filter(year >= 2000)

current_dvs <- dvs %>%
  dplyr::select(gwcode, year, country_name, country_id, country_text_id, e_regionpol_6C,
                v2x_veracc_osp, v2xcs_ccsi, v2xcl_rol, v2x_freexp_altinf, v2x_horacc_osp, v2x_pubcorr) %>%
  tidyr::gather(outcome, level_2019, -gwcode, -year, -country_name, -country_id, -country_text_id, -e_regionpol_6C) %>%
  #dplyr::mutate(level_2019 = as.numeric(level_2019)) %>%
  dplyr::group_by(outcome, country_name) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(change_2019 = c(NA,diff(level_2019, lag = 1))) %>%
  dplyr::filter(year == 2019) %>%
  dplyr::arrange(country_name)

region_labels <- data.frame(names = c("E. Europe and Central Asia", "Latin America and the Caribbean", "Middle East and N. Africa", "Sub-Saharan Africa", "W. Europe and N. America*", "Asia and Pacific"),
                            level = 1:6)
space_labels <-  data.frame(names = c("Electoral", "Governing", "Individual", "Associational", "Informational", "Economic"),
                            outcome = c("v2x_veracc_osp", "v2x_horacc_osp", "v2xcl_rol", "v2xcs_ccsi", "v2x_freexp_altinf", "v2x_pubcorr"), stringsAsFactors = F)

thres_labels <-  data.frame(thres = c("+/-0.08", "+/-0.06", "+/-0.04", "+/-0.05", "+/-0.05", "+/-0.03"),
                            outcome = c("v2x_veracc_osp", "v2x_horacc_osp", "v2xcl_rol", "v2xcs_ccsi", "v2x_freexp_altinf", "v2x_pubcorr"), stringsAsFactors = F)
vdem_labels <- data.frame(index_name = c("V-Dem&apos;s Vertical Accountability Index", "V-Dem&apos;s Horizontal Accountability Index", "V-Dem&apos;s Equality Before the Law and Individual Liberty Index", "V-Dem&apos;s Core Civil Society Index", "V-Dem&apos;s Freedom of Expression and Alternative Sources of Information Index", "V-Dem&apos;s Public Corruption Index"),
                          outcome = c("v2x_veracc_osp", "v2x_horacc_osp", "v2xcl_rol", "v2xcs_ccsi", "v2x_freexp_altinf", "v2x_pubcorr"), stringsAsFactors = F)

all_forecast_data <- current_forecast %>%
  left_join(current_dvs, by = c("outcome", "from_year" = "year", "gwcode")) %>%
  dplyr::select(gwcode, country_name, outcome, year = from_year, for_years, region = e_regionpol_6C, level_2019, change_2019, p_up, p_down, p_same) %>%
  group_by(outcome) %>%
  arrange(desc(p_down)) %>%
  dplyr::mutate(down_rank = row_number()) %>%
  arrange(desc(p_up)) %>%
  dplyr::mutate(up_rank = row_number()) %>%
  left_join(space_labels) %>%
  left_join(thres_labels) %>%
  left_join(vdem_labels)

all_forecast_data$change_2019 <- ifelse(all_forecast_data$change_2019 > 0, paste0("+", round(all_forecast_data$change_2019,3)), round(all_forecast_data$change_2019,3))

#add colors

colfunc1 <- colorRampPalette(c("#E2F1F7", "#0082BA"))
colfunc2 <- colorRampPalette(c("#FDEFE6", "#F37321"))
colors_down  = c(colfunc2(5), "#D0D0D1")
colors_up  = c(colfunc1(5), "#D0D0D1")

all_forecast_data$map_color_up <- ifelse(all_forecast_data$p_up < 0.05, colors_up[1],
                                         ifelse(all_forecast_data$p_up < 0.15, colors_up[2],
                                                ifelse(all_forecast_data$p_up < 0.25, colors_up[3],
                                                       ifelse(all_forecast_data$p_up < 0.35, colors_up[4],
                                                              ifelse(!is.na(all_forecast_data$p_up), colors_up[5], colors_up[6])))))


all_forecast_data$map_color_down <- ifelse(all_forecast_data$p_down < 0.05, colors_down[1],
                                           ifelse(all_forecast_data$p_down < 0.15, colors_down[2],
                                                  ifelse(all_forecast_data$p_down < 0.25, colors_down[3],
                                                         ifelse(all_forecast_data$p_down < 0.35, colors_down[4],
                                                                ifelse(!is.na(all_forecast_data$p_down), colors_down[5], colors_down[6])))))

all_forecast_data$popUp_text_up <- paste('<h3><b>', all_forecast_data$country_name,'</b></h3>',
                                         '<h5><span style="color:#002649">Event probabilities for the <b>', all_forecast_data$names, ' Space</b> <span style="font-size: 80%">(', all_forecast_data$thres, ' change in <b>', all_forecast_data$index_name, '</b>)</span></span></h5>',
                                         paste('<b><span style="color:#0082BA">Opening Event: ',floor(all_forecast_data$p_up * 100), '%</b></span><br>', sep = ''),
                                         paste('<b><span style="color:#777778">Stable: ',floor(all_forecast_data$p_same * 100), '%</b></span><br>', sep = ''),
                                         paste('<b><span style="color:#F37321">Closing Event: ',floor(all_forecast_data$p_down * 100), '%</b></span><br><br>', sep = ''),
                                         paste('<b><span style="color:#0082BA"> Opening </span><span style="color:#002649">Risk Ranking: ', all_forecast_data$up_rank, '</b></span><br>', sep = ''),
                                         paste('<b><span style="color:#F37321"> Closing </span><span style="color:#002649">Risk Ranking: ', all_forecast_data$down_rank, '</b></span><br>', sep = ''),
                                         paste('<b><span style="color:#002649"> ',all_forecast_data$names,' Level in 2019: ', all_forecast_data$level_2019, '</b></span><br>', sep = ''),
                                         paste('<b><span style="color:#002649"> ',all_forecast_data$names,' Change 2018-2019: ', all_forecast_data$change_2019, '</b></span>', sep = ''),sep = '')

all_forecast_data$popUp_text_down <- paste('<h3><b>', all_forecast_data$country_name,'</b></h3>', 
                                           '<h5><span style="color:#002649">Event probabilities for the <b>', all_forecast_data$names, ' Space</b> <span style="font-size: 80%">(', all_forecast_data$thres, ' change in the <b>', all_forecast_data$index_name, '</b>)</span></h5>',
                                           paste('<b><span style="color:#F37321">Closing Event: ',floor(all_forecast_data$p_down * 100), '%</b></span><br>', sep = ''), 
                                           paste('<b><span style="color:#777778">Stable: ',floor(all_forecast_data$p_same * 100), '%</b></span><br>', sep = ''), 
                                           paste('<b><span style="color:#0082BA">Opening Event: ',floor(all_forecast_data$p_up * 100), '%</b></span><br><br>', sep = ''), 
                                           paste('<b><span style="color:#F37321"> Closing </span><span style="color:#002649">Risk Ranking: ', all_forecast_data$down_rank, '</b></span><br>', sep = ''), 
                                           paste('<b><span style="color:#0082BA"> Opening </span><span style="color:#002649">Risk Ranking: ', all_forecast_data$up_rank, '</b></span><br>', sep = ''), 
                                           paste('<b><span style="color:#002649"> ',all_forecast_data$names,' Level in 2019: ', all_forecast_data$level_2019, '</b></span><br>', sep = ''),
                                           paste('<b><span style="color:#002649"> ',all_forecast_data$names,' Change 2018-2019: ', all_forecast_data$change_2019, '</b></span>', sep = ''),sep = '')

all_forecast_data$popUp_text_down <- paste('<h3><b>', all_forecast_data$country_name,'</b></h3>',
                                            '<h5><span style="color:#002649">Event probabilities for the <b>', all_forecast_data$names, ' Space</b> <span style="font-size: 80%">(', all_forecast_data$thres, ' change in the <b>', all_forecast_data$index_name, '</b>)</span></h5>',
                                            paste('<b><span style="color:#F37321">Closing Event: ',floor(all_forecast_data$p_down * 100), '%</b></span><br>', sep = ''),
                                            paste('<b><span style="color:#777778">Stable: ',floor(all_forecast_data$p_same * 100), '%</b></span><br>', sep = ''),
                                            paste('<b><span style="color:#0082BA">Opening Event: ',floor(all_forecast_data$p_up * 100), '%</b></span><br><br>', sep = ''),
                                            paste('<b><span style="color:#F37321"> Closing </span><span style="color:#002649">Risk Ranking: ', all_forecast_data$down_rank, '</b></span><br>', sep = ''),
                                            paste('<b><span style="color:#0082BA"> Opening </span><span style="color:#002649">Risk Ranking: ', all_forecast_data$up_rank, '</b></span><br>', sep = ''),
                                            paste('<b><span style="color:#002649"> ',all_forecast_data$names,' Level in 2019: ', all_forecast_data$level_2019, '</b></span><br>', sep = ''),
                                            paste('<b><span style="color:#002649"> ',all_forecast_data$names,' Change 2018-2019: ', all_forecast_data$change_2019, '</b></span>', sep = ''),sep = '')

##join this to the map shp file
GW_shp_file <- cshapes::cshp(date = as.Date("2013/01/01"), useGW = TRUE)
GW_shp_file@data$gwcode <- GW_shp_file@data$GWCODE
GW_shp_file@data <- GW_shp_file@data %>%
  select(-GWCODE)

country_centers <- SpatialPointsDataFrame(gCentroid(GW_shp_file, byid = TRUE),
                                          GW_shp_file@data, match.ID = FALSE)

GW_shp_file@data <- GW_shp_file@data %>%
  mutate(center_lon = country_centers@coords[, 1],
         center_lat = country_centers@coords[, 2])

forecast_colors <- all_forecast_data %>%
  dplyr::select(gwcode, country_name, year, outcome, map_color_up, map_color_down, p_up, p_down, p_same, popUp_text_up, popUp_text_down, level_2019, change_2019) %>%
  pivot_wider(names_from = outcome, values_from = c(map_color_down, map_color_up, level_2019, change_2019, p_up, p_down, p_same, popUp_text_up, popUp_text_down))

GW_shp_file@data <- GW_shp_file@data %>% 
  left_join(forecast_colors) 

ids <- NULL
for(i in 1:195){
  id <- GW_shp_file@polygons[[i]]@ID
  ids <- c(ids, id)
  }
row.names(GW_shp_file@data) <- ids

GW_shp_file_new2 <- rmapshaper::ms_simplify(GW_shp_file, keep = 0.2)
# object.size(GW_shp_file)
# object.size(GW_shp_file_new2)

write_rds(GW_shp_file_new2, "map_dat.rds")

country_characteristic_dat <- dvs %>%
  dplyr::select(gwcode, year, country_name,
                v2x_veracc_osp, v2xcs_ccsi, v2xcl_rol, v2x_freexp_altinf, v2x_horacc_osp, v2x_pubcorr) %>%
  filter(year >= 2010)

write_rds(country_characteristic_dat, "country_characteristic_dat.RDS")

prob1_dat <- all_forecast_data %>%
  dplyr::select(-for_years, -year, -gwcode, -level_2019) %>%
  pivot_longer(cols = c(p_up, p_down, p_same), names_to = "direction") %>%
  mutate(colors = case_when(direction == "p_up" ~ colors_up[5],
                            direction == "p_same" ~ "#D0D0D1",
                            direction == "p_down" ~ colors_down[5])) %>%
  
  mutate(direction = case_when(direction == "p_up" ~ "Opening",
                               direction == "p_same" ~ "Neither",
                               direction == "p_down" ~ "Closing")) %>%
  dplyr::select(-map_color_up, -map_color_down)

prob1_dat %<>%
  left_join(space_labels) %>%
  na.omit()

prob1_dat$names <- factor(prob1_dat$names, levels = c("Associational", "Economic", "Electoral", "Governing", "Individual", "Informational"), ordered = T)
prob1_dat$outcome <- factor(prob1_dat$outcome, levels = c( "v2xcs_ccsi", "v2x_pubcorr", "v2x_veracc_osp", "v2x_horacc_osp", "v2xcl_rol", "v2x_freexp_altinf"), ordered = T)

write_rds(prob1_dat, "prob1_dat.rds")

rank_data_up <- all_forecast_data %>%
  dplyr::group_by(outcome) %>%
  arrange(desc(p_up)) %>%
  dplyr::mutate(rank = row_number(), color_prob = map_color_up, risk_2019 = p_up) %>%
  filter(rank <= 20)

write_rds(rank_data_up, "rank_data_up.rds")

rank_data_down <- all_forecast_data %>%
  dplyr::group_by(outcome) %>%
  arrange(desc(p_down)) %>%
  dplyr::mutate(rank = row_number(), color_prob = map_color_down, risk_2019 = p_down) %>%
  filter(rank <= 20)

write_rds(rank_data_down, "rank_data_down.rds")
