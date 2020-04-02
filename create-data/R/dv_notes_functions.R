
# This function reads in the selected dv variable (dv_name),
# creates a lagged dv (var_lag), and
# identifies up, down, and no change cases (case)

# No "v2x_veracc_osp", Yes "v2x_pubcorr", Yes "v2xcs_ccsi", "v2x_freexp_altinf", "v2xcl_rol", "v2x_horacc_osp"

dv_BaseDatFun <- function(dv_name){

  dat <- read_csv(paste0("output-data/dv-lists/", list.files(path = "output-data/dv-lists", pattern = dv_name))) %>%
    select(-c(contains("next2"), contains("change"))) %>%
    mutate_at(vars(-c(gwcode, year, contains("y2y"))), lag) %>%
    rename(var_lag = dv_name,
           var_y2y = paste0(dv_name, "_diff_y2y")) %>%
    filter(complete.cases(.)) %>%
    mutate(case = case_when(var_y2y > 0 ~ "up",
                            var_y2y < 0 ~ "down",
                            TRUE ~ "no change"),
           case = as.factor(case))
  return(dat)
}

dv_SummaryTableFun <- function(dat, dv_name_text){

  # up_quantile <- quantile(dat$var_y2y[dat$case == "up"])
  # up_outlier <- as.numeric(up_quantile[2] - 1.5 * (up_quantile[2] - up_quantile[4]))
  #
  # down_quantile <- quantile(dat$var_y2y[dat$case == "down"])
  # down_outlier <- as.numeric(down_quantile[2] + 1.5 * (down_quantile[2] - down_quantile[4]))

  cutpoints <- dv_cutpointFun(dat)

  N <- dim(dat)[1]
  all_summary <- dat %>%
    summarise(Min = min(var_y2y),
      Max = max(var_y2y),
      Mean = mean(var_y2y),
      SD = sd(var_y2y),
      `Pos Cutoff` = 0 + cutpoints[["all"]],
      `Neg Cutoff` = 0 - cutpoints[["all"]]) %>%
    mutate(Sample = "Pooled")

  up_summary <- dat %>%
    filter(case != "down") %>%
    # filter(case == "up") %>%
    summarise(Min = min(var_y2y),
      Max = max(var_y2y),
      Mean = mean(var_y2y),
      SD = sd(var_y2y),
      `Pos Cutoff` = cutpoints[["up"]],
      `Neg Cutoff` = NA) %>%
    mutate(Sample = "Up")

  down_summary <- dat %>%
    filter(case != "up") %>%
    # filter(case == "down") %>%
    summarise(Min = min(var_y2y),
      Max = max(var_y2y),
      Mean = mean(var_y2y),
      SD = sd(var_y2y),
      `Pos Cutoff` = NA,
      `Neg Cutoff` = 0 - cutpoints[["down"]]) %>%
    mutate(Sample = "Down")

  cutpoint_table <- dat %>%
    mutate(All_up   = case_when(var_y2y >= 0 + cutpoints[["all"]]  ~ 1, TRUE ~ 0),
           All_down = case_when(var_y2y <= 0 - cutpoints[["all"]]  ~ 1, TRUE ~ 0),
           Up       = case_when(var_y2y >= 0 + cutpoints[["up"]]   ~ 1, TRUE ~ 0),
           Down     = case_when(var_y2y <= 0 - cutpoints[["down"]] ~ 1, TRUE ~ 0)) %>%
    summarise(All_up = sum(All_up),
              All_down = sum(All_down),
              Up = sum(Up),
              Down = sum(Down),
              All_up_p = All_up/N,
              All_down_p = All_down/N,
              Up_p = Up/N,
              Down_p = Down/N) %>%
    data.frame()

  dat0 <- as_tibble(rbind(all_summary,up_summary, down_summary)) %>% #
    mutate(`#-Pos` = c(cutpoint_table[1, 1], cutpoint_table[1, 3], NA), #
           `#-Neg` = c(cutpoint_table[1, 2], NA, cutpoint_table[1, 4]),#
           `%-Pos` = c(cutpoint_table[1, 5], cutpoint_table[1, 7], NA),#
           `%-Neg` = c(cutpoint_table[1, 6], NA, cutpoint_table[1, 8])) %>% #
    select(Sample, everything())

  options(knitr.kable.NA = "")
 table_out <- knitr::kable(dat0, digits = c(0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3), booktabs = TRUE,
                           caption = paste0("Summary Statistics for Different Directional Samples -- ", "1970-2018 (", dv_name_text, ")"), label = "")%>% #, "html"
    kable_styling(full_width = F, latex_options = c("HOLD_position"))

  return(table_out)

}

dv_cutpointFun <- function(dat){

  all  <- sd(dat$var_y2y)
  # v1: up/down is sd of !down, !up data
  # v2: average sd of !down, !up, round away from 0 and use that
  avg  <- mean(c(sd(dat$var_y2y[dat$case != "down"]), sd(dat$var_y2y[dat$case != "up"])))
  up <- down <- ceiling(avg*100)/100

  cutpoints <- data.frame(all= all,
                          up = up,
                          down = down)
  return(cutpoints)

}


dv_DatFun <- function(dv_name, up_cutpoint, down_cutpoint){

  dat <- read_csv(paste0("output-data/dv-lists/", list.files(path = "output-data/dv-lists", pattern = dv_name))) %>%
    select(-c(contains("next2"), contains("change"))) %>%
    rename(var_y2y = paste0(dv_name, "_diff_y2y"),
           var = dv_name) %>%
    mutate(case = case_when(var_y2y >= up_cutpoint ~ "up",
                              var_y2y <= down_cutpoint ~ "down",
                              TRUE ~ "no change"),
             up_next1 = case_when(case == "up" ~ 1, TRUE ~ 0),
             down_next1 = case_when(case == "down" ~ 1, TRUE ~ 0),
             case = as.factor(case)) %>%
    filter(complete.cases(.))

  return(dat)
}



dv_DiffTSPlotFun <- function(dat, dv_name_text, up_cutpoint, down_cutpoint, window = 1, percent = FALSE){

  dat0 <- dat %>%
    group_by(year) %>%
    summarize(n_states = n(),
              move_up = sum(up_next1),
              move_down = sum(down_next1),
              move_up_percent = move_up/n_states,
              move_down_percent = move_down/n_states) %>%
    gather(var, value, starts_with("move")) %>%
    mutate(var = factor(var, levels = unique(var)))

  if(window == 1 & percent == FALSE){
    out <- dat0 %>%
      filter(var == "move_up" | var == "move_down") %>%
      ggplot(., aes(x = year, y = value, group = var)) +
      geom_line(aes(linetype = var)) +
      scale_linetype_discrete(paste("One Year Diff:\n", dv_name_text, sep = ""),
                              labels = c("move_up" = paste("+", round(up_cutpoint, 3), sep = ""), "move_down" = round(down_cutpoint, 3))) +
      labs(x = "", y = "Count") +
      theme_classic()
  }
  if(window == 2 & percent == FALSE){
    out <- dat0%>%
      filter(var == "move_up" | var == "move_down") %>%
      ggplot(., aes(x = year, y = value, group = var)) +
      geom_line(aes(linetype = var)) +
      scale_linetype_discrete(paste("Two Year Diff:\n", dv_name_text, sep = ""),
                              labels = c("move_up" = paste("+", round(up_cutpoint, 3), sep = ""), "move_down" = round(down_cutpoint, 3))) +
      labs(x = "", y = "Count") +
      theme_classic()
  }

  if(window == 1 & percent == TRUE){
    out <- dat0%>%
      filter(var == "move_up_percent" | var == "move_down_percent") %>%
      ggplot(., aes(x = year, y = value, group = var)) +
      geom_line(aes(linetype = var)) +
      scale_linetype_discrete(paste("One Year Diff:\n", dv_name_text, sep = ""),
                              labels = c("move_up_percent" = paste("+", round(up_cutpoint, 3), sep = ""), "move_down_percent" = round(down_cutpoint, 3))) +
      labs(x = "", y = "Percent of Country-Year Observations") +
      theme_classic()
  }
  if(window == 2 & percent == TRUE){
    out <- dat0%>%
      filter(var == "move_up_percent" | var == "move_down_percent") %>%
      ggplot(., aes(x = year, y = value, group = var)) +
      geom_line(aes(linetype = var)) +
      scale_linetype_discrete(paste("Two Year Diff:\n", dv_name_text, sep = ""),
                              labels = c("move_up_percent" = paste("+", round(up_cutpoint, 3), sep = ""), "move_down_percent" = round(down_cutpoint, 3))) +
      labs(x = "", y = "Percent of Country-Year Observations") +
      theme_classic()
  }

  out
}

dv_down_CYPlotFun <- function(dat, dv_name, country_name_text, example_years) {

  country_select <- read_csv("input/country_year_set_1968-2019.csv") %>%
    filter(country_name == country_name_text & year == max(year)) %>%
    select(gwcode, country_name)

  dat_select <- dat %>%
    filter(gwcode == country_select$gwcode)
  dat_example_years <- dat_select %>%
    filter(year %in% example_years)

  the_plot <- dat_select %>%
    ggplot(., aes(x = year, y = var)) +
    geom_line() +
    labs(x = "Year", y = dv_name) +
    geom_point(data = dat_example_years, aes(y = var, x = year, shape = case)) +
    # scale_color_manual("", labels = c("Example captured", "Example not captured"), values = c(col_pal$pos3, col_pal$neg3)) +
    scale_shape_manual("", labels = c("Example captured", "Example not captured"), values = c(16, 17)) +
    ggtitle(paste0("IRI Case: ", country_name_text)) +
    theme_classic()
  return(the_plot)

}

# dat_check <- dv_DatFun(dv_name)
# dv_SummaryTableFun(dat_check, dv_name)

#
# dv_name <- "v2x_pubcorr"
#
#
# dat <- dat_check
#
# direction <- "down"
# cutpoint <- -0.05
# dv_SummaryFun
#
#
# ## Density plot of year to year changes
#
#
# dv_DensityPlotFun <- function(dv_name, cutpoint, direction){
#
#   dat0 <- dv_DatFun(dv_name) %>%
#     filter(case == direction | case == "no change")
#
#   dat1 <- tibble(x = density(dat0$var_y2y)$x, y = density(dat0$var_y2y)$y)
#
#   summary(dat1)
#
# }
#
#
#
#
# check_summary <- dv_SummaryFun(check_dat, "v2x_pubcorr")
# #
#
# ## Each is a "long" df, with "case" defining whether it was an increase (case == "up"), decrease (case == "down"),
# ## or no change (case == "no change")
#
# summary_dv_DatFun <- function(dv_name, cutoff, drop_coups = FALSE, window){
#   dat <- dv_DatFun(dv_name, cutoff, drop_coups)
#
#   p <- c(0.10, 0.25, 0.50, 0.75, 0.90)
#   p_names <- map_chr(p, ~paste0("sample_", .x*100, "_percent", "_y2y"))
#   p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
#     set_names(nm = p_names)
#
#   sample_summary_y2y <- dat %>%
#     group_by(case) %>%
#     summarise(sample_min_y2y = min(var_y2y),
#               sample_mean_y2y = mean(var_y2y),
#               sample_median_y2y = median(var_y2y),
#               sample_sd_y2y = sd(var_y2y),
#               sample_iqr_y2y = IQR(var_y2y),
#               sample_max = max(var_y2y)) %>%
#     ungroup()
#
#   quantiles_y2y <- dat %>%
#     group_by(case) %>%
#     summarise_at(vars(var_y2y), funs(!!!p_funs))%>%
#     ungroup() %>%
#     select(-case)
#
#   sample_summary_y2y <- cbind(sample_summary_y2y, quantiles_y2y)
#
#   ## Rolling stats
#   start_year <- min(dat$year)
#   end_year <- max(dat$year)
#
#   years_in_window <- c((start_year + window):end_year)
#   roll_summary_y2y <- NULL
#
#   roll_p_names <- map_chr(p, ~paste0("roll_", .x*100, "_percent_y2y"))
#   roll_p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
#     set_names(nm = roll_p_names)
#
#   for(r in 1:length(years_in_window)){
#     if(r == 1){
#       new_start_year <- start_year
#     } else new_start_year <- new_start_year + 1
#     if(r == 1){
#       new_end_year <- start_year + window
#     } else new_end_year <- new_end_year + 1
#
#     roll_dat0 <- dat %>%
#       filter(between(year, new_start_year, new_end_year)) %>%
#       group_by(case) %>%
#       arrange(year) %>%
#       summarise(year = max(year),
#                 roll_min_y2y = min(var_y2y),
#                 roll_mean_y2y = mean(var_y2y),
#                 roll_median_y2y = median(var_y2y),
#                 roll_sd_y2y = sd(var_y2y),
#                 roll_iqr_y2y = IQR(var_y2y),
#                 roll_max_y2y = max(var_y2y)) %>%
#       ungroup()
#
#     roll_quantiles <- dat %>%
#       filter(between(year, new_start_year, new_end_year)) %>%
#       group_by(case) %>%
#       arrange(year) %>%
#       summarise_at(vars(var_y2y), funs(!!!roll_p_funs))%>%
#       ungroup() %>%
#       select(-case)
#
#     roll_dat0 <- cbind(roll_dat0, roll_quantiles)
#     roll_summary_y2y <- rbind(roll_summary_y2y, roll_dat0)
#   }
#
#   roll_summary_y2y <- arrange(roll_summary_y2y, case)
#
#   return(list(sample_summary_y2y = sample_summary_y2y, roll_summary_y2y = roll_summary_y2y))
# }
#
# ## This function plots year-to-year differences relative to the lagged value of the DV according to case.
# ## mark_coups changes the color of the points where pt_coup == 1
#
# y2y_lag1_PlotFun <- function(dv_name, cutoff, mark_coups = FALSE) {
#   dat <- dv_DatFun(dv_name, cutoff)
#   the_plot <- dat %>%
#     ggplot(aes(y = var_y2y, x = var_lag)) +
#     geom_point(aes(color = case)) +
#     # scale_color_manual("Direction of Change", values = c(col_pal$neg3, "#99999950", col_pal$pos3),
#     #                    labels = c(paste0("Decrease <= ", cutoff), "No Change", paste0("Increase >= ", cutoff))) +
#     scale_y_continuous(breaks = round(seq(min(dat$var_y2y), max(dat$var_y2y), by = 0.1), 1)) +
#     geom_smooth(method = lm, aes(color = case)) +
#     labs(x = paste0("Lagged values of ", dv_name), y = paste0("Year-to-year difference in ", dv_name)) +
#     ggtitle(paste0("Year-to-year differences relative to lagged scores, ", dv_name)) +
#     theme_classic()
#
#   if(mark_coups == TRUE){
#     coup_dat <- dat %>%
#       filter(pt_coup == 1) %>%
#       mutate(coup_col = "#f79421")
#
#     the_plot <- the_plot +
#       geom_point(data = coup_dat, aes(y = var_y2y, x = var_lag, color = coup_col)) +
#       scale_color_manual("Direction of Change", values = c("#f79421", col_pal$neg3, "#99999950", col_pal$pos3),
#                          labels = c("Coup in year prior", paste0("Decrease <= ", cutoff), "No Change", paste0("Increase >= ", cutoff)))
#     return(the_plot)
#   }
#
#   if(mark_coups == FALSE){
#     the_plot <- the_plot +
#       scale_color_manual("Direction of Change", values = c(col_pal$neg3, "#99999950", col_pal$pos3),
#                          labels = c(paste0("Decrease <= ", cutoff), "No Change", paste0("Increase >= ", cutoff)))
#     return(the_plot)
#   }
#
# }
#
# ## Box plots?
#
# y2y_BoxPlotFun <- function(dv_name, cutoff, drop_coups = FALSE){
#   dat <- dv_DatFun(dv_name, cutoff, drop_coups)
#   dat %>%
#     filter(case != "no change") %>%
#     ggplot(aes(y = var_y2y, x = case, fill = case)) +
#     geom_boxplot() +
#     scale_fill_manual("Direction of Change", values = c(col_pal$neg2, col_pal$pos2),
#                       labels = c(paste0("Decrease <= ", cutoff), paste0("Increase >= ", cutoff))) +
#     scale_y_continuous(breaks = round(seq(min(dat$var_y2y), max(dat$var_y2y), by = 0.1), 1)) +
#     theme_classic()
# }
#
# ## What about rolling summary stats
#
# y2y_RollMean_TSPlotFun <- function(dv_name, cutoff, drop_coups = FALSE, window){
#
#   dat <- summary_dv_DatFun(dv_name, cutoff, drop_coups, window)
#   roll_mean_plot <- dat$roll_summary_y2y %>%
#     select(case, year, roll_mean_y2y) %>%
#     filter(case != "no change") %>%
#     ggplot(., aes(x = year, y = roll_mean_y2y, group = case)) +
#     geom_line(aes(color = case), size = 1.25) +
#     scale_color_manual("", labels = c(paste0("Any Decrease ", "(<=", cutoff, ")"), paste0("Any Increase ", "(>=", cutoff, ")")), values = c(col_pal$neg3, col_pal$pos3)) +
#     geom_hline(yintercept = dat$sample_summary_y2y$sample_mean_y2y[dat$sample_summary_y2y$case == "down"],
#                linetype = 2, color = col_pal$neg2, size = 1.05) +
#     geom_text(aes(x = max(dat$roll_summary_y2y$year), y =
#                     dat$sample_summary_y2y$sample_mean_y2y[dat$sample_summary_y2y$case == "down"],
#                   label = "Sample Mean Decrease", hjust = -0.2), size = 2.5) +
#     geom_hline(yintercept = dat$sample_summary_y2y$sample_mean_y2y[dat$sample_summary_y2y$case == "up"], linetype = 2, color = col_pal$pos2, size = 1.05) +
#     geom_text(aes(x = max(dat$roll_summary_y2y$year), y = dat$sample_summary_y2y$sample_mean_y2y[dat$sample_summary_y2y$case == "up"],
#                   label = "Sample Mean Increase", hjust = -0.2), size = 2.5) +
#     coord_cartesian(clip = "off") +
#     theme_classic() +
#     theme(legend.position = "bottom", plot.margin = unit(c(1, 8, 1, 1), "lines"))
#
#   if(window > 1){
#     roll_mean_plot <- roll_mean_plot +
#       ggtitle(paste0(window, "-year rolling mean of year-to-year changes in ", dv_name)) +
#       labs(x = "Year", y = paste0(window, "-year rolling mean"))
#   }
#
#   if(window == 1){
#     roll_mean_plot <- roll_mean_plot +
#       ggtitle(paste0("Yearly mean of year-to-year changes in ", dv_name)) +
#       labs(x = "Year", y = "Yearly mean")
#
#   }
#   return(roll_mean_plot)
# }
#
#
# y2y_RollSD_TSPlotFun <- function(dv_name, cutoff, drop_coups = FALSE, window){
#
#   dat <- summary_dv_DatFun(dv_name, cutoff, drop_coups, window)
#   roll_sd_plot <- dat$roll_summary_y2y %>%
#     select(case, year, roll_sd_y2y) %>%
#     filter(case != "no change") %>%
#     ggplot(., aes(x = year, y = roll_sd_y2y, group = case)) +
#     geom_line(aes(color = case), size = 1.25) +
#     scale_color_manual("", labels = c(paste0("Any Decrease ", "(<=", cutoff, ")"), paste0("Any Increase ", "(>=", cutoff, ")")), values = c(col_pal$neg3, col_pal$pos3)) +
#     geom_hline(yintercept = dat$sample_summary_y2y$sample_sd_y2y[dat$sample_summary_y2y$case == "down"],
#                linetype = 2, color = col_pal$neg2, size = 1.05) +
#     geom_text(aes(x = max(dat$roll_summary_y2y$year), y =
#                     dat$sample_summary_y2y$sample_sd_y2y[dat$sample_summary_y2y$case == "down"],
#                   label = "Sample SD Decrease", hjust = -0.2), size = 2.5) +
#     geom_hline(yintercept = dat$sample_summary_y2y$sample_sd_y2y[dat$sample_summary_y2y$case == "up"], linetype = 2, color = col_pal$pos2, size = 1.05) +
#     geom_text(aes(x = max(dat$roll_summary_y2y$year), y = dat$sample_summary_y2y$sample_sd_y2y[dat$sample_summary_y2y$case == "up"],
#                   label = "Sample SD Increase", hjust = -0.2), size = 2.5) +
#     coord_cartesian(clip = "off") +
#     theme_classic() +
#     theme(legend.position = "bottom", plot.margin = unit(c(1, 8, 1, 1), "lines"))
#
#   if(window > 1){
#     roll_sd_plot <- roll_sd_plot +
#       ggtitle(paste0(window, "-year rolling SD of year-to-year changes in ", dv_name)) +
#       labs(x = "Year", y = paste0(window, "-year rolling SD"))
#   }
#
#   if(window == 1){
#     roll_sd_plot <- roll_sd_plot +
#       ggtitle(paste0("Yearly SD of year-to-year changes in ", dv_name)) +
#       labs(x = "Year", y = "Yearly SD")
#
#   }
#   return(roll_sd_plot)
# }
#
# y2y_RollMedian_TSPlotFun <- function(dv_name, cutoff, drop_coups = FALSE, window){
#
#   dat <- summary_dv_DatFun(dv_name, cutoff, drop_coups, window)
#   roll_plot <- dat$roll_summary_y2y %>%
#     select(case, year, roll_median_y2y) %>%
#     filter(case != "no change") %>%
#     ggplot(., aes(x = year, y = roll_median_y2y, group = case)) +
#     geom_line(aes(color = case), size = 1.25) +
#     scale_color_manual("", labels = c(paste0("Any Decrease ", "(<=", cutoff, ")"), paste0("Any Increase ", "(>=", cutoff, ")")), values = c(col_pal$neg3, col_pal$pos3)) +
#     geom_hline(yintercept = dat$sample_summary_y2y$sample_median_y2y[dat$sample_summary_y2y$case == "down"],
#                linetype = 2, color = col_pal$neg2, size = 1.05) +
#     geom_text(aes(x = max(dat$roll_summary_y2y$year), y =
#                     dat$sample_summary_y2y$sample_median_y2y[dat$sample_summary_y2y$case == "down"],
#                   label = "Sample Median Decrease", hjust = -0.2), size = 2.5) +
#     geom_hline(yintercept = dat$sample_summary_y2y$sample_median_y2y[dat$sample_summary_y2y$case == "up"], linetype = 2, color = col_pal$pos2, size = 1.05) +
#     geom_text(aes(x = max(dat$roll_summary_y2y$year), y = dat$sample_summary_y2y$sample_median_y2y[dat$sample_summary_y2y$case == "up"],
#                   label = "Sample Median Increase", hjust = -0.2), size = 2.5) +
#     coord_cartesian(clip = "off") +
#     theme_classic() +
#     theme(legend.position = "bottom", plot.margin = unit(c(1, 8, 1, 1), "lines"))
#
#   if(window > 1){
#     roll_plot <- roll_plot +
#       ggtitle(paste0(window, "-year rolling Median of year-to-year changes in ", dv_name)) +
#       labs(x = "Year", y = paste0(window, "-year rolling Median"))
#   }
#
#   if(window == 1){
#     roll_plot <- roll_plot +
#       ggtitle(paste0("Yearly Median of year-to-year changes in ", dv_name)) +
#       labs(x = "Year", y = "Yearly Median")
#
#   }
#   return(roll_plot)
# }
#
#
# y2y_RollIQR_TSPlotFun <- function(dv_name, cutoff, drop_coups = FALSE, window){
#
#   dat <- summary_dv_DatFun(dv_name, cutoff, drop_coups, window)
#   roll_plot <- dat$roll_summary_y2y %>%
#     select(case, year, roll_iqr_y2y) %>%
#     filter(case != "no change") %>%
#     ggplot(., aes(x = year, y = roll_iqr_y2y, group = case)) +
#     geom_line(aes(color = case), size = 1.25) +
#     scale_color_manual("", labels = c(paste0("Any Decrease ", "(<=", cutoff, ")"), paste0("Any Increase ", "(>=", cutoff, ")")), values = c(col_pal$neg3, col_pal$pos3)) +
#     geom_hline(yintercept = dat$sample_summary_y2y$sample_iqr_y2y[dat$sample_summary_y2y$case == "down"],
#                linetype = 2, color = col_pal$neg2, size = 1.05) +
#     geom_text(aes(x = max(dat$roll_summary_y2y$year), y =
#                     dat$sample_summary_y2y$sample_iqr_y2y[dat$sample_summary_y2y$case == "down"],
#                   label = "Sample IQR Decrease", hjust = -0.2), size = 2.5) +
#     geom_hline(yintercept = dat$sample_summary_y2y$sample_iqr_y2y[dat$sample_summary_y2y$case == "up"], linetype = 2, color = col_pal$pos2, size = 1.05) +
#     geom_text(aes(x = max(dat$roll_summary_y2y$year), y = dat$sample_summary_y2y$sample_iqr_y2y[dat$sample_summary_y2y$case == "up"],
#                   label = "Sample IQR Increase", hjust = -0.2), size = 2.5) +
#     coord_cartesian(clip = "off") +
#     theme_classic() +
#     theme(legend.position = "bottom", plot.margin = unit(c(1, 8, 1, 1), "lines"))
#
#   if(window > 1){
#     roll_plot <- roll_plot +
#       ggtitle(paste0(window, "-year rolling IQR of year-to-year changes in ", dv_name)) +
#       labs(x = "Year", y = paste0(window, "-year rolling IQR"))
#   }
#
#   if(window == 1){
#     roll_plot <- roll_plot +
#       ggtitle(paste0("Yearly IQR of year-to-year changes in ", dv_name)) +
#       labs(x = "Year", y = "Yearly IQR")
#
#   }
#   return(roll_plot)
# }

