Create DV data
================

This script applies the cutpoints to the raw DV indicators to create the
actual targets that we will use for the models, i.e. indicators for an
up or down movement over the 2 years following the year in a
row/observation. These targets are denoted with a "\_next2" suffix, as
well as the direction (“up”, “down”) in which they go.

Inputs:

  - `trafo-data/dv_data_1968_on.csv`
  - `output-data/cutpoints.csv`

Outputs:

The main output is:

  - `trafo-data/dv-data.rds`

The script also writes CSV files for the target outcome variables, so
that changes are easier to identify on git.

  - `output-data/dv-lists/[vdem indicator].csv`

<!-- end list -->

``` r
dv <- read_csv("../trafo-data/dv_data_1968_on.csv") %>%
  select(-country_name, -country_id, -country_text_id) %>%
  filter(complete.cases(.)) %>%
  arrange(gwcode, year)
```

    ## Parsed with column specification:
    ## cols(
    ##   gwcode = col_double(),
    ##   year = col_double(),
    ##   country_name = col_character(),
    ##   country_id = col_double(),
    ##   country_text_id = col_character(),
    ##   v2x_veracc_osp = col_double(),
    ##   v2xcs_ccsi = col_double(),
    ##   v2xcl_rol = col_double(),
    ##   v2x_freexp_altinf = col_double(),
    ##   v2x_horacc_osp = col_double(),
    ##   v2x_pubcorr = col_double()
    ## )

``` r
range(dv$year)
```

    ## [1] 1968 2019

``` r
cutpoints <- read_csv("../output-data/cutpoints.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   indicator = col_character(),
    ##   all = col_double(),
    ##   up = col_double(),
    ##   down = col_double()
    ## )

``` r
cp <- cutpoints[["up"]]
names(cp) <- cutpoints[["indicator"]]

dv_vars <- setdiff(names(dv), c("gwcode", "year"))

dv_piece <- list()
for (var_i in dv_vars) {
  dv_i <- dv
  dv_i <- dv %>%
    select(gwcode, year, !!var_i) %>%
    group_by(gwcode) %>%
    arrange(gwcode, year) %>%
    dplyr::mutate(
      # witches' magic that creates a 'dv_i' column that contains values as if in 
      # base R we did dv[, dv_i]; from now on i'll use dv_i for the remaining
      # computations
      var = !!rlang::sym(var_i),
      lag1_var = lag(var, 1L),
      y2y_diff_var = (var - lag1_var),
      # set y2y_diff to 0 if missing (first year of independence)
      y2y_diff_var = ifelse(is.na(y2y_diff_var), 0, y2y_diff_var)
    ) 
  dv_i <- dv_i %>%
    mutate(var_change = case_when(
      y2y_diff_var > cp[[var_i]]  ~ "up",
      y2y_diff_var < -cp[[var_i]] ~ "down",
      year==min(year)      ~ "first year of independence",
      is.na(y2y_diff_var) & year!=min(year) ~ NA_character_,
      TRUE ~ "same"
    ))
  dv_i <- dv_i %>%
    mutate(up = as.integer(var_change=="up"),
           lead1_up = lead(up, 1L),
           lead2_up = lead(up, 2L),
           next2_up = pmax(lead1_up, lead2_up),
           down = as.integer(var_change=="down"),
           lead1_down = lead(down, 1L),
           lead2_down = lead(down, 2L),
           next2_down = pmax(lead1_down, lead2_down)
    ) 
  # some states end during the data period; in these cases we can safely code
  # DV=0 since they are not really censored. 
  dv_i <- dv_i %>%
    ungroup() %>%
    mutate(
      next2_up = case_when(
        is.na(next2_up)   & (year < (max(year) - 1)) ~ 0L,
        TRUE ~ next2_up),
      next2_down = case_when(
        is.na(next2_down) & (year < (max(year) -1)) ~ 0L,
        TRUE ~ next2_down
      ))
  # rename variables
  dv_i <- dv_i %>%
    select(gwcode, year, var, lag1_var, y2y_diff_var, var_change, up, next2_up,
           down, next2_down) %>%
    setNames(c("gwcode", "year", paste0(var_i, c("", "_lag1", "_diff_y2y", 
                                                 "_change", "_up", "_up_next2", 
                                                 "_down", "_down_next2"))))
  
  dv_to_join <- select(dv_i, gwcode, year, !!var_i, ends_with("diff_y2y"), 
                       ends_with("change"), ends_with("next2"), 
                       ends_with("up"), ends_with("down")) %>%
    # sub in the actual variable name, not the placeholders
    setNames(c("gwcode", "year", paste0("", names(.)[3:ncol(.)]))) %>%
    # prepend "dv" to vars we should use as IVs
    rename_at(vars(ends_with("change"), ends_with("next2")), ~ paste0("dv_", .))
  
  dv_piece[[var_i]] <- dv_to_join
}
```

Write the pieces to CSV files so we can quickly see on git when anything
changes.

``` r
for (i in seq_along(dv_piece)) {
  var_i <- names(dv_piece)[i]
  dv_to_join <- dv_piece[[var_i]]
  dv_to_join <- dv_to_join %>% select(-ends_with("_up"), -ends_with("_down"),
                                      -starts_with("v2"))
  fn <- paste0(var_i, ".csv")
  write_csv(dv_to_join, file.path("../output-data/dv-lists", fn))
}
```

Combine into one large DV set.

``` r
dv_data <- Reduce(left_join, x = dv_piece)
```

    ## Joining, by = c("gwcode", "year")
    ## Joining, by = c("gwcode", "year")
    ## Joining, by = c("gwcode", "year")
    ## Joining, by = c("gwcode", "year")
    ## Joining, by = c("gwcode", "year")

Add moving averages and other history-type variables of DVs.

``` r
ma5 <- function(x) {
  rollapply(x, width = 5, FUN = mean, fill = 0, align = "right", partial = TRUE)
}

ma10 <- function(x) {
  rollapply(x, width = 10, FUN = mean, fill = 0, align = "right", partial = TRUE)
}

dv_data = dv_data %>%
  group_by(gwcode) %>%
  arrange(year) %>%
  mutate_at(vars(ends_with("up"), ends_with("down")), list(ma5 = ma5, ma10 = ma10)) %>%
  ungroup()
```

Add squared transformations for space indicators. Changes seem to be
more common at middle values and less common in high and low (partly
because beyond some range a change is impossible).

``` r
data("spaces")
stopifnot(all(spaces$Indicator %in% names(dv_data)))
dv_data = dv_data %>%
  mutate_at(vars(one_of(spaces$Indicator)), list(squared = ~`^`(., 2)))
```

Take out the plain up and down dv versions.

``` r
dv_data <- dv_data %>%
  select(-ends_with("up"), -ends_with("down"))

range(dv_data$year)
```

    ## [1] 1968 2019

``` r
write_rds(dv_data, "../trafo-data/dv-data.rds")
```

Check the values for one outcome/country to make sure they make sense:

``` r
dv_data %>%
  filter(gwcode==310) %>%
  select(gwcode, year, contains("veracc")) %>%
  filter(year > 2010)
```

    ## # A tibble: 9 x 12
    ##   gwcode  year v2x_veracc_osp v2x_veracc_osp_~ dv_v2x_veracc_o~
    ##    <dbl> <dbl>          <dbl>            <dbl> <chr>           
    ## 1    310  2011          0.913          -0.007  same            
    ## 2    310  2012          0.913           0      same            
    ## 3    310  2013          0.907          -0.006  same            
    ## 4    310  2014          0.836          -0.071  same            
    ## 5    310  2015          0.833          -0.003  same            
    ## 6    310  2016          0.834           0.001  same            
    ## 7    310  2017          0.826          -0.008  same            
    ## 8    310  2018          0.753          -0.0730 same            
    ## 9    310  2019          0.755           0.002  same            
    ## # ... with 7 more variables: dv_v2x_veracc_osp_up_next2 <int>,
    ## #   dv_v2x_veracc_osp_down_next2 <int>, v2x_veracc_osp_up_ma5 <dbl>,
    ## #   v2x_veracc_osp_down_ma5 <dbl>, v2x_veracc_osp_up_ma10 <dbl>,
    ## #   v2x_veracc_osp_down_ma10 <dbl>, v2x_veracc_osp_squared <dbl>

``` r
skim(dv_data)
```

|                                                  |          |
| :----------------------------------------------- | :------- |
| Name                                             | dv\_data |
| Number of rows                                   | 8120     |
| Number of columns                                | 62       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| character                                        | 6        |
| numeric                                          | 56       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type:
character**

| skim\_variable                  | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :------------------------------ | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| dv\_v2x\_veracc\_osp\_change    |          0 |              1 |   2 |  26 |     0 |         4 |          0 |
| dv\_v2xcs\_ccsi\_change         |          0 |              1 |   2 |  26 |     0 |         4 |          0 |
| dv\_v2xcl\_rol\_change          |          0 |              1 |   2 |  26 |     0 |         4 |          0 |
| dv\_v2x\_freexp\_altinf\_change |          0 |              1 |   2 |  26 |     0 |         4 |          0 |
| dv\_v2x\_horacc\_osp\_change    |          0 |              1 |   2 |  26 |     0 |         4 |          0 |
| dv\_v2x\_pubcorr\_change        |          0 |              1 |   2 |  26 |     0 |         4 |          0 |

**Variable type:
numeric**

| skim\_variable                       | n\_missing | complete\_rate |    mean |     sd |      p0 |     p25 |     p50 |     p75 |    p100 | hist  |
| :----------------------------------- | ---------: | -------------: | ------: | -----: | ------: | ------: | ------: | ------: | ------: | :---- |
| gwcode                               |          0 |           1.00 |  462.55 | 240.40 |    2.00 |  305.00 |  461.00 |  663.00 |  950.00 | ▅▆▇▇▃ |
| year                                 |          0 |           1.00 | 1994.60 |  14.87 | 1968.00 | 1982.00 | 1995.00 | 2007.00 | 2019.00 | ▆▆▇▇▇ |
| v2x\_veracc\_osp                     |          0 |           1.00 |    0.63 |   0.27 |    0.06 |    0.41 |    0.70 |    0.89 |    0.96 | ▃▂▃▃▇ |
| v2x\_veracc\_osp\_diff\_y2y          |          0 |           1.00 |    0.00 |   0.08 |  \-0.76 |    0.00 |    0.00 |    0.00 |    0.80 | ▁▁▇▁▁ |
| dv\_v2x\_veracc\_osp\_up\_next2      |        338 |           0.96 |    0.06 |   0.24 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| dv\_v2x\_veracc\_osp\_down\_next2    |        338 |           0.96 |    0.04 |   0.20 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| v2xcs\_ccsi                          |          0 |           1.00 |    0.57 |   0.31 |    0.01 |    0.28 |    0.64 |    0.87 |    0.98 | ▃▃▂▃▇ |
| v2xcs\_ccsi\_diff\_y2y               |          0 |           1.00 |    0.00 |   0.06 |  \-0.56 |    0.00 |    0.00 |    0.00 |    0.75 | ▁▁▇▁▁ |
| dv\_v2xcs\_ccsi\_up\_next2           |        338 |           0.96 |    0.12 |   0.32 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| dv\_v2xcs\_ccsi\_down\_next2         |        338 |           0.96 |    0.08 |   0.27 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| v2xcl\_rol                           |          0 |           1.00 |    0.60 |   0.30 |    0.00 |    0.35 |    0.64 |    0.89 |    0.99 | ▃▃▃▅▇ |
| v2xcl\_rol\_diff\_y2y                |          0 |           1.00 |    0.00 |   0.05 |  \-0.55 |    0.00 |    0.00 |    0.00 |    0.70 | ▁▁▇▁▁ |
| dv\_v2xcl\_rol\_up\_next2            |        338 |           0.96 |    0.11 |   0.31 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| dv\_v2xcl\_rol\_down\_next2          |        338 |           0.96 |    0.08 |   0.27 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| v2x\_freexp\_altinf                  |          0 |           1.00 |    0.57 |   0.33 |    0.01 |    0.23 |    0.67 |    0.88 |    0.98 | ▅▃▂▃▇ |
| v2x\_freexp\_altinf\_diff\_y2y       |          0 |           1.00 |    0.00 |   0.06 |  \-0.58 |    0.00 |    0.00 |    0.00 |    0.85 | ▁▂▇▁▁ |
| dv\_v2x\_freexp\_altinf\_up\_next2   |        338 |           0.96 |    0.09 |   0.28 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| dv\_v2x\_freexp\_altinf\_down\_next2 |        338 |           0.96 |    0.06 |   0.23 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| v2x\_horacc\_osp                     |          0 |           1.00 |    0.55 |   0.32 |    0.01 |    0.24 |    0.58 |    0.86 |    0.99 | ▅▅▃▃▇ |
| v2x\_horacc\_osp\_diff\_y2y          |          0 |           1.00 |    0.00 |   0.06 |  \-0.68 |    0.00 |    0.00 |    0.00 |    0.83 | ▁▁▇▁▁ |
| dv\_v2x\_horacc\_osp\_up\_next2      |        338 |           0.96 |    0.08 |   0.28 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| dv\_v2x\_horacc\_osp\_down\_next2    |        338 |           0.96 |    0.06 |   0.23 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| v2x\_pubcorr                         |          0 |           1.00 |    0.51 |   0.30 |    0.03 |    0.23 |    0.48 |    0.80 |    1.00 | ▇▇▅▅▇ |
| v2x\_pubcorr\_diff\_y2y              |          0 |           1.00 |    0.00 |   0.04 |  \-0.60 |    0.00 |    0.00 |    0.00 |    0.68 | ▁▁▇▁▁ |
| dv\_v2x\_pubcorr\_up\_next2          |        338 |           0.96 |    0.09 |   0.28 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| dv\_v2x\_pubcorr\_down\_next2        |        338 |           0.96 |    0.10 |   0.30 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| v2x\_veracc\_osp\_up\_ma5            |          0 |           1.00 |    0.03 |   0.08 |    0.00 |    0.00 |    0.00 |    0.00 |    0.60 | ▇▁▁▁▁ |
| v2xcs\_ccsi\_up\_ma5                 |          0 |           1.00 |    0.06 |   0.13 |    0.00 |    0.00 |    0.00 |    0.00 |    0.80 | ▇▂▁▁▁ |
| v2xcl\_rol\_up\_ma5                  |          0 |           1.00 |    0.06 |   0.12 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| v2x\_freexp\_altinf\_up\_ma5         |          0 |           1.00 |    0.05 |   0.12 |    0.00 |    0.00 |    0.00 |    0.00 |    0.80 | ▇▁▁▁▁ |
| v2x\_horacc\_osp\_up\_ma5            |          0 |           1.00 |    0.04 |   0.10 |    0.00 |    0.00 |    0.00 |    0.00 |    0.80 | ▇▁▁▁▁ |
| v2x\_pubcorr\_up\_ma5                |          0 |           1.00 |    0.04 |   0.10 |    0.00 |    0.00 |    0.00 |    0.00 |    0.80 | ▇▁▁▁▁ |
| v2x\_veracc\_osp\_down\_ma5          |          0 |           1.00 |    0.02 |   0.07 |    0.00 |    0.00 |    0.00 |    0.00 |    0.67 | ▇▁▁▁▁ |
| v2xcs\_ccsi\_down\_ma5               |          0 |           1.00 |    0.04 |   0.10 |    0.00 |    0.00 |    0.00 |    0.00 |    0.80 | ▇▁▁▁▁ |
| v2xcl\_rol\_down\_ma5                |          0 |           1.00 |    0.04 |   0.10 |    0.00 |    0.00 |    0.00 |    0.00 |    0.80 | ▇▁▁▁▁ |
| v2x\_freexp\_altinf\_down\_ma5       |          0 |           1.00 |    0.03 |   0.09 |    0.00 |    0.00 |    0.00 |    0.00 |    1.00 | ▇▁▁▁▁ |
| v2x\_horacc\_osp\_down\_ma5          |          0 |           1.00 |    0.03 |   0.09 |    0.00 |    0.00 |    0.00 |    0.00 |    0.67 | ▇▁▁▁▁ |
| v2x\_pubcorr\_down\_ma5              |          0 |           1.00 |    0.05 |   0.11 |    0.00 |    0.00 |    0.00 |    0.00 |    0.80 | ▇▂▁▁▁ |
| v2x\_veracc\_osp\_up\_ma10           |          0 |           1.00 |    0.03 |   0.06 |    0.00 |    0.00 |    0.00 |    0.00 |    0.50 | ▇▁▁▁▁ |
| v2xcs\_ccsi\_up\_ma10                |          0 |           1.00 |    0.06 |   0.10 |    0.00 |    0.00 |    0.00 |    0.10 |    0.60 | ▇▁▁▁▁ |
| v2xcl\_rol\_up\_ma10                 |          0 |           1.00 |    0.06 |   0.09 |    0.00 |    0.00 |    0.00 |    0.10 |    0.67 | ▇▁▁▁▁ |
| v2x\_freexp\_altinf\_up\_ma10        |          0 |           1.00 |    0.05 |   0.09 |    0.00 |    0.00 |    0.00 |    0.10 |    0.50 | ▇▁▁▁▁ |
| v2x\_horacc\_osp\_up\_ma10           |          0 |           1.00 |    0.04 |   0.08 |    0.00 |    0.00 |    0.00 |    0.10 |    0.67 | ▇▁▁▁▁ |
| v2x\_pubcorr\_up\_ma10               |          0 |           1.00 |    0.04 |   0.07 |    0.00 |    0.00 |    0.00 |    0.10 |    0.50 | ▇▁▁▁▁ |
| v2x\_veracc\_osp\_down\_ma10         |          0 |           1.00 |    0.02 |   0.06 |    0.00 |    0.00 |    0.00 |    0.00 |    0.67 | ▇▁▁▁▁ |
| v2xcs\_ccsi\_down\_ma10              |          0 |           1.00 |    0.04 |   0.08 |    0.00 |    0.00 |    0.00 |    0.00 |    0.67 | ▇▁▁▁▁ |
| v2xcl\_rol\_down\_ma10               |          0 |           1.00 |    0.03 |   0.08 |    0.00 |    0.00 |    0.00 |    0.00 |    0.75 | ▇▁▁▁▁ |
| v2x\_freexp\_altinf\_down\_ma10      |          0 |           1.00 |    0.03 |   0.07 |    0.00 |    0.00 |    0.00 |    0.00 |    0.67 | ▇▁▁▁▁ |
| v2x\_horacc\_osp\_down\_ma10         |          0 |           1.00 |    0.03 |   0.07 |    0.00 |    0.00 |    0.00 |    0.00 |    0.67 | ▇▁▁▁▁ |
| v2x\_pubcorr\_down\_ma10             |          0 |           1.00 |    0.05 |   0.09 |    0.00 |    0.00 |    0.00 |    0.10 |    0.80 | ▇▁▁▁▁ |
| v2x\_veracc\_osp\_squared            |          0 |           1.00 |    0.47 |   0.31 |    0.00 |    0.17 |    0.50 |    0.79 |    0.93 | ▇▅▃▅▇ |
| v2xcs\_ccsi\_squared                 |          0 |           1.00 |    0.43 |   0.34 |    0.00 |    0.08 |    0.40 |    0.76 |    0.96 | ▇▂▂▃▅ |
| v2xcl\_rol\_squared                  |          0 |           1.00 |    0.45 |   0.34 |    0.00 |    0.12 |    0.41 |    0.79 |    0.99 | ▇▃▃▃▆ |
| v2x\_freexp\_altinf\_squared         |          0 |           1.00 |    0.43 |   0.35 |    0.00 |    0.05 |    0.44 |    0.78 |    0.97 | ▇▂▃▃▆ |
| v2x\_horacc\_osp\_squared            |          0 |           1.00 |    0.40 |   0.34 |    0.00 |    0.06 |    0.34 |    0.75 |    0.98 | ▇▂▂▂▅ |
| v2x\_pubcorr\_squared                |          0 |           1.00 |    0.35 |   0.33 |    0.00 |    0.05 |    0.23 |    0.64 |    0.99 | ▇▂▂▂▃ |
