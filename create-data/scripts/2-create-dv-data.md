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
    ##   gwcode  year v2x_veracc_osp v2x_veracc_osp_… dv_v2x_veracc_o…
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
    ## # … with 7 more variables: dv_v2x_veracc_osp_up_next2 <int>,
    ## #   dv_v2x_veracc_osp_down_next2 <int>, v2x_veracc_osp_up_ma5 <dbl>,
    ## #   v2x_veracc_osp_down_ma5 <dbl>, v2x_veracc_osp_up_ma10 <dbl>,
    ## #   v2x_veracc_osp_down_ma10 <dbl>, v2x_veracc_osp_squared <dbl>

``` r
skim(dv_data)
```

    ## Skim summary statistics
    ##  n obs: 8120 
    ##  n variables: 62 
    ## 
    ## ── Variable type:character ──────────────────────────────────────────────────────────────
    ##                     variable missing complete    n min max empty n_unique
    ##  dv_v2x_freexp_altinf_change       0     8120 8120   2  26     0        4
    ##     dv_v2x_horacc_osp_change       0     8120 8120   2  26     0        4
    ##        dv_v2x_pubcorr_change       0     8120 8120   2  26     0        4
    ##     dv_v2x_veracc_osp_change       0     8120 8120   2  26     0        4
    ##          dv_v2xcl_rol_change       0     8120 8120   2  26     0        4
    ##         dv_v2xcs_ccsi_change       0     8120 8120   2  26     0        4
    ## 
    ## ── Variable type:integer ────────────────────────────────────────────────────────────────
    ##                         variable missing complete    n  mean   sd p0 p25
    ##  dv_v2x_freexp_altinf_down_next2     338     7782 8120 0.057 0.23  0   0
    ##    dv_v2x_freexp_altinf_up_next2     338     7782 8120 0.088 0.28  0   0
    ##     dv_v2x_horacc_osp_down_next2     338     7782 8120 0.057 0.23  0   0
    ##       dv_v2x_horacc_osp_up_next2     338     7782 8120 0.083 0.28  0   0
    ##        dv_v2x_pubcorr_down_next2     338     7782 8120 0.098 0.3   0   0
    ##          dv_v2x_pubcorr_up_next2     338     7782 8120 0.089 0.28  0   0
    ##     dv_v2x_veracc_osp_down_next2     338     7782 8120 0.042 0.2   0   0
    ##       dv_v2x_veracc_osp_up_next2     338     7782 8120 0.061 0.24  0   0
    ##          dv_v2xcl_rol_down_next2     338     7782 8120 0.076 0.27  0   0
    ##            dv_v2xcl_rol_up_next2     338     7782 8120 0.11  0.31  0   0
    ##         dv_v2xcs_ccsi_down_next2     338     7782 8120 0.08  0.27  0   0
    ##           dv_v2xcs_ccsi_up_next2     338     7782 8120 0.12  0.32  0   0
    ##  p50 p75 p100     hist
    ##    0   0    1 ▇▁▁▁▁▁▁▁
    ##    0   0    1 ▇▁▁▁▁▁▁▁
    ##    0   0    1 ▇▁▁▁▁▁▁▁
    ##    0   0    1 ▇▁▁▁▁▁▁▁
    ##    0   0    1 ▇▁▁▁▁▁▁▁
    ##    0   0    1 ▇▁▁▁▁▁▁▁
    ##    0   0    1 ▇▁▁▁▁▁▁▁
    ##    0   0    1 ▇▁▁▁▁▁▁▁
    ##    0   0    1 ▇▁▁▁▁▁▁▁
    ##    0   0    1 ▇▁▁▁▁▁▁▁
    ##    0   0    1 ▇▁▁▁▁▁▁▁
    ##    0   0    1 ▇▁▁▁▁▁▁▁
    ## 
    ## ── Variable type:numeric ────────────────────────────────────────────────────────────────
    ##                     variable missing complete    n       mean      sd
    ##                       gwcode       0     8120 8120  462.55    240.4  
    ##            v2x_freexp_altinf       0     8120 8120    0.57      0.33 
    ##   v2x_freexp_altinf_diff_y2y       0     8120 8120    0.005     0.057
    ##  v2x_freexp_altinf_down_ma10       0     8120 8120    0.027     0.072
    ##   v2x_freexp_altinf_down_ma5       0     8120 8120    0.029     0.092
    ##    v2x_freexp_altinf_squared       0     8120 8120    0.43      0.35 
    ##    v2x_freexp_altinf_up_ma10       0     8120 8120    0.047     0.088
    ##     v2x_freexp_altinf_up_ma5       0     8120 8120    0.048     0.12 
    ##               v2x_horacc_osp       0     8120 8120    0.55      0.32 
    ##      v2x_horacc_osp_diff_y2y       0     8120 8120    0.0039    0.061
    ##     v2x_horacc_osp_down_ma10       0     8120 8120    0.029     0.07 
    ##      v2x_horacc_osp_down_ma5       0     8120 8120    0.029     0.086
    ##       v2x_horacc_osp_squared       0     8120 8120    0.4       0.34 
    ##       v2x_horacc_osp_up_ma10       0     8120 8120    0.042     0.081
    ##        v2x_horacc_osp_up_ma5       0     8120 8120    0.042     0.1  
    ##                  v2x_pubcorr       0     8120 8120    0.51      0.3  
    ##         v2x_pubcorr_diff_y2y       0     8120 8120   -0.00081   0.04 
    ##        v2x_pubcorr_down_ma10       0     8120 8120    0.048     0.088
    ##         v2x_pubcorr_down_ma5       0     8120 8120    0.049     0.11 
    ##          v2x_pubcorr_squared       0     8120 8120    0.35      0.33 
    ##          v2x_pubcorr_up_ma10       0     8120 8120    0.036     0.071
    ##           v2x_pubcorr_up_ma5       0     8120 8120    0.041     0.1  
    ##               v2x_veracc_osp       0     8120 8120    0.63      0.27 
    ##      v2x_veracc_osp_diff_y2y       0     8120 8120    0.0038    0.083
    ##     v2x_veracc_osp_down_ma10       0     8120 8120    0.022     0.056
    ##      v2x_veracc_osp_down_ma5       0     8120 8120    0.021     0.068
    ##       v2x_veracc_osp_squared       0     8120 8120    0.47      0.31 
    ##       v2x_veracc_osp_up_ma10       0     8120 8120    0.031     0.06 
    ##        v2x_veracc_osp_up_ma5       0     8120 8120    0.031     0.079
    ##                    v2xcl_rol       0     8120 8120    0.6       0.3  
    ##           v2xcl_rol_diff_y2y       0     8120 8120    0.0036    0.045
    ##          v2xcl_rol_down_ma10       0     8120 8120    0.035     0.078
    ##           v2xcl_rol_down_ma5       0     8120 8120    0.037     0.1  
    ##            v2xcl_rol_squared       0     8120 8120    0.45      0.34 
    ##            v2xcl_rol_up_ma10       0     8120 8120    0.055     0.094
    ##             v2xcl_rol_up_ma5       0     8120 8120    0.056     0.12 
    ##                   v2xcs_ccsi       0     8120 8120    0.57      0.31 
    ##          v2xcs_ccsi_diff_y2y       0     8120 8120    0.0046    0.056
    ##         v2xcs_ccsi_down_ma10       0     8120 8120    0.037     0.08 
    ##          v2xcs_ccsi_down_ma5       0     8120 8120    0.04      0.1  
    ##           v2xcs_ccsi_squared       0     8120 8120    0.43      0.34 
    ##           v2xcs_ccsi_up_ma10       0     8120 8120    0.059     0.096
    ##            v2xcs_ccsi_up_ma5       0     8120 8120    0.061     0.13 
    ##                         year       0     8120 8120 1994.6      14.87 
    ##          p0      p25     p50      p75    p100     hist
    ##     2        305      461     663      950    ▅▅▃▇▇▇▅▂
    ##     0.013      0.23     0.67    0.88     0.98 ▅▃▂▂▂▃▅▇
    ##    -0.58       0        0       0.004    0.85 ▁▁▁▇▁▁▁▁
    ##     0          0        0       0        0.67 ▇▁▁▁▁▁▁▁
    ##     0          0        0       0        1    ▇▁▁▁▁▁▁▁
    ##     0.00017    0.052    0.44    0.78     0.97 ▇▂▁▂▂▂▃▅
    ##     0          0        0       0.1      0.5  ▇▂▁▁▁▁▁▁
    ##     0          0        0       0        0.8  ▇▁▁▁▁▁▁▁
    ##     0.013      0.24     0.58    0.86     0.99 ▅▅▃▂▂▃▅▇
    ##    -0.68      -0.002    0       0.003    0.83 ▁▁▁▇▁▁▁▁
    ##     0          0        0       0        0.67 ▇▁▁▁▁▁▁▁
    ##     0          0        0       0        0.67 ▇▁▁▁▁▁▁▁
    ##     0.00017    0.058    0.34    0.75     0.98 ▇▂▂▁▂▂▃▃
    ##     0          0        0       0.1      0.67 ▇▂▁▁▁▁▁▁
    ##     0          0        0       0        0.8  ▇▁▁▁▁▁▁▁
    ##     0.025      0.23     0.47    0.8      0.99 ▅▇▆▅▅▃▅▇
    ##    -0.6        0        0       0        0.68 ▁▁▁▇▁▁▁▁
    ##     0          0        0       0.1      0.8  ▇▁▁▁▁▁▁▁
    ##     0          0        0       0        0.8  ▇▂▁▁▁▁▁▁
    ##     0.00063    0.055    0.23    0.64     0.99 ▇▂▂▂▁▂▁▂
    ##     0          0        0       0.1      0.5  ▇▂▁▁▁▁▁▁
    ##     0          0        0       0        0.8  ▇▁▁▁▁▁▁▁
    ##     0.064      0.41     0.7     0.89     0.96 ▂▁▂▃▂▂▅▇
    ##    -0.76      -0.001    0       0.002    0.81 ▁▁▁▇▁▁▁▁
    ##     0          0        0       0        0.67 ▇▁▁▁▁▁▁▁
    ##     0          0        0       0        0.67 ▇▁▁▁▁▁▁▁
    ##     0.0041     0.17     0.5     0.79     0.93 ▆▅▃▃▃▃▃▇
    ##     0          0        0       0        0.5  ▇▂▁▁▁▁▁▁
    ##     0          0        0       0        0.6  ▇▁▁▁▁▁▁▁
    ##     0          0.35     0.64    0.89     0.99 ▂▃▂▃▃▃▃▇
    ##    -0.55       0        0       0.003    0.7  ▁▁▁▇▁▁▁▁
    ##     0          0        0       0        0.75 ▇▂▁▁▁▁▁▁
    ##     0          0        0       0        0.8  ▇▁▁▁▁▁▁▁
    ##     0          0.12     0.41    0.79     0.99 ▇▃▃▂▃▂▃▆
    ##     0          0        0       0.1      0.67 ▇▂▁▁▁▁▁▁
    ##     0          0        0       0        1    ▇▂▁▁▁▁▁▁
    ##     0.011      0.28     0.64    0.87     0.98 ▃▃▃▂▂▂▅▇
    ##    -0.56       0        0       0        0.75 ▁▁▁▇▁▁▁▁
    ##     0          0        0       0        0.67 ▇▂▁▁▁▁▁▁
    ##     0          0        0       0        0.8  ▇▁▁▁▁▁▁▁
    ##     0.00012    0.076    0.4     0.76     0.96 ▇▂▂▂▂▂▃▃
    ##     0          0        0       0.1      0.6  ▇▂▁▁▁▁▁▁
    ##     0          0        0       0        0.8  ▇▂▁▁▁▁▁▁
    ##  1968       1982     1995    2007     2019    ▆▆▇▆▇▇▇▇
