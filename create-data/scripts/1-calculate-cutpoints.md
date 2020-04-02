Calculate cutpoints
================

*Note (AB, 2020-03-24): the cutpoint calculations were originally done
in a file called “1-dv\_notes.Rmd”, written by Rick. That file also
created a memo/report. I split the cutpoint calculation part out because
there was a circular dependency with “2-create-dv-data.Rmd”, and
adjusted the code accordingly to remove this problem. See issue \#26 on
GitHub to see the commit that created this file.*

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
dv_vars <- setdiff(names(dv), c("gwcode", "year"))
dv_vars
```

    ## [1] "v2x_veracc_osp"    "v2xcs_ccsi"        "v2xcl_rol"        
    ## [4] "v2x_freexp_altinf" "v2x_horacc_osp"    "v2x_pubcorr"

``` r
# Create variables for DV year to year changes
dv_semi_long <- dv %>%
  # pivot to long data frame so we can do all 6 variables at the same time
  pivot_longer(-c(gwcode, year), names_to = "variable") %>%
  group_by(gwcode, variable) %>%
  arrange(gwcode, variable, year) %>%
  dplyr::mutate(
    # setting to 0 is missing, i.e. first year of independence
    diff_y2y = c(0, diff(value)),
  )  

# now there are 2 value columns (for original var, and y2y_diff version); 
# we can make it wide as is, but then have to fix some columns names ("value_")
dv_semi_long
```

    ## # A tibble: 48,720 x 5
    ## # Groups:   gwcode, variable [1,044]
    ##    gwcode  year variable          value diff_y2y
    ##     <dbl> <dbl> <chr>             <dbl>    <dbl>
    ##  1      2  1968 v2x_freexp_altinf 0.858  0      
    ##  2      2  1969 v2x_freexp_altinf 0.886  0.028  
    ##  3      2  1970 v2x_freexp_altinf 0.89   0.004  
    ##  4      2  1971 v2x_freexp_altinf 0.867 -0.023  
    ##  5      2  1972 v2x_freexp_altinf 0.884  0.017  
    ##  6      2  1973 v2x_freexp_altinf 0.908  0.024  
    ##  7      2  1974 v2x_freexp_altinf 0.924  0.016  
    ##  8      2  1975 v2x_freexp_altinf 0.93   0.006  
    ##  9      2  1976 v2x_freexp_altinf 0.938  0.00800
    ## 10      2  1977 v2x_freexp_altinf 0.936 -0.00200
    ## # … with 48,710 more rows

``` r
# make wide again
dv_with_diffs <- dv_semi_long %>%
  pivot_wider(names_from = variable, values_from = c(value, diff_y2y)) %>%
  arrange(gwcode, year) %>%
  # take out "value_" prefix
  setNames(names(.) %>% str_replace("value_", "")) %>%
  # move "diff_y2y_" prefix to "_diff_y2y" suffix
  setNames(names(.) %>% str_replace("diff_y2y_([a-z0-9\\_]+)", "\\1_diff_y2y"))
  
dv_with_diffs
```

    ## # A tibble: 8,120 x 14
    ## # Groups:   gwcode [174]
    ##    gwcode  year v2x_freexp_alti… v2x_horacc_osp v2x_pubcorr v2x_veracc_osp
    ##     <dbl> <dbl>            <dbl>          <dbl>       <dbl>          <dbl>
    ##  1      2  1968            0.858          0.906       0.917          0.852
    ##  2      2  1969            0.886          0.906       0.917          0.853
    ##  3      2  1970            0.89           0.912       0.941          0.866
    ##  4      2  1971            0.867          0.912       0.962          0.866
    ##  5      2  1972            0.884          0.911       0.962          0.864
    ##  6      2  1973            0.908          0.953       0.962          0.865
    ##  7      2  1974            0.924          0.955       0.962          0.873
    ##  8      2  1975            0.93           0.955       0.962          0.891
    ##  9      2  1976            0.938          0.955       0.962          0.904
    ## 10      2  1977            0.936          0.948       0.962          0.908
    ## # … with 8,110 more rows, and 8 more variables: v2xcl_rol <dbl>,
    ## #   v2xcs_ccsi <dbl>, v2x_freexp_altinf_diff_y2y <dbl>,
    ## #   v2x_horacc_osp_diff_y2y <dbl>, v2x_pubcorr_diff_y2y <dbl>,
    ## #   v2x_veracc_osp_diff_y2y <dbl>, v2xcl_rol_diff_y2y <dbl>,
    ## #   v2xcs_ccsi_diff_y2y <dbl>

``` r
# make sure we did not mess up the original data values
stopifnot(
  all.equal(cor(dv_with_diffs$v2x_freexp_altinf, dv$v2x_freexp_altinf), 1)
)

# Mimic what dv_BaseDatFun does, but without the hidden data read
dv_base_dat <- function(dv_name, dv_data) {
  dat <- dv_data %>%
    ungroup() %>%
    select(gwcode, year, contains(dv_name)) %>%
    rename(var = all_of(dv_name),
           var_y2y = paste0(dv_name, "_diff_y2y")) %>%
    filter(complete.cases(.)) %>%
    mutate(case = case_when(var_y2y > 0 ~ "up",
                            var_y2y < 0 ~ "down",
                            TRUE ~ "no change"),
           case = as.factor(case))
  dat
}

cp <- list()
for (dv_name in dv_vars) {
  cat("\n\n", dv_name, "\n\n")
  cp[[dv_name]] <- as_tibble(c(
    indicator = dv_name, 
    dv_cutpointFun(dv_base_dat(dv_name, dv_with_diffs))
  ))
  print(cp[[dv_name]])
}
```

    ## 
    ## 
    ##  v2x_veracc_osp 
    ## 
    ## # A tibble: 1 x 4
    ##   indicator         all    up  down
    ##   <chr>           <dbl> <dbl> <dbl>
    ## 1 v2x_veracc_osp 0.0831  0.08  0.08
    ## 
    ## 
    ##  v2xcs_ccsi 
    ## 
    ## # A tibble: 1 x 4
    ##   indicator     all    up  down
    ##   <chr>       <dbl> <dbl> <dbl>
    ## 1 v2xcs_ccsi 0.0565  0.05  0.05
    ## 
    ## 
    ##  v2xcl_rol 
    ## 
    ## # A tibble: 1 x 4
    ##   indicator    all    up  down
    ##   <chr>      <dbl> <dbl> <dbl>
    ## 1 v2xcl_rol 0.0451  0.04  0.04
    ## 
    ## 
    ##  v2x_freexp_altinf 
    ## 
    ## # A tibble: 1 x 4
    ##   indicator            all    up  down
    ##   <chr>              <dbl> <dbl> <dbl>
    ## 1 v2x_freexp_altinf 0.0566  0.05  0.05
    ## 
    ## 
    ##  v2x_horacc_osp 
    ## 
    ## # A tibble: 1 x 4
    ##   indicator         all    up  down
    ##   <chr>           <dbl> <dbl> <dbl>
    ## 1 v2x_horacc_osp 0.0605  0.06  0.06
    ## 
    ## 
    ##  v2x_pubcorr 
    ## 
    ## # A tibble: 1 x 4
    ##   indicator      all    up  down
    ##   <chr>        <dbl> <dbl> <dbl>
    ## 1 v2x_pubcorr 0.0400  0.03  0.03

``` r
cp <- bind_rows(cp)
write_csv(cp, "../output-data/cutpoints.csv")
```
