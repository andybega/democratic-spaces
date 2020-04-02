Democratic Spaces Barometer
================

This repo contains the code and data needed to reproduce and update the
Democratic Space Barometer forecasts.

The repo is organized into three self-contained folders:

  - `create-data/`: combine V-Dem and other data sources into the
    historical data that is used to code the dependent variables and
    estimate the forecast models
  - `modelrunner/`: contains the random forest forecast models and will
    generate both the test and live forecasts
  - `dashboard/`: the R Shiny dashboard at the V-Dem website

The folders are self-contained in the sense that each has a copy of the
inputs it needs to run, and will not reach into other folders to pull
code or data. For example, `modelrunner` saves the forecasts to
`output/fcasts-rf.csv`, and `dashboard` contains a copy of these
forecasts in `Data/fcasts-rf-2020.csv`. *(This also means that if there
are any changes in relevant data, they need to be manually copied
over.)*

The `forecasts/` folder contains a record of the forecasts we made in
2019 and updated forecasts from March 2020, when V-Dem version 10 was
released.

## Setup

The R packages needed to run all code in this repo are listed in
`required-packages.txt`. To check and install them all, try:

``` r
packs <- readLines("required-packages.txt")
need  <- packs[!packs %in% rownames(installed.packages())]
need
install.packages(need)
```

## Reproducing the forecasts

1.  In `create-data/`, run the data munging scripts in the `scripts/`
    folder to recreate the final data, `output-data/states.rds`. See
    `create-data/README.md` for more details.
2.  In `modelrunner/`, run `R/rf.R` to run the forecast models and
    create the test and live forecasts. See `modelrunner/README.md` for
    more details.
3.  Update the forecast data in `dashboard`.

## Updates

*Note: the v9 to v10 update process was before some file changes and
cleaning, so probably future updates will require adjusting and updating
the steps below.*

To update the forecasts:

1.  Update the input data sources in `create-data/input` as needed.
2.  Re-run each data munging script in `create-data/scripts`. This will
    probably require some changes here and there, e.g. if sets of
    missing values have changed, and thus should be done interactively
    (i.e. don’t source the scripts and assume everything is ok).
3.  Copy the updated `states.rds` data to `modelrunner/input/`.
4.  Adjust the end year in `modelrunner/R/rf.R` line 56 and re-run.
5.  Copy `modelrunner/output/fcasts-rf.csv` to `dashboard/data/`.

## System info

``` r
sessionInfo()
```

    ## R version 3.6.1 (2019-07-05)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS Catalina 10.15.4
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_3.6.1  magrittr_1.5    tools_3.6.1     htmltools_0.3.6
    ##  [5] yaml_2.2.0      Rcpp_1.0.4      stringi_1.4.3   rmarkdown_1.18 
    ##  [9] knitr_1.25      stringr_1.4.0   xfun_0.11       digest_0.6.25  
    ## [13] evaluate_0.14

``` r
packs <- readLines("required-packages.txt")
installed <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)
packs <- installed[installed$Package %in% packs, c("Package", "Version")]
rownames(packs) <- NULL
packs
```

    ##         Package    Version
    ## 1     demspaces      0.2.2
    ## 2      doFuture      0.9.0
    ## 3         dplyr      0.8.5
    ## 4        future     1.16.0
    ## 5        glmnet     2.0-18
    ## 6          here        0.1
    ## 7   highcharter      0.7.0
    ## 8      jsonlite        1.6
    ## 9       leaflet      2.0.3
    ## 10          lgr      0.3.3
    ## 11     magrittr        1.5
    ## 12       ranger     0.11.2
    ## 13        readr      1.3.1
    ## 14        rgdal      1.4-4
    ## 15        rgeos      0.5-1
    ## 16   rmapshaper      0.4.1
    ## 17           sf      0.8-0
    ## 18        shiny      1.3.2
    ## 19      shinyBS       0.61
    ## 20 shinyWidgets      0.5.0
    ## 21        skimr      1.0.7
    ## 22       states 0.2.2.9007
    ## 23      stringr      1.4.0
    ## 24       tibble      2.1.3
    ## 25        tidyr      1.0.0
    ## 26    tidyverse      1.2.1
    ## 27          zoo      1.8-6
