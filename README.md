Democratic Spaces Barometer
================

_NOTE: this repo has moved to https://github.com/vdeminstitute/demspaces_

******

The Demoractic Spaces Barometer forecasts significant changes, both
democratizing and autocratizing, for six facets of democratic governance
for all major countries in the world 2 years ahead. The current
forecasts cover 2020-2021 and can be explored with the dashboard at
<https://www.v-dem.net/en/analysis/DemSpace/>.

This repo contains the code and data needed to reproduce the forecasts
and dashboard. It is organized into three self-contained folders:

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
`required-packages.txt`. Two packages need special treatment:

  - **demspaces** is not on CRAN. This package contains some custom
    model wrappers to make it easier to work with the 12 outcome
    variables we are modeling. It can be installed with:
    
    ``` r
    remotes::install_github("andybega/demspaces")
    ```

  - **states** needs to be at least verion 0.2.2.9007, which by the time
    you are reading this may be met by the version on CRAN. If not, you
    can also install the development version from GitHub:
    
    ``` r
    # Check the package version
    packageVersion("states")
    if (packageVersion("states") < "0.2.2.9007") {
      remotes::install_github("andybega/states")
    }
    ```

To check for and install the remaining packages, try:

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

## Citation

Andreas Beger, Richard K. Morgan, and Laura Maxwell, 2020, “The
Democratic Spaces Barometer: global forecasts of autocratization and
democratization”, <https://www.v-dem.net/en/analysis/DemSpace/>

``` bibtex
@misc{beger2020democratic,
  author = {Beger, Andreas and Morgan, Richard K. and Maxwell, Laura},
  title = {The Democratic Spaces Barometer: Global Forecasts of Autocratization and Democratization},
  year  = {2020},
  url   = {https://www.v-dem.net/en/analysis/DemSpace/},
}
```

## Contributing

We welcome any error and bug reports dealing with mistakes in the
existing code and data. Please open an issue or email Andreas Beger at
[adbeger@gmail.com](mailto:adbeger+demspaces@gmail.com).

This repo is not under active development and mainly serves for the sake
of transparency and to allow reproduction of the forecasts and
dashboard. There is no plan for continuing development aside from,
potentially, annual forecast updates in the future. It is thus unlikely
that more substantive feedback, like suggestions about additional
features/predictors or alternative models, would be incorporated unless
you do most of the legwork and can clearly demonstrate improved
performance. This is not meant as discouragement, we simply don’t have
the resources to put more time in this and want to prevent
disappointment.

## Acknowledgement

The Democratic Space Barometer is the product of a collaboration between
Andreas Beger ([Predictive
Heuristics](https://www.predictiveheuristics.com)), Richard K. Morgan
([V-Dem](https://www.v-dem.net/en/)), and Laura Maxwell
([V-Dem](https://www.v-dem.net/en/)).

The six conceptual dimensions we focus on come from the International
Republican Institute’s Closing Space Barometer, which includes an
analytical framework for assessing the processes that facilitate a
substantial reduction (closing events) within these six spaces. This
framework was developed based on a series of workshops conducted with
Democracy, Human Rights, and Governance (DRG) donors and implementing
partners in 2016 and represent the conceptual features of democratic
space which are most amenable to DRG assistance programs.

We adapted these conceptual spaces, expanded the scope to include
substantial improvements (opening events), and developed an
operationalization method to identify opening and closing events within
each space. This dashboard, and the forecast that drive it, is the
output of these efforts.

## System info

``` r
sessionInfo()
```

    ## R version 4.0.0 (2020-04-24)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Catalina 10.15.4
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_4.0.0  magrittr_1.5    tools_4.0.0     htmltools_0.4.0
    ##  [5] yaml_2.2.1      Rcpp_1.0.4.6    stringi_1.4.6   rmarkdown_2.1  
    ##  [9] knitr_1.28      stringr_1.4.0   xfun_0.13       digest_0.6.25  
    ## [13] rlang_0.4.6     evaluate_0.14

``` r
packs <- readLines("required-packages.txt")
installed <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)
packs <- installed[installed$Package %in% packs, c("Package", "Version")]
rownames(packs) <- NULL
packs
```

    ##      Package    Version
    ## 1   doFuture      0.9.0
    ## 2      dplyr      0.8.5
    ## 3     future     1.17.0
    ## 4     glmnet      3.0-2
    ## 5       here        0.1
    ## 6   jsonlite      1.6.1
    ## 7    leaflet      2.0.3
    ## 8        lgr      0.3.4
    ## 9   magrittr        1.5
    ## 10    ranger     0.12.1
    ## 11     readr      1.3.1
    ## 12     rgeos      0.5-2
    ## 13        sf      0.9-2
    ## 14     shiny    1.4.0.2
    ## 15    states 0.2.2.9008
    ## 16   stringr      1.4.0
    ## 17    tibble      3.0.1
    ## 18     tidyr      1.0.2
    ## 19 tidyverse      1.3.0
    ## 20       zoo      1.8-7
