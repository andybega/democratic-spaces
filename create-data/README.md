Data cleaning and merging
=========================

Code to produce the closing spaces dataset (`output-data/states.rds`). 

There is a little bit of logic to the organization here:

- `scripts/`: contains scripts the create the DV and merged dataset
  + `0-DV_high_low_data_build.R`: 
  + `1-dv_notes.Rmd`: 
  + `2-combine-data.Rmd`: this script merges the DV and IV data sources together, producing as primary output the dataset in `output-data/states.rds`. 
- `R/`: R files containing functions used in the scripts
- `output-data`: artifacts created by the scripts, i.e. the final merged data and so on

The goal is to forecast for 2019 and 2020, using data from 1970 onwards. The data are structured so that the DV's are 2-year leads, i.e. records for 2018 have the DV forecasts for 2019-2020. Note that this is a different setup than we used in PART. 

The variable names follow a common scheme. For the 2 year dependent variables, the suffix is:

  - `..._next2`, indicating a event of interest within the next 2 years.

All DVs that **should not be used** as model inputs are prefixed with:

  - `dv...`

If there is no prefix or the previs is `lagX` they can be used as model inputs. Most variables are prefixed with `lagX` to indicate how much they were lagged in order to get up to 2018. 

  - `lagX...`



