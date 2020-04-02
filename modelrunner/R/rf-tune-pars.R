#
#   Go through all the model chunks and ID the tuned parameters
#
#   

library("ranger")
library("demspaces")
library("readr")
library("stringr")
library("tibble")
library("dplyr")
library("here")

setwd(here::here("modelrunner"))

model_files <- dir("output/rf-models", full.names = TRUE)

extract_pars <- function(mf) {
  mdl <- read_rds(mf)
  up <- bind_cols(
    direction = "up",
    as_tibble(mdl$up_mdl$model[c("num.trees", "mtry", "min.node.size")])
  )
  down <- bind_cols(
    direction = "down",
    as_tibble(mdl$down_mdl$model[c("num.trees", "mtry", "min.node.size")])
  )
  out <- bind_rows(up, down)
  out$outcome <- str_remove(basename(mf), "_[0-9]{4}.rds")
  out$year <- str_extract(basename(mf), "[0-9]{4}")
  
  out <- out[, c("outcome", "direction", "year", "num.trees", "min.node.size", "mtry")]
  out
}

pars <- lapply(model_files, extract_pars)
pars <- bind_rows(pars)

write_csv(pars, "output/rf-pars.csv")

