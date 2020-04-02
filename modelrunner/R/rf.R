#
#   Run random forest model
#
#   Because these models take longer to run (about 2.75 hours for a single year
#   and outcome on one thread), this script is set up to chunk by outcome-year
#   and then later re-combine the outcome-year chunks into one superset of 
#   forecasts similar in structure to the other model runners. 
#
#   **To start a fresh re-run, delete the year chunks in rf-chunks**
#

t0 <- proc.time()

library("dplyr")
library("lgr")
library("readr")
library("future")
library("doFuture")
library("jsonlite")
library("demspaces")
library("ranger")
library("here")

setwd(here::here("modelrunner"))

# chunks will be saved to this directory
chunk_dir <- "output/rf-chunks"
model_dir <- "output/rf-models"

# Setup directories, in case they don't exist
dir.create("output", showWarnings = FALSE)
dir.create("output/log", showWarnings = FALSE)
dir.create(chunk_dir, showWarnings = FALSE)
dir.create(model_dir, showWarnings = FALSE)

# Setup log file
timestamp <- Sys.Date()
log_file  <- sprintf("output/log/rf-%s.txt", timestamp)
lgr$add_appender(AppenderFile$new(log_file))

lgr$info("Running random forest model")
lgr$info("R package 'demspaces' version %s", packageVersion("demspaces"))

registerDoFuture()
plan(multisession(workers = availableCores()))

# redo existing chunks?
OVERWRITE = FALSE
set.seed(12348)

states <- readRDS("input/states.rds")

# Iterate over outcomes
outcomes <- c("v2x_veracc_osp", "v2xcs_ccsi", "v2xcl_rol", "v2x_freexp_altinf",
              "v2x_horacc_osp", "v2x_pubcorr")
years <- 2005:2019

model_grid <- expand.grid(outcome = outcomes, year = years, stringsAsFactors = FALSE)
model_grid$chunk <- paste0(model_grid$outcome, "_", model_grid$year)
model_grid$outfile <- file.path(chunk_dir, paste0(model_grid$chunk, ".csv"))
model_grid$modelfile <- file.path(model_dir, paste0(model_grid$chunk, ".rds"))
model_grid$time <- NA_real_

# shuffle so workers get more even work (hopefully)
model_grid <- model_grid[sample(1:nrow(model_grid)), ]
model_grid$id <- 1:nrow(model_grid)
model_grid <- model_grid[, c("id", "outcome", "year", "chunk", "outfile", "modelfile", "time")]

# Save model grid without timing info, in case something goes wrong
mg <- model_grid[, c("id", "outcome", "year")]
write_csv(mg, "output/rf-model-grid.csv")

if (!OVERWRITE) {
  done   <- dir(chunk_dir, full.names = TRUE)
  model_grid <- model_grid[!model_grid$outfile %in% done, ]
}


model_grid <- foreach(i = 1:nrow(model_grid),
                      .combine = bind_rows,
                      .export = c("model_grid")) %dopar% {
  
  t0 <- proc.time()
  
  id_i      <- model_grid$id[[i]]
  outcome_i <- model_grid$outcome[[i]]
  year_i    <- model_grid$year[[i]]
  chunk_i   <- model_grid$chunk[[i]]
  outfile_i <- model_grid$outfile[[i]]
  modfile_i <- model_grid$modelfile[[i]]
  
  tt <- year_i
  lgr$info("Start model %s", id_i)
  
  states_t <- states %>% filter(year <= tt)
  
  train_data <- states_t %>%
    ungroup() %>%
    filter(year < max(year))
  test_data  <- states_t %>%
    ungroup() %>%
    filter(year == max(year))
  
  mdl      <- ds_rf(outcome_i, train_data, num.threads = 1, 
                    num.trees = 900, min.node.size = 1)
  fcasts_i <- predict(mdl, new_data = test_data)
  
  runtime <- round((proc.time() - t0)["elapsed"])
  model_grid$time[i] <- runtime
  
  write_rds(mdl, modfile_i)
  write_csv(fcasts_i, outfile_i)
  
  # Score chunk
  score <- score_ds_fcast(fcasts_i, states)
  score <- tidyr::unite(score, Measure, Measure, Direction)
  ss <- as.list(score$Value)
  names(ss) <- score$Measure
  
  # log finish
  lgr$info("Finished model %s; time: %ss; performance: %s",
           id_i,
           runtime,
           jsonlite::toJSON(ss, auto_unbox = TRUE, digits = 3))
  
  model_grid[i, ]
}

# Save model grid with timing info
model_grid <- model_grid[, c("id", "outcome", "year", "time")]
write_csv(model_grid, "output/rf-model-grid.csv")

# Combine model chunks into one set
chunk_files <- dir(chunk_dir, full.names = TRUE)
chunks      <- lapply(chunk_files, readr::read_csv)
fcasts_y    <- do.call(rbind, chunks)

write_csv(fcasts_y, "output/fcasts-rf.csv")

score <- score_ds_fcast(fcasts_y, states)
score <- tidyr::unite(score, Measure, Measure, Direction)
ss <- as.list(score$Value)
names(ss) <- score$Measure
lgr$info("Finished random forest model")
lgr$info("Total script run time: %ss", round((proc.time() - t0)["elapsed"]))
lgr$info("Performance: %s", jsonlite::toJSON(ss, auto_unbox = TRUE, digits = 3))

warn <- warnings()
if (length(warn) > 1) {
  call <- as.character(warn)
  msg  <- names(warn)
  warn_strings <- paste0("In ", call, " : ", msg)
  lgr$warn("There were R warnings, printing below:")
  for (x in warn_strings) lgr$warn(x)
}

