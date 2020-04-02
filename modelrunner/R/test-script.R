#
#   Test script to make sure all models are running
#
#   This is mainly to test out the Docker container works as expected, 
#   without running a very long actual model script. 
#
#   Rscript test-script.R
#
#   If it breaks, see output/test for artifacts
#

suppressPackageStartupMessages({
  library("dplyr")
  library("lgr")
  library("readr")
  library("future")
  library("future.apply")
  library("jsonlite")
  library("demspaces")
  library("glmnet")
  library("ranger")
})

dir.create("output", showWarnings = FALSE)
dir.create("output/test", showWarnings = FALSE)
dir.create("output/test/log", showWarnings = FALSE)

timestamp <- Sys.Date()
log_file  <- sprintf("output/test/log/test-%s.txt", timestamp)
lgr$add_appender(AppenderFile$new(log_file))

lgr$info("Starting tests")

lgr$info("Loading data")
states <- readRDS("input/states.rds")

yy <- "v2x_veracc_osp"
tt <- 2005

states_t <- states %>% 
  filter(year >= 1995) %>%
  filter(year <= tt) %>%
  select(gwcode:lag0_v2x_polyarchy, lag0_log_state_age, lag0_log_pop, lag2_log_gdp_pc)

train_data <- states_t %>%
  ungroup() %>%
  filter(year < max(year))
test_data  <- states_t %>%
  ungroup() %>%
  filter(year == max(year))

lgr$info("Plain GLM")
mdl <- ds_logistic_reg(yy, train_data, normalize = FALSE)
fcasts <- predict(mdl, new_data = test_data)
write_csv(fcasts, "output/test/fcasts-logistic-reg.csv")

lgr$info("GLM with PCA")
mdl    <- ds_logistic_reg_featx(yy, train_data, n_comp = 40)
fcasts <- predict(mdl, new_data = test_data)
write_csv(fcasts, "output/test/fcasts-logistic-reg-featx.csv")

lgr$info("glmnet")
mdl <- ds_reg_logreg(yy, train_data, 
                     folds = 5, alpha_n = 3, cost = "auc",
                     lambda = "min")
fcasts <- predict(mdl, new_data = test_data)
write_csv(fcasts, "output/test/fcasts-reg-logreg.csv")

lgr$info("RF/ranger")
mdl      <- ds_rf(yy, train_data, num.threads = 1)
fcasts <- predict(mdl, new_data = test_data)
write_csv(fcasts, "output/test/fcasts-rf.csv")


lgr$info("Test successful, cleaning up artifacts")
unlink("output/test", recursive = TRUE)

