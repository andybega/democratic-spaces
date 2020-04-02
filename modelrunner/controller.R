
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  cmd <- args[1]
  if (is.na(cmd)) cmd <- "help"
  
  if (cmd=="help") {
    cat("R says help\n")
    return(invisible(NULL))
  }
  
  if (cmd=="test") {
    cat("Testing\n")
    source("R/test-script.R")
    return(invisible(NULL))
  }
  
  if (cmd=="rf") {
    cat("Training RF models\n")
    source("R/rf.R")
    return(invisible(NULL))
  }
  
  stop(sprintf("controller.R: something went wrong, no option for command '%s'", cmd),
       call. = FALSE)
}

main()
