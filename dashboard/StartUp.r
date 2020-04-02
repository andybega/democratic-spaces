# install.packages("shiny")

library("here")

setwd(here::here("4-dashboard"))

need <- c("shiny", "leaflet", "highcharter", "shinyWidgets", "shinyBS") #
have <- installed.packages()[, "Package"]
out <- sapply(need[!need %in% have], install.packages)

library("shiny")
# ## CHECK WOKING DIRECTORY!
## It should point to "../GitHub/closing-spaces/4-dashboard"

runApp()
