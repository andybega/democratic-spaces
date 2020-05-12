# install.packages("shiny")

library("shiny")

{
 ##
  ## CHANGE THE WOKING DIRECTORY!

  if (Sys.info()["user"] == "rickm") {
    setwd("C:/Users/rickm/Dropbox/VForecast Beta App/")
  }
  if (Sys.info()["user"] == "xricmo") {
    setwd("C:/Users/xricmo/Dropbox/VForecast Beta App/")
  }
}

runApp("ForecastApp")
