# Load shiny package
library(shiny)
setwd("~/Documents/sb_cbc_area_map")


# Run app
source("app.R")
shinyApp(ui, server)

# Publish app
library(rsconnect)
deployApp()

