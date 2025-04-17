# Load shiny package
library(shiny)
setwd("/Users/linusblomqvist/Library/CloudStorage/Dropbox/Birding/CBC/sb_cbc_area_map")


# Run app
source("app.R")
shinyApp(ui, server)

# Publish app
library(rsconnect)
deployApp()

