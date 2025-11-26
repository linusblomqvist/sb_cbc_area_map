# Load shiny package
library(shiny)
setwd("~/Documents/sb_cbc_area_map")


# Run app
source("app.R")
shinyApp(ui, server)

# Publish app
library(rsconnect)
rsconnect::deployApp(
  appFiles = c(
    "app.R",
    "output/sb_cbc_areas.rds",
    "species_groups.R",
    "google-analytics.Rhtml",
    "species_encoded_dec_jan.txt"
  )
)
