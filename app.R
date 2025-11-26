library(shiny)
library(leaflet)
library(sf)
library(tidyverse)

# Load spatial data
sf_data <- readRDS("output/sb_cbc_areas.rds")

# Load encoded species query string(s)
source("species_groups.R", local = TRUE)

ui <- fluidPage(
  tags$head(includeHTML("google-analytics.Rhtml")),
  titlePanel("Santa Barbara CBC Areas"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select an area:"),
      selectInput(
        inputId = "polygon_select",
        label = NULL,
        choices = c("None", sf_data$Name),
        selected = "None"
      ),
      actionButton("reset_map", "Reset map"),
      hr(),
      h4("Area description"),
      uiOutput("description_text"),
      hr(),
      h4("eBird hotspot information"),
      uiOutput("hotspot_links"),
      hr(),
      h4("Target species"),
      uiOutput("recent_observations"),
      hr(),
      h4("Shareable link"),
      uiOutput("shareable_link")
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = "500px")
    )
  ),
  
  tags$footer(
    style = "position: relative; margin-top: 20px; width: 100%; background-color: #f8f9fa; padding: 10px; text-align: center; font-size: 12px; color: #6c757d;",
    "For more information about Santa Barbara County Christmas Bird Count circles, click ",
    tags$a(href = "https://santabarbaraaudubon.org/santa-barbara-christmas-bird-count/", target = "_blank", "here."),
    " For more information about eBird hotspots, see ",
    tags$a(href = "https://birdinghotspots.org/region/US-CA-083", target = "_blank", "birdinghotspots.org.")
  )
)

server <- function(input, output, session) {
  selected_description <- reactiveVal("Select an area to see its description.")
  selected_birdview_url <- reactiveVal(NULL)
  
  # Handle query string on load
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$area)) {
      decoded_area <- URLdecode(query$area)
      if (decoded_area %in% sf_data$Name) {
        updateSelectInput(session, "polygon_select", selected = decoded_area)
      }
    }
  })
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet(sf_data) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite View") %>%
      addTiles(group = "Street View") %>%
      addPolygons(
        data = sf_data,
        color = "blue",
        fillColor = "lightblue",
        fillOpacity = 0.5,
        weight = 1,
        label = ~Name,
        popup = ~Name,
        layerId = ~Name,
        highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE)
      ) %>%
      addLayersControl(
        baseGroups = c("Satellite View", "Street View"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Click on a polygon to update the selectInput
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    req(click$id)
    updateSelectInput(session, "polygon_select", selected = as.character(click$id))
  })
  
  # Update based on polygon selection
  observeEvent(input$polygon_select, {
    req(input$polygon_select)
    
    if (input$polygon_select == "None") {
      selected_description("Select an area to see its description.")
      selected_birdview_url(NULL)
      
      leafletProxy("map") %>%
        clearGroup("highlight")
    } else {
      selected_polygon <- sf_data %>% filter(Name == input$polygon_select)
      
      if (nrow(selected_polygon) > 0) {
        selected_description(selected_polygon$Description)
        
        leafletProxy("map") %>%
          clearGroup("highlight") %>%
          addPolygons(
            data = selected_polygon,
            color = "red",
            fillColor = "red",
            fillOpacity = 0.5,
            weight = 3,
            group = "highlight",
            label = ~Name
          )
        
        bbox <- st_bbox(selected_polygon)
        leafletProxy("map") %>%
          fitBounds(
            lng1 = as.numeric(bbox$xmin),
            lat1 = as.numeric(bbox$ymin),
            lng2 = as.numeric(bbox$xmax),
            lat2 = as.numeric(bbox$ymax)
          )
      }
    }
  })
  
  output$description_text <- renderUI({
    HTML(selected_description())
  })
  
  output$hotspot_links <- renderUI({
    req(input$polygon_select)
    selected_polygon <- sf_data %>% filter(Name == input$polygon_select)
    
    if (nrow(selected_polygon) > 0 && !is.na(selected_polygon$hyperlinks)) {
      HTML(paste0("<p>", selected_polygon$hyperlinks, "</p>"))
    } else {
      HTML("<p>No hotspots available for this area.</p>")
    }
  })
  
  # --- BirdView v5 URLs using coords_formatted as latlon ---
  output$recent_observations <- renderUI({
    req(input$polygon_select)
    selected_polygon <- sf_data %>% dplyr::filter(Name == input$polygon_select)
    
    if (nrow(selected_polygon) == 0) {
      return(HTML("<p>No recent observations available.</p>"))
    }
    
    ## 1) Build latlon parameter from coords_formatted ----
    # coords_formatted is plain: "lat,lon lat,lon lat,lon"
    latlon_raw <- as.character(selected_polygon$coords_formatted[1])
    
    # Encode: "," -> "%2C", " " -> "%20"
    latlon_param <- latlon_raw
    latlon_param <- gsub(",", "%2C", latlon_param, fixed = TRUE)
    latlon_param <- gsub(" ", "%20", latlon_param, fixed = TRUE)
    
    ## 2) Get centroid for map center (lat/lon) ----
    poly <- selected_polygon
    if (!is.na(sf::st_crs(poly)$epsg) && sf::st_crs(poly)$epsg != 4326) {
      poly <- sf::st_transform(poly, 4326)
    }
    centroid <- sf::st_centroid(poly$geometry)
    cc <- sf::st_coordinates(centroid)
    lon <- cc[1]
    lat <- cc[2]
    zoom <- 9.5
    
    ## 3) Species query ----
    species_query <- species_groups[["dec_jan_special"]]
    
    ## 4) Base BirdView v5 URL ----
    base_url <- paste0(
      "https://s3.us-west-1.amazonaws.com/membot.com/BirdView.html?v=5",
      "&county=Santa%20Barbara%20Central%20Coast%20California%20United%20States%20(US)%20North%20America"
    )
    
    ## 5) Build the two URLs ----
    recent_url <- paste0(
      base_url,
      "&latlon=", latlon_param,
      "&withinArea=0",
      "&source=",
      "&", species_query,
      "&photosOnly=0",
      "&otherTaxa=0",
      "&latin=0",
      "&loc=",
      "&hotspotsOnly=0",
      "&nearby=0",
      "&date=2025-Dec%20-%202026-Jan",
      "&birder=",
      "&groupMode=checklists",
      "&listby=taxon",
      "&lat=", lat,
      "&lon=", lon,
      "&zoom=", zoom
    )
    
    dec_jan_url <- paste0(
      base_url,
      "&latlon=", latlon_param,
      "&withinArea=0",
      "&source=",
      "&", species_query,
      "&photosOnly=0",
      "&otherTaxa=0",
      "&latin=0",
      "&loc=",
      "&hotspotsOnly=0",
      "&nearby=0",
      "&date=Dec-Jan",
      "&birder=",
      "&groupMode=checklists",
      "&listby=taxon",
      "&lat=", lat,
      "&lon=", lon,
      "&zoom=", zoom
    )
    
    tagList(
      tags$p(
        "Click ",
        tags$a(href = recent_url, target = "_blank", "here"),
        " for recent observations of target species within this area."
      ),
      tags$p(
        "To view historical records of target species in December and January in this area, click ",
        tags$a(href = dec_jan_url, target = "_blank", "here.")
      )
    )
  })
  
  output$shareable_link <- renderUI({
    req(input$polygon_select)
    if (input$polygon_select != "None") {
      shareable_url <- paste0(
        "https://linusblomqvist.shinyapps.io/sb_cbc_area_map/?area=",
        URLencode(input$polygon_select)
      )
      tags$p(tags$a(
        href = shareable_url,
        target = "_blank",
        rel = "noopener noreferrer",
        "Shareable link to this area"
      ))
    } else {
      HTML("<p>Select an area to generate a shareable link.</p>")
    }
  })
  
  observeEvent(input$reset_map, {
    updateSelectInput(session, "polygon_select", selected = "None")
    bbox <- st_bbox(sf_data)
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      fitBounds(
        lng1 = as.numeric(bbox$xmin),
        lat1 = as.numeric(bbox$ymin),
        lng2 = as.numeric(bbox$xmax),
        lat2 = as.numeric(bbox$ymax)
      )
    selected_description("Select an area to see its description.")
    selected_birdview_url(NULL)
  })
}

shinyApp(ui, server)