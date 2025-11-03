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
  
  # NEW: Click on a polygon to update the selectInput (enables zoom + sidebar updates)
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
  
  output$recent_observations <- renderUI({
    req(input$polygon_select)
    selected_polygon <- sf_data %>% filter(Name == input$polygon_select)
    
    if (nrow(selected_polygon) > 0) {
      coords <- selected_polygon$coords_formatted
      species_query <- species_groups[["dec_jan_special"]]
      
      base_url <- "https://s3.us-west-1.amazonaws.com/membot.com/BirdView.html?v=2"
      
      recent_url <- paste0(
        base_url, "&", species_query,
        "&photosOnly=0&taxaLevel=full&rarity=&date=2024-Dec%20-%202025-Jan&loc=&hotspotsOnly=0&latlon=",
        coords,
        "&birder=&source=checklists&groupMode=checklists&listby=taxon"
      )
      
      dec_jan_url <- paste0(
        base_url, "&", species_query,
        "&photosOnly=0&taxaLevel=full&rarity=&date=Dec-Jan&loc=&hotspotsOnly=0&latlon=",
        coords,
        "&birder=&source=checklists&groupMode=checklists&listby=taxon"
      )
      
      tagList(
        tags$p("Click ", tags$a(href = recent_url, target = "_blank", "here"),
               " for recent observations of target species within this area."),
        tags$p("To view historical records of target species in December and January in this area, click ",
               tags$a(href = dec_jan_url, target = "_blank", "here."))
      )
    } else {
      HTML("<p>No recent observations available.</p>")
    }
  })
  
  output$shareable_link <- renderUI({
    req(input$polygon_select)
    if (input$polygon_select != "None") {
      shareable_url <- paste0(
        "https://linusblomqvist.shinyapps.io/sb_cbc_area_map/?area=",
        URLencode(input$polygon_select)
      )
      tags$p(tags$a(href = shareable_url, target = "_blank", rel = "noopener noreferrer",
                    "Shareable link to this area"))
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