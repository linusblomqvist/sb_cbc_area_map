library(shiny)
library(leaflet)
library(sf)
library(tidyverse)

sf_data <- readRDS("sb_cbc_areas.rds")

ui <- fluidPage(
  tags$head(includeHTML("google-analytics.Rhtml")),
  titlePanel("Santa Barbara CBC Areas"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select an area:"),
      selectInput(
        inputId = "polygon_select",
        label = NULL,
        choices = c("None", sf_data$Name),  # Populate directly with area names
        selected = "None"
      ),
      
      actionButton(
        inputId = "reset_map",
        label = "Reset map"
      ),
      
      hr(),
      h4("Area description"),
      uiOutput("description_text"),  # Display polygon description
      hr(),
      h4("eBird hotspot information"),
      uiOutput("hotspot_links"),  # Display links to hotspots
      hr(),
      h4("Target species"),
      uiOutput("recent_observations"),  # Display recent observations link
      hr(),
      h4("Shareable link"),
      uiOutput("shareable_link")  # Display shareable link
    ),
    
    mainPanel(
      leafletOutput("map", width = "100%", height = "500px")  # Map output
    )
  ),
  
  # Footer Section
  tags$footer(
    style = "position: relative; margin-top: 20px; width: 100%; background-color: #f8f9fa; padding: 10px; text-align: center; font-size: 12px; color: #6c757d;",
    "For more information about Santa Barbara County Christmas Bird Count circles, click ",
    tags$a(
      href = "https://santabarbaraaudubon.org/santa-barbara-christmas-bird-count/",
      target = "_blank",
      "here."
    ),
    " For more information about eBird hotspots, see ",
    tags$a(
      href = "https://birdinghotspots.org/region/US-CA-083",
      target = "_blank",
      "birdinghotspots.org."
    )
  )
)

server <- function(input, output, session) {
  # Reactive values for description and recent observations link
  selected_description <- reactiveVal("Select an area to see its description.")
  selected_birdview_url <- reactiveVal(NULL)
  
  # Parse query string and set the area at startup
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$area)) {
      decoded_area <- URLdecode(query$area)
      if (decoded_area %in% sf_data$Name) {
        updateSelectInput(session, "polygon_select", selected = decoded_area)
      }
    }
  })
  
  # Handle area selection
  observeEvent(input$polygon_select, {
    req(input$polygon_select)
    
    if (input$polygon_select == "None") {
      # Reset values when "None" is selected
      selected_description("Select an area to see its description.")
      selected_birdview_url(NULL)
      
      # Clear highlights on the map
      leafletProxy("map") %>%
        clearGroup("highlight")
    } else {
      # Filter the selected polygon
      selected_polygon <- sf_data %>% filter(Name == input$polygon_select)
      
      if (nrow(selected_polygon) > 0) {
        # Update description and birdview URL
        selected_description(selected_polygon$Description)
        selected_birdview_url(selected_polygon$birdview_url)
        
        # Highlight the selected polygon on the map
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
        
        # Zoom to the selected polygon's bounding box
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
  
  # Render map
  output$map <- renderLeaflet({
    leaflet(sf_data) %>%
      # Add Satellite tiles as the default layer
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite View") %>%
      # Add Street View tiles
      addTiles(group = "Street View") %>%
      # Add polygons
      addPolygons(
        data = sf_data,
        color = "blue",
        fillColor = "lightblue",
        fillOpacity = 0.5,
        weight = 1,
        label = ~Name,
        popup = ~Name,
        layerId = ~Name,
        highlight = highlightOptions(
          weight = 3,
          color = "red",
          bringToFront = TRUE
        )
      ) %>%
      # Add layer control to switch between views
      addLayersControl(
        baseGroups = c("Satellite View", "Street View"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Render area description
  output$description_text <- renderUI({
    HTML(selected_description())
  })
  
  # Render hotspot links
  output$hotspot_links <- renderUI({
    req(input$polygon_select)
    selected_polygon <- sf_data %>% filter(Name == input$polygon_select)
    
    if (nrow(selected_polygon) > 0) {
      links <- selected_polygon$hyperlinks
      
      if (!is.na(links)) {
        HTML(paste0("<p>", links, "</p>"))
      } else {
        HTML("<p>No hotspots available for this area.</p>")
      }
    } else {
      HTML("<p>Select an area to see its associated hotspots.</p>")
    }
  })
  
  # Render recent observations link
  output$recent_observations <- renderUI({
    req(input$polygon_select)
    selected_polygon <- sf_data %>% filter(Name == input$polygon_select)
    
    if (nrow(selected_polygon) > 0) {
      recent_url <- selected_polygon$birdview_url
      dec_jan_url <- selected_polygon$birdview_dec_jan
      
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
    } else {
      HTML("<p>No recent observations available.</p>")
    }
  })
  
  # Render shareable link
  output$shareable_link <- renderUI({
    req(input$polygon_select)
    if (input$polygon_select != "None") {
      shareable_url <- paste0(
        "https://linusblomqvist.shinyapps.io/sb_cbc_area_map/?area=",
        URLencode(input$polygon_select)
      )
      tags$p(
        tags$a(
          href = shareable_url,
          target = "_blank",
          rel = "noopener noreferrer",
          "Shareable link to this area"
        )
      )
    } else {
      HTML("<p>Select an area to generate a shareable link.</p>")
    }
  })
  
  # Reset map button
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
