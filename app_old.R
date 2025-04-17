library(shiny)
library(leaflet)
library(sf)
library(tidyverse)

sf_data <- readRDS("sb_cbc_areas.rds")

ui <- fluidPage(
  titlePanel("Santa Barbara CBC Areas"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select an area:"),
      selectInput(
        inputId = "polygon_select",
        label = NULL,
        choices = c("None", sf_data$Name), # Add "None" as the first option
        selected = "None"                  # Set "None" as the default selected value
      ),
      actionButton(
        inputId = "reset_map",
        label = "Reset map"
      ),
      hr(),
      h4("Area description:"),
      uiOutput("description_text")      # Display polygon description
    ),
    
    mainPanel(
      leafletOutput("map", width = "100%", height = "500px") # Map output
    )
  ),
  
  # Footer Section
  tags$footer(
    style = "position: relative; margin-top: 20px; width: 100%; background-color: #f8f9fa; padding: 10px; text-align: center; font-size: 12px; color: #6c757d;",
    "For more information about the Santa Barbara Christmas Bird Count, click ",
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
  # Compute the bounding box of all polygons
  full_bbox <- st_bbox(sf_data)
  full_lng1 <- as.numeric(full_bbox$xmin)
  full_lat1 <- as.numeric(full_bbox$ymin)
  full_lng2 <- as.numeric(full_bbox$xmax)
  full_lat2 <- as.numeric(full_bbox$ymax)
  
  output$map <- renderLeaflet({
    leaflet(sf_data) %>%
      # Add default tile layer (OpenStreetMap)
      addTiles(group = "Default View") %>%
      # Add satellite view layer
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Satellite View"
      ) %>%
      # Add polygons
      addPolygons(
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
      # Fit the initial view to encompass all polygons
      fitBounds(
        lng1 = full_lng1,
        lat1 = full_lat1,
        lng2 = full_lng2,
        lat2 = full_lat2
      ) %>%
      # Add layer control
      addLayersControl(
        baseGroups = c("Default View", "Satellite View"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Reactive value to store the selected polygon's description
  selected_description <- reactiveVal("Select an area to see its description.")
  
  # Observe when a polygon is selected from the dropdown
  observeEvent(input$polygon_select, {
    if (input$polygon_select == "None") {
      # If "None" is selected, reset to full view and clear highlight
      leafletProxy("map") %>%
        clearGroup("highlight") %>%
        fitBounds(
          lng1 = full_lng1,
          lat1 = full_lat1,
          lng2 = full_lng2,
          lat2 = full_lat2
        )
      selected_description("Select an area to see its description.")
    } else {
      # Zoom to the selected polygon
      selected_polygon <- sf_data[sf_data$Name == input$polygon_select, ]
      
      if (nrow(selected_polygon) > 0) {
        bbox <- st_bbox(selected_polygon)
        lng1 <- as.numeric(bbox$xmin)
        lat1 <- as.numeric(bbox$ymin)
        lng2 <- as.numeric(bbox$xmax)
        lat2 <- as.numeric(bbox$ymax)
        
        leafletProxy("map") %>%
          clearGroup("highlight") %>%
          addPolygons(
            data = selected_polygon,
            color = "red",
            fillColor = "orange",
            fillOpacity = 0.5,
            weight = 2,
            group = "highlight"
          ) %>%
          fitBounds(lng1, lat1, lng2, lat2)
        
        # Update the description
        selected_description(selected_polygon$Description)
      }
    }
  })
  
  # Reset button: Reset the map to its full extent and clear highlights
  observeEvent(input$reset_map, {
    updateSelectInput(session, "polygon_select", selected = "None")
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      fitBounds(
        lng1 = full_lng1,
        lat1 = full_lat1,
        lng2 = full_lng2,
        lat2 = full_lat2
      )
    selected_description("Select an area to see its description.")
  })
  
  # Update the description text
  output$description_text <- renderUI({
    req(input$polygon_select)  # Ensure a polygon is selected
    
    if (input$polygon_select == "None") {
      HTML("<p>Select a polygon to see its description.</p>")
    } else {
      selected_polygon <- sf_data[sf_data$Name == input$polygon_select, ]
      
      if (nrow(selected_polygon) > 0) {
        HTML(selected_polygon$Description)  # Render HTML content
      } else {
        HTML("<p style='color: red;'>Description not available.</p>")
      }
    }
  })
}

shinyApp(ui, server)
