library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)

reef <- read.csv("Reef_Check_with_cortad_variables_with_annual_rate_of_SST_change.csv")
fish_data <- read.csv("all_fish_data.csv")

ui <- fluidPage(
  titlePanel("Interactive Map with Year Slider"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Select Year:", min(fish_data$year), max(fish_data$year), value = min(fish_data$year), step = 1),
      plotOutput("barPlot")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    list(
      fish = fish_data %>% filter(year == input$yearInput),
      reef = reef %>% filter(year == input$yearInput)
    )
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(fish_data$longitude), lat = mean(fish_data$latitude), zoom = 10)
  })
  
  observe({
    leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = filtered_data()$fish,
        lng = ~longitude, lat = ~latitude,
        radius = 5,
        color = "red",
        stroke = FALSE, fillOpacity = 0.8,
        group = "circleMarkers"
      ) %>%
      addCircleMarkers(
        data = filtered_data()$reef,
        lng = ~Longitude.Degrees, lat = ~Latitude.Degrees,
        radius = ~Average_bleaching / 2,  # Adjust the size by changing the divisor
        color = ~colorNumeric("viridis", filtered_data()$reef$Average_bleaching)(Average_bleaching),
        stroke = FALSE, fillOpacity = 0.8,
        group = "reefMarkers",
        popup = ~paste("Average Bleaching:", round(Average_bleaching, 2))
      )
  })
  
  output$barPlot <- renderPlot({
    req(input$map_marker_click)
    clicked_data <- filtered_data()$fish[filtered_data()$fish$latitude == input$map_marker_click$lat & filtered_data()$fish$longitude == input$map_marker_click$lng, ]
    
    fish_families <- c("Labridae", "Mullidae", "Scaridae")
    biomass_values <- c(clicked_data$biomass_kgha_by_fish_family_avg_Labridae, clicked_data$biomass_kgha_by_fish_family_avg_Mullidae, clicked_data$biomass_kgha_by_fish_family_avg_Scaridae)
    
    df <- data.frame(fish_families, biomass_values)
    
    ggplot(df, aes(x = fish_families, y = biomass_values)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = paste0("Biomass for ", clicked_data$site_name),
           x = "Fish Family",
           y = "Biomass") +
      theme_minimal()
  })
  
}

shinyApp(ui = ui, server = server)
