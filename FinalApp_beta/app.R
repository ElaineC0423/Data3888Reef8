library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.providers)
library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(ggplot2)
library(caret)
library(randomForest)
library(plotly)
library(forecast)
library(iml)
library(glmnet)
library(ggplotify)


# Read data
reef_merged <- read.csv("reef_merged.csv") %>% distinct()
merged_effort <- read.csv("merged.csv")
regression_fish <- read.csv("three_selected_fish.csv")
regression_fish$binary_bleaching <- ifelse(regression_fish$average_bleaching > 0, 1, 0)
formula <- as.formula("binary_bleaching ~ yellowfin_tuna_rate_norm + scombroids_rate_norm + skipjack_tuna_rate_norm")
model <- glm(formula, data = regression_fish, family = "binomial")




# 生成预测概率
regression_fish$predicted_prob <- predict(model, regression_fish, type = "response")


ui <- fluidPage(
  navbarPage("Reef Analysis",
             tabPanel("Home",
                      fluidRow(
                        column(6, leafletOutput("map", height = "500px"), style = "border: 1px solid gray;"),
                        column(6, 
                               fluidRow(
                                 plotOutput("catch_bleaching", height = "250px", width = "100%"),
                                 plotOutput("year_bleaching", height = "250px", width = "100%")
                               ),
                               style = "border: 1px solid gray;"
                        )
                      )
             ),
             tabPanel("Model Prediction",
                      fluidRow(
                        column(3, wellPanel(
                          numericInput("ssta", "Sea Surface Temperature Anomaly (SSTA):", value = 0, min = -5, max = 5),
                          numericInput("fish", "Fishing Rate:", value = 0, min = 0, max = 100),
                          numericInput("distance", "Distance to Nearest Reef (km):", value = 0, min = 0, max = 100),
                          actionButton("predict", "Predict")
                        )),
                        column(9,
                               fluidRow(
                                 column(12,
                                        verbatimTextOutput("prediction_result"),
                                        
                                 )
                               ),
                               fluidRow(
                                 column(12,
                                        plotlyOutput("interactive_plot", height = "500px")
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(3, wellPanel(
                          numericInput("yellowfin_tuna_rate_norm", "Yellowfin Tuna RateNorm:", value = NULL),
                          numericInput("scombroids_rate_norm", "Scombroids RateNorm:", value = NULL),
                          numericInput("skipjack_tuna_rate_norm", "Skipjack Tuna RateNorm:", value = NULL),
                          actionButton("predict_fish", "Predict")
                        )),
                        column(9,
                               fluidRow(
                                 column(12,
                                        verbatimTextOutput("Fish_prediction_result"),
                                        
                                 )
                               ),
                               fluidRow(
                                 column(12,
                                        plotlyOutput("Fish_interactive_plot", height = "500px")
                                 )
                               )
                        )
                      )
                      
                      
             ),
             
             tabPanel(
               "Graph",
               tabsetPanel(
                 tabPanel(
                   "Importance of Fish Species and Environment",
                   fluidRow(
                     column(6, plotOutput("shap_plot2")),
                     column(6, plotOutput("shap_plot"))
                   )
                 ),
                 tabPanel(
                   "Overall Relationship between Fish and Bleaching / Bleaching Over the Years",
                   fluidRow(
                     column(6, wellPanel(
                       sliderInput("num_years", "Select number of years to predict:", min = 1, max = 10, value = 5)
                     )),
                     column(6, plotOutput("bleaching_by_year"))
                   ),
                   fluidRow(
                     column(4, wellPanel(
                       selectInput("selected_fish", "Select a fish:", 
                                   choices = c("Yellowfin Tuna" = "yellowfin_tuna_rate_norm",
                                               "Scombroids" = "scombroids_rate_norm",
                                               "Skipjack Tuna" = "skipjack_tuna_rate_norm"))
                     )),
                     column(8, plotOutput("bleaching_by_fish"))
                   )
                 )
               )
             )
             
             
  )
)

server <- function(input, output, session) {
  # Split the data into a training and testing set
  split_indices <- createDataPartition(merged_effort$average_bleaching, p = 0.8, list = FALSE)
  training_set <- merged_effort[split_indices, ]
  testing_set <- merged_effort[-split_indices, ]
  training_set$bleaching_occurred <- as.factor(ifelse(training_set$average_bleaching > 0, 1, 0))
  testing_set$bleaching_occurred <- as.factor(ifelse(testing_set$average_bleaching > 0, 1, 0))
  rf_model <- randomForest(bleaching_occurred ~ clim_sst + rate_norm + distance_to_nearest_reef,
                           data = training_set)
  rf_predictor <- Predictor$new(rf_model, data = testing_set[, c("clim_sst", "rate_norm", "distance_to_nearest_reef")], y = as.numeric(testing_set$bleaching_occurred), type = "prob")
  rf_importance <- iml::FeatureImp$new(rf_predictor, loss = "ce")
  
  split_indices_fish <- createDataPartition(regression_fish$average_bleaching, p = 0.8, list = FALSE)
  training_set_fish <- regression_fish[split_indices_fish, ]
  testing_set_fish <- regression_fish[-split_indices_fish, ]
  training_set_fish$bleaching_occurred <- as.factor(ifelse(training_set_fish$average_bleaching > 0, 1, 0))
  testing_set_fish$bleaching_occurred <- as.factor(ifelse(testing_set_fish$average_bleaching > 0, 1, 0))
  rf_model_fish <- randomForest(bleaching_occurred ~ yellowfin_tuna_rate_norm + scombroids_rate_norm + skipjack_tuna_rate_norm,
                           data = training_set_fish)
  rf_predictor_fish <- Predictor$new(rf_model_fish, data = testing_set_fish[, c("yellowfin_tuna_rate_norm", "scombroids_rate_norm", "skipjack_tuna_rate_norm")], y = as.numeric(testing_set_fish$bleaching_occurred), type = "prob")
  rf_importance_fish <- iml::FeatureImp$new(rf_predictor_fish, loss = "ce")
  
  model_prediction <- reactive({
    req(input$predict)
    
    input_data <- data.frame(
      clim_sst = input$ssta,
      rate_norm = input$fish,
      distance_to_nearest_reef = input$distance
    )
    pred <- predict(rf_model, input_data)
    
    pred
  })
  
  new_points <- reactiveVal(data.frame())
  
  observeEvent(input$predict, {
    new_point <- data.frame(
      clim_sst = input$ssta,
      rate_norm = input$fish,
      distance_to_nearest_reef = input$distance,
      bleaching_occurred = model_prediction(),
      stringsAsFactors = FALSE
    )
    
    # Colour for New point
    new_point$point_color <- ifelse(new_point$bleaching_occurred == 2, "blue","red" )
    new_point$point_label <- ifelse(new_point$bleaching_occurred == 2, "Bleaching occurred", "No bleaching")
    
    new_points(rbind(new_points(), new_point))
    
    
    
    output$interactive_plot <- renderPlotly({
      p <- plot_ly() %>%
        add_trace(data = training_set %>% filter(bleaching_occurred == 0), x = ~clim_sst, y = ~rate_norm, z = ~distance_to_nearest_reef, color = "green", type = "scatter3d", mode = "markers", name = "No Bleaching", marker = list(symbol = "circle", size = 6, line = list(color = "black", width = 1))) %>%
        add_trace(data = training_set %>% filter(bleaching_occurred == 1), x = ~clim_sst, y = ~rate_norm, z = ~distance_to_nearest_reef, color = "yellow", type = "scatter3d", mode = "markers", name = "Bleaching Occurred", marker = list(symbol = "circle", size = 6, line = list(color = "black", width = 1))) 
      
      if (nrow(new_points()) > 0) {
        p <- p %>%
          add_trace(data = new_points(), x = ~clim_sst, y = ~rate_norm, z = ~distance_to_nearest_reef, color = ~point_color, text = ~point_label, type = "scatter3d", mode = "markers+text", name = "New Points", marker = list(symbol = "circle", size = 8, line = list(color = "black", width = 2)))
      }
      
      p <- p %>% layout(legend = list(traceorder = "normal", itemsizing = "constant"))
      
      p
    })
    
  })
  
  
  output$prediction_result <- renderPrint({
    req(input$predict)
    pred_result <- model_prediction()
    pred_numeric <- as.numeric(levels(pred_result))[pred_result] # Convert factor to numeric
    print(pred_numeric)
    
    if (pred_numeric == 1) { # Update the condition to check the numeric value
      cat("Predicted: Reef health declined\n")
    } else {
      cat("Predicted: Reef health increased\n")
    }
  })
  
  
  
  
  
  reef_data_counts <- reef_merged %>%
    group_by(reef_id) %>%
    dplyr::summarise(data_count = n()) %>%
    ungroup()
  
  leaflet_data <- reactive({
    left_join(reef_merged, reef_data_counts, by = "reef_id") %>%
      dplyr::select(latitude, longitude, reef_id, data_count)
  })
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet_data() %>%
      mutate(fill_color = case_when(
        data_count <= 2 ~ "gray",
        TRUE ~ "red"
      )) %>%
      leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        layerId = ~reef_id,
        popup = ~as.character(reef_id),
        radius = 6, fillColor = ~fill_color,
        stroke = FALSE, fillOpacity = 1
      )
  })
  
  # Create a reactive expression for clicked_reef
  clicked_reef <- reactiveVal(NULL)
  
  observeEvent(input$map_marker_click, {
    req(input$map_marker_click)
    clicked_reef_id <- as.character(input$map_marker_click$id)
    clicked_reef(reef_merged %>% filter(reef_id == clicked_reef_id))
  })
  
  # Catch vs. Bleaching plot
  output$catch_bleaching <- renderPlot({
    req(clicked_reef())
    ggplot(clicked_reef(), aes(x = sum_effort, y = average_bleaching)) +
      geom_point() + 
      geom_line(aes(group = 1), linetype = "dashed", color = "blue") +
      labs(x = "Fishing Effort", y = "Average Bleaching")
  })
  
  # Year vs. Bleaching plot
  output$year_bleaching <- renderPlot({
    req(clicked_reef())
    ggplot(clicked_reef(), aes(x = year, y = average_bleaching)) +
      geom_point() +
      geom_line(aes(group = 1), linetype = "dashed", color = "blue") +
      labs(x = "Year", y = "Average Bleaching")
  })
  
  # Bleaching by Temperature
  output$bleaching_by_temp <- renderPlot({
    temp_bleaching <- merged_effort %>%
      filter(clim_sst > 270) %>%
      group_by(clim_sst) %>%
      dplyr::summarise(mean_bleaching = mean(average_bleaching, na.rm = TRUE))
    
    ggplot(temp_bleaching, aes(x = clim_sst, y = mean_bleaching)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(x = "Temperature", y = "Average Bleaching")
  })
  
  output$bleaching_by_year <- renderPlot({
    req(input$num_years)
    
    num_years <- input$num_years
    
    # Prepare the yearly_bleaching data
    yearly_bleaching <- merged_effort %>%
      filter(clim_sst >= 270) %>%
      group_by(year) %>%
      dplyr::summarise(mean_bleaching = mean(average_bleaching, na.rm = TRUE))
    
    
    
    # Fit ARIMA model
    # Fit ARIMA model with custom parameters
    # Perform first-order differencing
    # Perform first-order differencing
    yearly_bleaching$mean_bleaching_diff <- c(NA, diff(yearly_bleaching$mean_bleaching))
    
    
    arima_model <- arima(yearly_bleaching$mean_bleaching_diff, order = c(1,1,0)) # Here you can change the order parameters (p,d,q) as per your requirement
    
    # Forecast the next n years
    n_years <- input$num_years
    forecasted_years <- forecast(arima_model, h = n_years)
    
    # Combine the original and predicted data
    yearly_bleaching_forecasted <- bind_rows(
      yearly_bleaching,
      data.frame(
        year = base::seq((base::max(yearly_bleaching$year, na.rm = TRUE) + 1), (base::max(yearly_bleaching$year, na.rm = TRUE) + n_years), by = 1),
        mean_bleaching = as.numeric(forecasted_years$mean),
        stringsAsFactors = FALSE
      )
    )
    
    #Plot the combined data with ARIMA model predictions
    ggplot(yearly_bleaching_forecasted, aes(x = year, y = mean_bleaching)) +
      geom_point() +
      geom_line() +
      labs(x = "Year", y = "Mean Bleaching")
    
    

  })
  
  
  output$shap_plot <- renderPlot({
    
    plot(rf_importance_fish)
    
  })
  
  output$shap_plot2 <- renderPlot({
    
    plot(rf_importance)
    
  })
  
  
  fish_model_prediction <- reactive({
    req(input$predict_fish)
    
    input_data <- data.frame(
      yellowfin_tuna_rate_norm = input$yellowfin_tuna_rate_norm,
      scombroids_rate_norm = input$scombroids_rate_norm,
      skipjack_tuna_rate_norm = input$skipjack_tuna_rate_norm
    )
    pred <- predict(model, input_data, type = "response") 
    
    pred
  })
  
  
  output$Fish_prediction_result <- renderPrint({
    req(fish_model_prediction())
    pred <- fish_model_prediction()
    threshold <- 0.5 
    bleaching_occurrence <- ifelse(pred > threshold, "Coral bleaching occurred", "Coral bleaching did not occurre")
    paste("Prediction:", bleaching_occurrence)
  })
  
  output$bleaching_by_fish <- renderPlot({
    req(input$selected_fish)
    selected_fish <- input$selected_fish
    
    fish_data <- regression_fish %>%
      dplyr::select(average_bleaching, all_of(selected_fish)) %>%
      dplyr::rename(FishEffort = selected_fish)
    
    fish_lm <- lm(average_bleaching ~ FishEffort, data = fish_data)
    
    ggplot(fish_data, aes(x = FishEffort, y = average_bleaching)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(
        x = "Fishing Effort",
        y = "Bleaching",
        title = paste("Bleaching by", input$selected_fish)
      )
  })
  
  
}

shinyApp(ui = ui, server = server)