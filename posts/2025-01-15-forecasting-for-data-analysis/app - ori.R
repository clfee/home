#rm(list=ls())
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(tseries)
library(gridExtra)
library(corrplot)
library(knitr)
library(gganimate)
library(gifski)
library(zoo)
library(lubridate)
library(forecast)
#library(patchwork)
library(cowplot)
source("~/Github/ARDS/helper_ARDS.R")
source("~/data/helper.R")

dfs <- read_csv("food_trainx.csv")[,c(2:16)]

dfs <- dfs |> 
  #filter(center_id== store)|>
  group_by(week, center_id) |>
  summarise(num_orders = sum(num_orders),
            promo_email = sum(ifelse(emailer_for_promotion == 1 & homepage_featured == 0, 1, 0)),  # Create promotion indicators
            promo_web = sum(ifelse(emailer_for_promotion == 0 & homepage_featured == 1, 1, 0)),
            promo_double = sum(ifelse(emailer_for_promotion == 1 & homepage_featured == 1, 1, 0)),
            .groups = "drop")

df_avg <- dfs |> 
  group_by(week) |>
  #spread(center_id, num_orders)
  summarise(average_orders = mean(num_orders))


# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Demand Forecasting"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(

    # Input: Text for providing a caption ----
    # Note: Changes made to the caption in the textInput control
    # are updated in the output area immediately as you type
    textInput(
      inputId = "caption",
      label = "Caption:",
      value = "Data Summary"
    ),
    sliderInput("obs","Store Number to view:", min = 1, max = 140, value= 41, step = 1),
    sliderInput("forecast_horizon","Forecast Horizon", min = 10, max = 30, value= 10, step = 1)
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
  # Output: Formatted text for caption ----
  h3(textOutput("caption", container = span)),

  # Output: Verbatim text for data summary ----
  verbatimTextOutput("summary"),
  
  #Output plot
  plotOutput("plot"),

  # Output: HTML table with requested number of observations ----
  tableOutput("view")
)))  

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

  # Empty reactive values object
  reactive_objects=reactiveValues()
  dtInput  <- reactive({dfs})
  
  dtInputx  <- reactive({
        dtInput() |> 
        filter(center_id== input$obs)|>
        group_by(week, center_id) |>
        mutate(center_id = paste0("X",center_id))|>
        spread(center_id, num_orders)|>
        left_join(df_avg, by = "week")
  })
  
  dt_ts       <- reactive({dtInputx()[1: (nrow(dtInputx())-input$forecast_horizon ),] })
  dt_test     <- reactive({dtInputx()[(nrow(dtInputx())-(input$forecast_horizon-1)):nrow(dtInputx()),]})
  promo_data  <- reactive({ dt_ts() |>
                            select(promo_email, promo_web, promo_double) |>
                            as.matrix() 
                           })
  
  forecast_dt <- reactive({
    ts_data <- ts(dt_ts()[, 5], start = min(dt_ts()$week), frequency = 1) 
    # Apply Log Transformation to Handle Skewness
    ts_data <- log(ts_data + 1)
    # Fit ARIMA Model with Promotions as Regressors
    arima_model <- auto.arima(ts_data[,1], xreg = promo_data(), seasonal = FALSE)
    # Forecast Future Demand with Scenario Testing
    # **Scenario 1: No Future Promotions (Baseline)**
    future_promo_none <- matrix(rep(0, input$forecast_horizon * ncol(promo_data())), nrow = input$forecast_horizon, ncol = ncol(promo_data()))
    forecast_none <- forecast(arima_model, xreg = future_promo_none, h = input$forecast_horizon)
    
    # **Scenario 2: Mild Promotions (Continue Current Promotion Levels)**
    avg_promo <- colMeans(promo_data())  # Average past promotion frequency
    future_promo_mild <- matrix(rep(avg_promo, each = input$forecast_horizon), nrow = input$forecast_horizon, ncol = ncol(promo_data()))
    forecast_mild <- forecast(arima_model, xreg = future_promo_mild, h = input$forecast_horizon)
    
    # **Scenario 3: Aggressive Double Promotions**
    future_promo_aggressive <- future_promo_none  # Start with no promotions
    future_promo_aggressive[,3] <- max(promo_data()[,3])  # Maximize double promotions
    forecast_aggressive <- forecast(arima_model, xreg = future_promo_aggressive, h = input$forecast_horizon)
    
    # Convert Predictions Back to Original Scale
    forecast_values_none <- exp(forecast_none$mean) - 1
    forecast_values_mild <- exp(forecast_mild$mean) - 1
    forecast_values_aggressive <- exp(forecast_aggressive$mean) - 1
 
    df_forecast <- data.frame(
      Weeks = seq_along(forecast_values_none),
      No_Promotion = forecast_values_none,
      Mild_Promotion = forecast_values_mild,
      Aggressive_Promotion = forecast_values_aggressive,
      real_num_orders =  c(dt_test()[, paste0("X",input$obs)]) ) #,real_num_orders = dt_test()$store_id
      
    colnames(df_forecast)[5] <- "real_num_orders"
    df_forecast
    
    
    
    #promo_data()
  })
  
  datasetInput <- reactive({
    switch(
      input$dataset,
      dt_ts()
        ,
      "pressure" = dt_test(),
      "cars" = cars
    )
  })
  
  

  # Create caption to show time series results ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption.  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display

  output$caption <- renderText({
    ts_data <- ts(dt_ts()[, 5], start = min(dt_ts()$week), frequency = 1) 
    adf_test <- adf.test(ts_data[,1])  # ADF Test for first center
    
      if (adf_test$p.value > 0.05) {
        check_res <- "Warning : Data is non-stationary, applying differencing...\n"
        ts_data <- diff(ts_data, differences = 1)  # First-order differencing
        promo_data <- promo_data[-1,]  # Adjust regressor matrix to match new data length
      } else {
        check_res <- ""
      }
    
    paste("Analysis of Store: ", input$obs, check_res)
    
  })
  
  # Generate a plot of the forecast dataset ----
  # Fit ARIMA Model with Promotions as Regressors

  output$plot <- renderPlot({ 
    forecast_dt()|>
      pivot_longer(cols = -Weeks, names_to = "Scenario", values_to = "Forecasted_Orders")|>
      ggplot(aes(x = Weeks, y = Forecasted_Orders, color = Scenario)) +
      geom_line(linewidth = 1) +
      labs(title = paste0("Demand Forecasting with Promotion Scenarios for Store: ", input$obs),
           x = "Predicted future Weeks",
           y = "Predicted num_orders") +
      theme_minimal()})

  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
    dataset <- dt_ts()
    summary(dataset)
  })

  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    head(#dtInputx(), 
      forecast_dt(),
         n = input$forecast_horizon)
  })
}

# Create Shiny app ----
shinyApp(ui, server)
