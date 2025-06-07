#rm(list=ls())
library(shiny)
library(shinythemes)
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
library(xgboost)  
#library(patchwork)
library(cowplot)
source("~/Github/ARDS/helper_ARDS.R")
source("~/data/helper.R")

dfx <- read_csv("food_trainx.csv")[,c(2:16)]

dfs <- dfx |> 
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
ui <- navbarPage(
  theme = shinytheme('flatly'),
  inverse = F, 
  id = "master_store",
  # App Title
  tags$div(tags$img(src='mi_logo.png', width = 75, height = 50, style="float:left; margin-left: 5px; margin-right: 5px; margin-top: -12px")), 
  
  
  # Demand Forecasting ----
  tabPanel("Demand Forecasting",
           fluidRow(
             column(7,
                    wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                              plotOutput("plot", height = 680, click = "plot_click"))),
             column(5,
                    fluidRow(column(12, align = 'center',
                                    wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                                              p(tags$b("Select a Store", style = "font-size: 122%")),
                                              #br(),
                                              fluidRow(column(12, align = 'center',
                                                              radioButtons(inputId = 'obs',
                                                                          label = "Store Number:",
                                                                          choices = c(13, 42:43, 55, 86, 94, 110 ), selected = 55,
                                                                          inline = T, 
                                                                          width = "400px"),
                                                       hr(),
                                                       column(6, align = 'center',
                                                              sliderInput(inputId = "forecast_horizon", label = "Number of Weeks to Forecast: ", 
                                                                          min = 3, max = 10, value= 5, step = 1)),
                                                       column(4, align = 'left',
                                                              radioButtons(inputId = 'methods',
                                                                           label = "Forecasting Methods:",
                                                                           choices = c("Ets", "Naive", "Tbats", "xgboost"), selected = "Ets",
                                                                           inline = T, 
                                                                           width = "400px"))
                                                       
                                              )),
                                              br(),
                                              hr(),
                                              fluidRow(column(12, align= "center",
                                                              p(tags$b("Profile of Store :", style = "font-size: 80 %")),
                                                              #verbatimTextOutput("caption")
                                                              h5(textOutput("caption", container = span))
                                              ),
                                              hr(),
                                              fluidRow(column(12, align = "center",
                                                              #p(tags$b("Compare to the Market", style= "font-size: 100%")),
                                                              plotOutput("plot_delme", height = 280, click = "plot_click")))))))))),

  # Leaderboards and Historical Tables -------------------------------------------------------
  tabPanel("Tables",
           fluidRow(
             column(6,
                    wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 775px;",
                              fluidRow(style = "margin-top: 25px;",
                                       column(12, 
                                              p(tags$b('Store Demand Predictions vs Real Results', style = "font-size: 150%; font-family:Helvetica; color:#4c4c4c; text-align:left;")))),
                              br(),
                              tableOutput("view"))),
             column(4,
                    wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 775px;",
                              fluidRow(style = "margin-top: 25px;",
                                       column(12, 
                                              p(tags$b('Store Data Summary', style = "font-size: 150%; font-family:Helvetica; color:#4c4c4c; text-align:left;")))),
                              hr(),
                              verbatimTextOutput("summary")))
             )),
  # About Tab ------------------------------------------------
  tabPanel("About", icon = icon("bars"),
           fluidRow(
             column(12,
                    wellPanel(style = "background-color: #fff; border-color: #2c3e50;",
                              includeHTML("test.html")
                    )))))

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
    arima_model    <- auto.arima(ts_data[,1], xreg = promo_data(), seasonal = FALSE)
    ets_model      <- ets(ts_data[,1])
    naive_model    <- naive(ts_data[,1])
    tbats_model    <- tbats(ts_data[,1])

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
    
    # Forecast with other models
    forecast_ets   <- data.frame(forecast(ets_model,  h = input$forecast_horizon))
    forecast_naive <- data.frame(forecast(naive_model,  h = input$forecast_horizon))
    forecast_tbats <- data.frame(forecast(tbats_model,  h = input$forecast_horizon))
    
    
    # Convert Predictions Back to Original Scale
    forecast_values_none          <- exp(forecast_none$mean) - 1
    forecast_values_mild          <- exp(forecast_mild$mean) - 1
    forecast_values_aggressive    <- exp(forecast_aggressive$mean) - 1
    forecast_values_ets           <- exp(forecast_ets$Point.Forecast) - 1
    forecast_values_naive         <- exp(forecast_naive$Point.Forecast) - 1
    forecast_values_tbats         <- exp(forecast_tbats$Point.Forecast) - 1
 
    df_forecast <- data.frame(
      Weeks = seq_along(forecast_values_none),
      No_Promotion = forecast_values_none,
      Mild_Promotion = forecast_values_mild,
      Aggressive_Promotion = forecast_values_aggressive,
      Ets = forecast_values_ets,
      Naive  =forecast_values_naive ,
      Tbats =forecast_values_tbats,
      real_num_orders =  c(dt_test()[, paste0("X",input$obs)]) ) #,real_num_orders = dt_test()$store_id
      
    colnames(df_forecast)[8] <- "real_num_orders"
    df_forecast
    #promo_data()
  })
  
  combo <- reactive({ 
    
    df_forecast_long <- forecast_dt() |>pivot_longer(cols = -Weeks, names_to = "Scenario", values_to = paste0("X",input$obs))|>
                                          mutate(week = as.numeric(Weeks) +max(dt_ts()$week)) |>select(-Weeks)
    

    x <- dt_ts() |> mutate(Scenario = "real_num_orders") |> select(-c(promo_email,promo_web, promo_double))
    
    combo <- dplyr::full_join(x, df_forecast_long)
    colnames(combo)[2] <- "Forecasted_Orders"
    combo

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
    a <- dfx|>filter(center_id == as.numeric(gsub("X","", input$obs)))
    cus <- unique(a$cuisine)
    print(paste0("The store is ", unique(a$center_type),
          " locates in the code of ", unique(a$city_code),
          " with operation area of ", unique(a$op_area)," miles." ,check_res))
    
    
  })
  
  # Generate a plot of the forecast dataset ----
  # Fit ARIMA Model with Promotions as Regressors

  output$plot_delme <- renderPlot({
    dfx|>
      mutate(
        promotion_flag = case_when(
          emailer_for_promotion == 0 & homepage_featured == 0 ~ "No promotion",
          emailer_for_promotion == 1 & homepage_featured == 0 ~ "Email promotion",
          emailer_for_promotion == 0 & homepage_featured == 1 ~ "Homepage promotion",
          emailer_for_promotion == 1 & homepage_featured == 1 ~ "Double promotion"
        ))|>
      group_by(promotion_flag)|>
      dplyr::count(meal_id) |>
      mutate(p = round(100*(n / sum(n)),2))|>
      select(-n)|>
      ggplot(aes(x = reorder(factor(meal_id),p,, decreasing = TRUE),  fill = promotion_flag)) +
      geom_bar(aes(y = p), stat='identity', alpha=0.7) +
      scale_color_manual(values=c('Order Percent'="#d88bb4")) +
      labs(x = '', y = 'Percentage', 
           title = 'Popular Meals During Promotions and No Promotions') +
      theme_minimal()+
      theme(legend.position="bottom",
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank() ,
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+
      guides(fill="none")})
  
  output$plot <- renderPlot({ 
    combo()|>
      filter(week >100,
             Scenario%in% c("real_num_orders","No_Promotion","Aggressive_Promotion","Mild_Promotion",input$methods))|>
      #pivot_longer(cols = -Weeks, names_to = "Scenario", values_to = "Forecasted_Orders")|>
      ggplot(aes(x = week, y = Forecasted_Orders, color = Scenario)) +
      geom_line(linewidth = 1) +
      labs(title = paste0("Demand Forecasting with Promotion Scenarios for Store: ", input$obs),
           x = "Predicted future Weeks",
           y = "Predicted num_orders") +
      theme(
        axis.text.x=element_blank(),
        axis.text=element_text(size=18),
        legend.position = "bottom",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())}
    )

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
    head(
      #dtInputx() 
      forecast_dt() #,n = input$forecast_horizon
      )
  })
}

# Create Shiny app ----
shinyApp(ui, server)
