rm(list=ls())
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


train <- read_csv("food_trainx.csv")[,-1]

# --- Create a binary promotion variable ---
train$promotions <- ifelse(train$emailer_for_promotion == 1 | train$homepage_featured == 1, "Yes", "No")
train <- train |>
  #filter(center_id== 41)|>
  mutate(profit = ifelse(promotions == "No", 
                         0.5*checkout_price*num_orders,0.4*checkout_price*num_orders),
         promotion_flag = case_when(
           emailer_for_promotion == 0 & homepage_featured == 0 ~ "No promotion",
           emailer_for_promotion == 1 & homepage_featured == 0 ~ "Email promotion",
           emailer_for_promotion == 0 & homepage_featured == 1 ~ "Homepage promotion",
           emailer_for_promotion == 1 & homepage_featured == 1 ~ "Double promotion"
         ))

# --------------------------------------------
# Step 1: Convert Data into Time Series Format
# --------------------------------------------
# Aggregate by center and week

store <- 36
forecast_horizon <- 10  # Forecast next 10 weeks

df <- train |> 
  filter(center_id== store)|>
  group_by(week, center_id) |>
  summarise(num_orders = sum(num_orders),
            promo_email = sum(ifelse(promotion_flag == "Email promotion", 1, 0)),  # Create promotion indicators
            promo_web = sum(ifelse(promotion_flag== "Homepage promotion", 1, 0)),
            promo_double = sum(ifelse(promotion_flag == "Double promotion", 1, 0)),
            .groups = "drop") |>
  spread(center_id, num_orders)

dfs <- train |> 
  filter(center_id== store)|>
  group_by(week, center_id) |>
  summarise(num_orders = sum(num_orders),
            promo_email = sum(ifelse(emailer_for_promotion == 1 & homepage_featured == 0, 1, 0)),  # Create promotion indicators
            promo_web = sum(ifelse(emailer_for_promotion == 0 & homepage_featured == 1, 1, 0)),
            promo_double = sum(ifelse(emailer_for_promotion == 1 & homepage_featured == 1, 1, 0)),
            .groups = "drop") |>
  spread(center_id, num_orders)


colnames(df)[5] <- "store_id"

df_ts <- df[1: (nrow(df)-forecast_horizon ),]
df_test <- df[(nrow(df)-(forecast_horizon-1)):nrow(df),]


# Extract Promotion Regressors (Matrix Format)
promo_data <- df_ts |>
  select(promo_email, promo_web, promo_double) |>
  as.matrix()

# Convert data into time series format
ts_data <- ts(df_ts[, 5], start = min(df_ts$week), frequency = 1)  # Weekly frequency

# --------------------------------------------
# Step 2: Check for Stationarity & Transform Data
# --------------------------------------------
adf_test <- adf.test(ts_data[,1])  # ADF Test for first center

if (adf_test$p.value > 0.05) {
  cat("Data is non-stationary, applying differencing...\n")
  ts_data <- diff(ts_data, differences = 1)  # First-order differencing
  promo_data <- promo_data[-1,]  # Adjust regressor matrix to match new data length
} else {
  cat("Data is already stationary.\n")
}

# Apply Log Transformation to Handle Skewness
ts_data <- log(ts_data + 1)

# --------------------------------------------
# Step 3: Fit ARIMA Model with Promotions as Regressors
# --------------------------------------------
arima_model <- auto.arima(ts_data[,1], xreg = promo_data, seasonal = FALSE)
summary(arima_model)
# --------------------------------------------
# Step 4: Forecast Future Demand with Scenario Testing
# --------------------------------------------

# **Scenario 1: No Future Promotions (Baseline)**
future_promo_none <- matrix(rep(0, 10 * ncol(promo_data)), nrow = 10, ncol = ncol(promo_data))
forecast_none <- forecast(arima_model, xreg = future_promo_none, h = forecast_horizon)

# **Scenario 2: Mild Promotions (Continue Current Promotion Levels)**
avg_promo <- colMeans(promo_data)  # Average past promotion frequency
future_promo_mild <- matrix(rep(avg_promo, each = 10), nrow = 10, ncol = ncol(promo_data))
forecast_mild <- forecast(arima_model, xreg = future_promo_mild, h = forecast_horizon)

# **Scenario 3: Aggressive Double Promotions**
future_promo_aggressive <- future_promo_none  # Start with no promotions
future_promo_aggressive[,3] <- max(promo_data[,3])  # Maximize double promotions
forecast_aggressive <- forecast(arima_model, xreg = future_promo_aggressive, h = forecast_horizon)

# --------------------------------------------
# Step 5: Convert Predictions Back to Original Scale
# --------------------------------------------
forecast_values_none <- exp(forecast_none$mean) - 1
forecast_values_mild <- exp(forecast_mild$mean) - 1
forecast_values_aggressive <- exp(forecast_aggressive$mean) - 1

# --------------------------------------------
# Step 6: Visualize Forecast Results
# --------------------------------------------
df_forecast <- data.frame(
  Weeks = seq_along(forecast_values_none),
  No_Promotion = forecast_values_none,
  Mild_Promotion = forecast_values_mild,
  Aggressive_Promotion = forecast_values_aggressive,
  real_num_orders = df_test$store_id
)

df_forecast_long <- df_forecast |>
  pivot_longer(cols = -Weeks, names_to = "Scenario", values_to = "Forecasted_Orders")

# Plot Comparison of Forecasts
ggplot(df_forecast_long, aes(x = Weeks, y = Forecasted_Orders, color = Scenario)) +
  geom_line(linewidth = 1) +
  labs(title = paste0("Demand Forecasting with Promotion Scenarios for Store: ", store),
       x = "Predicted future Weeks",
       y = "Predicted num_orders") +
  theme_minimal()

# Combine forecast and original data 
df_forecast_long <- df_forecast_long|>mutate(Weeks = as.numeric(Weeks) +max(df_ts$week))
x <- df_ts |> mutate(Weeks = week,
                     Scenario = "real_num_orders",
                     Forecasted_Orders = store_id)|>
  select(Weeks,Scenario, Forecasted_Orders)
rbind(x, df_forecast_long)|>
  filter(Weeks>100)|>
  ggplot(aes(x = Weeks, y = Forecasted_Orders, color = Scenario)) +
  geom_line(linewidth = 1) +
  labs(title = paste0("Demand Forecasting with Promotion Scenarios for Store: ", store),
       x = "Predicted future Weeks",
       y = "Predicted num_orders") +
  theme_minimal()
