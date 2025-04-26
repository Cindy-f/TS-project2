library(tidyverse)
library(fpp3)
library(stlplus)



data_table = read.csv("data2.csv",col.names = c("Time", "Unemployment"))

data_table_ts <- tsibble(Month_index = yearmonth(seq(as.Date("2003-01-01"), 
                                                     as.Date("2025-02-01"), 
                                                     by = "1 month")),
                         Time = data_table$Time, 
                         Unemployment = data_table$Unemployment,
                         index = Month_index)
Month <- c(rep(1:12,22), 1, 2)
data_table_ts$Month <- as.factor(Month)


data_table_ts = data_table_ts |>
  mutate(logUnemployment = log(Unemployment))

# assign NA values to pandemic
data_table_ts$Unemployment[203:230] = NA
data_table_ts$logUnemployment[203:230] = NA

# create test and train dataset
n = nrow(data_table_ts)
n_test = 24
n_train = n - n_test

data_table_ts_train = data_table_ts[1:n_train,]
data_table_ts_test = data_table_ts[(n_train+1):n,]

# seasonally adjust log(Unemployment) using stlplus
logUnemployment_ts = ts(data_table_ts_train$logUnemployment, frequency = 12)
components = stlplus(logUnemployment_ts, s.window = "periodic", robust=TRUE)
data_table_ts_train$season = components$data$seasonal
data_table_ts_train$logA = components$data$raw - components$data$seasonal

# Use ARIMA to select the best model
result_ARIMA_select <- data_table_ts_train %>% 
  model(ARIMA(logA ~ PDQ(0,0,0), stepwise=FALSE, approximation=FALSE, trace=FALSE))
report(result_ARIMA_select)

# compute the forecast for logA
result_ARIMA_select_forecast <- result_ARIMA_select %>% forecast(h=n_test)
data_table_ts_test <- data_table_ts_test %>% 
  add_column(Fcast_logA = result_ARIMA_select_forecast$.mean)

# compute the forecast for season
data_table_ts_test <- data_table_ts_test %>% 
  add_column(Fcast_season = c(data_table_ts_train$season[(n_train-11):n_train],
                              data_table_ts_train$season[(n_train-11):n_train]))

# compute forecast of log(Unemployment) and Unemployment
data_table_ts_test <- data_table_ts_test %>% 
  mutate(Fcast_logUnemployment = Fcast_logA + Fcast_season,
         Fcast_Unemployment = exp(Fcast_logUnemployment))

# Compute root mean squared error
data_table_ts_test <- data_table_ts_test %>% mutate(Error = Unemployment - Fcast_Unemployment)
RMSE <- sqrt(sum(data_table_ts_test$Error^2)/24)
RMSE

# Compute mean absolute error
MAE <- sum(abs(data_table_ts_test$Error)/24)
MAE