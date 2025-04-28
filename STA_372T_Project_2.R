library(ggplot2)
library(tidyverse)
library(fpp3)
library(nortest)
library(forecast)
library(stlplus)

# Perform out-of-sample model validation for model not including the recession

file <- "LNU03000000.csv"
Unemployment_table1 <- read_csv(file)
Unemployment_table1 <- Unemployment_table1[745:926,]




data_table_ts <- tsibble(Month_index = yearmonth(seq(as.Date("2010-01-01"), 
                                                     as.Date("2025-02-01"), 
                                                     by = "1 month")),
                         Time = Unemployment_table1$observation_date, 
                         Unemployment = Unemployment_table1$LNU03000000,
                         index = Month_index)
Month <- c(rep(1:12,15), 1, 2)
data_table_ts$Month <- as.factor(Month)



data_table_ts = data_table_ts |>
  mutate(logUnemployment = log(Unemployment))

data_table_ts$Unemployment[123:146] = NA
data_table_ts$logUnemployment[123:146] = NA


n = nrow(data_table_ts)
n_test = 24
n_train = n - n_test

data_table_ts_train = data_table_ts[1:n_train,]
data_table_ts_test = data_table_ts[(n_train+1):n,]

logUnemployment_ts = ts(data_table_ts_train$logUnemployment, frequency = 12)
components = stlplus(logUnemployment_ts, s.window = 21, robust=TRUE)
data_table_ts_train$season = components$data$seasonal
data_table_ts_train$logA = components$data$raw - components$data$seasonal

result_ARIMA_select <- data_table_ts_train %>% 
  model(ARIMA(logA ~ PDQ(0,0,0), stepwise=FALSE, approximation=FALSE, trace=FALSE))
report(result_ARIMA_select)

result_ARIMA_select_forecast <- result_ARIMA_select %>% forecast(h=n_test)
data_table_ts_test <- data_table_ts_test %>% 
  add_column(Fcast_logA = result_ARIMA_select_forecast$.mean)

data_table_ts_test <- data_table_ts_test %>% 
  add_column(Fcast_season = c(data_table_ts_train$season[(n_train-11):n_train],
                              data_table_ts_train$season[(n_train-11):n_train]))

data_table_ts_test <- data_table_ts_test %>% 
  mutate(Fcast_logUnemployment = Fcast_logA + Fcast_season,
         Fcast_Unemployment = exp(Fcast_logUnemployment))

data_table_ts_test <- data_table_ts_test %>% mutate(Error = Unemployment - Fcast_Unemployment)
RMSE <- sqrt(sum(data_table_ts_test$Error^2)/24)
RMSE

MAE <- sum(abs(data_table_ts_test$Error)/24)
MAE






# Perform out-of-sample model validation for including the recession

file <- "LNU03000000.csv"
Unemployment_table2 <- read_csv("LNU03000000.csv")
With_Recession_table <- Unemployment_table2[661:926,]


data_table_ts <- tsibble(Month_index = yearmonth(seq(as.Date("2003-01-01"), 
                                                     as.Date("2025-02-01"), 
                                                     by = "1 month")),
                         Time = With_Recession_table$observation_date, 
                         Unemployment = With_Recession_table$LNU03000000,
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
components = stlplus(logUnemployment_ts, s.window = 21, robust=TRUE)
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







file <- "LNU03000000.csv"
Unemployment_table2 <- read_csv(file)
With_Recession_table <- Unemployment_table2[661:926,]

Unemployment_table_ts <- With_Recession_table %>%
  add_column(month=yearmonth("2003 January") + 0:265, .before=TRUE) %>%
  as_tsibble(index=month)

# Graph before making any changes
Month_colors <- c("black", "blue", "purple", "red", "orange", "darkred", "green",
                  "chartreuse4", "chocolate", "gray60", "gold4", "salmon")

Month_f <- rep(1:12, length.out = 266)
Unemployment_table_ts$Month_fct = as.factor(Month_f)

Unemployment_table_ts %>% autoplot(LNU03000000) + geom_point(aes(y=LNU03000000, color = Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + ggtitle("Unemployment Levels vs. Month without Recession") + 
  xlab("Months") + ylab("Unemployment Levels (Thousands of People)") + 
  geom_hline(aes(yintercept = mean(LNU03000000)), lty=2)




# Checking if log should be used
Unemployment_table_ts$Log_LNU03000000 <- log(Unemployment_table_ts$LNU03000000)


Unemployment_table_ts %>% autoplot(Log_LNU03000000) + geom_point(aes(y=Log_LNU03000000, color = Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + ggtitle("Log(Unemployment Levels) vs. Month") + 
  xlab("Months") + ylab("Log(Unemployment Levels in Thousands of People)") + 
  geom_hline(aes(yintercept = mean(Log_LNU03000000)), lty=2)


# Removing the Pandemic Period
Unemployment_table_ts$LNU03000000[207:232] <- NA
Unemployment_table_ts$Log_LNU03000000[207:232] <- NA

print(Unemployment_table_ts[204:235,], n=32)

# Graphing Regular Unemployment with Pandemic Removed
Unemployment_table_ts %>% autoplot(LNU03000000) + geom_point(aes(y=LNU03000000, color=Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + 
  ggtitle("Unemployment Levels vs. Time with observations removed") + xlab("Month") + 
  ylab("Unemployment Levels (in Thousands)")



# Graphing Log(Unemployment) with Pandemic Removed
Unemployment_table_ts %>% autoplot(Log_LNU03000000) + geom_point(aes(y=Log_LNU03000000, color=Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + 
  ggtitle("Log(Unemployment Levels in thousands) vs. Time with observations removed") + xlab("Month") +
  ylab("Log(Unemployment Levels in thousands)")


#Seasonally Adjust Log Unemployment
LogUnemployment_time_series <- ts(Unemployment_table_ts$Log_LNU03000000, frequency=12)
components <- stlplus(LogUnemployment_time_series, s.window=21, robust=TRUE)
Unemployment_table_ts$season <- components$data$seasonal
Unemployment_table_ts$LogA <- components$data$raw - components$data$seasonal

print(Unemployment_table_ts[120:148,], n=32)


# LogA vs time

Unemployment_table_ts %>% autoplot(LogA) +
  geom_point(aes(y=LogA, color=Month_fct)) +
  scale_color_manual(values = Month_colors) +
  theme_classic() +
  ggtitle("LogA vs. Month") + xlab("Month") + ylab("LogA")


# Test Stationarity

Unemployment_table_ts %>% ACF(LogA) %>% autoplot()

unitroot_kpss(Unemployment_table_ts$LogA)


# Test First Difference with Recession
Unemployment_table_ts <- Unemployment_table_ts %>% mutate(diff_LogA = 
                                                            difference(LogA))
head(Unemployment_table_ts)
Unemployment_table_ts %>% ACF(diff_LogA) %>% autoplot()
unitroot_kpss(Unemployment_table_ts$diff_LogA)

mean_diff_LogA <- mean(Unemployment_table_ts$diff_LogA, na.rm=TRUE)
Unemployment_table_ts %>% autoplot(diff_LogA) +
  geom_point(aes(y=diff_LogA, color=Month_fct)) +
  geom_hline(aes(yintercept=mean_diff_LogA), lty=2) +
  theme_classic() +
  ggtitle("diff_LogA vs. Time") + xlab("Time") + ylab("diff_LogA")

# Test Second Difference
Unemployment_table_ts <- Unemployment_table_ts %>% mutate(diff2_LogA = difference(diff_LogA))
head(Unemployment_table_ts)

Unemployment_table_ts %>% ACF(diff2_LogA) %>% autoplot()
unitroot_kpss(Unemployment_table_ts$diff2_LogA)


# Model Selection
Unemployment_ARIMA_LogA <- Unemployment_table_ts %>% model(ARIMA(LogA ~ PDQ(0,0,0), 
                                                                 stepwise=FALSE, approximation=FALSE, trace=FALSE))
report(Unemployment_ARIMA_LogA)

# Checking Normality of the Residuals
result_ARIMA_LogA_augment <- augment(Unemployment_ARIMA_LogA)
result_ARIMA_LogA_augment$.fitted
ad.test(result_ARIMA_LogA_augment$.resid)

result_ARIMA_LogA_augment$.fitted


# Forecasting the next 4 Months
Unemployment_ARIMA_LogA_forecast <- Unemployment_ARIMA_LogA %>% forecast(h=4)
hilo(Unemployment_ARIMA_LogA_forecast,95) %>% select(-.model)
var_forecast_errors_LogA <- distributional::variance(Unemployment_ARIMA_LogA_forecast$LogA)
var_forecast_errors_LogA

forecast_LogA <- Unemployment_ARIMA_LogA_forecast$.mean
forecast_LogA


# Random Walk Model
result_ARIMA_season <- Unemployment_table_ts %>% model(ARIMA(season ~ 0 + pdq(0,0,0) + PDQ(0,1,0)))
report(result_ARIMA_season)


# Forecasting Seasonality
result_ARIMA_season_forecast <- result_ARIMA_season %>% forecast(h=4)
result_ARIMA_season_forecast %>% select(-.model)
forecast_season <- result_ARIMA_season_forecast$.mean
forecast_season
var_forecast_errors_season <- distributional::variance(result_ARIMA_season_forecast$season)
var_forecast_errors_season


# Finding Correlation
Unemployment_ARIMA_LogA_augment <- augment(Unemployment_ARIMA_LogA)
Unemployment_ARIMA_season_augment <- augment(result_ARIMA_season)
correlation <- cor(Unemployment_ARIMA_LogA_augment$.resid, Unemployment_ARIMA_season_augment$.resid, use = 'complete') 
correlation


# Forecasting Unemployment
forecast_LogUnemployment <- forecast_LogA + forecast_season
forecast_LogUnemployment
forecast_Unemployment <- exp(forecast_LogUnemployment)
forecast_Unemployment



# Finding Confidence Intervals for Forecasts
covariance <- sqrt(var_forecast_errors_LogA) * sqrt(var_forecast_errors_season) * correlation
variance <- var_forecast_errors_LogA + var_forecast_errors_season + 2 * covariance

z_value <- qnorm(0.975, 0, 1)
PI_low_LogUnemployment <- forecast_LogUnemployment - z_value * sqrt(variance)
PI_low_LogUnemployment
PI_up_LogUnemployment <- forecast_LogUnemployment + z_value * sqrt(variance)
PI_up_LogUnemployment

PI_low_Unemployment <- exp(PI_low_LogUnemployment)
PI_low_Unemployment
PI_up_Unemployment <- exp(PI_up_LogUnemployment)
PI_up_Unemployment
