library(ggplot2)
library(tidyverse)
library(fpp3)
library(nortest)
library(forecast)
library(stlplus)

file <- "LNU03000000.csv"
Unemployment_table <- read_csv(file)
Unemployment_table <- Unemployment_table[745:926,]

Unemployment_table_ts <- Unemployment_table %>%
  add_column(month=yearmonth("2010 January") + 0:181, .before=TRUE) %>%
  as_tsibble(index=month)


# Graph before making an changes
Month_colors <- c("black", "blue", "purple", "red", "orange", "darkred", "green",
                  "chartreuse4", "chocolate", "gray60", "gold4", "salmon")

Month_f <- rep(1:12, length.out = 182)
Unemployment_table_ts$Month_fct = as.factor(Month_f)

Unemployment_table_ts %>% autoplot(LNU03000000) + geom_point(aes(y=LNU03000000, color = Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + ggtitle("Unemployment Levels vs. Month") + 
  xlab("Months") + ylab("Unemployment Levels (Thousands of People)") + 
  geom_hline(aes(yintercept = mean(LNU03000000)), lty=2)



# Checking if log should be used
Unemployment_table_ts$Log_LNU03000000 <- log(Unemployment_table_ts$LNU03000000)

Unemployment_table_ts %>% autoplot(Log_LNU03000000) + geom_point(aes(y=Log_LNU03000000, color = Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + ggtitle("Log(Unemployment Levels) vs. Month") + 
  xlab("Months") + ylab("Log(Unemployment Levels in Thousands of People)") + 
  geom_hline(aes(yintercept = mean(Log_LNU03000000)), lty=2)



# Removing the Pandemic Period
Unemployment_table_ts$LNU03000000[123:148] <- NA
Unemployment_table_ts$Log_LNU03000000[123:148] <- NA

print(Unemployment_table_ts[120:151,], n=32)


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
components <- stlplus(LogUnemployment_time_series, s.window=11, robust=TRUE)
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


# Model Selection

Unemployment_ARIMA_LogA <- Unemployment_table_ts %>% model(ARIMA(LogA ~ PDQ(0,0,0), stepwise=FALSE, approximation=FALSE, trace=FALSE))
report(Unemployment_ARIMA_LogA)

# Checking Normality of the Residuals

result_ARIMA_LogA_augment <- augment(Unemployment_ARIMA_LogA)
ad.test(result_ARIMA_LogA_augment$.resid)

result_ARIMA_LogA_augment$.fitted

# Forecasting the next 6 Months

Unemployment_ARIMA_LogA_forecast <- Unemployment_ARIMA_LogA %>% forecast(h=6)
hilo(Unemployment_ARIMA_LogA_forecast,95) %>% select(-.model)
var_forecast_errors_LogA <- distributional::variance(Unemployment_ARIMA_LogA_forecast$LogA)
var_forecast_errors_LogA

forecast_LogA <- Unemployment_ARIMA_LogA_forecast$.mean
forecast_LogA

var_forecast_errors_LogA <- distributional::variance(Unemployment_ARIMA_LogA_forecast$LogA)
var_forecast_errors_LogA


# Random Walk Model

result_ARIMA_season <- Unemployment_table_ts %>% model(ARIMA(season ~ 0 + pdq(0,0,0) + PDQ(0,1,0)))
report(result_ARIMA_season)

# Forecasting Seasonality

result_ARIMA_season_forecast <- result_ARIMA_season %>% forecast(h=6)
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

