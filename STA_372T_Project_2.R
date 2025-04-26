library(ggplot2)
library(tidyverse)
library(fpp3)
library(nortest)
library(forecast)
library(stlplus)

file <- "LNU03000000.csv"
Unemployment_table1 <- read_csv(file)
Unemployment_table1 <- Unemployment_table1[745:926,]

file <- "LNU03000000.csv"
Unemployment_table2 <- read_csv(file)
With_Recession_table <- Unemployment_table2[661:926,]

Unemployment_table_ts <- Unemployment_table1 %>%
  add_column(month=yearmonth("2010 January") + 0:181, .before=TRUE) %>%
  as_tsibble(index=month)

With_Recession_table_ts <- With_Recession_table %>%
  add_column(month=yearmonth("2003 January") + 0:265, .before=TRUE) %>%
  as_tsibble(index=month)

# Graph before making any changes
# Without Recession
Month_colors <- c("black", "blue", "purple", "red", "orange", "darkred", "green",
                  "chartreuse4", "chocolate", "gray60", "gold4", "salmon")

Month_f <- rep(1:12, length.out = 182)
Unemployment_table_ts$Month_fct = as.factor(Month_f)

Unemployment_table_ts %>% autoplot(LNU03000000) + geom_point(aes(y=LNU03000000, color = Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + ggtitle("Unemployment Levels vs. Month without Recession") + 
  xlab("Months") + ylab("Unemployment Levels (Thousands of People)") + 
  geom_hline(aes(yintercept = mean(LNU03000000)), lty=2)

# With Recession
Month_f <- rep(1:12, length.out = 266)
With_Recession_table_ts$Month_fct = as.factor(Month_f)

With_Recession_table_ts %>% autoplot(LNU03000000) + geom_point(aes(y=LNU03000000, color = Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + ggtitle("Unemployment Levels vs. Month with Recession") + 
  xlab("Months") + ylab("Unemployment Levels (Thousands of People)") + 
  geom_hline(aes(yintercept = mean(LNU03000000)), lty=2)



# Checking if log should be used with no recession
Unemployment_table_ts$Log_LNU03000000 <- log(Unemployment_table_ts$LNU03000000)

Unemployment_table_ts %>% autoplot(Log_LNU03000000) + geom_point(aes(y=Log_LNU03000000, color = Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + ggtitle("Log(Unemployment Levels) vs. Month") + 
  xlab("Months") + ylab("Log(Unemployment Levels in Thousands of People)") + 
  geom_hline(aes(yintercept = mean(Log_LNU03000000)), lty=2)

# Checking if log should be used with recession
With_Recession_table_ts$Log_LNU03000000 <- log(With_Recession_table_ts$LNU03000000)

With_Recession_table_ts %>% autoplot(Log_LNU03000000) + geom_point(aes(y=Log_LNU03000000, color = Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + ggtitle("Log(Unemployment Levels) vs. Month with Recession") + 
  xlab("Months") + ylab("Log(Unemployment Levels in Thousands of People)") + 
  geom_hline(aes(yintercept = mean(Log_LNU03000000)), lty=2)


# Removing the Pandemic Period
Unemployment_table_ts$LNU03000000[123:148] <- NA
Unemployment_table_ts$Log_LNU03000000[123:148] <- NA

print(Unemployment_table_ts[120:151,], n=32)

With_Recession_table_ts$LNU03000000[207:232] <- NA
With_Recession_table_ts$Log_LNU03000000[207:232] <- NA

print(With_Recession_table_ts[204:235,], n=32)

# Graphing Regular Unemployment without Recession with Pandemic Removed
Unemployment_table_ts %>% autoplot(LNU03000000) + geom_point(aes(y=LNU03000000, color=Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + 
  ggtitle("Unemployment Levels vs. Time with observations removed") + xlab("Month") + 
  ylab("Unemployment Levels (in Thousands)")


# Graphing Log(Unemployment) without Recession Pandemic Removed
Unemployment_table_ts %>% autoplot(Log_LNU03000000) + geom_point(aes(y=Log_LNU03000000, color=Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + 
  ggtitle("Log(Unemployment Levels in thousands) vs. Time with observations removed") + xlab("Month") +
  ylab("Log(Unemployment Levels in thousands)")


# Graphing Regular Unemployment without Recession with Pandemic Removed
With_Recession_table_ts %>% autoplot(LNU03000000) + geom_point(aes(y=LNU03000000, color=Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + 
  ggtitle("Unemployment Levels vs. Time with observations removed") + xlab("Month") + 
  ylab("Unemployment Levels (in Thousands)")


# Graphing Log(Unemployment) without Recession Pandemic Removed
With_Recession_table_ts %>% autoplot(Log_LNU03000000) + geom_point(aes(y=Log_LNU03000000, color=Month_fct)) + 
  scale_color_manual(values = Month_colors) + theme_classic() + 
  ggtitle("Log(Unemployment Levels in thousands) vs. Time with observations removed") + xlab("Month") +
  ylab("Log(Unemployment Levels in thousands)")


#Seasonally Adjust Log Unemployment without Recession
LogUnemployment_time_series <- ts(Unemployment_table_ts$Log_LNU03000000, frequency=12)
components <- stlplus(LogUnemployment_time_series, s.window="periodic", robust=TRUE)
Unemployment_table_ts$season <- components$data$seasonal
Unemployment_table_ts$LogA <- components$data$raw - components$data$seasonal

print(Unemployment_table_ts[120:148,], n=32)


#Seasonally Adjust Log Unemployment with Recession
Recession_LogUnemployment_time_series <- ts(With_Recession_table_ts$Log_LNU03000000, frequency=12)
components <- stlplus(Recession_LogUnemployment_time_series, s.window="periodic", robust=TRUE)
With_Recession_table_ts$season <- components$data$seasonal
With_Recession_table_ts$LogA <- components$data$raw - components$data$seasonal

print(Unemployment_table_ts[120:148,], n=32)

# LogA vs time without Recession

Unemployment_table_ts %>% autoplot(LogA) +
  geom_point(aes(y=LogA, color=Month_fct)) +
  scale_color_manual(values = Month_colors) +
  theme_classic() +
  ggtitle("LogA vs. Month") + xlab("Month") + ylab("LogA")


# LogA vs time with Recession

With_Recession_table_ts %>% autoplot(LogA) +
  geom_point(aes(y=LogA, color=Month_fct)) +
  scale_color_manual(values = Month_colors) +
  theme_classic() +
  ggtitle("LogA vs. Month") + xlab("Month") + ylab("LogA")


# Test Stationarity without Recession

Unemployment_table_ts %>% ACF(LogA) %>% autoplot()

unitroot_kpss(Unemployment_table_ts$LogA)


# Test Stationarity with Recession

With_Recession_table_ts %>% ACF(LogA) %>% autoplot()

unitroot_kpss(With_Recession_table_ts$LogA)

# Test First Difference without Recession
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
  ggtitle("diff_LogA vs. Time without Recession") + xlab("Time") + ylab("diff_LogA")

# Test First Difference with Recession
With_Recession_table_ts <- With_Recession_table_ts %>% mutate(diff_LogA = 
                                                            difference(LogA))
head(With_Recession_table_ts)
With_Recession_table_ts %>% ACF(diff_LogA) %>% autoplot()
unitroot_kpss(With_Recession_table_ts$diff_LogA)

mean_diff_LogA <- mean(With_Recession_table_ts$diff_LogA, na.rm=TRUE)
Unemployment_table_ts %>% autoplot(diff_LogA) +
  geom_point(aes(y=diff_LogA, color=Month_fct)) +
  geom_hline(aes(yintercept=mean_diff_LogA), lty=2) +
  theme_classic() +
  ggtitle("diff_LogA vs. Time with Recession") + xlab("Time") + ylab("diff_LogA")

# Test Second Difference with Recession
With_Recession_table_ts <- With_Recession_table_ts %>% mutate(diff2_LogA = difference(diff_LogA))
head(With_Recession_table_ts)

With_Recession_table_ts %>% ACF(diff2_LogA) %>% autoplot()
unitroot_kpss(With_Recession_table_ts$diff2_LogA)


















# Model Selection
# Without Recession
Unemployment_ARIMA_LogA <- Unemployment_table_ts %>% model(ARIMA(LogA ~ PDQ(0,0,0), 
stepwise=FALSE, approximation=FALSE, trace=FALSE))
report(Unemployment_ARIMA_LogA)

# With Recession
With_Recession_ARIMA_LogA <- With_Recession_table_ts %>% model(ARIMA(LogA ~ PDQ(0,0,0), 
stepwise=FALSE, approximation=FALSE, trace=FALSE))
report(With_Recession_ARIMA_LogA)

# Checking Normality of the Residuals
# Without Recession
result_ARIMA_LogA_augment <- augment(Unemployment_ARIMA_LogA)
result_ARIMA_LogA_augment$.fitted
ad.test(result_ARIMA_LogA_augment$.resid)

result_ARIMA_LogA_augment$.fitted

# With Recession
Recession_result_ARIMA_LogA_augment <- augment(With_Recession_ARIMA_LogA)
Recession_result_ARIMA_LogA_augment$.fitted
ad.test(Recession_result_ARIMA_LogA_augment$.resid)

result_ARIMA_LogA_augment$.fitted


# Forecasting the next 4 Months
# Without Recession
Unemployment_ARIMA_LogA_forecast <- Unemployment_ARIMA_LogA %>% forecast(h=4)
hilo(Unemployment_ARIMA_LogA_forecast,95) %>% select(-.model)
var_forecast_errors_LogA <- distributional::variance(Unemployment_ARIMA_LogA_forecast$LogA)
var_forecast_errors_LogA

forecast_LogA <- Unemployment_ARIMA_LogA_forecast$.mean
forecast_LogA

# With Recession

Recession_ARIMA_LogA_forecast <- With_Recession_ARIMA_LogA %>% forecast(h=4)
hilo(Recession_ARIMA_LogA_forecast,95) %>% select(-.model)
Recession_var_forecast_errors_LogA <- distributional::variance(Recession_ARIMA_LogA_forecast$LogA)
Recession_var_forecast_errors_LogA

Recession_forecast_LogA <- Unemployment_ARIMA_LogA_forecast$.mean
Recession_forecast_LogA

# Random Walk Model
# Without Recession
result_ARIMA_season <- Unemployment_table_ts %>% model(ARIMA(season ~ 0 + pdq(0,0,0) + PDQ(0,1,0)))
report(result_ARIMA_season)

# With Recession
Recession_result_ARIMA_season <- With_Recession_table_ts %>% model(ARIMA(season ~ 0 + pdq(0,0,0) + PDQ(0,1,0)))
report(Recession_result_ARIMA_season)


# Forecasting Seasonality
# Without Recession
result_ARIMA_season_forecast <- result_ARIMA_season %>% forecast(h=4)
result_ARIMA_season_forecast %>% select(-.model)
forecast_season <- result_ARIMA_season_forecast$.mean
forecast_season
var_forecast_errors_season <- distributional::variance(result_ARIMA_season_forecast$season)
var_forecast_errors_season

# With Recession
Recession_result_ARIMA_season_forecast <- Recession_result_ARIMA_season %>% forecast(h=4)
Recession_result_ARIMA_season_forecast %>% select(-.model)
Recession_forecast_season <- result_ARIMA_season_forecast$.mean
Recession_forecast_season
Recession_var_forecast_errors_season <- distributional::variance(Recession_result_ARIMA_season_forecast$season)
Recession_var_forecast_errors_season

# Finding Correlation
# Without Recession
Unemployment_ARIMA_LogA_augment <- augment(Unemployment_ARIMA_LogA)
Unemployment_ARIMA_season_augment <- augment(result_ARIMA_season)
correlation <- cor(Unemployment_ARIMA_LogA_augment$.resid, Unemployment_ARIMA_season_augment$.resid, use = 'complete') 
correlation

# With Recession
Recession_ARIMA_LogA_augment <- augment(With_Recession_ARIMA_LogA)
Recession_ARIMA_season_augment <- augment(Recession_result_ARIMA_season)
Recession_correlation <- cor(Recession_ARIMA_LogA_augment$.resid, Recession_ARIMA_season_augment$.resid, use = 'complete') 
Recession_correlation


# Forecasting Unemployment
# Without Recession
forecast_LogUnemployment <- forecast_LogA + forecast_season
forecast_LogUnemployment
forecast_Unemployment <- exp(forecast_LogUnemployment)
forecast_Unemployment

# With Recession
Recession_forecast_LogUnemployment <- Recession_forecast_LogA + Recession_forecast_season
Recession_forecast_LogUnemployment
Recession_forecast_Unemployment <- exp(Recession_forecast_LogUnemployment)
Recession_forecast_Unemployment


# Finding Confidence Intervals for Forecasts
# Without Recession
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

# With Recession

Recession_covariance <- sqrt(Recession_var_forecast_errors_LogA) * sqrt(Recession_var_forecast_errors_season) * Recession_correlation
Recession_variance <- Recession_var_forecast_errors_LogA + Recession_var_forecast_errors_season + 2 * Recession_covariance

z_value <- qnorm(0.975, 0, 1)
Recession_PI_low_LogUnemployment <- Recession_forecast_LogUnemployment - z_value * sqrt(Recession_variance)
Recession_PI_low_LogUnemployment
Recession_PI_up_LogUnemployment <- Recession_forecast_LogUnemployment + z_value * sqrt(Recession_variance)
Recession_PI_up_LogUnemployment

Recession_PI_low_Unemployment <- exp(Recession_PI_low_LogUnemployment)
Recession_PI_low_Unemployment
Recession_PI_up_Unemployment <- exp(Recession_PI_up_LogUnemployment)
Recession_PI_up_Unemployment
