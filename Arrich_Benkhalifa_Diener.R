library(tidyverse)
library(tseries)
library(lubridate)
library(forecast)
library(slider)
library(ggthemes)
library(broom)



# 2.a) --------------------------------------------------------------------

# Read data and convert date column to comfortable format
data <- read_csv2("./data/InternetRetailSales.csv") %>% 
  unite(col = "date", Year:Month, sep = " ", remove = FALSE) %>% 
  mutate_at("date", ~ymd(., truncated = 1))

# Convert data to time series object
ts_data <- ts(data$InternetRetail, frequency = 12, start = c(2011, 7))

# Plot the data
plot(ts_data)
ggplot(data, aes(x = date, y = InternetRetail)) + 
  geom_line() +
  theme_hc()

# 2.b) --------------------------------------------------------------------

# Create seasonal plot
ggseasonplot(ts_data) + theme_hc()

# Create acf plot
acf(ts_data, lag.max = 36)


# 2.c) --------------------------------------------------------------------

# Add simple moving average with window starting for t-6
data$simple_ma <- ts_data %>% slide_dbl(mean, .before = 6, .after = 6)
data$ma <- ma(ts_data, order = 12, centre = TRUE)

# Plot the data accordingly
ggplot(data, aes(x = date)) + 
  geom_line(aes(y = InternetRetail)) +
  geom_line(aes(y = simple_ma), color = "red") +
  theme_hc()


# 2.d) --------------------------------------------------------------------
# https://anomaly.io/seasonal-trend-decomposition-in-r/index.html
# The type of the time series determines how the trend, seasonal and residual comp
# onent interact. If the time series is multiplicative, the components multiply
# together. An increasing trend then results in an increased amplitude of the
# seasonal activity.
# Formula
# The additive time series is defined by an additive nature of the individual comp
# onents. An increasing trend then does not corresponds to increasing peaks and troughs
# of the time series. It stays relatively stable and no change in amplitude can
# be observed.
# Formula

# For this dataset a multiplicative decomposition should be chosen, since we clearly
# see an increase in the fluctuation with increasing trend. 


# 2.e) --------------------------------------------------------------------
data <- data %>% mutate(detrend = InternetRetail/simple_ma)
ggplot(data, aes(x = date)) + 
  geom_line(aes(y = detrend)) +
  theme_hc()

# No the data is not weakly stationary since the graph is obviously a 
# function of time and seasonal patterns pertain. Both mean and variance of the
# time series are a function of time.

# Decompose
data %>% group_by(Month) %>% summarize(Mean = mean(detrend))



# 2.f) --------------------------------------------------------------------

dec_data <- decompose(ts_data, type = "multiplicative")


# 2.g) --------------------------------------------------------------------
dec_data$random %>% acf(na.action = na.pass)


# 2.h) --------------------------------------------------------------------

arima <- dec_data$random %>% auto.arima
summary(arima)
dec_data$random %>% plot

ljung_test <- map_df(c(1:24), ~Box.test(dec_data$random, lag = ., type = "Box-Pierce") %>% tidy)
ljung_test %>% print(n = 50)
# 2.i) --------------------------------------------------------------------

preds <- forecast(arima, h = 24)
preds