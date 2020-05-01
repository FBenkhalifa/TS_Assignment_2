library(tidyverse)
library(tseries)
library(lubridate)
library(forecast)
library(slider)
library(ggthemes)
library(broom)
library(sweep)
library(pander)


# 2.a) --------------------------------------------------------------------

# Read data and convert date column to comfortable format
data_raw <- read_csv2("./data/InternetRetailSales.csv") %>% 
  unite(col = "date", Year:Month, sep = " ", remove = FALSE) %>% 
  mutate_at("date", ~ymd(., truncated = 1))

# Convert data to time series object
ts_data <- ts(data_raw$InternetRetail, frequency = 12, start = c(2011, 7))

# Plot the data
plot(ts_data)
ggplot(data_raw, aes(x = date, y = InternetRetail)) + 
  geom_line() +
  theme_hc()

#' Comment: From the plot we can observe that the sales are trending upwards.
#' Additionally, we observe sales to be highly seasonal, with a sharp spike
#' in the last quarter.

# 2.b) --------------------------------------------------------------------

# Create seasonal plot
ggseasonplot(ts_data) + theme_hc()

#' Comment: With this plot the time axis is fixed to one year and different
#' years are represented by different lines. We can thus better compare sales
#' across years and more clearly see how sales spike in november and december, 
#' due to christmas business.


# Create acf plot
acf(ts_data, lag.max = 36)

#' Comment: The acf shows how much the sales of one month correlate with the 
#' nth lag of monthly sales, up to 36 months. We can see that the correlation
#' is especially high between the holiday sale seasons, which are 12 months 
#' apart. The acf is significant up to a lag of 24, resembling a 2 year time 
#' period.


# 2.c) --------------------------------------------------------------------

# Add simple moving average with window starting for t-6
data <- data_raw %>% mutate(ma = ma(ts_data, order = 12))

# Plot the data accordingly
ggplot(data, aes(x = date)) + 
  geom_line(aes(y = InternetRetail)) +
  geom_line(aes(y = ma), color = "red") +
  theme_hc()

#' Comment: The MA process is of order 12, and thus averages over one year.
#' We can clearly see the smooth upwards trend of sales. 


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


# 2.e) --------------------------------------------------------------------XX 
data <- data %>% mutate(m_t = InternetRetail/ma) %>% na.omit
ggplot(data, aes(x = date)) + 
  geom_line(aes(y = m_t)) +
  theme_hc()

# No the data is not weakly stationary since the graph is obviously a 
# function of time and seasonal patterns pertain. Both mean and variance of the
# time series are a function of time.

# Decompose
seasonal_component <- data %>% 
  group_by(Month) %>% 
  summarize(s_t = mean(m_t)) %>% 
  mutate(Month = month(mdy(Month, truncated = 2), label = T, locale = "eng")) %>% 
  arrange(Month)


# Plot seasonal component
ts(rep(seasonal_component$s_t, 12),  frequency = 12, start = c(2012, 1)) %>% 
  plot()

# Plot residual
data <- data %>% right_join(., seasonal_component, by = "Month") %>% 
  mutate(u_t = InternetRetail / (ma * s_t))

data %>% ggplot(aes(x=date, y = u_t)) + 
  geom_line() + 
  theme_hc()
# 2.f) --------------------------------------------------------------------XX

dec_data <- decompose(ts_data, type = "multiplicative")
plot(dec_data)

#' Comment: The process is decomposed into the trend, seasonal and random component.
#' The trend is smooth and continuously rising. The random seems to be 
#' a stationary process with constant mean and variance. The seasonal component..(stationary?)


# 2.g) --------------------------------------------------------------------

dec_data$random %>% acf(na.action = na.pass)

#' Comment: The residual component of the decomposition, seem to be white noise
#' as the spikes of the acf are only rarely significant.

# 2.h) --------------------------------------------------------------------

arima <- auto.arima(ts_data)
arima_ord <- auto.arima(data$InternetRetail)
map_dfr(list(arima, arima_ord), sw_glance) %>% pander

#' Check model diagnostics: The model has low error measures suggesting a 
#' good fit to the data.


# 2.i) --------------------------------------------------------------------

preds <- forecast(arima, h = 24) %>% sweep::sw_sweep(timekit_idx = TRUE, rename_index = "date")

preds %>%
  ggplot(aes(x = date, y = value, color = key)) +
  # Prediction intervals
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  # Actual & Forecast
  geom_line(size = 1) + 
  geom_point(size = 2) +
  # Aesthetics
  theme_hc() +
  labs(title = "Sales, 2-Year Forecast", x = "date", y = "value") 
#' Comment: The forecast follows our expectations as it continues the seasonal
#' pattern closely.



