library(forecast)
library(tidyverse)
library(ggplot2)
library(bsts)
library(readxl)
library(caretForecast)


##################################################### Brazil 48 #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()

data <- read_excel('Brazil_Data.xlsx') %>% rename('Exchange_Rate_braz' = spot_ER_Brazil)                   
exchange_rate_braz <- ts(data$Exchange_Rate_braz)
reg_braz <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)

n = 48
set.seed(100)
train_braz_48 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_48 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_48 <- reg_braz[1:length(train_braz_48),]
test_reg_braz_48 <- reg_braz[1:n,]

# 1- ARIMAX
set.seed(100)
arima_braz_48 <- auto.arima(train_braz_48, xreg = train_reg_braz_48)
arima_braz_48_pred <- forecast(arima_braz_48, h = n, xreg = test_reg_braz_48)

# 2- BSTSX
ss <- AddSemilocalLinearTrend(list(), train_braz_48)
ss <- AddSeasonal(ss, train_braz_48, nseasons = 12)
bsts_braz_48 <- bsts(train_braz_48 ~ ., 
                     state.specification = ss,
                     niter = 1000,
                     ping = 100,
                     seed = 100,
                     data = cbind(train_reg_braz_48, train_braz_48))

bsts_braz_48_pred <- predict(bsts_braz_48, horizon = n, burn = SuggestBurn(.1, bsts_braz_48), newdata = test_reg_braz_48)

# 3 - ARFIMAX
arfima_braz_48 <- arfima(train_braz_48, xreg = train_reg_braz_48)
arfima_braz_48_pred <- forecast(arfima_braz_48, h = n, xreg = test_reg_braz_48)

# 4 - NARFIMA (skip = T)

arfima_er_braz_48 <-  residuals(arfima_braz_48)
arfima_er_braz_48[is.na(arfima_er_braz_48)] <-  0
set.seed(100)
narfimaT_braz_48 <-  auto_narfima(train_braz_48, arfima_er_braz_48, p = 4, q = 2, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_braz_48)
set.seed(100)
narfimaT_braz_48_pred <-  forecast_narfima_class(narfimaT_braz_48, PI = FALSE, h = n, xreg = test_reg_braz_48)
res_narfimaT_braz_48 <- abs(narfimaT_braz_48_pred$mean - test_braz_48)
conf_narfimaT_braz_48 <- conformalRegressor(res_narfimaT_braz_48, sigmas = NULL)
conf_narfimaT_braz_48_pred <- predict(conf_narfimaT_braz_48, y_hat = narfimaT_braz_48_pred$mean, sigmas = NULL, confidence = 0.95, y_min = -Inf, y_max = Inf)
posterior_interval_brazT_48 <- as_tibble(cbind.data.frame('L' = conf_narfimaT_braz_48_pred$lower_95,'U' = conf_narfimaT_braz_48_pred$upper_95))

mean(conf_narfimaT_braz_48_pred$upper_95 - conf_narfimaT_braz_48_pred$lower_95)

setwd('NARFIMA/Dataset/Dataset_Model_Forecasts/Brazil')
getwd()

data <- read_excel('Brazil Forecast 48.xlsx') %>% rename('Exchange_Rate_braz' = ExchangeRateBrazil)                   
data[data == 0] <- NA

n = 48
braz_48 <- data[(nrow(data) - n + 1):nrow(data),]


ggplot(data = braz_48, aes(x = as.Date(Date))) +
  geom_point(aes(y = Exchange_Rate_braz, color = 'Ground Truth')) +
  geom_line(aes(y = NARFIMAXT, color = 'NARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARIMAX, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = BSTSX, color = 'BSTS'), linewidth = 2) +
  geom_ribbon(aes(ymin = posterior_interval_brazT_48$L, ymax = posterior_interval_brazT_48$U), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "8 month", date_labels = "%Y-%m-%d", limits = c(ymd("2019-11-01"), ymd("2023-10-01"))) +
  scale_y_continuous(
    limits = round(c(min(posterior_interval_brazT_48$L) - 0.1, max(posterior_interval_brazT_48$U) + 0.1),1),  
    breaks = function(limits) seq(limits[1], limits[2], length.out = 5)
  ) +
  ylab('Exchange Rate') +
  xlab('Time') +
  labs(color = 'Models') +
  ggtitle('Brazil Exchange Rate: 48 Month Holdout') +
  scale_color_manual(values = c(
    'ARIMA' = '#9A32CD',
    'BSTS' = '#7BB661', 
    'NARFIMA' = '#1F75FE',    
    'Ground Truth' = '#FF3800'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')



##################################################### Russia 48 #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('Russia_Data.xlsx') %>% rename('Exchange_Rate_rus' = spot_ER_Russia)                   
exchange_rate_rus <- ts(data$Exchange_Rate_rus)
reg_rus <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)

n = 48
set.seed(100)
train_rus_48 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_48 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_48 <- reg_rus[1:length(train_rus_48),]
test_reg_rus_48 <- reg_rus[1:n,]

# 1- ARIMAX

set.seed(100)
arima_rus_48 <- auto.arima(train_rus_48, xreg = train_reg_rus_48)
arima_rus_48_pred <- forecast(arima_rus_48, h = n, xreg = test_reg_rus_48)

# 2- BSTSX

ss <- AddSemilocalLinearTrend(list(), train_rus_48)
ss <- AddSeasonal(ss, train_rus_48, nseasons = 12)
bsts_rus_48 <- bsts(train_rus_48 ~ ., 
                     state.specification = ss,
                     niter = 1000,
                     ping = 100,
                     seed = 100,
                     data = cbind(train_reg_rus_48, train_rus_48))

bsts_rus_48_pred <- predict(bsts_rus_48, horizon = n, burn = SuggestBurn(.1, bsts_rus_48), newdata = test_reg_rus_48)

# 3 - ARFIMAX

arfima_rus_48 <- arfima(train_rus_48, xreg = train_reg_rus_48)
arfima_rus_48_pred <- forecast(arfima_rus_48, h = n, xreg = test_reg_rus_48)

# 4 - NARFIMA (skip = F)

arfima_er_rus_48 <-  residuals(arfima_rus_48)
arfima_er_rus_48[is.na(arfima_er_rus_48)] <-  0
set.seed(100)
narfima_rus_48 <-  auto_narfima(train_rus_48, arfima_er_rus_48, p = 3, q = 2, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = F, xreg = train_reg_rus_48)
set.seed(100)
narfima_rus_48_pred <-  forecast_narfima_class(narfima_rus_48, PI = FALSE, h = n, xreg = test_reg_rus_48)
res_narfima_rus_48 <- abs(narfima_rus_48_pred$mean - test_rus_48)
conf_narfima_rus_48 <- conformalRegressor(res_narfima_rus_48, sigmas = NULL)
conf_narfima_rus_48_pred <- predict(conf_narfima_rus_48, y_hat = narfima_rus_48_pred$mean, sigmas = NULL, confidence = 0.95, y_min = -Inf, y_max = Inf)
posterior_interval_rus_48 <- as_tibble(cbind.data.frame('L' = conf_narfima_rus_48_pred$lower_95,'U' = conf_narfima_rus_48_pred$upper_95))
mean(conf_narfima_rus_48_pred$upper_95 - conf_narfima_rus_48_pred$lower_95)

setwd('NARFIMA/Dataset/Dataset_Model_Forecasts/Russia')
getwd()

data <- read_excel('Russia Forecast 48.xlsx') %>% rename('Exchange_Rate_rus' = ExchangeRateRussia)                   
data[data == 0] <- NA

n = 48
rus_48 <- data[(nrow(data) - n + 1):nrow(data),]

ggplot(data = rus_48, aes(x = as.Date(Date))) +
  geom_point(aes(y = Exchange_Rate_rus, color = 'Ground Truth')) +
  geom_line(aes(y = NARFIMAX, color = 'NARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARIMAX, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = BSTSX, color = 'BSTS'), linewidth = 2) +
  geom_ribbon(aes(ymin = posterior_interval_rus_48$L, ymax = posterior_interval_rus_48$U), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "8 month", date_labels = "%Y-%m-%d", limits = c(ymd("2019-11-01"), ymd("2023-10-01"))) +
  scale_y_continuous(
    limits = round(c(min(rus_48$Exchange_Rate_rus) - 0.1, max(rus_48$Exchange_Rate_rus) + 0.1),1),  
    breaks = function(limits) seq(limits[1], limits[2], length.out = 5)
  ) +
  ylab('Exchange Rate') +
  xlab('Time') +
  labs(color = 'Models') +
  ggtitle('Russia Exchange Rate: 48 Month Holdout') +
  scale_color_manual(values = c(
    'ARIMA' = '#9A32CD',
    'BSTS' = '#7BB661', 
    'NARFIMA' = '#1F75FE',    
    'Ground Truth' = '#FF3800'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')



##################################################### India 48 #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('India_Data.xlsx') %>% rename('Exchange_Rate_ind' = spot_ER_India)                   
exchange_rate_ind <- ts(data$Exchange_Rate_ind)
reg_ind <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)


n = 48
set.seed(100)
train_ind_48 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_48 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_48 <- reg_ind[1:length(train_ind_48),]
test_reg_ind_48 <- reg_ind[1:n,]

# 1- ARIMAX

set.seed(100)
arima_ind_48 <- auto.arima(train_ind_48, xreg = train_reg_ind_48)
arima_ind_48_pred <- forecast(arima_ind_48, h = n, xreg = test_reg_ind_48)

# 2- BSTSX

ss <- AddSemilocalLinearTrend(list(), train_ind_48)
ss <- AddSeasonal(ss, train_ind_48, nseasons = 12)
bsts_ind_48 <- bsts(train_ind_48 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_ind_48, train_ind_48))

bsts_ind_48_pred <- predict(bsts_ind_48, horizon = n, burn = SuggestBurn(.1, bsts_ind_48), newdata = test_reg_ind_48)

# 3 - ARFIMAX

arfima_ind_48 <- arfima(train_ind_48, xreg = train_reg_ind_48)
arfima_ind_48_pred <- forecast(arfima_ind_48, h = n, xreg = test_reg_ind_48)

# 4 - NARFIMA (skip = T)

arfima_er_ind_48 <-  residuals(arfima_ind_48)
arfima_er_ind_48[is.na(arfima_er_ind_48)] <-  0
set.seed(100)
narfimaT_ind_48 <-  auto_narfima(train_ind_48, arfima_er_ind_48, p = 2, q = 4, size = 4, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_ind_48)
set.seed(100)
narfimaT_ind_48_pred <-  forecast_narfima_class(narfimaT_ind_48, PI = FALSE, h = n, xreg = test_reg_ind_48)
res_narfimaT_ind_48 <- abs(narfimaT_ind_48_pred$mean - test_ind_48)
conf_narfimaT_ind_48 <- conformalRegressor(res_narfimaT_ind_48, sigmas = NULL)
conf_narfimaT_ind_48_pred <- predict(conf_narfimaT_ind_48, y_hat = narfimaT_ind_48_pred$mean, sigmas = NULL, confidence = 0.95, y_min = -Inf, y_max = Inf)
posterior_interval_indT_48 <- as_tibble(cbind.data.frame('L' = conf_narfimaT_ind_48_pred$lower_95,'U' = conf_narfimaT_ind_48_pred$upper_95))
mean(conf_narfimaT_ind_48_pred$upper_95 - conf_narfimaT_ind_48_pred$lower_95)

setwd('NARFIMA/Dataset/Dataset_Model_Forecasts/India')
getwd()

data <- read_excel('India Forecast 48.xlsx') %>% rename('Exchange_Rate_ind' = ExchangeRateIndia)                   
data[data == 0] <- NA

n = 48
ind_48 <- data[(nrow(data) - n + 1):nrow(data),]


ggplot(data = ind_48, aes(x = as.Date(Date))) +
  geom_point(aes(y = Exchange_Rate_ind, color = 'Ground Truth')) +
  geom_line(aes(y = NARFIMAXT, color = 'NARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARIMAX, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = BSTSX, color = 'BSTS'), linewidth = 2) +
  geom_ribbon(aes(ymin = posterior_interval_indT_48$L, ymax = posterior_interval_indT_48$U), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "8 month", date_labels = "%Y-%m-%d", limits = c(ymd("2019-11-01"), ymd("2023-10-01"))) +
  scale_y_continuous(
    limits = round(c(min(posterior_interval_indT_48$L) - 0.1, max(posterior_interval_indT_48$U) + 0.1),1),  
    breaks = function(limits) seq(limits[1], limits[2], length.out = 5)
  ) +
  ylab('Exchange Rate') +
  xlab('Time') +
  labs(color = 'Models') +
  ggtitle('India Exchange Rate: 48 Month Holdout') +
  scale_color_manual(values = c(
    'ARIMA' = '#9A32CD',
    'BSTS' = '#7BB661', 
    'NARFIMA' = '#1F75FE',    
    'Ground Truth' = '#FF3800'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')



##################################################### China 48 #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('China_Data.xlsx') %>% rename('Exchange_Rate_chn' = spot_ER_China)                   
exchange_rate_chn <- ts(data$Exchange_Rate_chn)
reg_chn <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)

n = 48
set.seed(100)
train_chn_48 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_48 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_48 <- reg_chn[1:length(train_chn_48),]
test_reg_chn_48 <- reg_chn[1:n,]

# 1- ARIMAX

set.seed(100)
arima_chn_48 <- auto.arima(train_chn_48, xreg = train_reg_chn_48)
arima_chn_48_pred <- forecast(arima_chn_48, h = n, xreg = test_reg_chn_48)

# 2- BSTSX

ss <- AddSemilocalLinearTrend(list(), train_chn_48)
ss <- AddSeasonal(ss, train_chn_48, nseasons = 12)
bsts_chn_48 <- bsts(train_chn_48 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_chn_48, train_chn_48))

bsts_chn_48_pred <- predict(bsts_chn_48, horizon = n, burn = SuggestBurn(.1, bsts_chn_48), newdata = test_reg_chn_48)

# 3 - ARFIMAX

arfima_chn_48 <- arfima(train_chn_48, xreg = train_reg_chn_48)
arfima_chn_48_pred <- forecast(arfima_chn_48, h = n, xreg = test_reg_chn_48)

# 4 - NARFIMA (skip = T)

arfima_er_chn_48 <-  residuals(arfima_chn_48)
arfima_er_chn_48[is.na(arfima_er_chn_48)] <-  0
set.seed(100)
narfimaT_chn_48 <-  auto_narfima(train_chn_48, arfima_er_chn_48, p = 4, q = 1, size = 2, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_chn_48)
set.seed(100)
narfimaT_chn_48_pred <-  forecast_narfima_class(narfimaT_chn_48, PI = FALSE, h = n, xreg = test_reg_chn_48)
res_narfimaT_chn_48 <- abs(narfimaT_chn_48_pred$mean - test_chn_48)
conf_narfimaT_chn_48 <- conformalRegressor(res_narfimaT_chn_48, sigmas = NULL)
conf_narfimaT_chn_48_pred <- predict(conf_narfimaT_chn_48, y_hat = narfimaT_chn_48_pred$mean, sigmas = NULL, confidence = 0.95, y_min = -Inf, y_max = Inf)
posterior_interval_chnT_48 <- as_tibble(cbind.data.frame('L' = conf_narfimaT_chn_48_pred$lower_95,'U' = conf_narfimaT_chn_48_pred$upper_95))

mean(conf_narfimaT_chn_48_pred$upper_95 - conf_narfimaT_chn_48_pred$lower_95)

setwd('NARFIMA/Dataset/Dataset_Model_Forecasts/China')
getwd()

data <- read_excel('China Forecast 48.xlsx') %>% rename('Exchange_Rate_chn' = ExchangeRateChina)                   
data[data == 0] <- NA

n = 48
chn_48 <- data[(nrow(data) - n + 1):nrow(data),]


ggplot(data = chn_48, aes(x = as.Date(Date))) +
  geom_point(aes(y = Exchange_Rate_chn, color = 'Ground Truth')) +
  geom_line(aes(y = NARFIMAXT, color = 'NARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARIMAX, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = BSTSX, color = 'BSTS'), linewidth = 2) +
  geom_ribbon(aes(ymin = posterior_interval_chnT_48$L, ymax = posterior_interval_chnT_48$U), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "8 month", date_labels = "%Y-%m-%d", limits = c(ymd("2019-11-01"), ymd("2023-10-01"))) +
  scale_y_continuous(
    limits = round(c(min(posterior_interval_chnT_48$L) - 0.1, max(posterior_interval_chnT_48$U) + 0.1),1),  
    breaks = function(limits) seq(limits[1], limits[2], length.out = 5)
  ) + 
  ylab('Exchange Rate') +
  xlab('Time') +
  labs(color = 'Models') +
  ggtitle('China Exchange Rate: 48 Month Holdout') +
  scale_color_manual(values = c(
    'ARIMA' = '#9A32CD',
    'BSTS' = '#7BB661', 
    'NARFIMA' = '#1F75FE',    
    'Ground Truth' = '#FF3800'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')
