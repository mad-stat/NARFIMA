library(forecast)
library(tidyverse)
library(ggplot2)
library(bsts)
library(readxl)
library(caretForecast)


##################################################### Brazil 24 #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()

data <- read_excel('Brazil_Data.xlsx') %>% rename('Exchange_Rate_braz' = spot_ER_Brazil)                   
exchange_rate_braz <- ts(data$Exchange_Rate_braz)
reg_braz <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)



n = 24
set.seed(100)
train_braz_24 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_24 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_24 <- reg_braz[1:length(train_braz_24),]
test_reg_braz_24 <- reg_braz[1:n,]

# 1- ARIMAX

set.seed(100)
arima_braz_24 <- auto.arima(train_braz_24, xreg = train_reg_braz_24)
arima_braz_24_pred <- forecast(arima_braz_24, h = n, xreg = test_reg_braz_24)

# 2- BSTSX

ss <- AddSemilocalLinearTrend(list(), train_braz_24)
ss <- AddSeasonal(ss, train_braz_24, nseasons = 12)
bsts_braz_24 <- bsts(train_braz_24 ~ ., 
                     state.specification = ss,
                     niter = 1000,
                     ping = 100,
                     seed = 100,
                     data = cbind(train_reg_braz_24, train_braz_24))

bsts_braz_24_pred <- predict(bsts_braz_24, horizon = n, burn = SuggestBurn(.1, bsts_braz_24), newdata = test_reg_braz_24)

# 3 - ARFIMAX

arfima_braz_24 <- arfima(train_braz_24, xreg = train_reg_braz_24)
arfima_braz_24_pred <- forecast(arfima_braz_24, h = n, xreg = test_reg_braz_24)

# 4 - NARFIMA (skip = F)

arfima_er_braz_24 <-  residuals(arfima_braz_24)
arfima_er_braz_24[is.na(arfima_er_braz_24)] <-  0
set.seed(100)
narfima_braz_24 <-  auto.narfima(train_braz_24, arfima_er_braz_24, p = 2, q = 5, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = F, xreg = train_reg_braz_24)
set.seed(100)
narfima_braz_24_pred <-  forecast.narfima(narfima_braz_24, PI = FALSE, h = n, xreg = test_reg_braz_24)
res_narfima_braz_24 <- abs(narfima_braz_24_pred$mean - test_braz_24)
conf_narfima_braz_24 <- conformalRegressor(res_narfima_braz_24, sigmas = NULL)
conf_narfima_braz_24_pred <- predict(conf_narfima_braz_24, y_hat = narfima_braz_24_pred$mean, sigmas = NULL, confidence = 0.80, y_min = -Inf, y_max = Inf)
posterior_interval_braz_24 <- as_tibble(cbind.data.frame('L' = conf_narfima_braz_24_pred$lower_80,'U' = conf_narfima_braz_24_pred$upper_80))

mean(conf_narfima_braz_24_pred$upper_80 - conf_narfima_braz_24_pred$lower_80)

setwd('NARFIMA/Dataset/Dataset_Model_Forecasts/Brazil')
getwd()

data <- read_excel('Brazil Forecast 24.xlsx') %>% rename('Exchange_Rate_braz' = ExchangeRateBrazil)                   
data[data == 0] <- NA

n = 24
braz_24 <- data[(nrow(data) - n + 1):nrow(data),]


ggplot(data = braz_24, aes(x = as.Date(Date))) +
  geom_point(aes(y = Exchange_Rate_braz, color = 'Ground Truth')) +
  geom_line(aes(y = NARFIMAX, color = 'NARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARIMAX, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = BSTSX, color = 'BSTS'), linewidth = 2) +
  geom_ribbon(aes(ymin = posterior_interval_braz_24$L, ymax = posterior_interval_braz_24$U), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "5 month", date_labels = "%Y-%m-%d", limits = c(ymd("2021-11-01"), ymd("2023-10-01"))) +
  scale_y_continuous(
    limits = round(c(min(braz_24$Exchange_Rate_braz) - 0.1, max(braz_24$BSTSX) + 0.1),1),  
    breaks = function(limits) seq(limits[1], limits[2], length.out = 5)
  ) +
  ylab('Exchange Rate') +
  xlab('Time') +
  labs(color = 'Models') +
  ggtitle('Brazil Exchange Rate: 24 Month Holdout') +
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



##################################################### Russia 24 #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()

data <- read_excel('Russia_Data.xlsx') %>% rename('Exchange_Rate_rus' = spot_ER_Russia)                   
exchange_rate_rus <- ts(data$Exchange_Rate_rus)
reg_rus <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)


n = 24
set.seed(100)
train_rus_24 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_24 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_24 <- reg_rus[1:length(train_rus_24),]
test_reg_rus_24 <- reg_rus[1:n,]

# 1- ARIMAX

set.seed(100)
arima_rus_24 <- auto.arima(train_rus_24, xreg = train_reg_rus_24)
arima_rus_24_pred <- forecast(arima_rus_24, h = n, xreg = test_reg_rus_24)

# 2- BSTSX

ss <- AddSemilocalLinearTrend(list(), train_rus_24)
ss <- AddSeasonal(ss, train_rus_24, nseasons = 12)
bsts_rus_24 <- bsts(train_rus_24 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_rus_24, train_rus_24))

bsts_rus_24_pred <- predict(bsts_rus_24, horizon = n, burn = SuggestBurn(.1, bsts_rus_24), newdata = test_reg_rus_24)

# 3 - ARFIMAX

arfima_rus_24 <- arfima(train_rus_24, xreg = train_reg_rus_24)
arfima_rus_24_pred <- forecast(arfima_rus_24, h = n, xreg = test_reg_rus_24)

# 4 - NARFIMA (skip = F)

arfima_er_rus_24 <-  residuals(arfima_rus_24)
arfima_er_rus_24[is.na(arfima_er_rus_24)] <-  0
set.seed(100)
narfima_rus_24 <-  auto.narfima(train_rus_24, arfima_er_rus_24, p = 1, q = 1, size = 5, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_rus_24, seed = 1)
set.seed(100)
narfima_rus_24_pred <-  forecast.narfima(narfima_rus_24, PI = FALSE, h = n, xreg = test_reg_rus_24)
res_narfima_rus_24 <- abs(narfima_rus_24_pred$mean - test_rus_24)
conf_narfima_rus_24 <- conformalRegressor(res_narfima_rus_24, sigmas = NULL)
conf_narfima_rus_24_pred <- predict(conf_narfima_rus_24, y_hat = narfima_rus_24_pred$mean, sigmas = NULL, confidence = 0.80, y_min = -Inf, y_max = Inf)
posterior_interval_rus_24 <- as_tibble(cbind.data.frame('L' = conf_narfima_rus_24_pred$lower_80,'U' = conf_narfima_rus_24_pred$upper_80))
mean(conf_narfima_rus_24_pred$upper_80 - conf_narfima_rus_24_pred$lower_80)

setwd('NARFIMA/Dataset/Dataset_Model_Forecasts/Russia')
getwd()

data <- read_excel('Russia Forecast 24.xlsx') %>% rename('Exchange_Rate_rus' = ExchangeRateRussia)                   
data[data == 0] <- NA

n = 24
rus_24 <- data[(nrow(data) - n + 1):nrow(data),]

ggplot(data = rus_24, aes(x = as.Date(Date))) +
  geom_point(aes(y = Exchange_Rate_rus, color = 'Ground Truth')) +
  geom_line(aes(y = NARFIMAX, color = 'NARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARIMAX, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = BSTSX, color = 'BSTS'), linewidth = 2) +
  geom_ribbon(aes(ymin = posterior_interval_rus_24$L, ymax = posterior_interval_rus_24$U), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "5 month", date_labels = "%Y-%m-%d", limits = c(ymd("2021-11-01"), ymd("2023-10-01"))) +
  scale_y_continuous(
    limits = round(c(min(rus_24$Exchange_Rate_rus) - 0.1, max(posterior_interval_rus_24$U) + 0.1),1),  
    breaks = function(limits) seq(limits[1], limits[2], length.out = 5)
  ) +
  ylab('Exchange Rate') +
  xlab('Time') +
  labs(color = 'Models') +
  ggtitle('Russia Exchange Rate: 24 Month Holdout') +
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



##################################################### India 24 #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()

data <- read_excel('India_Data.xlsx') %>% rename('Exchange_Rate_ind' = spot_ER_India)                   
exchange_rate_ind <- ts(data$Exchange_Rate_ind)
reg_ind <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)


n = 24
set.seed(100)
train_ind_24 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_24 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_24 <- reg_ind[1:length(train_ind_24),]
test_reg_ind_24 <- reg_ind[1:n,]

# 1- ARIMAX

set.seed(100)
arima_ind_24 <- auto.arima(train_ind_24, xreg = train_reg_ind_24)
arima_ind_24_pred <- forecast(arima_ind_24, h = n, xreg = test_reg_ind_24)

# 2- BSTSX

ss <- AddSemilocalLinearTrend(list(), train_ind_24)
ss <- AddSeasonal(ss, train_ind_24, nseasons = 12)
bsts_ind_24 <- bsts(train_ind_24 ~ ., 
                     state.specification = ss,
                     niter = 1000,
                     ping = 100,
                     seed = 100,
                     data = cbind(train_reg_ind_24, train_ind_24))

bsts_ind_24_pred <- predict(bsts_ind_24, horizon = n, burn = SuggestBurn(.1, bsts_ind_24), newdata = test_reg_ind_24)

# 3 - ARFIMAX

arfima_ind_24 <- arfima(train_ind_24, xreg = train_reg_ind_24)
arfima_ind_24_pred <- forecast(arfima_ind_24, h = n, xreg = test_reg_ind_24)

# 4 - NARFIMA (skip = T)

arfima_er_ind_24 <-  residuals(arfima_ind_24)
arfima_er_ind_24[is.na(arfima_er_ind_24)] <-  0
set.seed(100)
narfimaT_ind_24 <-  auto.narfima(train_ind_24, arfima_er_ind_24, p = 5, q = 4, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_ind_24)
set.seed(100)
narfimaT_ind_24_pred <-  forecast.narfima(narfimaT_ind_24, PI = FALSE, h = n, xreg = test_reg_ind_24)
res_narfimaT_ind_24 <- abs(narfimaT_ind_24_pred$mean - test_ind_24)
conf_narfimaT_ind_24 <- conformalRegressor(res_narfimaT_ind_24, sigmas = NULL)
conf_narfimaT_ind_24_pred <- predict(conf_narfimaT_ind_24, y_hat = narfimaT_ind_24_pred$mean, sigmas = NULL, confidence = 0.80, y_min = -Inf, y_max = Inf)
posterior_interval_indT_24 <- as_tibble(cbind.data.frame('L' = conf_narfimaT_ind_24_pred$lower_80,'U' = conf_narfimaT_ind_24_pred$upper_80))
mean(conf_narfimaT_ind_24_pred$upper_80 - conf_narfimaT_ind_24_pred$lower_80)

setwd('NARFIMA/Dataset/Dataset_Model_Forecasts/India')
getwd()

data <- read_excel('India Forecast 24.xlsx') %>% rename('Exchange_Rate_ind' = ExchangeRateIndia)                   
data[data == 0] <- NA

n = 24
ind_24 <- data[(nrow(data) - n + 1):nrow(data),]


ggplot(data = ind_24, aes(x = as.Date(Date))) +
  geom_point(aes(y = Exchange_Rate_ind, color = 'Ground Truth')) +
  geom_line(aes(y = NARFIMAXT, color = 'NARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARIMAX, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = BSTSX, color = 'BSTS'), linewidth = 2) +
  geom_ribbon(aes(ymin = posterior_interval_indT_24$L, ymax = posterior_interval_indT_24$U), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "5 month", date_labels = "%Y-%m-%d", limits = c(ymd("2021-11-01"), ymd("2023-10-01"))) +
  scale_y_continuous(
    limits = round(c(min(posterior_interval_indT_24$L) - 0.1, max(posterior_interval_indT_24$U) + 0.1),1),  
    breaks = function(limits) seq(limits[1], limits[2], length.out = 5)
  ) +
  ylab('Exchange Rate') +
  xlab('Time') +
  labs(color = 'Models') +
  ggtitle('India Exchange Rate: 24 Month Holdout') +
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



##################################################### China 24 #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('China_Data.xlsx') %>% rename('Exchange_Rate_chn' = spot_ER_China)                   
exchange_rate_chn <- ts(data$Exchange_Rate_chn)
reg_chn <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)


n = 24
set.seed(100)
train_chn_24 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_24 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_24 <- reg_chn[1:length(train_chn_24),]
test_reg_chn_24 <- reg_chn[1:n,]

# 1- ARIMAX

set.seed(100)
arima_chn_24 <- auto.arima(train_chn_24, xreg = train_reg_chn_24)
arima_chn_24_pred <- forecast(arima_chn_24, h = n, xreg = test_reg_chn_24)

# 2- BSTSX

ss <- AddSemilocalLinearTrend(list(), train_chn_24)
ss <- AddSeasonal(ss, train_chn_24, nseasons = 12)
bsts_chn_24 <- bsts(train_chn_24 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_chn_24, train_chn_24))

bsts_chn_24_pred <- predict(bsts_chn_24, horizon = n, burn = SuggestBurn(.1, bsts_chn_24), newdata = test_reg_chn_24)

# 3 - ARFIMAX

arfima_chn_24 <- arfima(train_chn_24, xreg = train_reg_chn_24)
arfima_chn_24_pred <- forecast(arfima_chn_24, h = n, xreg = test_reg_chn_24)

# 4 - NARFIMA (skip = T)

arfima_er_chn_24 <-  residuals(arfima_chn_24)
arfima_er_chn_24[is.na(arfima_er_chn_24)] <-  0
set.seed(100)
narfimaT_chn_24 <-  auto.narfima(train_chn_24, arfima_er_chn_24, p = 1, q = 2, size = 4, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_chn_24)
set.seed(100)
narfimaT_chn_24_pred <-  forecast.narfima(narfimaT_chn_24, PI = FALSE, h = n, xreg = test_reg_chn_24)
res_narfimaT_chn_24 <- abs(narfimaT_chn_24_pred$mean - test_chn_24)
conf_narfimaT_chn_24 <- conformalRegressor(res_narfimaT_chn_24, sigmas = NULL)
conf_narfimaT_chn_24_pred <- predict(conf_narfimaT_chn_24, y_hat = narfimaT_chn_24_pred$mean, sigmas = NULL, confidence = 0.80, y_min = -Inf, y_max = Inf)
posterior_interval_chnT_24 <- as_tibble(cbind.data.frame('L' = conf_narfimaT_chn_24_pred$lower_80,'U' = conf_narfimaT_chn_24_pred$upper_80))
mean(conf_narfimaT_chn_24_pred$upper_80 - conf_narfimaT_chn_24_pred$lower_80)

setwd('NARFIMA/Dataset/Dataset_Model_Forecasts/China')
getwd()

data <- read_excel('China Forecast 24.xlsx') %>% rename('Exchange_Rate_chn' = ExchangeRateChina)                   
data[data == 0] <- NA

n = 24
chn_24 <- data[(nrow(data) - n + 1):nrow(data),]


ggplot(data = chn_24, aes(x = as.Date(Date))) +
  geom_point(aes(y = Exchange_Rate_chn, color = 'Ground Truth')) +
  geom_line(aes(y = NARFIMAXT, color = 'NARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARIMAX, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = BSTSX, color = 'BSTS'), linewidth = 2) +
  geom_ribbon(aes(ymin = posterior_interval_chnT_24$L, ymax = posterior_interval_chnT_24$U), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "5 month", date_labels = "%Y-%m-%d", limits = c(ymd("2021-11-01"), ymd("2023-10-01"))) +
  scale_y_continuous(
    limits = round(c(min(chn_24$BSTSX) - 0.1, max(posterior_interval_chnT_24$U) + 0.1),1),  
    breaks = function(limits) seq(limits[1], limits[2], length.out = 5)
  ) +
  ylab('Exchange Rate') +
  xlab('Time') +
  labs(color = 'Models') +
  ggtitle('China Exchange Rate: 24 Month Holdout') +
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
