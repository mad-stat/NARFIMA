library(forecast)
library(tidyverse)
library(Metrics)
library(ggplot2)
library(tsDyn)
library(forecastHybrid)
library(WaveletArima)
library(FinTS)
library(tseries)
library(rugarch)
library(bsts)
library(readxl)


setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('Brazil_Data.xlsx') %>% rename('Exchange_Rate_braz' = spot_ER_Brazil)                   
exchange_rate_braz <- ts(data$Exchange_Rate_braz)
reg_braz <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)


##################################################### Evaluation Function #####################################################


evaluate <- function(test,pred,model){
  MAPE <- mape(test,pred)*100
  SMAPE <- smape(test,pred)
  MAE <- mae(test,pred)
  MASE <- mase(test,pred)
  RMSE <- rmse(test,pred)
  
  return(tibble('MODEL' = model,
                'MAPE' = MAPE,
                'SMAPE' = SMAPE,
                'MAE' = MAE,
                'MASE' = MASE,
                'RMSE' = RMSE))
}


##################################################### Brazil 1 #####################################################

n = 1
set.seed(100)
train_braz_1 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_1 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_1 <- reg_braz[1:length(train_braz_1),]
test_reg_braz_1 <- matrix(c(reg_braz[1,1], reg_braz[1,2], reg_braz[1,3], reg_braz[1,4], reg_braz[1,5]), ncol = 5, nrow = 1)
colnames(test_reg_braz_1) <- c( "Oil_price_growth_rate_WTI", "SR_Interest_rate_diff_B_U", "global_EPU(PPP)", "US_EMV" ,"US_MPU")

model_evaluate_braz_1 <- tibble()  
predict_braz_1 <- tibble(Date = as.Date(data$Date[length(train_braz_1) + 1:length(test_braz_1)]))  


# 1- Naive
set.seed(100)
naive_braz_1_pred <- naive(train_braz_1, h = n)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, naive_braz_1_pred$mean, model = 'Naive'))
predict_braz_1 <- predict_braz_1 %>% mutate('Naive' = naive_braz_1_pred$mean)


# 2- AR
set.seed(100)
ar_braz_1 <- ar(train_braz_1)
ar_braz_1_pred <- predict(ar_braz_1, n.ahead = n)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, ar_braz_1_pred$pred, model = 'AR(2)'))
predict_braz_1 <- predict_braz_1 %>% mutate('AR(2)' = ar_braz_1_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_braz_1 <- auto.arima(train_braz_1, xreg = train_reg_braz_1)
arima_braz_1_pred <- forecast(arima_braz_1, h = n, xreg = test_reg_braz_1)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, arima_braz_1_pred$mean, model = 'ARIMAx(0,1,0)'))
predict_braz_1 <- predict_braz_1 %>% mutate('ARIMAx(0,1,0)' = arima_braz_1_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_braz_1 <- nnetar(train_braz_1, xreg = train_reg_braz_1)
arnn_braz_1_pred <- forecast(arnn_braz_1, h = n, xreg = test_reg_braz_1)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, arnn_braz_1_pred$mean, model = paste0('ARNNx(', arnn_braz_1$p, ',', arnn_braz_1$size , ')')))
predict_braz_1 <- predict_braz_1 %>% mutate('ARNNx(2,4)' = arnn_braz_1_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_braz_1 <- arfima(train_braz_1, xreg = train_reg_braz_1)
arfima_braz_1_pred <- forecast(arfima_braz_1, h = n, xreg = test_reg_braz_1)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, arfima_braz_1_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_braz_1$ar),0), ',', round(arfima_braz_1$d,5), ',' , max(order(arfima_braz_1$ma),0) , ')')))
predict_braz_1 <- predict_braz_1 %>% mutate('ARFIMAx(0,0.49274,5)' = arfima_braz_1_pred$mean)


# 6- ETS
set.seed(100)
ets_braz_1 <- ets(train_braz_1)
ets_braz_1_pred <- forecast(ets_braz_1, h = n)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, ets_braz_1_pred$mean, model = 'ETS'))
predict_braz_1 <- predict_braz_1 %>% mutate('ETS' = ets_braz_1_pred$mean)


# 7- SETAR
set.seed(100)
setar_braz_1 <- setar(train_braz_1, m = 4)
setar_braz_1_pred <- predict(setar_braz_1, n.ahead = n)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, setar_braz_1_pred, model = 'SETAR'))
predict_braz_1 <- predict_braz_1 %>% mutate('SETAR' = setar_braz_1_pred)


# 8- TBATS
set.seed(100)
tbats_braz_1 <- tbats(train_braz_1)
tbats_braz_1_pred <- forecast(tbats_braz_1, h = n)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, tbats_braz_1_pred$mean, model = 'TBATS'))
predict_braz_1 <- predict_braz_1 %>% mutate('TBATS' = tbats_braz_1_pred$mean)


# 9- Garch
set.seed(100)
ArchTest(train_braz_1)
garch(train_braz_1, grad = 'numerical', trabraze = F)
garch_braz_1 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_braz_1_fit <- ugarchfit(garch_braz_1, data = train_braz_1)
garch_braz_1_pred <- ugarchforecast(garch_braz_1_fit, n.ahead = n)
GARCH_braz_1_pred <- as.vector(garch_braz_1_pred@forecast$sigmaFor + garch_braz_1_pred@forecast$seriesFor)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, GARCH_braz_1_pred, model = 'GARCH'))
predict_braz_1 <- predict_braz_1 %>% mutate('GARCH' = GARCH_braz_1_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_braz_1)
ss <- AddSeasonal(ss, train_braz_1, nseasons = 12)
bsts_braz_1 <- bsts(train_braz_1 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_braz_1, train_braz_1))

bsts_braz_1_pred <- predict(bsts_braz_1, horizon = n, burn = SuggestBurn(.1, bsts_braz_1), newdata = test_reg_braz_1)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, bsts_braz_1_pred$mean, model = 'BSTSx'))
predict_braz_1 <- predict_braz_1 %>% mutate('BSTSx' = bsts_braz_1_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/Brazil')
getwd()


# 11- DeepAR
braz_reg_1_DeepAR <- as.numeric(read_csv('DeepAR_1.csv')[2])  
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, braz_reg_1_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
braz_reg_1_NBEATS <- as.numeric(read_csv('NBEATS_1.csv')[2])      
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, braz_reg_1_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
braz_reg_1_NHiTS <- as.numeric(read_csv('NHiTS_1.csv')[2])  
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, braz_reg_1_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
braz_reg_1_Dlinear <- as.numeric(read_csv('Dlinear_1.csv')[2]) 
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, braz_reg_1_Dlinear, model = 'DLinearx'))


# 15- NLinearx
braz_reg_1_Nlinear <- as.numeric(read_csv('Nlinear_1.csv')[2])
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, braz_reg_1_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
braz_reg_1_TSMixer <- as.numeric(read_csv('TSMIxer_1.csv')[2]) 
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, braz_reg_1_TSMixer, model = 'TSMixerx'))


# 17 - NARFIMA
arfima_er_braz_1 <-  residuals(arfima_braz_1)
arfima_er_braz_1[is.na(arfima_er_braz_1)] <-  0

set.seed(100)
narfima_braz_1 <-  auto.narfima(train_braz_1, arfima_er_braz_1, p = 5, q = 4, size = 4, lambda = 0, lambdae = 0, repeats = 1000, skip = F, xreg = train_reg_braz_1)
set.seed(100)
narfima_braz_1_pred <-  forecast.narfima(narfima_braz_1, PI = FALSE, h = n, xreg = test_reg_braz_1)
set.seed(100)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, narfima_braz_1_pred$mean, model = paste0('NARFIMA(', narfima_braz_1$p, ',', narfima_braz_1$q, ',', narfima_braz_1$size, ',', narfima_braz_1$skip,')' )))
predict_braz_1 <- predict_braz_1 %>% mutate('NARFIMA(5,4,4,F)' = narfima_braz_1_pred$mean)


write.csv(model_evaluate_braz_1, 'Brazil Evaluation 1.csv', row.names = FALSE)
write.csv(predict_braz_1, 'Brazil Forecast 1.csv', row.names = FALSE)



##################################################### Brazil 3 #####################################################

n = 3
set.seed(100)
train_braz_3 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_3 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_3 <- reg_braz[1:length(train_braz_3),]
test_reg_braz_3 <- reg_braz[1:n,]

model_evaluate_braz_3 <- tibble()  
predict_braz_3 <- tibble(Date = as.Date(data$Date[length(train_braz_3) + 1:length(test_braz_3)]))  


# 1- Naive
set.seed(100)
naive_braz_3_pred <- naive(train_braz_3, h = n)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, naive_braz_3_pred$mean, model = 'Naive'))
predict_braz_3 <- predict_braz_3 %>% mutate('Naive' = naive_braz_3_pred$mean)


# 2- AR
set.seed(100)
ar_braz_3 <- ar(train_braz_3)
ar_braz_3_pred <- predict(ar_braz_3, n.ahead = n)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, ar_braz_3_pred$pred, model = 'AR(2)'))
predict_braz_3 <- predict_braz_3 %>% mutate('AR(2)' = ar_braz_3_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_braz_3 <- auto.arima(train_braz_3, xreg = train_reg_braz_3)
arima_braz_3_pred <- forecast(arima_braz_3, h = n, xreg = test_reg_braz_3)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, arima_braz_3_pred$mean, model = 'ARIMAx(0,1,0)'))
predict_braz_3 <- predict_braz_3 %>% mutate('ARIMAx(0,1,0)' = arima_braz_3_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_braz_3 <- nnetar(train_braz_3, xreg = train_reg_braz_3)
arnn_braz_3_pred <- forecast(arnn_braz_3, h = n, xreg = test_reg_braz_3)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, arnn_braz_3_pred$mean, model = paste0('ARNNx(', arnn_braz_3$p, ',', arnn_braz_3$size , ')')))
predict_braz_3 <- predict_braz_3 %>% mutate('ARNNx(2,4)' = arnn_braz_3_pred$mean)


# 5- ARFIMAx
arfima_braz_3 <- arfima(train_braz_3, xreg = train_reg_braz_3)
arfima_braz_3_pred <- forecast(arfima_braz_3, h = n, xreg = test_reg_braz_3)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, arfima_braz_3_pred$mean, model = paste0('ARFIMA(', max(order(arfima_braz_3$ar),0), ',', round(arfima_braz_3$d,5), ',' , max(order(arfima_braz_3$ma),0) , ')')))
predict_braz_3 <- predict_braz_3 %>% mutate('ARFIMA(0,0.49242,5)' = arfima_braz_3_pred$mean)


# 6- ETS
ets_braz_3 <- ets(train_braz_3)
ets_braz_3_pred <- forecast(ets_braz_3, h = n)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, ets_braz_3_pred$mean, model = 'ETS'))
predict_braz_3 <- predict_braz_3 %>% mutate('ETS' = ets_braz_3_pred$mean)


# 7- SETAR
setar_braz_3 <- setar(train_braz_3, m = 4)
setar_braz_3_pred <- predict(setar_braz_3, n.ahead = n)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, setar_braz_3_pred, model = 'SETAR'))
predict_braz_3 <- predict_braz_3 %>% mutate('SETAR' = setar_braz_3_pred)


# 8- TBATS
tbats_braz_3 <- tbats(train_braz_3)
tbats_braz_3_pred <- forecast(tbats_braz_3, h = n)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, tbats_braz_3_pred$mean, model = 'TBATS'))
predict_braz_3 <- predict_braz_3 %>% mutate('TBATS' = tbats_braz_3_pred$mean)


# 9 - Garch
ArchTest(train_braz_3)
garch(train_braz_3, grad = 'numerical', trabraze = F)
garch_braz_3 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_braz_3_fit <- ugarchfit(garch_braz_3, data = train_braz_3)
garch_braz_3_pred <- ugarchforecast(garch_braz_3_fit, n.ahead = n)
GARCH_braz_3_pred <- as.vector(garch_braz_3_pred@forecast$sigmaFor + garch_braz_3_pred@forecast$seriesFor)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, GARCH_braz_3_pred, model = 'GARCH'))
predict_braz_3 <- predict_braz_3 %>% mutate('GARCH' = GARCH_braz_3_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_braz_3)
ss <- AddSeasonal(ss, train_braz_3, nseasons = 12)
bsts_braz_3 <- bsts(train_braz_3 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_braz_3, train_braz_3))

bsts_braz_3_pred <- predict(bsts_braz_3, horizon = n, burn = SuggestBurn(.1, bsts_braz_3), newdata = test_reg_braz_3)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, bsts_braz_3_pred$mean, model = 'BSTSx'))
predict_braz_3 <- predict_braz_3 %>% mutate('BSTSx' = bsts_braz_3_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/Brazil')
getwd()


# 11- DeepAR
braz_reg_3_DeepAR <- unlist(read_csv('DeepAR_3.csv')[2])     
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, braz_reg_3_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
braz_reg_3_NBEATS <- unlist(read_csv('NBEATS_3.csv')[2])      
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, braz_reg_3_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
braz_reg_3_NHiTS <- unlist(read_csv('NHiTS_3.csv')[2])      
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, braz_reg_3_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
braz_reg_3_Dlinear <- unlist(read_csv('Dlinear_3.csv')[2]) 
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, braz_reg_3_Dlinear, model = 'DLinearx'))


# 15- NLinearx
braz_reg_3_Nlinear <- unlist(read_csv('Nlinear_3.csv')[2]) 
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, braz_reg_3_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
braz_reg_3_TSMixer <- unlist(read_csv('TSMIxer_3.csv')[2])  
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, braz_reg_3_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_braz_3 <-  residuals(arfima_braz_3)
arfima_er_braz_3[is.na(arfima_er_braz_3)] <-  0

set.seed(100)
narfima_braz_3 <-  auto.narfima(train_braz_3, arfima_er_braz_3, p = 2, q = 5, size = 2, lambda = 0, lambdae = 0, repeats = 1000, skip = F, xreg = train_reg_braz_3)
set.seed(100)
narfima_braz_3_pred <-  forecast.narfima(narfima_braz_3, PI = FALSE, h = n, xreg = test_reg_braz_3)
set.seed(100)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, narfima_braz_3_pred$mean, model = paste0('NARFIMA(', narfima_braz_3$p, ',', narfima_braz_3$q, ',', narfima_braz_3$size, ',', narfima_braz_3$skip, ')')))
predict_braz_3 <- predict_braz_3 %>% mutate('NARFIMA(2,5,2,F)' = narfima_braz_3_pred$mean)


write.csv(model_evaluate_braz_3, 'Brazil Evaluation 3.csv', row.names = FALSE)
write.csv(predict_braz_3, 'Brazil Forecast 3.csv', row.names = FALSE)



##################################################### Brazil 6 #####################################################

n = 6
set.seed(100)
train_braz_6 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_6 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_6 <- reg_braz[1:length(train_braz_6),]
test_reg_braz_6 <- reg_braz[1:n,]

model_evaluate_braz_6 <- tibble()  
predict_braz_6 <- tibble(Date = as.Date(data$Date[length(train_braz_6) + 1:length(test_braz_6)]))   


# 1- Naive
set.seed(100)
naive_braz_6_pred <- naive(train_braz_6, h = n)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, naive_braz_6_pred$mean, model = 'Naive'))
predict_braz_6 <- predict_braz_6 %>% mutate('Naive' = naive_braz_6_pred$mean)


# 2- AR
set.seed(100)
ar_braz_6 <- ar(train_braz_6)
ar_braz_6_pred <- predict(ar_braz_6, n.ahead = n)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, ar_braz_6_pred$pred, model = 'AR(2)'))
predict_braz_6 <- predict_braz_6 %>% mutate('AR(2)' = ar_braz_6_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_braz_6 <- auto.arima(train_braz_6, xreg = train_reg_braz_6)
arima_braz_6_pred <- forecast(arima_braz_6, h = n, xreg = test_reg_braz_6)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, arima_braz_6_pred$mean, model = 'ARIMAx(0,1,0)'))
predict_braz_6 <- predict_braz_6 %>% mutate('ARIMAx(0,1,0)' = arima_braz_6_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_braz_6 <- nnetar(train_braz_6, xreg = train_reg_braz_6)
arnn_braz_6_pred <- forecast(arnn_braz_6, h = n, xreg = test_reg_braz_6)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, arnn_braz_6_pred$mean, model = paste0('ARNNx(', arnn_braz_6$p, ',', arnn_braz_6$size , ')')))
predict_braz_6 <- predict_braz_6 %>% mutate('ARNNx(2,4)' = arnn_braz_6_pred$mean)


# 5 - ARFIMAx
set.seed(100)
arfima_braz_6 <- arfima(train_braz_6, xreg = train_reg_braz_6)
arfima_braz_6_pred <- forecast(arfima_braz_6, h = n, xreg = test_reg_braz_6)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, arfima_braz_6_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_braz_6$ar),0), ',', round(arfima_braz_6$d,5), ',' , max(order(arfima_braz_6$ma),0) , ')')))
predict_braz_6 <- predict_braz_6 %>% mutate('ARFIMAx(0,0.49234,5)' = arfima_braz_6_pred$mean)


# 6- ETS
set.seed(100)
ets_braz_6 <- ets(train_braz_6)
ets_braz_6_pred <- forecast(ets_braz_6, h = n)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, ets_braz_6_pred$mean, model = 'ETS'))
predict_braz_6 <- predict_braz_6 %>% mutate('ETS' = ets_braz_6_pred$mean)


# 7- SETAR
set.seed(100)
setar_braz_6 <- setar(train_braz_6, m = 4)
setar_braz_6_pred <- predict(setar_braz_6, n.ahead = n)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, setar_braz_6_pred, model = 'SETAR'))
predict_braz_6 <- predict_braz_6 %>% mutate('SETAR' = setar_braz_6_pred)


# 8- TBATS
set.seed(100)
tbats_braz_6 <- tbats(train_braz_6)
tbats_braz_6_pred <- forecast(tbats_braz_6, h = n)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, tbats_braz_6_pred$mean, model = 'TBATS'))
predict_braz_6 <- predict_braz_6 %>% mutate('TBATS' = tbats_braz_6_pred$mean)


# 9- Garch
set.seed(100)
ArchTest(train_braz_6)
garch(train_braz_6, grad = 'numerical', trabraze = F)
garch_braz_6 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_braz_6_fit <- ugarchfit(garch_braz_6, data = train_braz_6)
garch_braz_6_pred <- ugarchforecast(garch_braz_6_fit, n.ahead = n)
GARCH_braz_6_pred <- as.vector(garch_braz_6_pred@forecast$sigmaFor + garch_braz_6_pred@forecast$seriesFor)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, GARCH_braz_6_pred, model = 'GARCH'))
predict_braz_6 <- predict_braz_6 %>% mutate('GARCH' = GARCH_braz_6_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_braz_6)
ss <- AddSeasonal(ss, train_braz_6, nseasons = 12)
bsts_braz_6 <- bsts(train_braz_6 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_braz_6, train_braz_6))

bsts_braz_6_pred <- predict(bsts_braz_6, horizon = n, burn = SuggestBurn(.1, bsts_braz_6), newdata = test_reg_braz_6)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, bsts_braz_6_pred$mean, model = 'BSTSx'))
predict_braz_6 <- predict_braz_6 %>% mutate('BSTSx' = bsts_braz_6_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/Brazil')
getwd()


# 11- DeepAR
braz_reg_6_DeepAR <- unlist(read_csv('DeepAR_6.csv')[2])  
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, braz_reg_6_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
braz_reg_6_NBEATS <- unlist(read_csv('NBEATS_6.csv')[2])      
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, braz_reg_6_NBEATS, model = 'NBEATSx'))


# 16- NHiTSx
braz_reg_6_NHiTS <- unlist(read_csv('NHiTS_6.csv')[2])  
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, braz_reg_6_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
braz_reg_6_Dlinear <- unlist(read_csv('Dlinear_6.csv')[2]) 
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, braz_reg_6_Dlinear, model = 'DLinearx'))


# 15- NLinearx
braz_reg_6_Nlinear <- unlist(read_csv('Nlinear_6.csv')[2])
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, braz_reg_6_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
braz_reg_6_TSMixer <- unlist(read_csv('TSMIxer_6.csv')[2]) 
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, braz_reg_6_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_braz_6 <-  residuals(arfima_braz_6)
arfima_er_braz_6[is.na(arfima_er_braz_6)] <-  0

set.seed(100)
narfima_braz_6 <-  auto.narfima(train_braz_6, arfima_er_braz_6, p = 1, q = 2, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = F, xreg = train_reg_braz_6)
set.seed(100)
narfima_braz_6_pred <-  forecast.narfima(narfima_braz_6, PI = FALSE, h = n, xreg = test_reg_braz_6)
set.seed(100)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, narfima_braz_6_pred$mean, model = paste0('NARFIMA(', narfima_braz_6$p, ',', narfima_braz_6$q, ',', narfima_braz_6$size, ',', narfima_braz_6$skip,')')))
predict_braz_6 <- predict_braz_6 %>% mutate('NARFIMA(1,2,1,F)' = narfima_braz_6_pred$mean)


write.csv(model_evaluate_braz_6, 'Brazil Evaluation 6.csv', row.names = FALSE)
write.csv(predict_braz_6, 'Brazil Forecast 6.csv', row.names = FALSE)



##################################################### Brazil 12 #####################################################

n = 12
set.seed(100)
train_braz_12 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_12 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_12 <- reg_braz[1:length(train_braz_12),]
test_reg_braz_12 <- reg_braz[1:n,]

model_evaluate_braz_12 <- tibble()  
predict_braz_12 <- tibble(Date = as.Date(data$Date[length(train_braz_12) + 1:length(test_braz_12)]))  


# 1- Naive
set.seed(100)
naive_braz_12_pred <- naive(train_braz_12, h = n)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, naive_braz_12_pred$mean, model = 'Naive'))
predict_braz_12 <- predict_braz_12 %>% mutate('Naive' = naive_braz_12_pred$mean)


# 2- AR
set.seed(100)
ar_braz_12 <- ar(train_braz_12)
ar_braz_12_pred <- predict(ar_braz_12, n.ahead = n)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, ar_braz_12_pred$pred, model = 'AR(2)'))
predict_braz_12 <- predict_braz_12 %>% mutate('AR(2)' = ar_braz_12_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_braz_12 <- auto.arima(train_braz_12, xreg = train_reg_braz_12)
arima_braz_12_pred <- forecast(arima_braz_12, h = n, xreg = test_reg_braz_12)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, arima_braz_12_pred$mean, model = 'ARIMAx(0,1,0)'))
predict_braz_12 <- predict_braz_12 %>% mutate('ARIMAx(0,1,0)' = arima_braz_12_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_braz_12 <- nnetar(train_braz_12, xreg = train_reg_braz_12)
arnn_braz_12_pred <- forecast(arnn_braz_12, h = n, xreg = test_reg_braz_12)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, arnn_braz_12_pred$mean, model = paste0('ARNNx(', arnn_braz_12$p, ',', arnn_braz_12$size , ')')))
predict_braz_12 <- predict_braz_12 %>% mutate('ARNNx(2,4)' = arnn_braz_12_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_braz_12 <- arfima(train_braz_12, xreg = train_reg_braz_12)
arfima_braz_12_pred <- forecast(arfima_braz_12, h = n, xreg = test_reg_braz_12)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, arfima_braz_12_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_braz_12$ar),0), ',', round(arfima_braz_12$d,5), ',' , max(order(arfima_braz_12$ma),0) , ')')))
predict_braz_12 <- predict_braz_12 %>% mutate('ARFIMAx(0,0.49233,5)' = arfima_braz_12_pred$mean)


# 6- ETS
set.seed(100)
ets_braz_12 <- ets(train_braz_12)
ets_braz_12_pred <- forecast(ets_braz_12, h = n)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, ets_braz_12_pred$mean, model = 'ETS'))
predict_braz_12 <- predict_braz_12 %>% mutate('ETS' = ets_braz_12_pred$mean)


# 7- SETAR
set.seed(100)
setar_braz_12 <- setar(train_braz_12, m = 4)
setar_braz_12_pred <- predict(setar_braz_12, n.ahead = n)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, setar_braz_12_pred, model = 'SETAR'))
predict_braz_12 <- predict_braz_12 %>% mutate('SETAR' = setar_braz_12_pred)


# 8- TBATS
set.seed(100)
tbats_braz_12 <- tbats(train_braz_12)
tbats_braz_12_pred <- forecast(tbats_braz_12, h = n)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, tbats_braz_12_pred$mean, model = 'TBATS'))
predict_braz_12 <- predict_braz_12 %>% mutate('TBATS' = tbats_braz_12_pred$mean)


# 9 - Garch
set.seed(100)
ArchTest(train_braz_12)
garch(train_braz_12, grad = 'numerical', trabraze = F)
garch_braz_12 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_braz_12_fit <- ugarchfit(garch_braz_12, data = train_braz_12)
garch_braz_12_pred <- ugarchforecast(garch_braz_12_fit, n.ahead = n)
GARCH_braz_12_pred <- as.vector(garch_braz_12_pred@forecast$sigmaFor + garch_braz_12_pred@forecast$seriesFor)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, GARCH_braz_12_pred, model = 'GARCH'))
predict_braz_12 <- predict_braz_12 %>% mutate('GARCH' = GARCH_braz_12_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_braz_12)
ss <- AddSeasonal(ss, train_braz_12, nseasons = 12)
bsts_braz_12 <- bsts(train_braz_12 ~ ., 
                     state.specification = ss,
                     niter = 1000,
                     ping = 100,
                     seed = 100,
                     data = cbind(train_reg_braz_12, train_braz_12))

bsts_braz_12_pred <- predict(bsts_braz_12, horizon = n, burn = SuggestBurn(.1, bsts_braz_12), newdata = test_reg_braz_12)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, bsts_braz_12_pred$mean, model = 'BSTSx'))
predict_braz_12 <- predict_braz_12 %>% mutate('BSTSx' = bsts_braz_12_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/Brazil')
getwd()


# 11- DeepAR
braz_reg_12_DeepAR <- unlist(read_csv('DeepAR_12.csv')[2])  
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, braz_reg_12_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
braz_reg_12_NBEATS <- unlist(read_csv('NBEATS_12.csv')[2])      
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, braz_reg_12_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
braz_reg_12_NHiTS <- unlist(read_csv('NHiTS_12.csv')[2])  
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, braz_reg_12_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
braz_reg_12_Dlinear <- unlist(read_csv('Dlinear_12.csv')[2]) 
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, braz_reg_12_Dlinear, model = 'DLinearx'))


# 15- NLinearx
braz_reg_12_Nlinear <- unlist(read_csv('Nlinear_12.csv')[2])
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, braz_reg_12_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
braz_reg_12_TSMixer <- unlist(read_csv('TSMIxer_12.csv')[2]) 
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, braz_reg_12_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_braz_12 <-  residuals(arfima_braz_12)
arfima_er_braz_12[is.na(arfima_er_braz_12)] <-  0

set.seed(100)
narfima_braz_12 <-  auto.narfima(train_braz_12, arfima_er_braz_12, p = 1, q = 1, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = F, xreg = train_reg_braz_12)
set.seed(100)
narfima_braz_12_pred <-  forecast.narfima(narfima_braz_12, PI = FALSE, h = n, xreg = test_reg_braz_12)
set.seed(100)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, narfima_braz_12_pred$mean, model = paste0('NARFIMAX(', narfima_braz_12$p, ',', narfima_braz_12$q, ',', narfima_braz_12$size, ',', narfima_braz_12$skip,')')))
predict_braz_12 <- predict_braz_12 %>% mutate('NARFIMAX(1,1,1,F)' = narfima_braz_12_pred$mean)


write.csv(model_evaluate_braz_12, 'Brazil Evaluation 12.csv', row.names = FALSE)
write.csv(predict_braz_12, 'Brazil Forecast 12.csv', row.names = FALSE)



##################################################### Brazil 24 #####################################################

n = 24
set.seed(100)
train_braz_24 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_24 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_24 <- reg_braz[1:length(train_braz_24),]
test_reg_braz_24 <- reg_braz[1:n,]

model_evaluate_braz_24 <- tibble()  
predict_braz_24 <- tibble(Date = as.Date(data$Date[length(train_braz_24) + 1:length(test_braz_24)]))  


# 1- Naive
set.seed(100)
naive_braz_24_pred <- naive(train_braz_24, h = n)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, naive_braz_24_pred$mean, model = 'Naive'))
predict_braz_24 <- predict_braz_24 %>% mutate('Naive' = naive_braz_24_pred$mean)


# 2- AR
set.seed(100)
ar_braz_24 <- ar(train_braz_24)
ar_braz_24_pred <- predict(ar_braz_24, n.ahead = n)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, ar_braz_24_pred$pred, model = 'AR(1)'))
predict_braz_24 <- predict_braz_24 %>% mutate('AR(1)' = ar_braz_24_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_braz_24 <- auto.arima(train_braz_24, xreg = train_reg_braz_24)
arima_braz_24_pred <- forecast(arima_braz_24, h = n, xreg = test_reg_braz_24)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, arima_braz_24_pred$mean, model = 'ARIMAx(0,1,0)'))
predict_braz_24 <- predict_braz_24 %>% mutate('ARIMAx(0,1,0)' = arima_braz_24_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_braz_24 <- nnetar(train_braz_24, xreg = train_reg_braz_24)
arnn_braz_24_pred <- forecast(arnn_braz_24, h = n, xreg = test_reg_braz_24)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, arnn_braz_24_pred$mean, model = paste0('ARNNx(', arnn_braz_24$p, ',', arnn_braz_24$size , ')')))
predict_braz_24 <- predict_braz_24 %>% mutate('ARNNx(1,4)' = arnn_braz_24_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_braz_24 <- arfima(train_braz_24, xreg = train_reg_braz_24)
arfima_braz_24_pred <- forecast(arfima_braz_24, h = n, xreg = test_reg_braz_24)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, arfima_braz_24_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_braz_24$ar),0), ',', round(arfima_braz_24$d,5), ',' , max(order(arfima_braz_24$ma),0) , ')')))
predict_braz_24 <- predict_braz_24 %>% mutate('ARFIMAx(0,0.49261,5)' = arfima_braz_24_pred$mean)


# 6- ETS
set.seed(100)
ets_braz_24 <- ets(train_braz_24)
ets_braz_24_pred <- forecast(ets_braz_24, h = n)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, ets_braz_24_pred$mean, model = 'ETS'))
predict_braz_24 <- predict_braz_24 %>% mutate('ETS' = ets_braz_24_pred$mean)


# 7- SETAR
set.seed(100)
setar_braz_24 <- setar(train_braz_24, m = 4)
setar_braz_24_pred <- predict(setar_braz_24, n.ahead = n)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, setar_braz_24_pred, model = 'SETAR'))
predict_braz_24 <- predict_braz_24 %>% mutate('SETAR' = setar_braz_24_pred)


# 8- TBATS
set.seed(100)
tbats_braz_24 <- tbats(train_braz_24)
tbats_braz_24_pred <- forecast(tbats_braz_24, h = n)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, tbats_braz_24_pred$mean, model = 'TBATS'))
predict_braz_24 <- predict_braz_24 %>% mutate('TBATS' = tbats_braz_24_pred$mean)


# 9 - Garch
set.seed(100)
ArchTest(train_braz_24)
garch(train_braz_24, grad = 'numerical', trabraze = F)
garch_braz_24 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_braz_24_fit <- ugarchfit(garch_braz_24, data = train_braz_24)
garch_braz_24_pred <- ugarchforecast(garch_braz_24_fit, n.ahead = n)
GARCH_braz_24_pred <- as.vector(garch_braz_24_pred@forecast$sigmaFor + garch_braz_24_pred@forecast$seriesFor)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, GARCH_braz_24_pred, model = 'GARCH'))
predict_braz_24 <- predict_braz_24 %>% mutate('GARCH' = GARCH_braz_24_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_braz_24)
ss <- AddSeasonal(ss, train_braz_24, nseasons = 12)
bsts_braz_24 <- bsts(train_braz_24 ~ ., 
                     state.specification = ss,
                     niter = 1000,
                     ping = 100,
                     seed = 100,
                     data = cbind(train_reg_braz_24, train_braz_24))

bsts_braz_24_pred <- predict(bsts_braz_24, horizon = n, burn = SuggestBurn(.1, bsts_braz_24), newdata = test_reg_braz_24)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, bsts_braz_24_pred$mean, model = 'BSTSx'))
predict_braz_24 <- predict_braz_24 %>% mutate('BSTSx' = bsts_braz_24_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/Brazil')
getwd()


# 11- DeepAR
braz_reg_24_DeepAR <- unlist(read_csv('DeepAR_24.csv')[2])  
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, braz_reg_24_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
braz_reg_24_NBEATS <- unlist(read_csv('NBEATS_24.csv')[2])      
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, braz_reg_24_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
braz_reg_24_NHiTS <- unlist(read_csv('NHiTS_24.csv')[2])  
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, braz_reg_24_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
braz_reg_24_Dlinear <- unlist(read_csv('Dlinear_24.csv')[2]) 
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, braz_reg_24_Dlinear, model = 'DLinearx'))


# 15- NLinearx
braz_reg_24_Nlinear <- unlist(read_csv('Nlinear_24.csv')[2])
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, braz_reg_24_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
braz_reg_24_TSMixer <- unlist(read_csv('TSMIxer_24.csv')[2]) 
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, braz_reg_24_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_braz_24 <-  residuals(arfima_braz_24)
arfima_er_braz_24[is.na(arfima_er_braz_24)] <-  0

set.seed(100)
narfima_braz_24 <-  auto.narfima(train_braz_24, arfima_er_braz_24, p = 2, q = 5, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = F, xreg = train_reg_braz_24)
set.seed(100)
narfima_braz_24_pred <-  forecast.narfima(narfima_braz_24, PI = FALSE, h = n, xreg = test_reg_braz_24)
set.seed(100)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, narfima_braz_24_pred$mean, model = paste0('NARFIMA(', narfima_braz_24$p, ',', narfima_braz_24$q, ',', narfima_braz_24$size, ',', narfima_braz_24$skip,')')))
predict_braz_24 <- predict_braz_24 %>% mutate('NARFIMA(2,5,1,F)' = narfima_braz_24_pred$mean)


write.csv(model_evaluate_braz_24, 'Brazil Evaluation 24.csv', row.names = FALSE)
write.csv(predict_braz_24, 'Brazil Forecast 24.csv', row.names = FALSE)



##################################################### Brazil 48 #####################################################

n = 48
set.seed(100)
train_braz_48 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_48 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_48 <- reg_braz[1:length(train_braz_48),]
test_reg_braz_48 <- reg_braz[1:n,]

model_evaluate_braz_48 <- tibble()  
predict_braz_48 <- tibble(Date = as.Date(data$Date[length(train_braz_48) + 1:length(test_braz_48)]))  


# 1- Naive
set.seed(100)
naive_braz_48_pred <- naive(train_braz_48, h = n)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, naive_braz_48_pred$mean, model = 'Naive'))
predict_braz_48 <- predict_braz_48 %>% mutate('Naive' = naive_braz_48_pred$mean)


# 2- AR
set.seed(100)
ar_braz_48 <- ar(train_braz_48)
ar_braz_48_pred <- predict(ar_braz_48, n.ahead = n)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, ar_braz_48_pred$pred, model = 'AR(2)'))
predict_braz_48 <- predict_braz_48 %>% mutate('AR(2)' = ar_braz_48_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_braz_48 <- auto.arima(train_braz_48, xreg = train_reg_braz_48)
arima_braz_48_pred <- forecast(arima_braz_48, h = n, xreg = test_reg_braz_48)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, arima_braz_48_pred$mean, model = 'ARIMAx(0,1,0)'))
predict_braz_48 <- predict_braz_48 %>% mutate('ARIMAx(0,1,0)' = arima_braz_48_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_braz_48 <- nnetar(train_braz_48, xreg = train_reg_braz_48)
arnn_braz_48_pred <- forecast(arnn_braz_48, h = n, xreg = test_reg_braz_48)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, arnn_braz_48_pred$mean, model = paste0('ARNNx(', arnn_braz_48$p, ',', arnn_braz_48$size , ')')))
predict_braz_48 <- predict_braz_48 %>% mutate('ARNNx(2,4)' = arnn_braz_48_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_braz_48 <- arfima(train_braz_48, xreg = train_reg_braz_48)
arfima_braz_48_pred <- forecast(arfima_braz_48, h = n, xreg = test_reg_braz_48)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, arfima_braz_48_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_braz_48$ar),0), ',', round(arfima_braz_48$d,5), ',' , max(order(arfima_braz_48$ma),0) , ')')))
predict_braz_48 <- predict_braz_48 %>% mutate('ARFIMAx(0,0.49035,5)' = arfima_braz_48_pred$mean)


# 6- ETS
set.seed(100)
ets_braz_48 <- ets(train_braz_48)
ets_braz_48_pred <- forecast(ets_braz_48, h = n)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, ets_braz_48_pred$mean, model = 'ETS'))
predict_braz_48 <- predict_braz_48 %>% mutate('ETS' = ets_braz_48_pred$mean)


# 7- SETAR
set.seed(100)
setar_braz_48 <- setar(train_braz_48, m = 4)
setar_braz_48_pred <- predict(setar_braz_48, n.ahead = n)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, setar_braz_48_pred, model = 'SETAR'))
predict_braz_48 <- predict_braz_48 %>% mutate('SETAR' = setar_braz_48_pred)


# 8- TBATS
set.seed(100)
tbats_braz_48 <- tbats(train_braz_48)
tbats_braz_48_pred <- forecast(tbats_braz_48, h = n)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, tbats_braz_48_pred$mean, model = 'TBATS'))
predict_braz_48 <- predict_braz_48 %>% mutate('TBATS' = tbats_braz_48_pred$mean)


# 9- Garch
set.seed(100)
ArchTest(train_braz_48)
garch(train_braz_48, grad = 'numerical', trabraze = F)
garch_braz_48 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_braz_48_fit <- ugarchfit(garch_braz_48, data = train_braz_48)
garch_braz_48_pred <- ugarchforecast(garch_braz_48_fit, n.ahead = n)
GARCH_braz_48_pred <- as.vector(garch_braz_48_pred@forecast$sigmaFor + garch_braz_48_pred@forecast$seriesFor)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, GARCH_braz_48_pred, model = 'GARCH'))
predict_braz_48 <- predict_braz_48 %>% mutate('GARCH' = GARCH_braz_48_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_braz_48)
ss <- AddSeasonal(ss, train_braz_48, nseasons = 12)
bsts_braz_48 <- bsts(train_braz_48 ~ ., 
                     state.specification = ss,
                     niter = 1000,
                     ping = 100,
                     seed = 100,
                     data = cbind(train_reg_braz_48, train_braz_48))

bsts_braz_48_pred <- predict(bsts_braz_48, horizon = n, burn = SuggestBurn(.1, bsts_braz_48), newdata = test_reg_braz_48)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, bsts_braz_48_pred$mean, model = 'BSTSx'))
predict_braz_48 <- predict_braz_48 %>% mutate('BSTSx' = bsts_braz_48_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/Brazil')
getwd()


# 11- DeepAR
braz_reg_48_DeepAR <- unlist(read_csv('DeepAR_48.csv')[2])  
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, braz_reg_48_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
braz_reg_48_NBEATS <- unlist(read_csv('NBEATS_48.csv')[2])      
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, braz_reg_48_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
braz_reg_48_NHiTS <- unlist(read_csv('NHiTS_48.csv')[2])  
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, braz_reg_48_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
braz_reg_48_Dlinear <- unlist(read_csv('Dlinear_48.csv')[2]) 
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, braz_reg_48_Dlinear, model = 'DLinearx'))


# 15- NLinearx
braz_reg_48_Nlinear <- unlist(read_csv('Nlinear_48.csv')[2])
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, braz_reg_48_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
braz_reg_48_TSMixer <- unlist(read_csv('TSMIxer_48.csv')[2]) 
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, braz_reg_48_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_braz_48 <-  residuals(arfima_braz_48)
arfima_er_braz_48[is.na(arfima_er_braz_48)] <-  0

set.seed(100)
narfimaT_braz_48 <-  auto.narfima(train_braz_48, arfima_er_braz_48, p = 4, q = 2, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_braz_48)
set.seed(100)
narfimaT_braz_48_pred <-  forecast.narfima(narfimaT_braz_48, PI = FALSE, h = n, xreg = test_reg_braz_48)
set.seed(100)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, narfimaT_braz_48_pred$mean, model = paste0('NARFIMA(', narfimaT_braz_48$p, ',', narfimaT_braz_48$q, ',', narfimaT_braz_48$size, ',', narfimaT_braz_48$skip,')')))
predict_braz_48 <- predict_braz_48 %>% mutate('NARFIMA(4,2,1,T)' = narfimaT_braz_48_pred$mean)



write.csv(model_evaluate_braz_48, 'Brazil Evaluation 48.csv', row.names = FALSE)
write.csv(predict_braz_48, 'Brazil Forecast 48.csv', row.names = FALSE)
