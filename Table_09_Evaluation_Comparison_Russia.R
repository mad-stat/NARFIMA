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
data <- read_excel('Russia_Data.xlsx') %>% rename('Exchange_Rate_rus' = spot_ER_Russia)                   
exchange_rate_rus <- ts(data$Exchange_Rate_rus)
reg_rus <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)


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


##################################################### Russia 1 #####################################################

n = 1
set.seed(100)
train_rus_1 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_1 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_1 <- reg_rus[1:length(train_rus_1),]
test_reg_rus_1 <- matrix(c(reg_rus[1,1], reg_rus[1,2], reg_rus[1,3], reg_rus[1,4], reg_rus[1,5]), ncol = 5, nrow = 1)
colnames(test_reg_rus_1) <- c("Oil_price_growth_rate_WTI", "SR_Interest_rate_diff_R_U", "global_EPU(PPP)", "US_EMV" ,"US_MPU")

model_evaluate_rus_1 <- tibble()  
predict_rus_1 <- tibble(Date = as.Date(data$Date[length(train_rus_1) + 1:length(test_rus_1)]))  


# 1- Naive
set.seed(100)
naive_rus_1_pred <- naive(train_rus_1, h = n)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, naive_rus_1_pred$mean, model = 'Naive'))
predict_rus_1 <- predict_rus_1 %>% mutate('Naive' = naive_rus_1_pred$mean)


# 2- AR
set.seed(100)
ar_rus_1 <- ar(train_rus_1)
ar_rus_1_pred <- predict(ar_rus_1, n.ahead = n)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, ar_rus_1_pred$pred, model = 'AR(1)'))
predict_rus_1 <- predict_rus_1 %>% mutate('AR(1)' = ar_rus_1_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_rus_1 <- auto.arima(train_rus_1, xreg = train_reg_rus_1)
arima_rus_1_pred <- forecast(arima_rus_1, h = n, xreg = test_reg_rus_1)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, arima_rus_1_pred$mean, model = 'ARIMAx(0,1,3)'))
predict_rus_1 <- predict_rus_1 %>% mutate('ARIMAx(0,1,3)' = arima_rus_1_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_rus_1 <- nnetar(train_rus_1, xreg = train_reg_rus_1)
arnn_rus_1_pred <- forecast(arnn_rus_1, h = n, xreg = test_reg_rus_1)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, arnn_rus_1_pred$mean, model = paste0('ARNNx(', arnn_rus_1$p, ',', arnn_rus_1$size , ')')))
predict_rus_1 <- predict_rus_1 %>% mutate('ARNNx(1,4)' = arnn_rus_1_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_rus_1 <- arfima(train_rus_1, xreg = train_reg_rus_1)
arfima_rus_1_pred <- forecast(arfima_rus_1, h = n, xreg = test_reg_rus_1)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, arfima_rus_1_pred$mean, model = paste0('ARFIMA(', max(order(arfima_rus_1$ar),0), ',', round(arfima_rus_1$d,5), ',' , max(order(arfima_rus_1$ma),0) , ')')))
predict_rus_1 <- predict_rus_1 %>% mutate('ARFIMAx(0,0.49021,5)' = arfima_rus_1_pred$mean)


# 6- ETS
set.seed(100)
ets_rus_1 <- ets(train_rus_1)
ets_rus_1_pred <- forecast(ets_rus_1, h = n)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, ets_rus_1_pred$mean, model = 'ETS'))
predict_rus_1 <- predict_rus_1 %>% mutate('ETS' = ets_rus_1_pred$mean)


# 7- SETAR
set.seed(100)
setar_rus_1 <- setar(train_rus_1, m = 4)
setar_rus_1_pred <- predict(setar_rus_1, n.ahead = n)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, setar_rus_1_pred, model = 'SETAR'))
predict_rus_1 <- predict_rus_1 %>% mutate('SETAR' = setar_rus_1_pred)


# 8- TBATS
set.seed(100)
tbats_rus_1 <- tbats(train_rus_1)
tbats_rus_1_pred <- forecast(tbats_rus_1, h = n)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, tbats_rus_1_pred$mean, model = 'TBATS'))
predict_rus_1 <- predict_rus_1 %>% mutate('TBATS' = tbats_rus_1_pred$mean)


# 9- Garch
set.seed(100)
ArchTest(train_rus_1)
garch(train_rus_1, grad = 'numerical', traruse = F)
garch_rus_1 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_rus_1_fit <- ugarchfit(garch_rus_1, data = train_rus_1)
garch_rus_1_pred <- ugarchforecast(garch_rus_1_fit, n.ahead = n)
GARCH_rus_1_pred <- as.vector(garch_rus_1_pred@forecast$sigmaFor + garch_rus_1_pred@forecast$seriesFor)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, GARCH_rus_1_pred, model = 'GARCH'))
predict_rus_1 <- predict_rus_1 %>% mutate('GARCH' = GARCH_rus_1_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_rus_1)
ss <- AddSeasonal(ss, train_rus_1, nseasons = 12)
bsts_rus_1 <- bsts(train_rus_1 ~ ., 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   data = cbind(train_reg_rus_1, train_rus_1))

bsts_rus_1_pred <- predict(bsts_rus_1, horizon = n, burn = SuggestBurn(.1, bsts_rus_1), newdata = test_reg_rus_1)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, bsts_rus_1_pred$mean, model = 'BSTSx'))
predict_rus_1 <- predict_rus_1 %>% mutate('BSTSx' = bsts_rus_1_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/Russia')
getwd()


# 11- DeepAR
rus_reg_1_DeepAR <- as.numeric(read_csv('DeepAR_1.csv')[2])  
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, rus_reg_1_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
rus_reg_1_NBEATS <- as.numeric(read_csv('NBEATS_1.csv')[2])      
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, rus_reg_1_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
rus_reg_1_NHiTS <- as.numeric(read_csv('NHiTS_1.csv')[2])  
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, rus_reg_1_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
rus_reg_1_Dlinear <- as.numeric(read_csv('Dlinear_1.csv')[2]) 
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, rus_reg_1_Dlinear, model = 'DLinearx'))


# 15- NLinearx
rus_reg_1_Nlinear <- as.numeric(read_csv('Nlinear_1.csv')[2])
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, rus_reg_1_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
rus_reg_1_TSMixer <- as.numeric(read_csv('TSMIxer_1.csv')[2]) 
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, rus_reg_1_TSMixer, model = 'TSMixerx'))


# 17 - NARFIMA
arfima_er_rus_1 <-  residuals(arfima_rus_1)
arfima_er_rus_1[is.na(arfima_er_rus_1)] <-  0

set.seed(100)
narfimaT_rus_1 <-  auto.narfima(train_rus_1, arfima_er_rus_1, p = 1, q = 2, size = 2, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_rus_1)
set.seed(100)
narfimaT_rus_1_pred <-  forecast.narfima(narfimaT_rus_1, PI = FALSE, h = n, xreg = test_reg_rus_1)
set.seed(100)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, narfimaT_rus_1_pred$mean, model = paste0('NARFIMA(', narfimaT_rus_1$p, ',', narfimaT_rus_1$q, ',', narfimaT_rus_1$size, ',', narfimaT_rus_1$skip,')')))
predict_rus_1 <- predict_rus_1 %>% mutate('NARFIMA(1,2,2,T)' = narfimaT_rus_1_pred$mean)


write.csv(model_evaluate_rus_1, 'Russia Evaluation 1.csv', row.names = FALSE)
write.csv(predict_rus_1, 'Russia Forecast 1.csv', row.names = FALSE)



##################################################### Russia 3 #####################################################

n = 3
set.seed(100)
train_rus_3 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_3 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_3 <- reg_rus[1:length(train_rus_3),]
test_reg_rus_3 <- reg_rus[1:n,]

model_evaluate_rus_3 <- tibble()  
predict_rus_3 <- tibble(Date = as.Date(data$Date[length(train_rus_3) + 1:length(test_rus_3)]))  


# 1- Naive
set.seed(100)
naive_rus_3_pred <- naive(train_rus_3, h = n)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, naive_rus_3_pred$mean, model = 'Naive'))
predict_rus_3 <- predict_rus_3 %>% mutate('Naive' = naive_rus_3_pred$mean)


# 2- AR
set.seed(100)
ar_rus_3 <- ar(train_rus_3)
ar_rus_3_pred <- predict(ar_rus_3, n.ahead = n)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, ar_rus_3_pred$pred, model = 'AR(1)'))
predict_rus_3 <- predict_rus_3 %>% mutate('AR(1)' = ar_rus_3_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_rus_3 <- auto.arima(train_rus_3, xreg = train_reg_rus_3)
arima_rus_3_pred <- forecast(arima_rus_3, h = n, xreg = test_reg_rus_3)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, arima_rus_3_pred$mean, model = 'ARIMAx(0,1,3)'))
predict_rus_3 <- predict_rus_3 %>% mutate('ARIMAx(0,1,3)' = arima_rus_3_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_rus_3 <- nnetar(train_rus_3, xreg = train_reg_rus_3)
arnn_rus_3_pred <- forecast(arnn_rus_3, h = n, xreg = test_reg_rus_3)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, arnn_rus_3_pred$mean, model = paste0('ARNNx(', arnn_rus_3$p, ',', arnn_rus_3$size , ')')))
predict_rus_3 <- predict_rus_3 %>% mutate('ARNNx(1,4)' = arnn_rus_3_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_rus_3 <- arfima(train_rus_3, xreg = train_reg_rus_3)
arfima_rus_3_pred <- forecast(arfima_rus_3, h = n, xreg = test_reg_rus_3)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, arfima_rus_3_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_rus_3$ar),0), ',', round(arfima_rus_3$d,5), ',' , max(order(arfima_rus_3$ma),0) , ')')))
predict_rus_3 <- predict_rus_3 %>% mutate('ARFIMAx(0,0.49002,5)' = arfima_rus_3_pred$mean)


# 6- ETS
ets_rus_3 <- ets(train_rus_3)
ets_rus_3_pred <- forecast(ets_rus_3, h = n)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, ets_rus_3_pred$mean, model = 'ETS'))
predict_rus_3 <- predict_rus_3 %>% mutate('ETS' = ets_rus_3_pred$mean)


# 7- SETAR
setar_rus_3 <- setar(train_rus_3, m = 4)
setar_rus_3_pred <- predict(setar_rus_3, n.ahead = n)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, setar_rus_3_pred, model = 'SETAR'))
predict_rus_3 <- predict_rus_3 %>% mutate('SETAR' = setar_rus_3_pred)


# 8- TBATS
tbats_rus_3 <- tbats(train_rus_3)
tbats_rus_3_pred <- forecast(tbats_rus_3, h = n)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, tbats_rus_3_pred$mean, model = 'TBATS'))
predict_rus_3 <- predict_rus_3 %>% mutate('TBATS' = tbats_rus_3_pred$mean)


# 9 - Garch
ArchTest(train_rus_3)
garch(train_rus_3, grad = 'numerical', traruse = F)
garch_rus_3 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_rus_3_fit <- ugarchfit(garch_rus_3, data = train_rus_3)
garch_rus_3_pred <- ugarchforecast(garch_rus_3_fit, n.ahead = n)
GARCH_rus_3_pred <- as.vector(garch_rus_3_pred@forecast$sigmaFor + garch_rus_3_pred@forecast$seriesFor)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, GARCH_rus_3_pred, model = 'GARCH'))
predict_rus_3 <- predict_rus_3 %>% mutate('GARCH' = GARCH_rus_3_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_rus_3)
ss <- AddSeasonal(ss, train_rus_3, nseasons = 12)
bsts_rus_3 <- bsts(train_rus_3 ~ ., 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   data = cbind(train_reg_rus_3, train_rus_3))

bsts_rus_3_pred <- predict(bsts_rus_3, horizon = n, burn = SuggestBurn(.1, bsts_rus_3), newdata = test_reg_rus_3)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, bsts_rus_3_pred$mean, model = 'BSTSx'))
predict_rus_3 <- predict_rus_3 %>% mutate('BSTSx' = bsts_rus_3_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/Russia')
getwd()


# 11- DeepAR
rus_reg_3_DeepAR <- unlist(read_csv('DeepAR_3.csv')[2])     
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, rus_reg_3_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
rus_reg_3_NBEATS <- unlist(read_csv('NBEATS_3.csv')[2])      
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, rus_reg_3_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
rus_reg_3_NHiTS <- unlist(read_csv('NHiTS_3.csv')[2])      
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, rus_reg_3_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
rus_reg_3_Dlinear <- unlist(read_csv('Dlinear_3.csv')[2]) 
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, rus_reg_3_Dlinear, model = 'DLinearx'))


# 15- NLinearx
rus_reg_3_Nlinear <- unlist(read_csv('Nlinear_3.csv')[2]) 
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, rus_reg_3_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
rus_reg_3_TSMixer <- unlist(read_csv('TSMIxer_3.csv')[2])  
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, rus_reg_3_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_rus_3 <-  residuals(arfima_rus_3)
arfima_er_rus_3[is.na(arfima_er_rus_3)] <-  0

set.seed(100)
narfima_rus_3 <-  auto.narfima(train_rus_3, arfima_er_rus_3, p = 1, q = 1, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = F, xreg = train_reg_rus_3)
set.seed(100)
narfima_rus_3_pred <-  forecast.narfima(narfima_rus_3, PI = FALSE, h = n, xreg = test_reg_rus_3)
set.seed(100)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, narfima_rus_3_pred$mean, model = paste0('NARFIMA(', narfima_rus_3$p, ',', narfima_rus_3$q, ',', narfima_rus_3$size, ',', narfima_rus_3$skip,')')))
predict_rus_3 <- predict_rus_3 %>% mutate('NARFIMA(1,1,1,F)' = narfima_rus_3_pred$mean)



write.csv(model_evaluate_rus_3, 'Russia Evaluation 3.csv', row.names = FALSE)
write.csv(predict_rus_3, 'Russia Forecast 3.csv', row.names = FALSE)



##################################################### Russia 6 #####################################################

n = 6
set.seed(100)
train_rus_6 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_6 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_6 <- reg_rus[1:length(train_rus_6),]
test_reg_rus_6 <- reg_rus[1:n,]

model_evaluate_rus_6 <- tibble()  
predict_rus_6 <- tibble(Date = as.Date(data$Date[length(train_rus_6) + 1:length(test_rus_6)]))   


# 1- Naive
set.seed(100)
naive_rus_6_pred <- naive(train_rus_6, h = n)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, naive_rus_6_pred$mean, model = 'Naive'))
predict_rus_6 <- predict_rus_6 %>% mutate('Naive' = naive_rus_6_pred$mean)


# 2- AR
set.seed(100)
ar_rus_6 <- ar(train_rus_6)
ar_rus_6_pred <- predict(ar_rus_6, n.ahead = n)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, ar_rus_6_pred$pred, model = 'AR(14)'))
predict_rus_6 <- predict_rus_6 %>% mutate('AR(14)' = ar_rus_6_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_rus_6 <- auto.arima(train_rus_6, xreg = train_reg_rus_6)
arima_rus_6_pred <- forecast(arima_rus_6, h = n, xreg = test_reg_rus_6)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, arima_rus_6_pred$mean, model = 'ARIMAx(0,1,4)'))
predict_rus_6 <- predict_rus_6 %>% mutate('ARIMAx(0,1,4)' = arima_rus_6_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_rus_6 <- nnetar(train_rus_6, xreg = train_reg_rus_6)
arnn_rus_6_pred <- forecast(arnn_rus_6, h = n, xreg = test_reg_rus_6)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, arnn_rus_6_pred$mean, model = paste0('ARNNx(', arnn_rus_6$p, ',', arnn_rus_6$size , ')')))
predict_rus_6 <- predict_rus_6 %>% mutate('ARNNx(14,10)' = arnn_rus_6_pred$mean)


# 5 - ARFIMAx
set.seed(100)
arfima_rus_6 <- arfima(train_rus_6, xreg = train_reg_rus_6)
arfima_rus_6_pred <- forecast(arfima_rus_6, h = n, xreg = test_reg_rus_6)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, arfima_rus_6_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_rus_6$ar),0), ',', round(arfima_rus_6$d,5), ',' , max(order(arfima_rus_6$ma),0) , ')')))
predict_rus_6 <- predict_rus_6 %>% mutate('ARFIMAx(0,0.48916,5)' = arfima_rus_6_pred$mean)


# 6- ETS
set.seed(100)
ets_rus_6 <- ets(train_rus_6)
ets_rus_6_pred <- forecast(ets_rus_6, h = n)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, ets_rus_6_pred$mean, model = 'ETS'))
predict_rus_6 <- predict_rus_6 %>% mutate('ETS' = ets_rus_6_pred$mean)


# 7- SETAR
set.seed(100)
setar_rus_6 <- setar(train_rus_6, m = 4)
setar_rus_6_pred <- predict(setar_rus_6, n.ahead = n)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, setar_rus_6_pred, model = 'SETAR'))
predict_rus_6 <- predict_rus_6 %>% mutate('SETAR' = setar_rus_6_pred)


# 8- TBATS
set.seed(100)
tbats_rus_6 <- tbats(train_rus_6)
tbats_rus_6_pred <- forecast(tbats_rus_6, h = n)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, tbats_rus_6_pred$mean, model = 'TBATS'))
predict_rus_6 <- predict_rus_6 %>% mutate('TBATS' = tbats_rus_6_pred$mean)


# 9- Garch
set.seed(100)
ArchTest(train_rus_6)
garch(train_rus_6, grad = 'numerical', traruse = F)
garch_rus_6 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_rus_6_fit <- ugarchfit(garch_rus_6, data = train_rus_6)
garch_rus_6_pred <- ugarchforecast(garch_rus_6_fit, n.ahead = n)
GARCH_rus_6_pred <- as.vector(garch_rus_6_pred@forecast$sigmaFor + garch_rus_6_pred@forecast$seriesFor)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, GARCH_rus_6_pred, model = 'GARCH'))
predict_rus_6 <- predict_rus_6 %>% mutate('GARCH' = GARCH_rus_6_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_rus_6)
ss <- AddSeasonal(ss, train_rus_6, nseasons = 12)
bsts_rus_6 <- bsts(train_rus_6 ~ ., 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   data = cbind(train_reg_rus_6, train_rus_6))

bsts_rus_6_pred <- predict(bsts_rus_6, horizon = n, burn = SuggestBurn(.1, bsts_rus_6), newdata = test_reg_rus_6)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, bsts_rus_6_pred$mean, model = 'BSTSx'))
predict_rus_6 <- predict_rus_6 %>% mutate('BSTSx' = bsts_rus_6_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/Russia')
getwd()


# 11- DeepAR
rus_reg_6_DeepAR <- unlist(read_csv('DeepAR_6.csv')[2])  
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, rus_reg_6_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
rus_reg_6_NBEATS <- unlist(read_csv('NBEATS_6.csv')[2])      
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, rus_reg_6_NBEATS, model = 'NBEATSx'))


# 16- NHiTSx
rus_reg_6_NHiTS <- unlist(read_csv('NHiTS_6.csv')[2])  
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, rus_reg_6_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
rus_reg_6_Dlinear <- unlist(read_csv('Dlinear_6.csv')[2]) 
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, rus_reg_6_Dlinear, model = 'DLinearx'))


# 15- NLinearx
rus_reg_6_Nlinear <- unlist(read_csv('Nlinear_6.csv')[2])
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, rus_reg_6_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
rus_reg_6_TSMixer <- unlist(read_csv('TSMIxer_6.csv')[2]) 
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, rus_reg_6_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_rus_6 <-  residuals(arfima_rus_6)
arfima_er_rus_6[is.na(arfima_er_rus_6)] <-  0

set.seed(100)
narfima_rus_6 <-  auto.narfima(train_rus_6, arfima_er_rus_6, p = 1, q = 1, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = F, xreg = train_reg_rus_6)
set.seed(100)
narfima_rus_6_pred <-  forecast.narfima(narfima_rus_6, PI = FALSE, h = n, xreg = test_reg_rus_6)
set.seed(100)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, narfima_rus_6_pred$mean, model = paste0('NARFIMA(', narfima_rus_6$p, ',', narfima_rus_6$q, ',', narfima_rus_6$size, ',', narfima_rus_6$skip,')')))
predict_rus_6 <- predict_rus_6 %>% mutate('NARFIMA(1,1,1,F)' = narfima_rus_6_pred$mean)


write.csv(model_evaluate_rus_6, 'Russia Evaluation 6.csv', row.names = FALSE)
write.csv(predict_rus_6, 'Russia Forecast 6.csv', row.names = FALSE)



##################################################### Russia 12 #####################################################

n = 12
set.seed(100)
train_rus_12 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_12 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_12 <- reg_rus[1:length(train_rus_12),]
test_reg_rus_12 <- reg_rus[1:n,]

model_evaluate_rus_12 <- tibble()  
predict_rus_12 <- tibble(Date = as.Date(data$Date[length(train_rus_12) + 1:length(test_rus_12)]))  



# 1- Naive
set.seed(100)
naive_rus_12_pred <- naive(train_rus_12, h = n)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, naive_rus_12_pred$mean, model = 'Naive'))
predict_rus_12 <- predict_rus_12 %>% mutate('Naive' = naive_rus_12_pred$mean)


# 2- AR
set.seed(100)
ar_rus_12 <- ar(train_rus_12)
ar_rus_12_pred <- predict(ar_rus_12, n.ahead = n)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, ar_rus_12_pred$pred, model = 'AR(1)'))
predict_rus_12 <- predict_rus_12 %>% mutate('AR(1)' = ar_rus_12_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_rus_12 <- auto.arima(train_rus_12, xreg = train_reg_rus_12)
arima_rus_12_pred <- forecast(arima_rus_12, h = n, xreg = test_reg_rus_12)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, arima_rus_12_pred$mean, model = 'ARIMAx(2,1,2)'))
predict_rus_12 <- predict_rus_12 %>% mutate('ARIMAx(2,1,2)' = arima_rus_12_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_rus_12 <- nnetar(train_rus_12, xreg = train_reg_rus_12)
arnn_rus_12_pred <- forecast(arnn_rus_12, h = n, xreg = test_reg_rus_12)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, arnn_rus_12_pred$mean, model = paste0('ARNNx(', arnn_rus_12$p, ',', arnn_rus_12$size , ')')))
predict_rus_12 <- predict_rus_12 %>% mutate('ARNNx(1,4)' = arnn_rus_12_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_rus_12 <- arfima(train_rus_12, xreg = train_reg_rus_12)
arfima_rus_12_pred <- forecast(arfima_rus_12, h = n, xreg = test_reg_rus_12)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, arfima_rus_12_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_rus_12$ar),0), ',', round(arfima_rus_12$d,5), ',' , max(order(arfima_rus_12$ma),0) , ')')))
predict_rus_12 <- predict_rus_12 %>% mutate('ARFIMAx(0,0.49401,2)' = arfima_rus_12_pred$mean)


# 6- ETS
set.seed(100)
ets_rus_12 <- ets(train_rus_12)
ets_rus_12_pred <- forecast(ets_rus_12, h = n)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, ets_rus_12_pred$mean, model = 'ETS'))
predict_rus_12 <- predict_rus_12 %>% mutate('ETS' = ets_rus_12_pred$mean)


# 7- SETAR
set.seed(100)
setar_rus_12 <- setar(train_rus_12, m = 4)
setar_rus_12_pred <- predict(setar_rus_12, n.ahead = n)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, setar_rus_12_pred, model = 'SETAR'))
predict_rus_12 <- predict_rus_12 %>% mutate('SETAR' = setar_rus_12_pred)


# 8- TBATS
set.seed(100)
tbats_rus_12 <- tbats(train_rus_12)
tbats_rus_12_pred <- forecast(tbats_rus_12, h = n)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, tbats_rus_12_pred$mean, model = 'TBATS'))
predict_rus_12 <- predict_rus_12 %>% mutate('TBATS' = tbats_rus_12_pred$mean)


# 9 - Garch
set.seed(100)
ArchTest(train_rus_12)
garch(train_rus_12, grad = 'numerical', traruse = F)
garch_rus_12 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_rus_12_fit <- ugarchfit(garch_rus_12, data = train_rus_12)
garch_rus_12_pred <- ugarchforecast(garch_rus_12_fit, n.ahead = n)
GARCH_rus_12_pred <- as.vector(garch_rus_12_pred@forecast$sigmaFor + garch_rus_12_pred@forecast$seriesFor)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, GARCH_rus_12_pred, model = 'GARCH'))
predict_rus_12 <- predict_rus_12 %>% mutate('GARCH' = GARCH_rus_12_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_rus_12)
ss <- AddSeasonal(ss, train_rus_12, nseasons = 12)
bsts_rus_12 <- bsts(train_rus_12 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_rus_12, train_rus_12))

bsts_rus_12_pred <- predict(bsts_rus_12, horizon = n, burn = SuggestBurn(.1, bsts_rus_12), newdata = test_reg_rus_12)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, bsts_rus_12_pred$mean, model = 'BSTSx'))
predict_rus_12 <- predict_rus_12 %>% mutate('BSTSx' = bsts_rus_12_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/Russia')
getwd()


# 11- DeepAR
rus_reg_12_DeepAR <- unlist(read_csv('DeepAR_12.csv')[2])  
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, rus_reg_12_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
rus_reg_12_NBEATS <- unlist(read_csv('NBEATS_12.csv')[2])      
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, rus_reg_12_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
rus_reg_12_NHiTS <- unlist(read_csv('NHiTS_12.csv')[2])  
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, rus_reg_12_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
rus_reg_12_Dlinear <- unlist(read_csv('Dlinear_12.csv')[2]) 
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, rus_reg_12_Dlinear, model = 'DLinearx'))


# 15- NLinearx
rus_reg_12_Nlinear <- unlist(read_csv('Nlinear_12.csv')[2])
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, rus_reg_12_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
rus_reg_12_TSMixer <- unlist(read_csv('TSMIxer_12.csv')[2]) 
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, rus_reg_12_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_rus_12 <-  residuals(arfima_rus_12)
arfima_er_rus_12[is.na(arfima_er_rus_12)] <-  0

set.seed(100)
narfimaT_rus_12 <-  auto.narfima(train_rus_12, arfima_er_rus_12, p = 5, q = 2, size = 5, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_rus_12)
set.seed(100)
narfimaT_rus_12_pred <-  forecast.narfima(narfimaT_rus_12, PI = FALSE, h = n, xreg = test_reg_rus_12)
set.seed(100)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, narfimaT_rus_12_pred$mean, model = paste0('NARFIMA(', narfimaT_rus_12$p, ',', narfimaT_rus_12$q, ',', narfimaT_rus_12$size, ',', narfimaT_rus_12$skip,')')))
predict_rus_12 <- predict_rus_12 %>% mutate('NARFIMA(5,2,5,T)' = narfimaT_rus_12_pred$mean)


write.csv(model_evaluate_rus_12, 'Russia Evaluation 12.csv', row.names = FALSE)
write.csv(predict_rus_12, 'Russia Forecast 12.csv', row.names = FALSE)



##################################################### Russia 24 #####################################################

n = 24
set.seed(100)
train_rus_24 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_24 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_24 <- reg_rus[1:length(train_rus_24),]
test_reg_rus_24 <- reg_rus[1:n,]

model_evaluate_rus_24 <- tibble()  
predict_rus_24 <- tibble(Date = as.Date(data$Date[length(train_rus_24) + 1:length(test_rus_24)]))  


# 1- Naive
set.seed(100)
naive_rus_24_pred <- naive(train_rus_24, h = n)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, naive_rus_24_pred$mean, model = 'Naive'))
predict_rus_24 <- predict_rus_24 %>% mutate('Naive' = naive_rus_24_pred$mean)


# 2- AR
set.seed(100)
ar_rus_24 <- ar(train_rus_24)
ar_rus_24_pred <- predict(ar_rus_24, n.ahead = n)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, ar_rus_24_pred$pred, model = 'AR(2)'))
predict_rus_24 <- predict_rus_24 %>% mutate('AR(2)' = ar_rus_24_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_rus_24 <- auto.arima(train_rus_24, xreg = train_reg_rus_24)
arima_rus_24_pred <- forecast(arima_rus_24, h = n, xreg = test_reg_rus_24)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, arima_rus_24_pred$mean, model = 'ARIMAx(2,1,2)'))
predict_rus_24 <- predict_rus_24 %>% mutate('ARIMAx(2,1,2)' = arima_rus_24_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_rus_24 <- nnetar(train_rus_24, xreg = train_reg_rus_24)
arnn_rus_24_pred <- forecast(arnn_rus_24, h = n, xreg = test_reg_rus_24)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, arnn_rus_24_pred$mean, model = paste0('ARNNx(', arnn_rus_24$p, ',', arnn_rus_24$size , ')')))
predict_rus_24 <- predict_rus_24 %>% mutate('ARNNx(2,4)' = arnn_rus_24_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_rus_24 <- arfima(train_rus_24, xreg = train_reg_rus_24)
arfima_rus_24_pred <- forecast(arfima_rus_24, h = n, xreg = test_reg_rus_24)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, arfima_rus_24_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_rus_24$ar),0), ',', round(arfima_rus_24$d,5), ',' , max(order(arfima_rus_24$ma),0) , ')')))
predict_rus_24 <- predict_rus_24 %>% mutate('ARFIMAx(0,0.4893,5)' = arfima_rus_24_pred$mean)


# 6- ETS
set.seed(100)
ets_rus_24 <- ets(train_rus_24)
ets_rus_24_pred <- forecast(ets_rus_24, h = n)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, ets_rus_24_pred$mean, model = 'ETS'))
predict_rus_24 <- predict_rus_24 %>% mutate('ETS' = ets_rus_24_pred$mean)


# 7- SETAR
set.seed(100)
setar_rus_24 <- setar(train_rus_24, m = 4)
setar_rus_24_pred <- predict(setar_rus_24, n.ahead = n)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, setar_rus_24_pred, model = 'SETAR'))
predict_rus_24 <- predict_rus_24 %>% mutate('SETAR' = setar_rus_24_pred)


# 8- TBATS
set.seed(100)
tbats_rus_24 <- tbats(train_rus_24)
tbats_rus_24_pred <- forecast(tbats_rus_24, h = n)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, tbats_rus_24_pred$mean, model = 'TBATS'))
predict_rus_24 <- predict_rus_24 %>% mutate('TBATS' = tbats_rus_24_pred$mean)


# 9 - Garch
set.seed(100)
ArchTest(train_rus_24)
garch(train_rus_24, grad = 'numerical', traruse = F)
garch_rus_24 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_rus_24_fit <- ugarchfit(garch_rus_24, data = train_rus_24)
garch_rus_24_pred <- ugarchforecast(garch_rus_24_fit, n.ahead = n)
GARCH_rus_24_pred <- as.vector(garch_rus_24_pred@forecast$sigmaFor + garch_rus_24_pred@forecast$seriesFor)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, GARCH_rus_24_pred, model = 'GARCH'))
predict_rus_24 <- predict_rus_24 %>% mutate('GARCH' = GARCH_rus_24_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_rus_24)
ss <- AddSeasonal(ss, train_rus_24, nseasons = 12)
bsts_rus_24 <- bsts(train_rus_24 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_rus_24, train_rus_24))

bsts_rus_24_pred <- predict(bsts_rus_24, horizon = n, burn = SuggestBurn(.1, bsts_rus_24), newdata = test_reg_rus_24)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, bsts_rus_24_pred$mean, model = 'BSTSx'))
predict_rus_24 <- predict_rus_24 %>% mutate('BSTSx' = bsts_rus_24_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/Russia')
getwd()


# 11- DeepAR
rus_reg_24_DeepAR <- unlist(read_csv('DeepAR_24.csv')[2])  
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, rus_reg_24_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
rus_reg_24_NBEATS <- unlist(read_csv('NBEATS_24.csv')[2])      
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, rus_reg_24_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
rus_reg_24_NHiTS <- unlist(read_csv('NHiTS_24.csv')[2])  
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, rus_reg_24_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
rus_reg_24_Dlinear <- unlist(read_csv('Dlinear_24.csv')[2]) 
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, rus_reg_24_Dlinear, model = 'DLinearx'))


# 15- NLinearx
rus_reg_24_Nlinear <- unlist(read_csv('Nlinear_24.csv')[2])
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, rus_reg_24_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
rus_reg_24_TSMixer <- unlist(read_csv('TSMIxer_24.csv')[2]) 
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, rus_reg_24_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_rus_24 <-  residuals(arfima_rus_24)
arfima_er_rus_24[is.na(arfima_er_rus_24)] <-  0

set.seed(100)
narfimaT_rus_24 <-  auto.narfima(train_rus_24, arfima_er_rus_24, p = 1, q = 1, size = 5, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_rus_24)
set.seed(100)
narfimaT_rus_24_pred <-  forecast.narfima(narfimaT_rus_24, PI = FALSE, h = n, xreg = test_reg_rus_24)
set.seed(100)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, narfimaT_rus_24_pred$mean, model = paste0('NARFIMA(', narfimaT_rus_24$p, ',', narfimaT_rus_24$q, ',', narfimaT_rus_24$size, ',', narfimaT_rus_24$skip,')')))
predict_rus_24 <- predict_rus_24 %>% mutate('NARFIMA(1,1,5,T)' = narfimaT_rus_24_pred$mean)


write.csv(model_evaluate_rus_24, 'Russia Evaluation 24.csv', row.names = FALSE)
write.csv(predict_rus_24, 'Russia Forecast 24.csv', row.names = FALSE)



##################################################### Russia 48 #####################################################

n = 48
set.seed(100)
train_rus_48 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_48 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_48 <- reg_rus[1:length(train_rus_48),]
test_reg_rus_48 <- reg_rus[1:n,]

model_evaluate_rus_48 <- tibble()  
predict_rus_48 <- tibble(Date = as.Date(data$Date[length(train_rus_48) + 1:length(test_rus_48)]))  


# 1- Naive
set.seed(100)
naive_rus_48_pred <- naive(train_rus_48, h = n)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, naive_rus_48_pred$mean, model = 'Naive'))
predict_rus_48 <- predict_rus_48 %>% mutate('Naive' = naive_rus_48_pred$mean)


# 2- AR
set.seed(100)
ar_rus_48 <- ar(train_rus_48)
ar_rus_48_pred <- predict(ar_rus_48, n.ahead = n)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, ar_rus_48_pred$pred, model = 'AR(2)'))
predict_rus_48 <- predict_rus_48 %>% mutate('AR(2)' = ar_rus_48_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_rus_48 <- auto.arima(train_rus_48, xreg = train_reg_rus_48)
arima_rus_48_pred <- forecast(arima_rus_48, h = n, xreg = test_reg_rus_48)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, arima_rus_48_pred$mean, model = 'ARIMAx(0,1,1)'))
predict_rus_48 <- predict_rus_48 %>% mutate('ARIMAx(0,1,1)' = arima_rus_48_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_rus_48 <- nnetar(train_rus_48, xreg = train_reg_rus_48)
arnn_rus_48_pred <- forecast(arnn_rus_48, h = n, xreg = test_reg_rus_48)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, arnn_rus_48_pred$mean, model = paste0('ARNNx(', arnn_rus_48$p, ',', arnn_rus_48$size , ')')))
predict_rus_48 <- predict_rus_48 %>% mutate('ARNNx(2,4)' = arnn_rus_48_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_rus_48 <- arfima(train_rus_48, xreg = train_reg_rus_48)
arfima_rus_48_pred <- forecast(arfima_rus_48, h = n, xreg = test_reg_rus_48)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, arfima_rus_48_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_rus_48$ar),0), ',', round(arfima_rus_48$d,5), ',' , max(order(arfima_rus_48$ma),0) , ')')))
predict_rus_48 <- predict_rus_48 %>% mutate('ARFIMAx(0,0.49151,3)' = arfima_rus_48_pred$mean)


# 6- ETS
set.seed(100)
ets_rus_48 <- ets(train_rus_48)
ets_rus_48_pred <- forecast(ets_rus_48, h = n)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, ets_rus_48_pred$mean, model = 'ETS'))
predict_rus_48 <- predict_rus_48 %>% mutate('ETS' = ets_rus_48_pred$mean)


# 7- SETAR
set.seed(100)
setar_rus_48 <- setar(train_rus_48, m = 4)
setar_rus_48_pred <- predict(setar_rus_48, n.ahead = n)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, setar_rus_48_pred, model = 'SETAR'))
predict_rus_48 <- predict_rus_48 %>% mutate('SETAR' = setar_rus_48_pred)


# 8- TBATS
set.seed(100)
tbats_rus_48 <- tbats(train_rus_48)
tbats_rus_48_pred <- forecast(tbats_rus_48, h = n)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, tbats_rus_48_pred$mean, model = 'TBATS'))
predict_rus_48 <- predict_rus_48 %>% mutate('TBATS' = tbats_rus_48_pred$mean)


# 9- Garch
set.seed(100)
ArchTest(train_rus_48)
garch(train_rus_48, grad = 'numerical', traruse = F)
garch_rus_48 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_rus_48_fit <- ugarchfit(garch_rus_48, data = train_rus_48)
garch_rus_48_pred <- ugarchforecast(garch_rus_48_fit, n.ahead = n)
GARCH_rus_48_pred <- as.vector(garch_rus_48_pred@forecast$sigmaFor + garch_rus_48_pred@forecast$seriesFor)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, GARCH_rus_48_pred, model = 'GARCH'))
predict_rus_48 <- predict_rus_48 %>% mutate('GARCH' = GARCH_rus_48_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_rus_48)
ss <- AddSeasonal(ss, train_rus_48, nseasons = 12)
bsts_rus_48 <- bsts(train_rus_48 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_rus_48, train_rus_48))

bsts_rus_48_pred <- predict(bsts_rus_48, horizon = n, burn = SuggestBurn(.1, bsts_rus_48), newdata = test_reg_rus_48)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, bsts_rus_48_pred$mean, model = 'BSTSx'))
predict_rus_48 <- predict_rus_48 %>% mutate('BSTSx' = bsts_rus_48_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/Russia')
getwd()


# 11- DeepAR
rus_reg_48_DeepAR <- unlist(read_csv('DeepAR_48.csv')[2])  
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, rus_reg_48_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
rus_reg_48_NBEATS <- unlist(read_csv('NBEATS_48.csv')[2])      
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, rus_reg_48_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
rus_reg_48_NHiTS <- unlist(read_csv('NHiTS_48.csv')[2])  
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, rus_reg_48_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
rus_reg_48_Dlinear <- unlist(read_csv('Dlinear_48.csv')[2]) 
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, rus_reg_48_Dlinear, model = 'DLinearx'))


# 15- NLinearx
rus_reg_48_Nlinear <- unlist(read_csv('Nlinear_48.csv')[2])
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, rus_reg_48_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
rus_reg_48_TSMixer <- unlist(read_csv('TSMIxer_48.csv')[2]) 
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, rus_reg_48_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_rus_48 <-  residuals(arfima_rus_48)
arfima_er_rus_48[is.na(arfima_er_rus_48)] <-  0

set.seed(100)
narfima_rus_48 <-  auto.narfima(train_rus_48, arfima_er_rus_48, p = 3, q = 2, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = F, xreg = train_reg_rus_48)
set.seed(100)
narfima_rus_48_pred <-  forecast.narfima(narfima_rus_48, PI = FALSE, h = n, xreg = test_reg_rus_48)
set.seed(100)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, narfima_rus_48_pred$mean, model = paste0('NARFIMA(', narfima_rus_48$p, ',', narfima_rus_48$q, ',', narfima_rus_48$size, ',', narfima_rus_48$skip,')')))
predict_rus_48 <- predict_rus_48 %>% mutate('NARFIMA(3,2,1,F)' = narfima_rus_48_pred$mean)



write.csv(model_evaluate_rus_48, 'Russia Evaluation 48.csv', row.names = FALSE)
write.csv(predict_rus_48, 'Russia Forecast 48.csv', row.names = FALSE)
