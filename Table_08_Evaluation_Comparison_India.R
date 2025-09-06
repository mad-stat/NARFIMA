library(forecast)
library(tidyverse)
library(Metrics)
library(tsDyn)
library(tseries)
library(bsts)
library(readxl)

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('India_Data.xlsx') %>% rename('Exchange_Rate_ind' = spot_ER_India)                   
exchange_rate_ind <- ts(data$Exchange_Rate_ind)
reg_ind <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)


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


##################################################### India 1 #####################################################

n = 1
set.seed(100)
train_ind_1 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_1 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_1 <- reg_ind[1:length(train_ind_1),]
test_reg_ind_1 <- matrix(c(reg_ind[1,1], reg_ind[1,2], reg_ind[1,3], reg_ind[1,4], reg_ind[1,5]), ncol = 5, nrow = 1)
colnames(test_reg_ind_1) <- c("Oil_price_growth_rate_WTI", "SR_Interest_rate_diff_I_U", "global_EPU(PPP)", "US_EMV" ,"US_MPU")

model_evaluate_ind_1 <- tibble()  
predict_ind_1 <- tibble(Date = as.Date(data$Date[length(train_ind_1) + 1:length(test_ind_1)]))  


# 1- Naive
set.seed(100)
naive_ind_1_pred <- naive(train_ind_1, h = n)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, naive_ind_1_pred$mean, model = 'Naive'))
predict_ind_1 <- predict_ind_1 %>% mutate('Naive' = naive_ind_1_pred$mean)


# 2- AR
set.seed(100)
ar_ind_1 <- ar(train_ind_1)
ar_ind_1_pred <- predict(ar_ind_1, n.ahead = n)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, ar_ind_1_pred$pred, model = 'AR(1)'))
predict_ind_1 <- predict_ind_1 %>% mutate('AR(1)' = ar_ind_1_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_ind_1 <- auto.arima(train_ind_1, xreg = train_reg_ind_1)
arima_ind_1_pred <- forecast(arima_ind_1, h = n, xreg = test_reg_ind_1)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, arima_ind_1_pred$mean, model = 'ARIMAx(0,1,1)'))
predict_ind_1 <- predict_ind_1 %>% mutate('ARIMAx(0,1,1)' = arima_ind_1_pred$mean)



# 4- ARNNx
set.seed(100)
arnn_ind_1 <- nnetar(train_ind_1, xreg = train_reg_ind_1)
arnn_ind_1_pred <- forecast(arnn_ind_1, h = n, xreg = test_reg_ind_1)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, arnn_ind_1_pred$mean, model = paste0('ARNNx(', arnn_ind_1$p, ',', arnn_ind_1$size , ')')))
predict_ind_1 <- predict_ind_1 %>% mutate('ARNNx(1,4)' = arnn_ind_1_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_ind_1 <- arfima(train_ind_1, xreg = train_reg_ind_1)
arfima_ind_1_pred <- forecast(arfima_ind_1, h = n, xreg = test_reg_ind_1)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, arfima_ind_1_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_ind_1$ar),0), ',', round(arfima_ind_1$d,5), ',' , max(order(arfima_ind_1$ma),0) , ')')))
predict_ind_1 <- predict_ind_1 %>% mutate('ARFIMAx(0,0.49356,5)' = arfima_ind_1_pred$mean)


# 6- ETS
set.seed(100)
ets_ind_1 <- ets(train_ind_1)
ets_ind_1_pred <- forecast(ets_ind_1, h = n)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, ets_ind_1_pred$mean, model = 'ETS'))
predict_ind_1 <- predict_ind_1 %>% mutate('ETS' = ets_ind_1_pred$mean)


# 7- SETAR
set.seed(100)
setar_ind_1 <- setar(train_ind_1, m = 4)
setar_ind_1_pred <- predict(setar_ind_1, n.ahead = n)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, setar_ind_1_pred, model = 'SETAR'))
predict_ind_1 <- predict_ind_1 %>% mutate('SETAR' = setar_ind_1_pred)


# 8- TBATS
set.seed(100)
tbats_ind_1 <- tbats(train_ind_1)
tbats_ind_1_pred <- forecast(tbats_ind_1, h = n)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, tbats_ind_1_pred$mean, model = 'TBATS'))
predict_ind_1 <- predict_ind_1 %>% mutate('TBATS' = tbats_ind_1_pred$mean)


# 9- Garch
set.seed(100)
ArchTest(train_ind_1)
garch(train_ind_1, grad = 'numerical', trainde = F)
garch_ind_1 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_ind_1_fit <- ugarchfit(garch_ind_1, data = train_ind_1)
garch_ind_1_pred <- ugarchforecast(garch_ind_1_fit, n.ahead = n)
GARCH_ind_1_pred <- as.vector(garch_ind_1_pred@forecast$sigmaFor + garch_ind_1_pred@forecast$seriesFor)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, GARCH_ind_1_pred, model = 'GARCH'))
predict_ind_1 <- predict_ind_1 %>% mutate('GARCH' = GARCH_ind_1_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_ind_1)
ss <- AddSeasonal(ss, train_ind_1, nseasons = 12)
bsts_ind_1 <- bsts(train_ind_1 ~ ., 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   data = cbind(train_reg_ind_1, train_ind_1))

bsts_ind_1_pred <- predict(bsts_ind_1, horizon = n, burn = SuggestBurn(.1, bsts_ind_1), newdata = test_reg_ind_1)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, bsts_ind_1_pred$mean, model = 'BSTSx'))
predict_ind_1 <- predict_ind_1 %>% mutate('BSTSx' = bsts_ind_1_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/India')
getwd()


# 11- DeepAR
ind_reg_1_DeepAR <- as.numeric(read_csv('DeepAR_1.csv')[2])  
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, ind_reg_1_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
ind_reg_1_NBEATS <- as.numeric(read_csv('NBEATS_1.csv')[2])      
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, ind_reg_1_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
ind_reg_1_NHiTS <- as.numeric(read_csv('NHiTS_1.csv')[2])  
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, ind_reg_1_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
ind_reg_1_Dlinear <- as.numeric(read_csv('Dlinear_1.csv')[2]) 
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, ind_reg_1_Dlinear, model = 'DLinearx'))


# 15- NLinearx
ind_reg_1_Nlinear <- as.numeric(read_csv('Nlinear_1.csv')[2])
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, ind_reg_1_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
ind_reg_1_TSMixer <- as.numeric(read_csv('TSMIxer_1.csv')[2]) 
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, ind_reg_1_TSMixer, model = 'TSMixerx'))


# 17 - NARFIMA
arfima_er_ind_1 <-  residuals(arfima_ind_1)
arfima_er_ind_1[is.na(arfima_er_ind_1)] <-  0

set.seed(100)
narfimaT_ind_1 <-  auto_narfima(train_ind_1, arfima_er_ind_1, p = 2, q = 1, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_ind_1)
set.seed(100)
narfimaT_ind_1_pred <-  forecast_narfima_class(narfimaT_ind_1, PI = FALSE, h = n, xreg = test_reg_ind_1)
set.seed(100)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, narfimaT_ind_1_pred$mean, model = paste0('NARFIMA(', narfimaT_ind_1$p, ',', narfimaT_ind_1$q, ',', narfimaT_ind_1$size, ',', narfimaT_ind_1$skip,')')))
predict_ind_1 <- predict_ind_1 %>% mutate('NARFIMA(2,1,1,T)' = narfimaT_ind_1_pred$mean)


write.csv(model_evaluate_ind_1, 'India Evaluation 1.csv', row.names = FALSE)
write.csv(predict_ind_1, 'India Forecast 1.csv', row.names = FALSE)



##################################################### India 3 #####################################################

n = 3
set.seed(100)
train_ind_3 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_3 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_3 <- reg_ind[1:length(train_ind_3),]
test_reg_ind_3 <- reg_ind[1:n,]

model_evaluate_ind_3 <- tibble()  
predict_ind_3 <- tibble(Date = as.Date(data$Date[length(train_ind_3) + 1:length(test_ind_3)]))  


# 1- Naive
set.seed(100)
naive_ind_3_pred <- naive(train_ind_3, h = n)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, naive_ind_3_pred$mean, model = 'Naive'))
predict_ind_3 <- predict_ind_3 %>% mutate('Naive' = naive_ind_3_pred$mean)


# 2- AR
set.seed(100)
ar_ind_3 <- ar(train_ind_3)
ar_ind_3_pred <- predict(ar_ind_3, n.ahead = n)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, ar_ind_3_pred$pred, model = 'AR(1)'))
predict_ind_3 <- predict_ind_3 %>% mutate('AR(1)' = ar_ind_3_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_ind_3 <- auto.arima(train_ind_3, xreg = train_reg_ind_3)
arima_ind_3_pred <- forecast(arima_ind_3, h = n, xreg = test_reg_ind_3)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, arima_ind_3_pred$mean, model = 'ARIMAx(0,1,1)'))
predict_ind_3 <- predict_ind_3 %>% mutate('ARIMAx(0,1,1)' = arima_ind_3_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_ind_3 <- nnetar(train_ind_3, xreg = train_reg_ind_3)
arnn_ind_3_pred <- forecast(arnn_ind_3, h = n, xreg = test_reg_ind_3)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, arnn_ind_3_pred$mean, model = paste0('ARNNx(', arnn_ind_3$p, ',', arnn_ind_3$size , ')')))
predict_ind_3 <- predict_ind_3 %>% mutate('ARNNx(1,4)' = arnn_ind_3_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_ind_3 <- arfima(train_ind_3, xreg = train_reg_ind_3)
arfima_ind_3_pred <- forecast(arfima_ind_3, h = n, xreg = test_reg_ind_3)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, arfima_ind_3_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_ind_3$ar),0), ',', round(arfima_ind_3$d,5), ',' , max(order(arfima_ind_3$ma),0) , ')')))
predict_ind_3 <- predict_ind_3 %>% mutate('ARFIMAx(0,0.49337,5)' = arfima_ind_3_pred$mean)


# 6- ETS
ets_ind_3 <- ets(train_ind_3)
ets_ind_3_pred <- forecast(ets_ind_3, h = n)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, ets_ind_3_pred$mean, model = 'ETS'))
predict_ind_3 <- predict_ind_3 %>% mutate('ETS' = ets_ind_3_pred$mean)


# 7- SETAR
setar_ind_3 <- setar(train_ind_3, m = 4)
setar_ind_3_pred <- predict(setar_ind_3, n.ahead = n)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, setar_ind_3_pred, model = 'SETAR'))
predict_ind_3 <- predict_ind_3 %>% mutate('SETAR' = setar_ind_3_pred)


# 8- TBATS
tbats_ind_3 <- tbats(train_ind_3)
tbats_ind_3_pred <- forecast(tbats_ind_3, h = n)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, tbats_ind_3_pred$mean, model = 'TBATS'))
predict_ind_3 <- predict_ind_3 %>% mutate('TBATS' = tbats_ind_3_pred$mean)


# 9 - Garch
ArchTest(train_ind_3)
garch(train_ind_3, grad = 'numerical', trainde = F)
garch_ind_3 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_ind_3_fit <- ugarchfit(garch_ind_3, data = train_ind_3)
garch_ind_3_pred <- ugarchforecast(garch_ind_3_fit, n.ahead = n)
GARCH_ind_3_pred <- as.vector(garch_ind_3_pred@forecast$sigmaFor + garch_ind_3_pred@forecast$seriesFor)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, GARCH_ind_3_pred, model = 'GARCH'))
predict_ind_3 <- predict_ind_3 %>% mutate('GARCH' = GARCH_ind_3_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_ind_3)
ss <- AddSeasonal(ss, train_ind_3, nseasons = 12)
bsts_ind_3 <- bsts(train_ind_3 ~ ., 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   data = cbind(train_reg_ind_3, train_ind_3))

bsts_ind_3_pred <- predict(bsts_ind_3, horizon = n, burn = SuggestBurn(.1, bsts_ind_3), newdata = test_reg_ind_3)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, bsts_ind_3_pred$mean, model = 'BSTSx'))
predict_ind_3 <- predict_ind_3 %>% mutate('BSTSx' = bsts_ind_3_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/India')
getwd()


# 11- DeepAR
ind_reg_3_DeepAR <- unlist(read_csv('DeepAR_3.csv')[2])     
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, ind_reg_3_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
ind_reg_3_NBEATS <- unlist(read_csv('NBEATS_3.csv')[2])      
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, ind_reg_3_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
ind_reg_3_NHiTS <- unlist(read_csv('NHiTS_3.csv')[2])      
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, ind_reg_3_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
ind_reg_3_Dlinear <- unlist(read_csv('Dlinear_3.csv')[2]) 
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, ind_reg_3_Dlinear, model = 'DLinearx'))


# 15- NLinearx
ind_reg_3_Nlinear <- unlist(read_csv('Nlinear_3.csv')[2]) 
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, ind_reg_3_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
ind_reg_3_TSMixer <- unlist(read_csv('TSMIxer_3.csv')[2])  
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, ind_reg_3_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_ind_3 <-  residuals(arfima_ind_3)
arfima_er_ind_3[is.na(arfima_er_ind_3)] <-  0

set.seed(100)
narfimaT_ind_3 <-  auto_narfima(train_ind_3, arfima_er_ind_3, p = 4, q = 3, size = 4, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_ind_3)
set.seed(100)
narfimaT_ind_3_pred <-  forecast_narfima_class(narfimaT_ind_3, PI = FALSE, h = n, xreg = test_reg_ind_3)
set.seed(100)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, narfimaT_ind_3_pred$mean, model = paste0('NARFIMA(', narfimaT_ind_3$p, ',', narfimaT_ind_3$q, ',', narfimaT_ind_3$size, ',', narfimaT_ind_3$skip,')')))
predict_ind_3 <- predict_ind_3 %>% mutate('NARFIMA(4,3,4,T)' = narfimaT_ind_3_pred$mean)



write.csv(model_evaluate_ind_3, 'India Evaluation 3.csv', row.names = FALSE)
write.csv(predict_ind_3, 'India Forecast 3.csv', row.names = FALSE)



##################################################### India 6 #####################################################

n = 6
set.seed(100)
train_ind_6 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_6 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_6 <- reg_ind[1:length(train_ind_6),]
test_reg_ind_6 <- reg_ind[1:n,]

model_evaluate_ind_6 <- tibble()  
predict_ind_6 <- tibble(Date = as.Date(data$Date[length(train_ind_6) + 1:length(test_ind_6)]))   


# 1- Naive
set.seed(100)
naive_ind_6_pred <- naive(train_ind_6, h = n)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, naive_ind_6_pred$mean, model = 'Naive'))
predict_ind_6 <- predict_ind_6 %>% mutate('Naive' = naive_ind_6_pred$mean)


# 2- AR
set.seed(100)
ar_ind_6 <- ar(train_ind_6)
ar_ind_6_pred <- predict(ar_ind_6, n.ahead = n)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, ar_ind_6_pred$pred, model = 'AR(1)'))
predict_ind_6 <- predict_ind_6 %>% mutate('AR(1)' = ar_ind_6_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_ind_6 <- auto.arima(train_ind_6, xreg = train_reg_ind_6)
arima_ind_6_pred <- forecast(arima_ind_6, h = n, xreg = test_reg_ind_6)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, arima_ind_6_pred$mean, model = 'ARIMAx(0,1,1)'))
predict_ind_6 <- predict_ind_6 %>% mutate('ARIMAx(0,1,1)' = arima_ind_6_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_ind_6 <- nnetar(train_ind_6, xreg = train_reg_ind_6)
arnn_ind_6_pred <- forecast(arnn_ind_6, h = n, xreg = test_reg_ind_6)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, arnn_ind_6_pred$mean, model = paste0('ARNNx(', arnn_ind_6$p, ',', arnn_ind_6$size , ')')))
predict_ind_6 <- predict_ind_6 %>% mutate('ARNNx(1,4)' = arnn_ind_6_pred$mean)


# 5 - ARFIMAx
set.seed(100)
arfima_ind_6 <- arfima(train_ind_6, xreg = train_reg_ind_6)
arfima_ind_6_pred <- forecast(arfima_ind_6, h = n, xreg = test_reg_ind_6)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, arfima_ind_6_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_ind_6$ar),0), ',', round(arfima_ind_6$d,5), ',' , max(order(arfima_ind_6$ma),0) , ')')))
predict_ind_6 <- predict_ind_6 %>% mutate('ARFIMAx(0,0.49333,5)' = arfima_ind_6_pred$mean)


# 6- ETS
set.seed(100)
ets_ind_6 <- ets(train_ind_6)
ets_ind_6_pred <- forecast(ets_ind_6, h = n)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, ets_ind_6_pred$mean, model = 'ETS'))
predict_ind_6 <- predict_ind_6 %>% mutate('ETS' = ets_ind_6_pred$mean)


# 7- SETAR
set.seed(100)
setar_ind_6 <- setar(train_ind_6, m = 4)
setar_ind_6_pred <- predict(setar_ind_6, n.ahead = n)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, setar_ind_6_pred, model = 'SETAR'))
predict_ind_6 <- predict_ind_6 %>% mutate('SETAR' = setar_ind_6_pred)


# 8- TBATS
set.seed(100)
tbats_ind_6 <- tbats(train_ind_6)
tbats_ind_6_pred <- forecast(tbats_ind_6, h = n)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, tbats_ind_6_pred$mean, model = 'TBATS'))
predict_ind_6 <- predict_ind_6 %>% mutate('TBATS' = tbats_ind_6_pred$mean)


# 9- Garch
set.seed(100)
ArchTest(train_ind_6)
garch(train_ind_6, grad = 'numerical', trainde = F)
garch_ind_6 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_ind_6_fit <- ugarchfit(garch_ind_6, data = train_ind_6)
garch_ind_6_pred <- ugarchforecast(garch_ind_6_fit, n.ahead = n)
GARCH_ind_6_pred <- as.vector(garch_ind_6_pred@forecast$sigmaFor + garch_ind_6_pred@forecast$seriesFor)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, GARCH_ind_6_pred, model = 'GARCH'))
predict_ind_6 <- predict_ind_6 %>% mutate('GARCH' = GARCH_ind_6_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_ind_6)
ss <- AddSeasonal(ss, train_ind_6, nseasons = 12)
bsts_ind_6 <- bsts(train_ind_6 ~ ., 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   data = cbind(train_reg_ind_6, train_ind_6))

bsts_ind_6_pred <- predict(bsts_ind_6, horizon = n, burn = SuggestBurn(.1, bsts_ind_6), newdata = test_reg_ind_6)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, bsts_ind_6_pred$mean, model = 'BSTSx'))
predict_ind_6 <- predict_ind_6 %>% mutate('BSTSx' = bsts_ind_6_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/India')
getwd()


# 11- DeepAR
ind_reg_6_DeepAR <- unlist(read_csv('DeepAR_6.csv')[2])  
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, ind_reg_6_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
ind_reg_6_NBEATS <- unlist(read_csv('NBEATS_6.csv')[2])      
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, ind_reg_6_NBEATS, model = 'NBEATSx'))


# 16- NHiTSx
ind_reg_6_NHiTS <- unlist(read_csv('NHiTS_6.csv')[2])  
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, ind_reg_6_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
ind_reg_6_Dlinear <- unlist(read_csv('Dlinear_6.csv')[2]) 
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, ind_reg_6_Dlinear, model = 'DLinearx'))


# 15- NLinearx
ind_reg_6_Nlinear <- unlist(read_csv('Nlinear_6.csv')[2])
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, ind_reg_6_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
ind_reg_6_TSMixer <- unlist(read_csv('TSMIxer_6.csv')[2]) 
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, ind_reg_6_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_ind_6 <-  residuals(arfima_ind_6)
arfima_er_ind_6[is.na(arfima_er_ind_6)] <-  0

set.seed(100)
narfimaT_ind_6 <-  auto_narfima(train_ind_6, arfima_er_ind_6, p = 4, q = 2, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_ind_6)
set.seed(100)
narfimaT_ind_6_pred <-  forecast_narfima_class(narfimaT_ind_6, PI = FALSE, h = n, xreg = test_reg_ind_6)
set.seed(100)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, narfimaT_ind_6_pred$mean, model = paste0('NARFIMA(', narfimaT_ind_6$p, ',', narfimaT_ind_6$q, ',', narfimaT_ind_6$size, ',', narfimaT_ind_6$skip,')')))
predict_ind_6 <- predict_ind_6 %>% mutate('NARFIMA(4,2,1,T)' = narfimaT_ind_6_pred$mean)


write.csv(model_evaluate_ind_6, 'India Evaluation 6.csv', row.names = FALSE)
write.csv(predict_ind_6, 'India Forecast 6.csv', row.names = FALSE)



##################################################### India 12 #####################################################

n = 12
set.seed(100)
train_ind_12 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_12 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_12 <- reg_ind[1:length(train_ind_12),]
test_reg_ind_12 <- reg_ind[1:n,]

model_evaluate_ind_12 <- tibble()  
predict_ind_12 <- tibble(Date = as.Date(data$Date[length(train_ind_12) + 1:length(test_ind_12)]))  



# 1- Naive
set.seed(100)
naive_ind_12_pred <- naive(train_ind_12, h = n)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, naive_ind_12_pred$mean, model = 'Naive'))
predict_ind_12 <- predict_ind_12 %>% mutate('Naive' = naive_ind_12_pred$mean)


# 2- AR
set.seed(100)
ar_ind_12 <- ar(train_ind_12)
ar_ind_12_pred <- predict(ar_ind_12, n.ahead = n)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, ar_ind_12_pred$pred, model = 'AR(1)'))
predict_ind_12 <- predict_ind_12 %>% mutate('AR(1)' = ar_ind_12_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_ind_12 <- auto.arima(train_ind_12, xreg = train_reg_ind_12)
arima_ind_12_pred <- forecast(arima_ind_12, h = n, xreg = test_reg_ind_12)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, arima_ind_12_pred$mean, model = 'ARIMAx(0,1,1)'))
predict_ind_12 <- predict_ind_12 %>% mutate('ARIMAx(0,1,1)' = arima_ind_12_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_ind_12 <- nnetar(train_ind_12, xreg = train_reg_ind_12)
arnn_ind_12_pred <- forecast(arnn_ind_12, h = n, xreg = test_reg_ind_12)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, arnn_ind_12_pred$mean, model = paste0('ARNNx(', arnn_ind_12$p, ',', arnn_ind_12$size , ')')))
predict_ind_12 <- predict_ind_12 %>% mutate('ARNNx(1,4)' = arnn_ind_12_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_ind_12 <- arfima(train_ind_12, xreg = train_reg_ind_12)
arfima_ind_12_pred <- forecast(arfima_ind_12, h = n, xreg = test_reg_ind_12)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, arfima_ind_12_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_ind_12$ar),0), ',', round(arfima_ind_12$d,5), ',' , max(order(arfima_ind_12$ma),0) , ')')))
predict_ind_12 <- predict_ind_12 %>% mutate('ARFIMAx(0,0.49331,5)' = arfima_ind_12_pred$mean)


# 6- ETS
set.seed(100)
ets_ind_12 <- ets(train_ind_12)
ets_ind_12_pred <- forecast(ets_ind_12, h = n)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, ets_ind_12_pred$mean, model = 'ETS'))
predict_ind_12 <- predict_ind_12 %>% mutate('ETS' = ets_ind_12_pred$mean)


# 7- SETAR
set.seed(100)
setar_ind_12 <- setar(train_ind_12, m = 4)
setar_ind_12_pred <- predict(setar_ind_12, n.ahead = n)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, setar_ind_12_pred, model = 'SETAR'))
predict_ind_12 <- predict_ind_12 %>% mutate('SETAR' = setar_ind_12_pred)


# 8- TBATS
set.seed(100)
tbats_ind_12 <- tbats(train_ind_12)
tbats_ind_12_pred <- forecast(tbats_ind_12, h = n)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, tbats_ind_12_pred$mean, model = 'TBATS'))
predict_ind_12 <- predict_ind_12 %>% mutate('TBATS' = tbats_ind_12_pred$mean)


# 9 - Garch
set.seed(100)
ArchTest(train_ind_12)
garch(train_ind_12, grad = 'numerical', trainde = F)
garch_ind_12 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_ind_12_fit <- ugarchfit(garch_ind_12, data = train_ind_12)
garch_ind_12_pred <- ugarchforecast(garch_ind_12_fit, n.ahead = n)
GARCH_ind_12_pred <- as.vector(garch_ind_12_pred@forecast$sigmaFor + garch_ind_12_pred@forecast$seriesFor)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, GARCH_ind_12_pred, model = 'GARCH'))
predict_ind_12 <- predict_ind_12 %>% mutate('GARCH' = GARCH_ind_12_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_ind_12)
ss <- AddSeasonal(ss, train_ind_12, nseasons = 12)
bsts_ind_12 <- bsts(train_ind_12 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_ind_12, train_ind_12))

bsts_ind_12_pred <- predict(bsts_ind_12, horizon = n, burn = SuggestBurn(.1, bsts_ind_12), newdata = test_reg_ind_12)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, bsts_ind_12_pred$mean, model = 'BSTSx'))
predict_ind_12 <- predict_ind_12 %>% mutate('BSTSx' = bsts_ind_12_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/India')
getwd()


# 11- DeepAR
ind_reg_12_DeepAR <- unlist(read_csv('DeepAR_12.csv')[2])  
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, ind_reg_12_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
ind_reg_12_NBEATS <- unlist(read_csv('NBEATS_12.csv')[2])      
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, ind_reg_12_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
ind_reg_12_NHiTS <- unlist(read_csv('NHiTS_12.csv')[2])  
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, ind_reg_12_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
ind_reg_12_Dlinear <- unlist(read_csv('Dlinear_12.csv')[2]) 
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, ind_reg_12_Dlinear, model = 'DLinearx'))


# 15- NLinearx
ind_reg_12_Nlinear <- unlist(read_csv('Nlinear_12.csv')[2])
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, ind_reg_12_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
ind_reg_12_TSMixer <- unlist(read_csv('TSMIxer_12.csv')[2]) 
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, ind_reg_12_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_ind_12 <-  residuals(arfima_ind_12)
arfima_er_ind_12[is.na(arfima_er_ind_12)] <-  0

set.seed(100)
narfimaT_ind_12 <-  auto_narfima(train_ind_12, arfima_er_ind_12, p = 1, q = 3, size = 4, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_ind_12)
set.seed(100)
narfimaT_ind_12_pred <-  forecast_narfima_class(narfimaT_ind_12, PI = FALSE, h = n, xreg = test_reg_ind_12)
set.seed(100)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, narfimaT_ind_12_pred$mean, model = paste0('NARFIMAx(', narfimaT_ind_12$p, ',', narfimaT_ind_12$q, ',', narfimaT_ind_12$size, ',', narfimaT_ind_12$skip,')')))
predict_ind_12 <- predict_ind_12 %>% mutate('NARFIMA(1,3,4,T)' = narfimaT_ind_12_pred$mean)


write.csv(model_evaluate_ind_12, 'India Evaluation 12.csv', row.names = FALSE)
write.csv(predict_ind_12, 'India Forecast 12.csv', row.names = FALSE)



##################################################### India 24 #####################################################

n = 24
set.seed(100)
train_ind_24 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_24 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_24 <- reg_ind[1:length(train_ind_24),]
test_reg_ind_24 <- reg_ind[1:n,]

model_evaluate_ind_24 <- tibble()  
predict_ind_24 <- tibble(Date = as.Date(data$Date[length(train_ind_24) + 1:length(test_ind_24)]))  


# 1- Naive
set.seed(100)
naive_ind_24_pred <- naive(train_ind_24, h = n)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, naive_ind_24_pred$mean, model = 'Naive'))
predict_ind_24 <- predict_ind_24 %>% mutate('Naive' = naive_ind_24_pred$mean)


# 2- AR
set.seed(100)
ar_ind_24 <- ar(train_ind_24)
ar_ind_24_pred <- predict(ar_ind_24, n.ahead = n)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, ar_ind_24_pred$pred, model = 'AR(1)'))
predict_ind_24 <- predict_ind_24 %>% mutate('AR(1)' = ar_ind_24_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_ind_24 <- auto.arima(train_ind_24, xreg = train_reg_ind_24)
arima_ind_24_pred <- forecast(arima_ind_24, h = n, xreg = test_reg_ind_24)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, arima_ind_24_pred$mean, model = 'ARIMAx(0,1,1)'))
predict_ind_24 <- predict_ind_24 %>% mutate('ARIMAx(0,1,1)' = arima_ind_24_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_ind_24 <- nnetar(train_ind_24, xreg = train_reg_ind_24)
arnn_ind_24_pred <- forecast(arnn_ind_24, h = n, xreg = test_reg_ind_24)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, arnn_ind_24_pred$mean, model = paste0('ARNNx(', arnn_ind_24$p, ',', arnn_ind_24$size , ')')))
predict_ind_24 <- predict_ind_24 %>% mutate('ARNNx(1,4)' = arnn_ind_24_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_ind_24 <- arfima(train_ind_24, xreg = train_reg_ind_24)
arfima_ind_24_pred <- forecast(arfima_ind_24, h = n, xreg = test_reg_ind_24)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, arfima_ind_24_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_ind_24$ar),0), ',', round(arfima_ind_24$d,5), ',' , max(order(arfima_ind_24$ma),0) , ')')))
predict_ind_24 <- predict_ind_24 %>% mutate('ARFIMAx(0,0.49247,5)' = arfima_ind_24_pred$mean)


# 6- ETS
set.seed(100)
ets_ind_24 <- ets(train_ind_24)
ets_ind_24_pred <- forecast(ets_ind_24, h = n)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, ets_ind_24_pred$mean, model = 'ETS'))
predict_ind_24 <- predict_ind_24 %>% mutate('ETS' = ets_ind_24_pred$mean)


# 7- SETAR
set.seed(100)
setar_ind_24 <- setar(train_ind_24, m = 4)
setar_ind_24_pred <- predict(setar_ind_24, n.ahead = n)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, setar_ind_24_pred, model = 'SETAR'))
predict_ind_24 <- predict_ind_24 %>% mutate('SETAR' = setar_ind_24_pred)


# 8- TBATS
set.seed(100)
tbats_ind_24 <- tbats(train_ind_24)
tbats_ind_24_pred <- forecast(tbats_ind_24, h = n)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, tbats_ind_24_pred$mean, model = 'TBATS'))
predict_ind_24 <- predict_ind_24 %>% mutate('TBATS' = tbats_ind_24_pred$mean)


# 9 - Garch
set.seed(100)
ArchTest(train_ind_24)
garch(train_ind_24, grad = 'numerical', trainde = F)
garch_ind_24 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_ind_24_fit <- ugarchfit(garch_ind_24, data = train_ind_24)
garch_ind_24_pred <- ugarchforecast(garch_ind_24_fit, n.ahead = n)
GARCH_ind_24_pred <- as.vector(garch_ind_24_pred@forecast$sigmaFor + garch_ind_24_pred@forecast$seriesFor)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, GARCH_ind_24_pred, model = 'GARCH'))
predict_ind_24 <- predict_ind_24 %>% mutate('GARCH' = GARCH_ind_24_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_ind_24)
ss <- AddSeasonal(ss, train_ind_24, nseasons = 12)
bsts_ind_24 <- bsts(train_ind_24 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_ind_24, train_ind_24))

bsts_ind_24_pred <- predict(bsts_ind_24, horizon = n, burn = SuggestBurn(.1, bsts_ind_24), newdata = test_reg_ind_24)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, bsts_ind_24_pred$mean, model = 'BSTSx'))
predict_ind_24 <- predict_ind_24 %>% mutate('BSTSx' = bsts_ind_24_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/India')
getwd()


# 11- DeepAR
ind_reg_24_DeepAR <- unlist(read_csv('DeepAR_24.csv')[2])  
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, ind_reg_24_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
ind_reg_24_NBEATS <- unlist(read_csv('NBEATS_24.csv')[2])      
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, ind_reg_24_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
ind_reg_24_NHiTS <- unlist(read_csv('NHiTS_24.csv')[2])  
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, ind_reg_24_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
ind_reg_24_Dlinear <- unlist(read_csv('Dlinear_24.csv')[2]) 
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, ind_reg_24_Dlinear, model = 'DLinearx'))


# 15- NLinearx
ind_reg_24_Nlinear <- unlist(read_csv('Nlinear_24.csv')[2])
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, ind_reg_24_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
ind_reg_24_TSMixer <- unlist(read_csv('TSMIxer_24.csv')[2]) 
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, ind_reg_24_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_ind_24 <-  residuals(arfima_ind_24)
arfima_er_ind_24[is.na(arfima_er_ind_24)] <-  0

set.seed(100)
narfimaT_ind_24 <-  auto_narfima(train_ind_24, arfima_er_ind_24, p = 5, q = 4, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_ind_24)
set.seed(100)
narfimaT_ind_24_pred <-  forecast_narfima_class(narfimaT_ind_24, PI = FALSE, h = n, xreg = test_reg_ind_24)
set.seed(100)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, narfimaT_ind_24_pred$mean, model = paste0('NARFIMA(', narfimaT_ind_24$p, ',', narfimaT_ind_24$q, ',', narfimaT_ind_24$size, ',', narfimaT_ind_24$skip,')')))
predict_ind_24 <- predict_ind_24 %>% mutate('NARFIMA(5,4,1,T)' = narfimaT_ind_24_pred$mean)


write.csv(model_evaluate_ind_24, 'India Evaluation 24.csv', row.names = FALSE)
write.csv(predict_ind_24, 'India Forecast 24.csv', row.names = FALSE)



##################################################### India 48 #####################################################

n = 48
set.seed(100)
train_ind_48 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_48 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_48 <- reg_ind[1:length(train_ind_48),]
test_reg_ind_48 <- reg_ind[1:n,]

model_evaluate_ind_48 <- tibble()  
predict_ind_48 <- tibble(Date = as.Date(data$Date[length(train_ind_48) + 1:length(test_ind_48)]))  


# 1- Naive
set.seed(100)
naive_ind_48_pred <- naive(train_ind_48, h = n)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, naive_ind_48_pred$mean, model = 'Naive'))
predict_ind_48 <- predict_ind_48 %>% mutate('Naive' = naive_ind_48_pred$mean)


# 2- AR
set.seed(100)
ar_ind_48 <- ar(train_ind_48)
ar_ind_48_pred <- predict(ar_ind_48, n.ahead = n)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, ar_ind_48_pred$pred, model = 'AR(2)'))
predict_ind_48 <- predict_ind_48 %>% mutate('AR(2)' = ar_ind_48_pred$pred)


# 3- ARIMAx
set.seed(100)
arima_ind_48 <- auto.arima(train_ind_48, xreg = train_reg_ind_48)
arima_ind_48_pred <- forecast(arima_ind_48, h = n, xreg = test_reg_ind_48)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, arima_ind_48_pred$mean, model = 'ARIMAx(0,1,1)'))
predict_ind_48 <- predict_ind_48 %>% mutate('ARIMAx(0,1,1)' = arima_ind_48_pred$mean)


# 4- ARNNx
set.seed(100)
arnn_ind_48 <- nnetar(train_ind_48, xreg = train_reg_ind_48)
arnn_ind_48_pred <- forecast(arnn_ind_48, h = n, xreg = test_reg_ind_48)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, arnn_ind_48_pred$mean, model = paste0('ARNNx(', arnn_ind_48$p, ',', arnn_ind_48$size , ')')))
predict_ind_48 <- predict_ind_48 %>% mutate('ARNNx(2,4)' = arnn_ind_48_pred$mean)


# 5- ARFIMAx
set.seed(100)
arfima_ind_48 <- arfima(train_ind_48, xreg = train_reg_ind_48)
arfima_ind_48_pred <- forecast(arfima_ind_48, h = n, xreg = test_reg_ind_48)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, arfima_ind_48_pred$mean, model = paste0('ARFIMAx(', max(order(arfima_ind_48$ar),0), ',', round(arfima_ind_48$d,5), ',' , max(order(arfima_ind_48$ma),0) , ')')))
predict_ind_48 <- predict_ind_48 %>% mutate('ARFIMAx(0,0.49135,5)' = arfima_ind_48_pred$mean)


# 6- ETS
set.seed(100)
ets_ind_48 <- ets(train_ind_48)
ets_ind_48_pred <- forecast(ets_ind_48, h = n)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, ets_ind_48_pred$mean, model = 'ETS'))
predict_ind_48 <- predict_ind_48 %>% mutate('ETS' = ets_ind_48_pred$mean)


# 7- SETAR
set.seed(100)
setar_ind_48 <- setar(train_ind_48, m = 4)
setar_ind_48_pred <- predict(setar_ind_48, n.ahead = n)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, setar_ind_48_pred, model = 'SETAR'))
predict_ind_48 <- predict_ind_48 %>% mutate('SETAR' = setar_ind_48_pred)


# 8- TBATS
set.seed(100)
tbats_ind_48 <- tbats(train_ind_48)
tbats_ind_48_pred <- forecast(tbats_ind_48, h = n)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, tbats_ind_48_pred$mean, model = 'TBATS'))
predict_ind_48 <- predict_ind_48 %>% mutate('TBATS' = tbats_ind_48_pred$mean)


# 9- Garch
set.seed(100)
ArchTest(train_ind_48)
garch(train_ind_48, grad = 'numerical', trainde = F)
garch_ind_48 <- ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch_ind_48_fit <- ugarchfit(garch_ind_48, data = train_ind_48)
garch_ind_48_pred <- ugarchforecast(garch_ind_48_fit, n.ahead = n)
GARCH_ind_48_pred <- as.vector(garch_ind_48_pred@forecast$sigmaFor + garch_ind_48_pred@forecast$seriesFor)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, GARCH_ind_48_pred, model = 'GARCH'))
predict_ind_48 <- predict_ind_48 %>% mutate('GARCH' = GARCH_ind_48_pred)


# 10- BSTSx
ss <- AddSemilocalLinearTrend(list(), train_ind_48)
ss <- AddSeasonal(ss, train_ind_48, nseasons = 12)
bsts_ind_48 <- bsts(train_ind_48 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_ind_48, train_ind_48))

bsts_ind_48_pred <- predict(bsts_ind_48, horizon = n, burn = SuggestBurn(.1, bsts_ind_48), newdata = test_reg_ind_48)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, bsts_ind_48_pred$mean, model = 'BSTSx'))
predict_ind_48 <- predict_ind_48 %>% mutate('BSTSx' = bsts_ind_48_pred$mean)


setwd('NARFIMA/Dataset/Dataset_Deep_Learning_Models_Forecasts/India')
getwd()


# 11- DeepAR
ind_reg_48_DeepAR <- unlist(read_csv('DeepAR_48.csv')[2])  
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, ind_reg_48_DeepAR, model = 'DeepAR'))


# 12- NBEATSx
ind_reg_48_NBEATS <- unlist(read_csv('NBEATS_48.csv')[2])      
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, ind_reg_48_NBEATS, model = 'NBEATSx'))


# 13- NHiTSx
ind_reg_48_NHiTS <- unlist(read_csv('NHiTS_48.csv')[2])  
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, ind_reg_48_NHiTS, model = 'NHiTSx'))


# 14- DLinearx
ind_reg_48_Dlinear <- unlist(read_csv('Dlinear_48.csv')[2]) 
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, ind_reg_48_Dlinear, model = 'DLinearx'))


# 15- NLinearx
ind_reg_48_Nlinear <- unlist(read_csv('Nlinear_48.csv')[2])
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, ind_reg_48_Nlinear, model = 'NLinearx'))


# 16- TSMixerx
ind_reg_48_TSMixer <- unlist(read_csv('TSMIxer_48.csv')[2]) 
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, ind_reg_48_TSMixer, model = 'TSMixerx'))


# 17- NARFIMA
arfima_er_ind_48 <-  residuals(arfima_ind_48)
arfima_er_ind_48[is.na(arfima_er_ind_48)] <-  0

set.seed(100)
narfimaT_ind_48 <-  auto_narfima(train_ind_48, arfima_er_ind_48, p = 2, q = 4, size = 4, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_ind_48)
set.seed(100)
narfimaT_ind_48_pred <-  forecast_narfima_class(narfimaT_ind_48, PI = FALSE, h = n, xreg = test_reg_ind_48)
set.seed(100)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, narfimaT_ind_48_pred$mean, model = paste0('NARFIMAx(', narfimaT_ind_48$p, ',', narfimaT_ind_48$q, ',', narfimaT_ind_48$size, ',', narfimaT_ind_48$skip,')')))
predict_ind_48 <- predict_ind_48 %>% mutate('NARFIMAx(2,4,4,T)' = narfimaT_ind_48_pred$mean)



write.csv(model_evaluate_ind_48, 'India Evaluation 48.csv', row.names = FALSE)
write.csv(predict_ind_48, 'India Forecast 48.csv', row.names = FALSE)
