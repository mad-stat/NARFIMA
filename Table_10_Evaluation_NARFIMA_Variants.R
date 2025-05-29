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


##################################################### Evaluation Function #####################################################


evaluate <- function(test,pred,model){
  RMSE <- rmse(test,pred)
  
  return(tibble('MODEL' = model,
                'RMSE' = RMSE))
}


##################################################### Brazil 1 #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('Brazil_Data.xlsx') %>% rename('Exchange_Rate_braz' = spot_ER_Brazil)                   
exchange_rate_braz <- ts(data$Exchange_Rate_braz)
reg_braz <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)

n = 1
set.seed(100)
train_braz_1 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_1 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_1 <- reg_braz[1:length(train_braz_1),]
test_reg_braz_1 <- matrix(c(reg_braz[1,1], reg_braz[1,2], reg_braz[1,3], reg_braz[1,4], reg_braz[1,5]), ncol = 5, nrow = 1)
colnames(test_reg_braz_1) <- c( "Oil_price_growth_rate_WTI", "SR_Interest_rate_diff_B_U", "global_EPU(PPP)", "US_EMV" ,"US_MPU")

model_evaluate_braz_1 <- tibble()  



# 1- NARFIMA - ARFIMA Error
arfima_braz_1 <- arfima(train_braz_1, xreg = train_reg_braz_1)
arfima_er_braz_1 <-  residuals(arfima_braz_1)
arfima_er_braz_1[is.na(arfima_er_braz_1)] <-  0
set.seed(100)
narfima_braz_1 <-  auto.narfima(train_braz_1, arfima_er_braz_1, p = 5, q = 4, size = 4  , skip = F, xreg = train_reg_braz_1, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_1_pred <-  forecast.narfima(narfima_braz_1, PI = FALSE, h = n, xreg = test_reg_braz_1)
set.seed(100)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, narfima_braz_1_pred$mean, model = paste0('NARFIMA(', narfima_braz_1$p, ',', narfima_braz_1$q, ',', narfima_braz_1$size, ',', narfima_braz_1$skip,')')))


# 2- NARIMA - ARIMA Error
arima_braz_1 <- auto.arima(train_braz_1)
arima_er_braz_1 <-  residuals(arima_braz_1)
arima_er_braz_1[is.na(arima_er_braz_1)] <-  0
set.seed(100)
narfima_braz_1 <-  auto.narfima(train_braz_1, arima_er_braz_1, p = 5, q = 4, size = 4  , skip = F, xreg = train_reg_braz_1, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_1_pred <-  forecast.narfima(narfima_braz_1, PI = FALSE, h = n, xreg = test_reg_braz_1)
set.seed(100)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, narfima_braz_1_pred$mean, model = paste0('NARIMA(', narfima_braz_1$p, ',', narfima_braz_1$q, ',', narfima_braz_1$size, ',', narfima_braz_1$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_braz_1)
ss <- AddSeasonal(ss, train_braz_1, nseasons = 12)
bsts_braz_1 <- bsts(train_braz_1 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_braz_1, train_braz_1))

bsts_er_braz_1 <-  residuals(bsts_braz_1)
bsts_er_braz_1[is.na(bsts_er_braz_1)] <-  0
set.seed(100)
narfima_braz_1 <-  auto.narfima(train_braz_1, bsts_er_braz_1, p = 5, q = 4, size = 4  , skip = F, xreg = train_reg_braz_1, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_1_pred <-  forecast.narfima(narfima_braz_1, PI = FALSE, h = n, xreg = test_reg_braz_1)
set.seed(100)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, narfima_braz_1_pred$mean, model = paste0('NBSTS(', narfima_braz_1$p, ',', narfima_braz_1$q, ',', narfima_braz_1$size, ',', narfima_braz_1$skip,')')))


# 4- NNaïve - Naïve Error
naive_braz_1 <- naive(train_braz_1, h = n)
naive_er_braz_1 <-  residuals(naive_braz_1)
naive_er_braz_1[is.na(naive_er_braz_1)] <-  0
set.seed(100)
narfima_braz_1 <-  auto.narfima(train_braz_1, naive_er_braz_1, p = 5, q = 4, size = 4, skip = F, xreg = train_reg_braz_1, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_braz_1_pred <-  forecast.narfima(narfima_braz_1, PI = FALSE, h = n, xreg = test_reg_braz_1)
set.seed(100)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, narfima_braz_1_pred$mean, model = paste0('NNaïve(', narfima_braz_1$p, ',', narfima_braz_1$q, ',', narfima_braz_1$size, ',', narfima_braz_1$skip,')')))



# 4- NTBATS - TBATS Error
#tbats_braz_1 <- tbats(train_braz_1)
#tbats_er_braz_1 <-  residuals(tbats_braz_1)
#tbats_er_braz_1[is.na(tbats_er_braz_1)] <-  0
#set.seed(100)
#narfima_braz_1 <-  auto.narfima(train_braz_1, tbats_er_braz_1, p = 5, q = 4, size = 4  , skip = F, xreg = train_reg_braz_1, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_braz_1_pred <-  forecast.narfima(narfima_braz_1, PI = FALSE, h = n, xreg = test_reg_braz_1)
#set.seed(100)
#model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, narfima_braz_1_pred$mean, model = paste0('NTBATS(', narfima_braz_1$p, ',', narfima_braz_1$q, ',', narfima_braz_1$size, ',', narfima_braz_1$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_braz_1 <- nnetar(train_braz_1, xreg = train_reg_braz_1)
arnn_braz_1_pred <- forecast(arnn_braz_1, h = n, xreg = test_reg_braz_1)
model_evaluate_braz_1 <- rbind(model_evaluate_braz_1, evaluate(test_braz_1, arnn_braz_1_pred$mean, model = paste0('ARNNx(', arnn_braz_1$p, ',', arnn_braz_1$size , ')')))


write.csv(model_evaluate_braz_1, 'Brazil NARFIMA Variants 1.csv', row.names = FALSE)



##################################################### Brazil 3 #####################################################

n = 3
set.seed(100)
train_braz_3 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_3 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_3 <- reg_braz[1:length(train_braz_3),]
test_reg_braz_3 <- reg_braz[1:n,]

model_evaluate_braz_3 <- tibble() 


# 1- NARFIMA
arfima_braz_3 <- arfima(train_braz_3, xreg = train_reg_braz_3)
arfima_er_braz_3 <-  residuals(arfima_braz_3)
arfima_er_braz_3[is.na(arfima_er_braz_3)] <-  0
set.seed(100)
narfima_braz_3 <-  auto.narfima(train_braz_3, arfima_er_braz_3, p = 2, q = 5, size = 2, lambda = 0, lambdae = 0, repeats = 1000, skip = F, xreg = train_reg_braz_3)
set.seed(100)
narfima_braz_3_pred <-  forecast.narfima(narfima_braz_3, PI = FALSE, h = n, xreg = test_reg_braz_3)
set.seed(100)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, narfima_braz_3_pred$mean, model = paste0('NARFIMA(', narfima_braz_3$p, ',', narfima_braz_3$q, ',', narfima_braz_3$size, ',', narfima_braz_3$skip, ')')))


# 2- NARFIMA - ARIMA Error
arima_braz_3 <- auto.arima(train_braz_3)
arima_er_braz_3 <-  residuals(arima_braz_3)
arima_er_braz_3[is.na(arima_er_braz_3)] <-  0
set.seed(100)
narfima_braz_3 <-  auto.narfima(train_braz_3, arima_er_braz_3, p = 2, q = 5, size = 2  , skip = F, xreg = train_reg_braz_3, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_3_pred <-  forecast.narfima(narfima_braz_3, PI = FALSE, h = n, xreg = test_reg_braz_3)
set.seed(100)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, narfima_braz_3_pred$mean, model = paste0('NARIMA(', narfima_braz_3$p, ',', narfima_braz_3$q, ',', narfima_braz_3$size, ',', narfima_braz_3$skip,')')))


# 3- NBSTS - BSTS Error 
ss <- AddSemilocalLinearTrend(list(), train_braz_3)
ss <- AddSeasonal(ss, train_braz_3, nseasons = 12)
bsts_braz_3 <- bsts(train_braz_3 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_braz_3, train_braz_3))

bsts_er_braz_3 <-  residuals(bsts_braz_3)
bsts_er_braz_3[is.na(bsts_er_braz_3)] <-  0
set.seed(100)
narfima_braz_3 <-  auto.narfima(train_braz_3, bsts_er_braz_3, p = 2, q = 5, size = 2  , skip = F, xreg = train_reg_braz_3, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_3_pred <-  forecast.narfima(narfima_braz_3, PI = FALSE, h = n, xreg = test_reg_braz_3)
set.seed(100)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, narfima_braz_3_pred$mean, model = paste0('NBSTS(', narfima_braz_3$p, ',', narfima_braz_3$q, ',', narfima_braz_3$size, ',', narfima_braz_3$skip,')')))


# 4- NNaive - Naive Error
naive_braz_3 <- naive(train_braz_3)
naive_er_braz_3 <-  residuals(naive_braz_3)
naive_er_braz_3[is.na(naive_er_braz_3)] <-  0
set.seed(100)
narfima_braz_3 <-  auto.narfima(train_braz_3, naive_er_braz_3, p = 2, q = 5, size = 2  , skip = F, xreg = train_reg_braz_3, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_braz_3_pred <-  forecast.narfima(narfima_braz_3, PI = FALSE, h = n, xreg = test_reg_braz_3)
set.seed(100)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, narfima_braz_3_pred$mean, model = paste0('NNaive(', narfima_braz_3$p, ',', narfima_braz_3$q, ',', narfima_braz_3$size, ',', narfima_braz_3$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_braz_3 <- tbats(train_braz_3)
#tbats_er_braz_3 <-  residuals(tbats_braz_3)
#tbats_er_braz_3[is.na(tbats_er_braz_3)] <-  0
#set.seed(100)
#narfima_braz_3 <-  auto.narfima(train_braz_3, tbats_er_braz_3, p = 2, q = 5, size = 2  , skip = F, xreg = train_reg_braz_3, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_braz_3_pred <-  forecast.narfima(narfima_braz_3, PI = FALSE, h = n, xreg = test_reg_braz_3)
#set.seed(100)
#model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, narfima_braz_3_pred$mean, model = paste0('NTBATS(', narfima_braz_3$p, ',', narfima_braz_3$q, ',', narfima_braz_3$size, ',', narfima_braz_3$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_braz_3 <- nnetar(train_braz_3, xreg = train_reg_braz_3)
arnn_braz_3_pred <- forecast(arnn_braz_3, h = n, xreg = test_reg_braz_3)
model_evaluate_braz_3 <- rbind(model_evaluate_braz_3, evaluate(test_braz_3, arnn_braz_3_pred$mean, model = paste0('ARNNx(', arnn_braz_3$p, ',', arnn_braz_3$size , ')')))

write.csv(model_evaluate_braz_3, 'Brazil NARFIMA Variants 3.csv', row.names = FALSE)



##################################################### Brazil 6 #####################################################

n = 6
set.seed(100)
train_braz_6 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_6 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_6 <- reg_braz[1:length(train_braz_6),]
test_reg_braz_6 <- reg_braz[1:n,]

model_evaluate_braz_6 <- tibble()



# 1- NARFIMA - ARFIMA Error
arfima_braz_6 <- arfima(train_braz_6, xreg = train_reg_braz_6)
arfima_er_braz_6 <-  residuals(arfima_braz_6)
arfima_er_braz_6[is.na(arfima_er_braz_6)] <-  0
set.seed(100)
narfima_braz_6 <-  auto.narfima(train_braz_6, arfima_er_braz_6, p = 1, q = 2, size = 1  , skip = F, xreg = train_reg_braz_6, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_6_pred <-  forecast.narfima(narfima_braz_6, PI = FALSE, h = n, xreg = test_reg_braz_6)
set.seed(100)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, narfima_braz_6_pred$mean, model = paste0('NARFIMA(', narfima_braz_6$p, ',', narfima_braz_6$q, ',', narfima_braz_6$size, ',', narfima_braz_6$skip,')')))


# 2- NARIMA - ARIMA Error
arima_braz_6 <- auto.arima(train_braz_6)
arima_er_braz_6 <-  residuals(arima_braz_6)
arima_er_braz_6[is.na(arima_er_braz_6)] <-  0
set.seed(100)
narfima_braz_6 <-  auto.narfima(train_braz_6, arima_er_braz_6, p = 1, q = 2, size = 1  , skip = F, xreg = train_reg_braz_6, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_6_pred <-  forecast.narfima(narfima_braz_6, PI = FALSE, h = n, xreg = test_reg_braz_6)
set.seed(100)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, narfima_braz_6_pred$mean, model = paste0('NARIMA(', narfima_braz_6$p, ',', narfima_braz_6$q, ',', narfima_braz_6$size, ',', narfima_braz_6$skip,')')))


# 3- NBSTS - BSTS Error
bsts_braz_6 <- bsts(train_braz_6 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_braz_6, train_braz_6))
bsts_er_braz_6 <-  residuals(bsts_braz_6)
bsts_er_braz_6[is.na(bsts_er_braz_6)] <-  0
set.seed(100)
narfima_braz_6 <-  auto.narfima(train_braz_6, bsts_er_braz_6, p = 1, q = 2, size = 1  , skip = F, xreg = train_reg_braz_6, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_6_pred <-  forecast.narfima(narfima_braz_6, PI = FALSE, h = n, xreg = test_reg_braz_6)
set.seed(100)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, narfima_braz_6_pred$mean, model = paste0('NBSTS(', narfima_braz_6$p, ',', narfima_braz_6$q, ',', narfima_braz_6$size, ',', narfima_braz_6$skip,')')))


# 4- NNaive - Naive Error
naive_braz_6 <- naive(train_braz_6)
naive_er_braz_6 <-  residuals(naive_braz_6)
naive_er_braz_6[is.na(naive_er_braz_6)] <-  0
set.seed(100)
narfima_braz_6 <-  auto.narfima(train_braz_6, naive_er_braz_6, p = 1, q = 2, size = 1, skip = F, xreg = train_reg_braz_6, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_braz_6_pred <-  forecast.narfima(narfima_braz_6, PI = FALSE, h = n, xreg = test_reg_braz_6)
set.seed(100)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, narfima_braz_6_pred$mean, model = paste0('NNaive(', narfima_braz_6$p, ',', narfima_braz_6$q, ',', narfima_braz_6$size, ',', narfima_braz_6$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_braz_6 <- tbats(train_braz_6)
#tbats_er_braz_6 <-  residuals(tbats_braz_6)
#tbats_er_braz_6[is.na(tbats_er_braz_6)] <-  0
#set.seed(100)
#narfima_braz_6 <-  auto.narfima(train_braz_6, tbats_er_braz_6, p = 1, q = 2, size = 1, skip = F, xreg = train_reg_braz_6, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_braz_6_pred <-  forecast.narfima(narfima_braz_6, PI = FALSE, h = n, xreg = test_reg_braz_6)
#set.seed(100)
#model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, narfima_braz_6_pred$mean, model = paste0('NTBATS(', narfima_braz_6$p, ',', narfima_braz_6$q, ',', narfima_braz_6$size, ',', narfima_braz_6$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_braz_6 <- nnetar(train_braz_6, xreg = train_reg_braz_6)
arnn_braz_6_pred <- forecast(arnn_braz_6, h = n, xreg = test_reg_braz_6)
model_evaluate_braz_6 <- rbind(model_evaluate_braz_6, evaluate(test_braz_6, arnn_braz_6_pred$mean, model = paste0('ARNNx(', arnn_braz_6$p, ',', arnn_braz_6$size , ')')))


write.csv(model_evaluate_braz_6, 'Brazil NARFIMA Variants 6.csv', row.names = FALSE)



##################################################### Brazil 12 #####################################################

n = 12
set.seed(100)
train_braz_12 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_12 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_12 <- reg_braz[1:length(train_braz_12),]
test_reg_braz_12 <- reg_braz[1:n,]

model_evaluate_braz_12 <- tibble() 



# 1- NARFIMA - ARFIMA Error
arfima_braz_12 <- arfima(train_braz_12, xreg = train_reg_braz_12)
arfima_er_braz_12 <-  residuals(arfima_braz_12)
arfima_er_braz_12[is.na(arfima_er_braz_12)] <-  0
set.seed(100)
narfima_braz_12 <-  auto.narfima(train_braz_12, arfima_er_braz_12, p = 1, q = 1, size = 1  , skip = F, xreg = train_reg_braz_12, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_12_pred <-  forecast.narfima(narfima_braz_12, PI = FALSE, h = n, xreg = test_reg_braz_12)
set.seed(100)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, narfima_braz_12_pred$mean, model = paste0('NARFIMA(', narfima_braz_12$p, ',', narfima_braz_12$q, ',', narfima_braz_12$size, ',', narfima_braz_12$skip,')')))


# 2- NARIMA - ARIMA Error
arima_braz_12 <- auto.arima(train_braz_12)
arima_er_braz_12 <-  residuals(arima_braz_12)
arima_er_braz_12[is.na(arima_er_braz_12)] <-  0
set.seed(100)
narfima_braz_12 <-  auto.narfima(train_braz_12, arima_er_braz_12, p = 1, q = 1, size = 1  , skip = F, xreg = train_reg_braz_12, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_12_pred <-  forecast.narfima(narfima_braz_12, PI = FALSE, h = n, xreg = test_reg_braz_12)
set.seed(100)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, narfima_braz_12_pred$mean, model = paste0('NARIMA(', narfima_braz_12$p, ',', narfima_braz_12$q, ',', narfima_braz_12$size, ',', narfima_braz_12$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_braz_12)
ss <- AddSeasonal(ss, train_braz_12, nseasons = 12)
bsts_braz_12 <- bsts(train_braz_12 ~ ., 
                     state.specification = ss,
                     niter = 1000,
                     ping = 100,
                     seed = 100,
                     data = cbind(train_reg_braz_12, train_braz_12))
bsts_er_braz_12 <-  residuals(bsts_braz_12)
bsts_er_braz_12[is.na(bsts_er_braz_12)] <-  0
set.seed(100)
narfima_braz_12 <-  auto.narfima(train_braz_12, bsts_er_braz_12, p = 1, q = 1, size = 1  , skip = F, xreg = train_reg_braz_12, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_12_pred <-  forecast.narfima(narfima_braz_12, PI = FALSE, h = n, xreg = test_reg_braz_12)
set.seed(100)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, narfima_braz_12_pred$mean, model = paste0('NBSTS(', narfima_braz_12$p, ',', narfima_braz_12$q, ',', narfima_braz_12$size, ',', narfima_braz_12$skip,')')))


# 4- NNaive - Naive Error
naive_braz_12 <- naive(train_braz_12)
naive_er_braz_12 <-  residuals(naive_braz_12)
naive_er_braz_12[is.na(naive_er_braz_12)] <-  0
set.seed(100)
narfima_braz_12 <-  auto.narfima(train_braz_12, naive_er_braz_12, p = 1, q = 1, size = 1, skip = F, xreg = train_reg_braz_12, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_braz_12_pred <-  forecast.narfima(narfima_braz_12, PI = FALSE, h = n, xreg = test_reg_braz_12)
set.seed(100)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, narfima_braz_12_pred$mean, model = paste0('NNaive(', narfima_braz_12$p, ',', narfima_braz_12$q, ',', narfima_braz_12$size, ',', narfima_braz_12$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_braz_12 <- tbats(train_braz_12)
#tbats_er_braz_12 <-  residuals(tbats_braz_12)
#tbats_er_braz_12[is.na(tbats_er_braz_12)] <-  0
#set.seed(100)
#narfima_braz_12 <-  auto.narfima(train_braz_12, tbats_er_braz_12, p = 1, q = 1, size = 1  , skip = F, xreg = train_reg_braz_12, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_braz_12_pred <-  forecast.narfima(narfima_braz_12, PI = FALSE, h = n, xreg = test_reg_braz_12)
#set.seed(100)
#model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, narfima_braz_12_pred$mean, model = paste0('NTBATS(', narfima_braz_12$p, ',', narfima_braz_12$q, ',', narfima_braz_12$size, ',', narfima_braz_12$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_braz_12 <- nnetar(train_braz_12, xreg = train_reg_braz_12)
arnn_braz_12_pred <- forecast(arnn_braz_12, h = n, xreg = test_reg_braz_12)
model_evaluate_braz_12 <- rbind(model_evaluate_braz_12, evaluate(test_braz_12, arnn_braz_12_pred$mean, model = paste0('ARNNx(', arnn_braz_12$p, ',', arnn_braz_12$size , ')')))


write.csv(model_evaluate_braz_12, 'Brazil NARFIMA Variants 12.csv', row.names = FALSE)



##################################################### Brazil 24 #####################################################

n = 24
set.seed(100)
train_braz_24 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_24 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_24 <- reg_braz[1:length(train_braz_24),]
test_reg_braz_24 <- reg_braz[1:n,]

model_evaluate_braz_24 <- tibble()  



# 1- NARFIMA - ARFIMA Error
arfima_braz_24 <- arfima(train_braz_24, xreg = train_reg_braz_24)
arfima_er_braz_24 <-  residuals(arfima_braz_24)
arfima_er_braz_24[is.na(arfima_er_braz_24)] <-  0
set.seed(100)
narfima_braz_24 <-  auto.narfima(train_braz_24, arfima_er_braz_24, p = 2, q = 5, size = 1, skip = F, xreg = train_reg_braz_24, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_24_pred <-  forecast.narfima(narfima_braz_24, PI = FALSE, h = n, xreg = test_reg_braz_24)
set.seed(100)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, narfima_braz_24_pred$mean, model = paste0('NARFIMA(', narfima_braz_24$p, ',', narfima_braz_24$q, ',', narfima_braz_24$size, ',', narfima_braz_24$skip,')')))


# 2- NARIMA - ARIMA Error
arima_braz_24 <- auto.arima(train_braz_24)
arima_er_braz_24 <-  residuals(arima_braz_24)
arima_er_braz_24[is.na(arima_er_braz_24)] <-  0
set.seed(100)
narfima_braz_24 <-  auto.narfima(train_braz_24, arima_er_braz_24, p = 2, q = 5, size = 1, skip = F, xreg = train_reg_braz_24, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_24_pred <-  forecast.narfima(narfima_braz_24, PI = FALSE, h = n, xreg = test_reg_braz_24)
set.seed(100)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, narfima_braz_24_pred$mean, model = paste0('NARIMA(', narfima_braz_24$p, ',', narfima_braz_24$q, ',', narfima_braz_24$size, ',', narfima_braz_24$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_braz_24)
ss <- AddSeasonal(ss, train_braz_24, nseasons = 12)
bsts_braz_24 <- bsts(train_braz_24 ~ ., 
                     state.specification = ss,
                     niter = 1000,
                     ping = 100,
                     seed = 100,
                     data = cbind(train_reg_braz_24, train_braz_24))
bsts_er_braz_24 <-  residuals(bsts_braz_24)
bsts_er_braz_24[is.na(bsts_er_braz_24)] <-  0
set.seed(100)
narfima_braz_24 <-  auto.narfima(train_braz_24, bsts_er_braz_24, p = 2, q = 5, size = 1, skip = F, xreg = train_reg_braz_24, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_24_pred <-  forecast.narfima(narfima_braz_24, PI = FALSE, h = n, xreg = test_reg_braz_24)
set.seed(100)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, narfima_braz_24_pred$mean, model = paste0('NBSTS(', narfima_braz_24$p, ',', narfima_braz_24$q, ',', narfima_braz_24$size, ',', narfima_braz_24$skip,')')))


# 4- NTBATS - TBATS Error
naive_braz_24 <- naive(train_braz_24)
naive_er_braz_24 <-  residuals(naive_braz_24)
naive_er_braz_24[is.na(naive_er_braz_24)] <-  0
set.seed(100)
narfima_braz_24 <-  auto.narfima(train_braz_24, naive_er_braz_24, p = 2, q = 5, size = 1, skip = F, xreg = train_reg_braz_24, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_braz_24_pred <-  forecast.narfima(narfima_braz_24, PI = FALSE, h = n, xreg = test_reg_braz_24)
set.seed(100)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, narfima_braz_24_pred$mean, model = paste0('NNaive(', narfima_braz_24$p, ',', narfima_braz_24$q, ',', narfima_braz_24$size, ',', narfima_braz_24$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_braz_24 <- tbats(train_braz_24)
#tbats_er_braz_24 <-  residuals(tbats_braz_24)
#tbats_er_braz_24[is.na(tbats_er_braz_24)] <-  0
#set.seed(100)
#narfima_braz_24 <-  auto.narfima(train_braz_24, tbats_er_braz_24, p = 2, q = 5, size = 1, skip = F, xreg = train_reg_braz_24, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_braz_24_pred <-  forecast.narfima(narfima_braz_24, PI = FALSE, h = n, xreg = test_reg_braz_24)
#set.seed(100)
#model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, narfima_braz_24_pred$mean, model = paste0('NTBATS(', narfima_braz_24$p, ',', narfima_braz_24$q, ',', narfima_braz_24$size, ',', narfima_braz_24$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_braz_24 <- nnetar(train_braz_24, xreg = train_reg_braz_24)
arnn_braz_24_pred <- forecast(arnn_braz_24, h = n, xreg = test_reg_braz_24)
model_evaluate_braz_24 <- rbind(model_evaluate_braz_24, evaluate(test_braz_24, arnn_braz_24_pred$mean, model = paste0('ARNNx(', arnn_braz_24$p, ',', arnn_braz_24$size , ')')))


write.csv(model_evaluate_braz_24, 'Brazil NARFIMA Variants 24.csv', row.names = FALSE)



##################################################### Brazil 48 #####################################################

n = 48
set.seed(100)
train_braz_48 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_48 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_48 <- reg_braz[1:length(train_braz_48),]
test_reg_braz_48 <- reg_braz[1:n,]

model_evaluate_braz_48 <- tibble()  



# 1- NARFIMA - ARFIMA Error
arfima_braz_48 <- arfima(train_braz_48, xreg = train_reg_braz_48)
arfima_er_braz_48 <-  residuals(arfima_braz_48)
arfima_er_braz_48[is.na(arfima_er_braz_48)] <-  0
set.seed(100)
narfima_braz_48 <-  auto.narfima(train_braz_48, arfima_er_braz_48, p = 4, q = 2, size = 1  , skip = T, xreg = train_reg_braz_48, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_48_pred <-  forecast.narfima(narfima_braz_48, PI = FALSE, h = n, xreg = test_reg_braz_48)
set.seed(100)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, narfima_braz_48_pred$mean, model = paste0('NARFIMA(', narfima_braz_48$p, ',', narfima_braz_48$q, ',', narfima_braz_48$size, ',', narfima_braz_48$skip,')')))


# 2- NARIMA - ARIMA Error
arima_braz_48 <- auto.arima(train_braz_48)
arima_er_braz_48 <-  residuals(arima_braz_48)
arima_er_braz_48[is.na(arima_er_braz_48)] <-  0
set.seed(100)
narfima_braz_48 <-  auto.narfima(train_braz_48, arima_er_braz_48, p = 4, q = 2, size = 1  , skip = T, xreg = train_reg_braz_48, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_48_pred <-  forecast.narfima(narfima_braz_48, PI = FALSE, h = n, xreg = test_reg_braz_48)
set.seed(100)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, narfima_braz_48_pred$mean, model = paste0('NARIMA(', narfima_braz_48$p, ',', narfima_braz_48$q, ',', narfima_braz_48$size, ',', narfima_braz_48$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_braz_48)
ss <- AddSeasonal(ss, train_braz_48, nseasons = 12)
bsts_braz_48 <- bsts(train_braz_48 ~ ., 
                     state.specification = ss,
                     niter = 1000,
                     ping = 100,
                     seed = 100,
                     data = cbind(train_reg_braz_48, train_braz_48))
bsts_er_braz_48 <-  residuals(bsts_braz_48)
bsts_er_braz_48[is.na(bsts_er_braz_48)] <-  0
set.seed(100)
narfima_braz_48 <-  auto.narfima(train_braz_48, bsts_er_braz_48, p = 4, q = 2, size = 1  , skip = T, xreg = train_reg_braz_48, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_braz_48_pred <-  forecast.narfima(narfima_braz_48, PI = FALSE, h = n, xreg = test_reg_braz_48)
set.seed(100)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, narfima_braz_48_pred$mean, model = paste0('NBSTS(', narfima_braz_48$p, ',', narfima_braz_48$q, ',', narfima_braz_48$size, ',', narfima_braz_48$skip,')')))


# 4- NNaive - Naive Error
naive_braz_48 <- naive(train_braz_48)
naive_er_braz_48 <-  residuals(naive_braz_48)
naive_er_braz_48[is.na(naive_er_braz_48)] <-  0
set.seed(100)
narfima_braz_48 <-  auto.narfima(train_braz_48, naive_er_braz_48, p = 4, q = 2, size = 1, skip = T, xreg = train_reg_braz_48, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_braz_48_pred <-  forecast.narfima(narfima_braz_48, PI = FALSE, h = n, xreg = test_reg_braz_48)
set.seed(100)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, narfima_braz_48_pred$mean, model = paste0('NNaive(', narfima_braz_48$p, ',', narfima_braz_48$q, ',', narfima_braz_48$size, ',', narfima_braz_48$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_braz_48 <- tbats(train_braz_48)
#tbats_er_braz_48 <-  residuals(tbats_braz_48)
#tbats_er_braz_48[is.na(tbats_er_braz_48)] <-  0
#set.seed(100)
#narfima_braz_48 <-  auto.narfima(train_braz_48, tbats_er_braz_48, p = 4, q = 2, size = 1  , skip = T, xreg = train_reg_braz_48, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_braz_48_pred <-  forecast.narfima(narfima_braz_48, PI = FALSE, h = n, xreg = test_reg_braz_48)
#set.seed(100)
#model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, narfima_braz_48_pred$mean, model = paste0('NTBATS(', narfima_braz_48$p, ',', narfima_braz_48$q, ',', narfima_braz_48$size, ',', narfima_braz_48$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_braz_48 <- nnetar(train_braz_48, xreg = train_reg_braz_48)
arnn_braz_48_pred <- forecast(arnn_braz_48, h = n, xreg = test_reg_braz_48)
model_evaluate_braz_48 <- rbind(model_evaluate_braz_48, evaluate(test_braz_48, arnn_braz_48_pred$mean, model = paste0('ARNNx(', arnn_braz_48$p, ',', arnn_braz_48$size , ')')))



write.csv(model_evaluate_braz_48, 'Brazil NARFIMA Variants 48.csv', row.names = FALSE)



##################################################### Russia 1 #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('Russia_Data.xlsx') %>% rename('Exchange_Rate_rus' = spot_ER_Russia)                   
exchange_rate_rus <- ts(data$Exchange_Rate_rus)
reg_rus <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)

n = 1
set.seed(100)
train_rus_1 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_1 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_1 <- reg_rus[1:length(train_rus_1),]
test_reg_rus_1 <- matrix(c(reg_rus[1,1], reg_rus[1,2], reg_rus[1,3], reg_rus[1,4], reg_rus[1,5]), ncol = 5, nrow = 1)
colnames(test_reg_rus_1) <- c( "Oil_price_growth_rate_WTI", "SR_Interest_rate_diff_R_U", "global_EPU(PPP)", "US_EMV" ,"US_MPU")

model_evaluate_rus_1 <- tibble()  



# 1- NARFIMA - ARFIMA Error
arfima_rus_1 <- arfima(train_rus_1, xreg = train_reg_rus_1)
arfima_er_rus_1 <-  residuals(arfima_rus_1)
arfima_er_rus_1[is.na(arfima_er_rus_1)] <-  0
set.seed(100)
narfima_rus_1 <-  auto.narfima(train_rus_1, arfima_er_rus_1, p = 1, q = 2, size = 2, skip = T, xreg = train_reg_rus_1, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_1_pred <-  forecast.narfima(narfima_rus_1, PI = FALSE, h = n, xreg = test_reg_rus_1)
set.seed(100)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, narfima_rus_1_pred$mean, model = paste0('NARFIMA(', narfima_rus_1$p, ',', narfima_rus_1$q, ',', narfima_rus_1$size, ',', narfima_rus_1$skip,')')))


# 2- NARIMA - ARIMA Error
arima_rus_1 <- auto.arima(train_rus_1)
arima_er_rus_1 <-  residuals(arima_rus_1)
arima_er_rus_1[is.na(arima_er_rus_1)] <-  0
set.seed(100)
narfima_rus_1 <-  auto.narfima(train_rus_1, arima_er_rus_1, p = 1, q = 2, size = 2, skip = T, xreg = train_reg_rus_1, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_1_pred <-  forecast.narfima(narfima_rus_1, PI = FALSE, h = n, xreg = test_reg_rus_1)
set.seed(100)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, narfima_rus_1_pred$mean, model = paste0('NARIMA(', narfima_rus_1$p, ',', narfima_rus_1$q, ',', narfima_rus_1$size, ',', narfima_rus_1$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_rus_1)
ss <- AddSeasonal(ss, train_rus_1, nseasons = 12)
bsts_rus_1 <- bsts(train_rus_1 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_rus_1, train_rus_1))

bsts_er_rus_1 <-  residuals(bsts_rus_1)
bsts_er_rus_1[is.na(bsts_er_rus_1)] <-  0
set.seed(100)
narfima_rus_1 <-  auto.narfima(train_rus_1, bsts_er_rus_1, p = 1, q = 2, size = 2, skip = T, xreg = train_reg_rus_1, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_1_pred <-  forecast.narfima(narfima_rus_1, PI = FALSE, h = n, xreg = test_reg_rus_1)
set.seed(100)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, narfima_rus_1_pred$mean, model = paste0('NBSTS(', narfima_rus_1$p, ',', narfima_rus_1$q, ',', narfima_rus_1$size, ',', narfima_rus_1$skip,')')))


# 4- NNaive - Naive Error
naive_rus_1 <- naive(train_rus_1)
naive_er_rus_1 <-  residuals(naive_rus_1)
naive_er_rus_1[is.na(naive_er_rus_1)] <-  0
set.seed(100)
narfima_rus_1 <-  auto.narfima(train_rus_1, naive_er_rus_1, p = 1, q = 2, size = 2, skip = T, xreg = train_reg_rus_1, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_rus_1_pred <-  forecast.narfima(narfima_rus_1, PI = FALSE, h = n, xreg = test_reg_rus_1)
set.seed(100)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, narfima_rus_1_pred$mean, model = paste0('NNaive(', narfima_rus_1$p, ',', narfima_rus_1$q, ',', narfima_rus_1$size, ',', narfima_rus_1$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_rus_1 <- tbats(train_rus_1)
#tbats_er_rus_1 <-  residuals(tbats_rus_1)
#tbats_er_rus_1[is.na(tbats_er_rus_1)] <-  0
#set.seed(100)
#narfima_rus_1 <-  auto.narfima(train_rus_1, tbats_er_rus_1, p = 1, q = 2, size = 2, skip = T, xreg = train_reg_rus_1, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_rus_1_pred <-  forecast.narfima(narfima_rus_1, PI = FALSE, h = n, xreg = test_reg_rus_1)
#set.seed(100)
#model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, narfima_rus_1_pred$mean, model = paste0('NTBATS(', narfima_rus_1$p, ',', narfima_rus_1$q, ',', narfima_rus_1$size, ',', narfima_rus_1$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_rus_1 <- nnetar(train_rus_1, xreg = train_reg_rus_1)
arnn_rus_1_pred <- forecast(arnn_rus_1, h = n, xreg = test_reg_rus_1)
model_evaluate_rus_1 <- rbind(model_evaluate_rus_1, evaluate(test_rus_1, arnn_rus_1_pred$mean, model = paste0('ARNNx(', arnn_rus_1$p, ',', arnn_rus_1$size , ')')))


write.csv(model_evaluate_rus_1, 'Russia NARFIMA Variants 1.csv', row.names = FALSE)



##################################################### Russia 3 #####################################################

n = 3
set.seed(100)
train_rus_3 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_3 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_3 <- reg_rus[1:length(train_rus_3),]
test_reg_rus_3 <- reg_rus[1:n,]

model_evaluate_rus_3 <- tibble() 


# 1- NARFIMA
arfima_rus_3 <- arfima(train_rus_3, xreg = train_reg_rus_3)
arfima_er_rus_3 <-  residuals(arfima_rus_3)
arfima_er_rus_3[is.na(arfima_er_rus_3)] <-  0
set.seed(100)
narfima_rus_3 <-  auto.narfima(train_rus_3, arfima_er_rus_3, p = 1, q = 1, size = 1, skip = F, lambda = 0, lambdae = 0, repeats = 1000, xreg = train_reg_rus_3)
set.seed(100)
narfima_rus_3_pred <-  forecast.narfima(narfima_rus_3, PI = FALSE, h = n, xreg = test_reg_rus_3)
set.seed(100)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, narfima_rus_3_pred$mean, model = paste0('NARFIMA(', narfima_rus_3$p, ',', narfima_rus_3$q, ',', narfima_rus_3$size, ',', narfima_rus_3$skip, ')')))


# 2- NARFIMA - ARIMA Error
arima_rus_3 <- auto.arima(train_rus_3)
arima_er_rus_3 <-  residuals(arima_rus_3)
arima_er_rus_3[is.na(arima_er_rus_3)] <-  0
set.seed(100)
narfima_rus_3 <-  auto.narfima(train_rus_3, arima_er_rus_3, p = 1, q = 1, size = 1, skip = F, xreg = train_reg_rus_3, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_3_pred <-  forecast.narfima(narfima_rus_3, PI = FALSE, h = n, xreg = test_reg_rus_3)
set.seed(100)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, narfima_rus_3_pred$mean, model = paste0('NARIMA(', narfima_rus_3$p, ',', narfima_rus_3$q, ',', narfima_rus_3$size, ',', narfima_rus_3$skip,')')))


# 3- NBSTS - BSTS Error 
ss <- AddSemilocalLinearTrend(list(), train_rus_3)
ss <- AddSeasonal(ss, train_rus_3, nseasons = 12)
bsts_rus_3 <- bsts(train_rus_3 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_rus_3, train_rus_3))

bsts_er_rus_3 <-  residuals(bsts_rus_3)
bsts_er_rus_3[is.na(bsts_er_rus_3)] <-  0
set.seed(100)
narfima_rus_3 <-  auto.narfima(train_rus_3, bsts_er_rus_3, p = 1, q = 1, size = 1, skip = F, xreg = train_reg_rus_3, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_3_pred <-  forecast.narfima(narfima_rus_3, PI = FALSE, h = n, xreg = test_reg_rus_3)
set.seed(100)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, narfima_rus_3_pred$mean, model = paste0('NBSTS(', narfima_rus_3$p, ',', narfima_rus_3$q, ',', narfima_rus_3$size, ',', narfima_rus_3$skip,')')))


# 4- NNaive - Naive Error
naive_rus_3 <- naive(train_rus_3)
naive_er_rus_3 <-  residuals(naive_rus_3)
naive_er_rus_3[is.na(naive_er_rus_3)] <-  0
set.seed(100)
narfima_rus_3 <-  auto.narfima(train_rus_3, naive_er_rus_3, p = 1, q = 1, size = 1, skip = F, xreg = train_reg_rus_3, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_rus_3_pred <-  forecast.narfima(narfima_rus_3, PI = FALSE, h = n, xreg = test_reg_rus_3)
set.seed(100)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, narfima_rus_3_pred$mean, model = paste0('NNaive(', narfima_rus_3$p, ',', narfima_rus_3$q, ',', narfima_rus_3$size, ',', narfima_rus_3$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_rus_3 <- tbats(train_rus_3)
#tbats_er_rus_3 <-  residuals(tbats_rus_3)
#tbats_er_rus_3[is.na(tbats_er_rus_3)] <-  0
#set.seed(100)
#narfima_rus_3 <-  auto.narfima(train_rus_3, tbats_er_rus_3, p = 1, q = 1, size = 1, skip = F, xreg = train_reg_rus_3, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_rus_3_pred <-  forecast.narfima(narfima_rus_3, PI = FALSE, h = n, xreg = test_reg_rus_3)
#set.seed(100)
#model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, narfima_rus_3_pred$mean, model = paste0('NTBATS(', narfima_rus_3$p, ',', narfima_rus_3$q, ',', narfima_rus_3$size, ',', narfima_rus_3$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_rus_3 <- nnetar(train_rus_3, xreg = train_reg_rus_3)
arnn_rus_3_pred <- forecast(arnn_rus_3, h = n, xreg = test_reg_rus_3)
model_evaluate_rus_3 <- rbind(model_evaluate_rus_3, evaluate(test_rus_3, arnn_rus_3_pred$mean, model = paste0('ARNNx(', arnn_rus_3$p, ',', arnn_rus_3$size , ')')))

write.csv(model_evaluate_rus_3, 'Russia NARFIMA Variants 3.csv', row.names = FALSE)



##################################################### Russia 6 #####################################################

n = 6
set.seed(100)
train_rus_6 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_6 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_6 <- reg_rus[1:length(train_rus_6),]
test_reg_rus_6 <- reg_rus[1:n,]

model_evaluate_rus_6 <- tibble()



# 1- NARFIMA - ARFIMA Error
arfima_rus_6 <- arfima(train_rus_6, xreg = train_reg_rus_6)
arfima_er_rus_6 <-  residuals(arfima_rus_6)
arfima_er_rus_6[is.na(arfima_er_rus_6)] <-  0
set.seed(100)
narfima_rus_6 <-  auto.narfima(train_rus_6, arfima_er_rus_6, p = 1, q = 1, size = 1, skip = F, xreg = train_reg_rus_6, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_6_pred <-  forecast.narfima(narfima_rus_6, PI = FALSE, h = n, xreg = test_reg_rus_6)
set.seed(100)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, narfima_rus_6_pred$mean, model = paste0('NARFIMA(', narfima_rus_6$p, ',', narfima_rus_6$q, ',', narfima_rus_6$size, ',', narfima_rus_6$skip,')')))


# 2- NARIMA - ARIMA Error
arima_rus_6 <- auto.arima(train_rus_6)
arima_er_rus_6 <-  residuals(arima_rus_6)
arima_er_rus_6[is.na(arima_er_rus_6)] <-  0
set.seed(100)
narfima_rus_6 <-  auto.narfima(train_rus_6, arima_er_rus_6, p = 1, q = 1, size = 1, skip = F, xreg = train_reg_rus_6, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_6_pred <-  forecast.narfima(narfima_rus_6, PI = FALSE, h = n, xreg = test_reg_rus_6)
set.seed(100)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, narfima_rus_6_pred$mean, model = paste0('NARIMA(', narfima_rus_6$p, ',', narfima_rus_6$q, ',', narfima_rus_6$size, ',', narfima_rus_6$skip,')')))


# 3- NBSTS - BSTS Error
bsts_rus_6 <- bsts(train_rus_6 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_rus_6, train_rus_6))
bsts_er_rus_6 <-  residuals(bsts_rus_6)
bsts_er_rus_6[is.na(bsts_er_rus_6)] <-  0
set.seed(100)
narfima_rus_6 <-  auto.narfima(train_rus_6, bsts_er_rus_6, p = 1, q = 1, size = 1, skip = F, xreg = train_reg_rus_6, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_6_pred <-  forecast.narfima(narfima_rus_6, PI = FALSE, h = n, xreg = test_reg_rus_6)
set.seed(100)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, narfima_rus_6_pred$mean, model = paste0('NBSTS(', narfima_rus_6$p, ',', narfima_rus_6$q, ',', narfima_rus_6$size, ',', narfima_rus_6$skip,')')))


# 4- NNaive - Naive Error
naive_rus_6 <- naive(train_rus_6)
naive_er_rus_6 <-  residuals(naive_rus_6)
naive_er_rus_6[is.na(naive_er_rus_6)] <-  0
set.seed(100)
narfima_rus_6 <-  auto.narfima(train_rus_6, naive_er_rus_6, p = 1, q = 1, size = 1, skip = F, xreg = train_reg_rus_6, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_rus_6_pred <-  forecast.narfima(narfima_rus_6, PI = FALSE, h = n, xreg = test_reg_rus_6)
set.seed(100)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, narfima_rus_6_pred$mean, model = paste0('NNaive(', narfima_rus_6$p, ',', narfima_rus_6$q, ',', narfima_rus_6$size, ',', narfima_rus_6$skip,')')))


# 4- NTBATS - TBATS Error
tbats_rus_6 <- tbats(train_rus_6)
tbats_er_rus_6 <-  residuals(tbats_rus_6)
tbats_er_rus_6[is.na(tbats_er_rus_6)] <-  0
set.seed(100)
narfima_rus_6 <-  auto.narfima(train_rus_6, tbats_er_rus_6, p = 1, q = 1, size = 1, skip = F, xreg = train_reg_rus_6, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_6_pred <-  forecast.narfima(narfima_rus_6, PI = FALSE, h = n, xreg = test_reg_rus_6)
set.seed(100)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, narfima_rus_6_pred$mean, model = paste0('NTBATS(', narfima_rus_6$p, ',', narfima_rus_6$q, ',', narfima_rus_6$size, ',', narfima_rus_6$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_rus_6 <- nnetar(train_rus_6, xreg = train_reg_rus_6)
arnn_rus_6_pred <- forecast(arnn_rus_6, h = n, xreg = test_reg_rus_6)
model_evaluate_rus_6 <- rbind(model_evaluate_rus_6, evaluate(test_rus_6, arnn_rus_6_pred$mean, model = paste0('ARNNx(', arnn_rus_6$p, ',', arnn_rus_6$size , ')')))


write.csv(model_evaluate_rus_6, 'Russia NARFIMA Variants 6.csv', row.names = FALSE)



##################################################### Russia 12 #####################################################

n = 12
set.seed(100)
train_rus_12 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_12 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_12 <- reg_rus[1:length(train_rus_12),]
test_reg_rus_12 <- reg_rus[1:n,]

model_evaluate_rus_12 <- tibble() 



# 1- NARFIMA - ARFIMA Error
arfima_rus_12 <- arfima(train_rus_12, xreg = train_reg_rus_12)
arfima_er_rus_12 <-  residuals(arfima_rus_12)
arfima_er_rus_12[is.na(arfima_er_rus_12)] <-  0
set.seed(100)
narfima_rus_12 <-  auto.narfima(train_rus_12, arfima_er_rus_12, p = 5, q = 2, size = 5, skip = T, xreg = train_reg_rus_12, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_12_pred <-  forecast.narfima(narfima_rus_12, PI = FALSE, h = n, xreg = test_reg_rus_12)
set.seed(100)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, narfima_rus_12_pred$mean, model = paste0('NARFIMA(', narfima_rus_12$p, ',', narfima_rus_12$q, ',', narfima_rus_12$size, ',', narfima_rus_12$skip,')')))


# 2- NARIMA - ARIMA Error
arima_rus_12 <- auto.arima(train_rus_12)
arima_er_rus_12 <-  residuals(arima_rus_12)
arima_er_rus_12[is.na(arima_er_rus_12)] <-  0
set.seed(100)
narfima_rus_12 <-  auto.narfima(train_rus_12, arima_er_rus_12, p = 5, q = 2, size = 5, skip = T, xreg = train_reg_rus_12, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_12_pred <-  forecast.narfima(narfima_rus_12, PI = FALSE, h = n, xreg = test_reg_rus_12)
set.seed(100)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, narfima_rus_12_pred$mean, model = paste0('NARIMA(', narfima_rus_12$p, ',', narfima_rus_12$q, ',', narfima_rus_12$size, ',', narfima_rus_12$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_rus_12)
ss <- AddSeasonal(ss, train_rus_12, nseasons = 12)
bsts_rus_12 <- bsts(train_rus_12 ~ ., 
                     state.specification = ss,
                     niter = 1000,
                     ping = 100,
                     seed = 100,
                     data = cbind(train_reg_rus_12, train_rus_12))
bsts_er_rus_12 <-  residuals(bsts_rus_12)
bsts_er_rus_12[is.na(bsts_er_rus_12)] <-  0
set.seed(100)
narfima_rus_12 <-  auto.narfima(train_rus_12, bsts_er_rus_12, p = 5, q = 2, size = 5, skip = T, xreg = train_reg_rus_12, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_12_pred <-  forecast.narfima(narfima_rus_12, PI = FALSE, h = n, xreg = test_reg_rus_12)
set.seed(100)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, narfima_rus_12_pred$mean, model = paste0('NBSTS(', narfima_rus_12$p, ',', narfima_rus_12$q, ',', narfima_rus_12$size, ',', narfima_rus_12$skip,')')))


# 4- NNaive - Naive Error
naive_rus_12 <- naive(train_rus_12)
naive_er_rus_12 <-  residuals(naive_rus_12)
naive_er_rus_12[is.na(naive_er_rus_12)] <-  0
set.seed(100)
narfima_rus_12 <-  auto.narfima(train_rus_12, naive_er_rus_12, p = 5, q = 2, size = 5, skip = T, xreg = train_reg_rus_12, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_rus_12_pred <-  forecast.narfima(narfima_rus_12, PI = FALSE, h = n, xreg = test_reg_rus_12)
set.seed(100)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, narfima_rus_12_pred$mean, model = paste0('NNaive(', narfima_rus_12$p, ',', narfima_rus_12$q, ',', narfima_rus_12$size, ',', narfima_rus_12$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_rus_12 <- tbats(train_rus_12)
#tbats_er_rus_12 <-  residuals(tbats_rus_12)
#tbats_er_rus_12[is.na(tbats_er_rus_12)] <-  0
#set.seed(100)
#narfima_rus_12 <-  auto.narfima(train_rus_12, tbats_er_rus_12, p = 5, q = 2, size = 5, skip = T, xreg = train_reg_rus_12, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_rus_12_pred <-  forecast.narfima(narfima_rus_12, PI = FALSE, h = n, xreg = test_reg_rus_12)
#set.seed(100)
#model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, narfima_rus_12_pred$mean, model = paste0('NTBATS(', narfima_rus_12$p, ',', narfima_rus_12$q, ',', narfima_rus_12$size, ',', narfima_rus_12$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_rus_12 <- nnetar(train_rus_12, xreg = train_reg_rus_12)
arnn_rus_12_pred <- forecast(arnn_rus_12, h = n, xreg = test_reg_rus_12)
model_evaluate_rus_12 <- rbind(model_evaluate_rus_12, evaluate(test_rus_12, arnn_rus_12_pred$mean, model = paste0('ARNNx(', arnn_rus_12$p, ',', arnn_rus_12$size , ')')))


write.csv(model_evaluate_rus_12, 'Russia NARFIMA Variants 12.csv', row.names = FALSE)



##################################################### Russia 24 #####################################################

n = 24
set.seed(100)
train_rus_24 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_24 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_24 <- reg_rus[1:length(train_rus_24),]
test_reg_rus_24 <- reg_rus[1:n,]

model_evaluate_rus_24 <- tibble()  



# 1- NARFIMA - ARFIMA Error
arfima_rus_24 <- arfima(train_rus_24, xreg = train_reg_rus_24)
arfima_er_rus_24 <-  residuals(arfima_rus_24)
arfima_er_rus_24[is.na(arfima_er_rus_24)] <-  0
set.seed(100)
narfima_rus_24 <-  auto.narfima(train_rus_24, arfima_er_rus_24, p = 1, q = 1, size = 5, skip = T, xreg = train_reg_rus_24, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_24_pred <-  forecast.narfima(narfima_rus_24, PI = FALSE, h = n, xreg = test_reg_rus_24)
set.seed(100)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, narfima_rus_24_pred$mean, model = paste0('NARFIMA(', narfima_rus_24$p, ',', narfima_rus_24$q, ',', narfima_rus_24$size, ',', narfima_rus_24$skip,')')))


# 2- NARIMA - ARIMA Error
arima_rus_24 <- auto.arima(train_rus_24)
arima_er_rus_24 <-  residuals(arima_rus_24)
arima_er_rus_24[is.na(arima_er_rus_24)] <-  0
set.seed(100)
narfima_rus_24 <-  auto.narfima(train_rus_24, arima_er_rus_24, p = 1, q = 1, size = 5, skip = T, xreg = train_reg_rus_24, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_24_pred <-  forecast.narfima(narfima_rus_24, PI = FALSE, h = n, xreg = test_reg_rus_24)
set.seed(100)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, narfima_rus_24_pred$mean, model = paste0('NARIMA(', narfima_rus_24$p, ',', narfima_rus_24$q, ',', narfima_rus_24$size, ',', narfima_rus_24$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_rus_24)
ss <- AddSeasonal(ss, train_rus_24, nseasons = 12)
bsts_rus_24 <- bsts(train_rus_24 ~ ., 
                     state.specification = ss,
                     niter = 1000,
                     ping = 100,
                     seed = 100,
                     data = cbind(train_reg_rus_24, train_rus_24))
bsts_er_rus_24 <-  residuals(bsts_rus_24)
bsts_er_rus_24[is.na(bsts_er_rus_24)] <-  0
set.seed(100)
narfima_rus_24 <-  auto.narfima(train_rus_24, bsts_er_rus_24, p = 1, q = 1, size = 5, skip = T, xreg = train_reg_rus_24, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_24_pred <-  forecast.narfima(narfima_rus_24, PI = FALSE, h = n, xreg = test_reg_rus_24)
set.seed(100)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, narfima_rus_24_pred$mean, model = paste0('NBSTS(', narfima_rus_24$p, ',', narfima_rus_24$q, ',', narfima_rus_24$size, ',', narfima_rus_24$skip,')')))

# 4- NNaive - Naive Error
naive_rus_24 <- naive(train_rus_24)
naive_er_rus_24 <-  residuals(naive_rus_24)
naive_er_rus_24[is.na(naive_er_rus_24)] <-  0
set.seed(100)
narfima_rus_24 <-  auto.narfima(train_rus_24, naive_er_rus_24, p = 1, q = 1, size = 5, skip = T, xreg = train_reg_rus_24, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_rus_24_pred <-  forecast.narfima(narfima_rus_24, PI = FALSE, h = n, xreg = test_reg_rus_24)
set.seed(100)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, narfima_rus_24_pred$mean, model = paste0('NNaive(', narfima_rus_24$p, ',', narfima_rus_24$q, ',', narfima_rus_24$size, ',', narfima_rus_24$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_rus_24 <- tbats(train_rus_24)
#tbats_er_rus_24 <-  residuals(tbats_rus_24)
#tbats_er_rus_24[is.na(tbats_er_rus_24)] <-  0
#set.seed(100)
#narfima_rus_24 <-  auto.narfima(train_rus_24, tbats_er_rus_24, p = 1, q = 1, size = 5, skip = T, xreg = train_reg_rus_24, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_rus_24_pred <-  forecast.narfima(narfima_rus_24, PI = FALSE, h = n, xreg = test_reg_rus_24)
#set.seed(100)
#model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, narfima_rus_24_pred$mean, model = paste0('NTBATS(', narfima_rus_24$p, ',', narfima_rus_24$q, ',', narfima_rus_24$size, ',', narfima_rus_24$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_rus_24 <- nnetar(train_rus_24, xreg = train_reg_rus_24)
arnn_rus_24_pred <- forecast(arnn_rus_24, h = n, xreg = test_reg_rus_24)
model_evaluate_rus_24 <- rbind(model_evaluate_rus_24, evaluate(test_rus_24, arnn_rus_24_pred$mean, model = paste0('ARNNx(', arnn_rus_24$p, ',', arnn_rus_24$size , ')')))


write.csv(model_evaluate_rus_24, 'Russia NARFIMA Variants 24.csv', row.names = FALSE)



##################################################### Russia 48 #####################################################

n = 48
set.seed(100)
train_rus_48 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_48 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_48 <- reg_rus[1:length(train_rus_48),]
test_reg_rus_48 <- reg_rus[1:n,]

model_evaluate_rus_48 <- tibble()  



# 1- NARFIMA - ARFIMA Error
arfima_rus_48 <- arfima(train_rus_48, xreg = train_reg_rus_48)
arfima_er_rus_48 <-  residuals(arfima_rus_48)
arfima_er_rus_48[is.na(arfima_er_rus_48)] <-  0
set.seed(100)
narfima_rus_48 <-  auto.narfima(train_rus_48, arfima_er_rus_48, p = 3, q = 2, size = 1, skip = F, xreg = train_reg_rus_48, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_48_pred <-  forecast.narfima(narfima_rus_48, PI = FALSE, h = n, xreg = test_reg_rus_48)
set.seed(100)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, narfima_rus_48_pred$mean, model = paste0('NARFIMA(', narfima_rus_48$p, ',', narfima_rus_48$q, ',', narfima_rus_48$size, ',', narfima_rus_48$skip,')')))


# 2- NARIMA - ARIMA Error
arima_rus_48 <- auto.arima(train_rus_48)
arima_er_rus_48 <-  residuals(arima_rus_48)
arima_er_rus_48[is.na(arima_er_rus_48)] <-  0
set.seed(100)
narfima_rus_48 <-  auto.narfima(train_rus_48, arima_er_rus_48, p = 3, q = 2, size = 1, skip = F, xreg = train_reg_rus_48, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_48_pred <-  forecast.narfima(narfima_rus_48, PI = FALSE, h = n, xreg = test_reg_rus_48)
set.seed(100)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, narfima_rus_48_pred$mean, model = paste0('NARIMA(', narfima_rus_48$p, ',', narfima_rus_48$q, ',', narfima_rus_48$size, ',', narfima_rus_48$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_rus_48)
ss <- AddSeasonal(ss, train_rus_48, nseasons = 12)
bsts_rus_48 <- bsts(train_rus_48 ~ ., 
                     state.specification = ss,
                     niter = 1000,
                     ping = 100,
                     seed = 100,
                     data = cbind(train_reg_rus_48, train_rus_48))
bsts_er_rus_48 <-  residuals(bsts_rus_48)
bsts_er_rus_48[is.na(bsts_er_rus_48)] <-  0
set.seed(100)
narfima_rus_48 <-  auto.narfima(train_rus_48, bsts_er_rus_48, p = 3, q = 2, size = 1, skip = F, xreg = train_reg_rus_48, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_rus_48_pred <-  forecast.narfima(narfima_rus_48, PI = FALSE, h = n, xreg = test_reg_rus_48)
set.seed(100)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, narfima_rus_48_pred$mean, model = paste0('NBSTS(', narfima_rus_48$p, ',', narfima_rus_48$q, ',', narfima_rus_48$size, ',', narfima_rus_48$skip,')')))


# 4- NNaive - Naive Error
naive_rus_48 <- naive(train_rus_48)
naive_er_rus_48 <-  residuals(naive_rus_48)
naive_er_rus_48[is.na(naive_er_rus_48)] <-  0
set.seed(100)
narfima_rus_48 <-  auto.narfima(train_rus_48, naive_er_rus_48, p = 3, q = 2, size = 1, skip = F, xreg = train_reg_rus_48, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_rus_48_pred <-  forecast.narfima(narfima_rus_48, PI = FALSE, h = n, xreg = test_reg_rus_48)
set.seed(100)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, narfima_rus_48_pred$mean, model = paste0('NNaive(', narfima_rus_48$p, ',', narfima_rus_48$q, ',', narfima_rus_48$size, ',', narfima_rus_48$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_rus_48 <- tbats(train_rus_48)
#tbats_er_rus_48 <-  residuals(tbats_rus_48)
#tbats_er_rus_48[is.na(tbats_er_rus_48)] <-  0
#set.seed(100)
#narfima_rus_48 <-  auto.narfima(train_rus_48, tbats_er_rus_48, p = 3, q = 2, size = 1, skip = F, xreg = train_reg_rus_48, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_rus_48_pred <-  forecast.narfima(narfima_rus_48, PI = FALSE, h = n, xreg = test_reg_rus_48)
#set.seed(100)
#model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, narfima_rus_48_pred$mean, model = paste0('NTBATS(', narfima_rus_48$p, ',', narfima_rus_48$q, ',', narfima_rus_48$size, ',', narfima_rus_48$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_rus_48 <- nnetar(train_rus_48, xreg = train_reg_rus_48)
arnn_rus_48_pred <- forecast(arnn_rus_48, h = n, xreg = test_reg_rus_48)
model_evaluate_rus_48 <- rbind(model_evaluate_rus_48, evaluate(test_rus_48, arnn_rus_48_pred$mean, model = paste0('ARNNx(', arnn_rus_48$p, ',', arnn_rus_48$size , ')')))



write.csv(model_evaluate_rus_48, 'Russia NARFIMA Variants 48.csv', row.names = FALSE)



##################################################### India 1 #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('India_Data.xlsx') %>% rename('Exchange_Rate_ind' = spot_ER_India)                   
exchange_rate_ind <- ts(data$Exchange_Rate_ind)
reg_ind <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)

n = 1
set.seed(100)
train_ind_1 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_1 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_1 <- reg_ind[1:length(train_ind_1),]
test_reg_ind_1 <- matrix(c(reg_ind[1,1], reg_ind[1,2], reg_ind[1,3], reg_ind[1,4], reg_ind[1,5]), ncol = 5, nrow = 1)
colnames(test_reg_ind_1) <- c( "Oil_price_growth_rate_WTI", "SR_Interest_rate_diff_I_U", "global_EPU(PPP)", "US_EMV" ,"US_MPU")

model_evaluate_ind_1 <- tibble()  



# 1- NARFIMA - ARFIMA Error
arfima_ind_1 <- arfima(train_ind_1, xreg = train_reg_ind_1)
arfima_er_ind_1 <-  residuals(arfima_ind_1)
arfima_er_ind_1[is.na(arfima_er_ind_1)] <-  0
set.seed(100)
narfima_ind_1 <-  auto.narfima(train_ind_1, arfima_er_ind_1, p = 2, q = 1, size = 1, skip = T, xreg = train_reg_ind_1, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_1_pred <-  forecast.narfima(narfima_ind_1, PI = FALSE, h = n, xreg = test_reg_ind_1)
set.seed(100)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, narfima_ind_1_pred$mean, model = paste0('NARFIMA(', narfima_ind_1$p, ',', narfima_ind_1$q, ',', narfima_ind_1$size, ',', narfima_ind_1$skip,')')))


# 2- NARIMA - ARIMA Error
arima_ind_1 <- auto.arima(train_ind_1)
arima_er_ind_1 <-  residuals(arima_ind_1)
arima_er_ind_1[is.na(arima_er_ind_1)] <-  0
set.seed(100)
narfima_ind_1 <-  auto.narfima(train_ind_1, arima_er_ind_1, p = 2, q = 1, size = 1, skip = T, xreg = train_reg_ind_1, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_1_pred <-  forecast.narfima(narfima_ind_1, PI = FALSE, h = n, xreg = test_reg_ind_1)
set.seed(100)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, narfima_ind_1_pred$mean, model = paste0('NARIMA(', narfima_ind_1$p, ',', narfima_ind_1$q, ',', narfima_ind_1$size, ',', narfima_ind_1$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_ind_1)
ss <- AddSeasonal(ss, train_ind_1, nseasons = 12)
bsts_ind_1 <- bsts(train_ind_1 ~ ., 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   data = cbind(train_reg_ind_1, train_ind_1))

bsts_er_ind_1 <-  residuals(bsts_ind_1)
bsts_er_ind_1[is.na(bsts_er_ind_1)] <-  0
set.seed(100)
narfima_ind_1 <-  auto.narfima(train_ind_1, bsts_er_ind_1, p = 2, q = 1, size = 1, skip = T, xreg = train_reg_ind_1, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_1_pred <-  forecast.narfima(narfima_ind_1, PI = FALSE, h = n, xreg = test_reg_ind_1)
set.seed(100)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, narfima_ind_1_pred$mean, model = paste0('NBSTS(', narfima_ind_1$p, ',', narfima_ind_1$q, ',', narfima_ind_1$size, ',', narfima_ind_1$skip,')')))


# 4- NNaive - Naive Error
naive_ind_1 <- naive(train_ind_1)
naive_er_ind_1 <-  residuals(naive_ind_1)
naive_er_ind_1[is.na(naive_er_ind_1)] <-  0
set.seed(100)
narfima_ind_1 <-  auto.narfima(train_ind_1, naive_er_ind_1, p = 2, q = 1, size = 1, skip = T, xreg = train_reg_ind_1, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_ind_1_pred <-  forecast.narfima(narfima_ind_1, PI = FALSE, h = n, xreg = test_reg_ind_1)
set.seed(100)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, narfima_ind_1_pred$mean, model = paste0('NNaive(', narfima_ind_1$p, ',', narfima_ind_1$q, ',', narfima_ind_1$size, ',', narfima_ind_1$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_ind_1 <- tbats(train_ind_1)
#tbats_er_ind_1 <-  residuals(tbats_ind_1)
#tbats_er_ind_1[is.na(tbats_er_ind_1)] <-  0
#set.seed(100)
#narfima_ind_1 <-  auto.narfima(train_ind_1, tbats_er_ind_1, p = 2, q = 1, size = 1, skip = T, xreg = train_reg_ind_1, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_ind_1_pred <-  forecast.narfima(narfima_ind_1, PI = FALSE, h = n, xreg = test_reg_ind_1)
#set.seed(100)
#model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, narfima_ind_1_pred$mean, model = paste0('NTBATS(', narfima_ind_1$p, ',', narfima_ind_1$q, ',', narfima_ind_1$size, ',', narfima_ind_1$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_ind_1 <- nnetar(train_ind_1, xreg = train_reg_ind_1)
arnn_ind_1_pred <- forecast(arnn_ind_1, h = n, xreg = test_reg_ind_1)
model_evaluate_ind_1 <- rbind(model_evaluate_ind_1, evaluate(test_ind_1, arnn_ind_1_pred$mean, model = paste0('ARNNx(', arnn_ind_1$p, ',', arnn_ind_1$size , ')')))


write.csv(model_evaluate_ind_1, 'India NARFIMA Variants 1.csv', row.names = FALSE)



##################################################### India 3 #####################################################

n = 3
set.seed(100)
train_ind_3 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_3 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_3 <- reg_ind[1:length(train_ind_3),]
test_reg_ind_3 <- reg_ind[1:n,]

model_evaluate_ind_3 <- tibble() 


# 1- NARFIMA
arfima_ind_3 <- arfima(train_ind_3, xreg = train_reg_ind_3)
arfima_er_ind_3 <-  residuals(arfima_ind_3)
arfima_er_ind_3[is.na(arfima_er_ind_3)] <-  0
set.seed(100)
narfima_ind_3 <-  auto.narfima(train_ind_3, arfima_er_ind_3, p = 4, q = 3, size = 4, skip = T, lambda = 0, lambdae = 0, repeats = 1000, xreg = train_reg_ind_3)
set.seed(100)
narfima_ind_3_pred <-  forecast.narfima(narfima_ind_3, PI = FALSE, h = n, xreg = test_reg_ind_3)
set.seed(100)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, narfima_ind_3_pred$mean, model = paste0('NARFIMA(', narfima_ind_3$p, ',', narfima_ind_3$q, ',', narfima_ind_3$size, ',', narfima_ind_3$skip, ')')))


# 2- NARFIMA - ARIMA Error
arima_ind_3 <- auto.arima(train_ind_3)
arima_er_ind_3 <-  residuals(arima_ind_3)
arima_er_ind_3[is.na(arima_er_ind_3)] <-  0
set.seed(100)
narfima_ind_3 <-  auto.narfima(train_ind_3, arima_er_ind_3, p = 4, q = 3, size = 4, skip = T, xreg = train_reg_ind_3, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_3_pred <-  forecast.narfima(narfima_ind_3, PI = FALSE, h = n, xreg = test_reg_ind_3)
set.seed(100)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, narfima_ind_3_pred$mean, model = paste0('NARIMA(', narfima_ind_3$p, ',', narfima_ind_3$q, ',', narfima_ind_3$size, ',', narfima_ind_3$skip,')')))


# 3- NBSTS - BSTS Error 
ss <- AddSemilocalLinearTrend(list(), train_ind_3)
ss <- AddSeasonal(ss, train_ind_3, nseasons = 12)
bsts_ind_3 <- bsts(train_ind_3 ~ ., 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   data = cbind(train_reg_ind_3, train_ind_3))

bsts_er_ind_3 <-  residuals(bsts_ind_3)
bsts_er_ind_3[is.na(bsts_er_ind_3)] <-  0
set.seed(100)
narfima_ind_3 <-  auto.narfima(train_ind_3, bsts_er_ind_3, p = 4, q = 3, size = 4, skip = T, xreg = train_reg_ind_3, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_3_pred <-  forecast.narfima(narfima_ind_3, PI = FALSE, h = n, xreg = test_reg_ind_3)
set.seed(100)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, narfima_ind_3_pred$mean, model = paste0('NBSTS(', narfima_ind_3$p, ',', narfima_ind_3$q, ',', narfima_ind_3$size, ',', narfima_ind_3$skip,')')))


# 4- NNaive - Naive Error
naive_ind_3 <- naive(train_ind_3)
naive_er_ind_3 <-  residuals(naive_ind_3)
naive_er_ind_3[is.na(naive_er_ind_3)] <-  0
set.seed(100)
narfima_ind_3 <-  auto.narfima(train_ind_3, naive_er_ind_3, p = 4, q = 3, size = 4, skip = T, xreg = train_reg_ind_3, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_ind_3_pred <-  forecast.narfima(narfima_ind_3, PI = FALSE, h = n, xreg = test_reg_ind_3)
set.seed(100)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, narfima_ind_3_pred$mean, model = paste0('NNaive(', narfima_ind_3$p, ',', narfima_ind_3$q, ',', narfima_ind_3$size, ',', narfima_ind_3$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_ind_3 <- tbats(train_ind_3)
#tbats_er_ind_3 <-  residuals(tbats_ind_3)
#tbats_er_ind_3[is.na(tbats_er_ind_3)] <-  0
#set.seed(100)
#narfima_ind_3 <-  auto.narfima(train_ind_3, tbats_er_ind_3, p = 4, q = 3, size = 4, skip = T, xreg = train_reg_ind_3, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_ind_3_pred <-  forecast.narfima(narfima_ind_3, PI = FALSE, h = n, xreg = test_reg_ind_3)
#set.seed(100)
#model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, narfima_ind_3_pred$mean, model = paste0('NTBATS(', narfima_ind_3$p, ',', narfima_ind_3$q, ',', narfima_ind_3$size, ',', narfima_ind_3$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_ind_3 <- nnetar(train_ind_3, xreg = train_reg_ind_3)
arnn_ind_3_pred <- forecast(arnn_ind_3, h = n, xreg = test_reg_ind_3)
model_evaluate_ind_3 <- rbind(model_evaluate_ind_3, evaluate(test_ind_3, arnn_ind_3_pred$mean, model = paste0('ARNNx(', arnn_ind_3$p, ',', arnn_ind_3$size , ')')))

write.csv(model_evaluate_ind_3, 'India NARFIMA Variants 3.csv', row.names = FALSE)



##################################################### India 6 #####################################################

n = 6
set.seed(100)
train_ind_6 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_6 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_6 <- reg_ind[1:length(train_ind_6),]
test_reg_ind_6 <- reg_ind[1:n,]

model_evaluate_ind_6 <- tibble()



# 1- NARFIMA - ARFIMA Error
arfima_ind_6 <- arfima(train_ind_6, xreg = train_reg_ind_6)
arfima_er_ind_6 <-  residuals(arfima_ind_6)
arfima_er_ind_6[is.na(arfima_er_ind_6)] <-  0
set.seed(100)
narfima_ind_6 <-  auto.narfima(train_ind_6, arfima_er_ind_6, p = 4, q = 2, size = 1, skip = T, xreg = train_reg_ind_6, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_6_pred <-  forecast.narfima(narfima_ind_6, PI = FALSE, h = n, xreg = test_reg_ind_6)
set.seed(100)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, narfima_ind_6_pred$mean, model = paste0('NARFIMA(', narfima_ind_6$p, ',', narfima_ind_6$q, ',', narfima_ind_6$size, ',', narfima_ind_6$skip,')')))


# 2- NARIMA - ARIMA Error
arima_ind_6 <- auto.arima(train_ind_6)
arima_er_ind_6 <-  residuals(arima_ind_6)
arima_er_ind_6[is.na(arima_er_ind_6)] <-  0
set.seed(100)
narfima_ind_6 <-  auto.narfima(train_ind_6, arima_er_ind_6, p = 4, q = 2, size = 1, skip = T, xreg = train_reg_ind_6, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_6_pred <-  forecast.narfima(narfima_ind_6, PI = FALSE, h = n, xreg = test_reg_ind_6)
set.seed(100)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, narfima_ind_6_pred$mean, model = paste0('NARIMA(', narfima_ind_6$p, ',', narfima_ind_6$q, ',', narfima_ind_6$size, ',', narfima_ind_6$skip,')')))


# 3- NBSTS - BSTS Error
bsts_ind_6 <- bsts(train_ind_6 ~ ., 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   data = cbind(train_reg_ind_6, train_ind_6))
bsts_er_ind_6 <-  residuals(bsts_ind_6)
bsts_er_ind_6[is.na(bsts_er_ind_6)] <-  0
set.seed(100)
narfima_ind_6 <-  auto.narfima(train_ind_6, bsts_er_ind_6, p = 4, q = 2, size = 1, skip = T, xreg = train_reg_ind_6, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_6_pred <-  forecast.narfima(narfima_ind_6, PI = FALSE, h = n, xreg = test_reg_ind_6)
set.seed(100)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, narfima_ind_6_pred$mean, model = paste0('NBSTS(', narfima_ind_6$p, ',', narfima_ind_6$q, ',', narfima_ind_6$size, ',', narfima_ind_6$skip,')')))


# 4- NNaive - Naive Error
naive_ind_6 <- naive(train_ind_6)
naive_er_ind_6 <-  residuals(naive_ind_6)
naive_er_ind_6[is.na(naive_er_ind_6)] <-  0
set.seed(100)
narfima_ind_6 <-  auto.narfima(train_ind_6, naive_er_ind_6, p = 4, q = 2, size = 1, skip = T, xreg = train_reg_ind_6, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_ind_6_pred <-  forecast.narfima(narfima_ind_6, PI = FALSE, h = n, xreg = test_reg_ind_6)
set.seed(100)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, narfima_ind_6_pred$mean, model = paste0('NNaive(', narfima_ind_6$p, ',', narfima_ind_6$q, ',', narfima_ind_6$size, ',', narfima_ind_6$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_ind_6 <- tbats(train_ind_6)
#tbats_er_ind_6 <-  residuals(tbats_ind_6)
#tbats_er_ind_6[is.na(tbats_er_ind_6)] <-  0
#set.seed(100)
#narfima_ind_6 <-  auto.narfima(train_ind_6, tbats_er_ind_6, p = 4, q = 2, size = 1, skip = T, xreg = train_reg_ind_6, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_ind_6_pred <-  forecast.narfima(narfima_ind_6, PI = FALSE, h = n, xreg = test_reg_ind_6)
#set.seed(100)
#model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, narfima_ind_6_pred$mean, model = paste0('NTBATS(', narfima_ind_6$p, ',', narfima_ind_6$q, ',', narfima_ind_6$size, ',', narfima_ind_6$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_ind_6 <- nnetar(train_ind_6, xreg = train_reg_ind_6)
arnn_ind_6_pred <- forecast(arnn_ind_6, h = n, xreg = test_reg_ind_6)
model_evaluate_ind_6 <- rbind(model_evaluate_ind_6, evaluate(test_ind_6, arnn_ind_6_pred$mean, model = paste0('ARNNx(', arnn_ind_6$p, ',', arnn_ind_6$size , ')')))


write.csv(model_evaluate_ind_6, 'India NARFIMA Variants 6.csv', row.names = FALSE)



##################################################### India 12 #####################################################

n = 12
set.seed(100)
train_ind_12 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_12 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_12 <- reg_ind[1:length(train_ind_12),]
test_reg_ind_12 <- reg_ind[1:n,]

model_evaluate_ind_12 <- tibble() 



# 1- NARFIMA - ARFIMA Error
arfima_ind_12 <- arfima(train_ind_12, xreg = train_reg_ind_12)
arfima_er_ind_12 <-  residuals(arfima_ind_12)
arfima_er_ind_12[is.na(arfima_er_ind_12)] <-  0
set.seed(100)
narfima_ind_12 <-  auto.narfima(train_ind_12, arfima_er_ind_12, p = 1, q = 3, size = 4, skip = T, xreg = train_reg_ind_12, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_12_pred <-  forecast.narfima(narfima_ind_12, PI = FALSE, h = n, xreg = test_reg_ind_12)
set.seed(100)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, narfima_ind_12_pred$mean, model = paste0('NARFIMA(', narfima_ind_12$p, ',', narfima_ind_12$q, ',', narfima_ind_12$size, ',', narfima_ind_12$skip,')')))


# 2- NARIMA - ARIMA Error
arima_ind_12 <- auto.arima(train_ind_12)
arima_er_ind_12 <-  residuals(arima_ind_12)
arima_er_ind_12[is.na(arima_er_ind_12)] <-  0
set.seed(100)
narfima_ind_12 <-  auto.narfima(train_ind_12, arima_er_ind_12, p = 1, q = 3, size = 4, skip = T, xreg = train_reg_ind_12, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_12_pred <-  forecast.narfima(narfima_ind_12, PI = FALSE, h = n, xreg = test_reg_ind_12)
set.seed(100)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, narfima_ind_12_pred$mean, model = paste0('NARIMA(', narfima_ind_12$p, ',', narfima_ind_12$q, ',', narfima_ind_12$size, ',', narfima_ind_12$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_ind_12)
ss <- AddSeasonal(ss, train_ind_12, nseasons = 12)
bsts_ind_12 <- bsts(train_ind_12 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_ind_12, train_ind_12))
bsts_er_ind_12 <-  residuals(bsts_ind_12)
bsts_er_ind_12[is.na(bsts_er_ind_12)] <-  0
set.seed(100)
narfima_ind_12 <-  auto.narfima(train_ind_12, bsts_er_ind_12, p = 1, q = 3, size = 4, skip = T, xreg = train_reg_ind_12, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_12_pred <-  forecast.narfima(narfima_ind_12, PI = FALSE, h = n, xreg = test_reg_ind_12)
set.seed(100)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, narfima_ind_12_pred$mean, model = paste0('NBSTS(', narfima_ind_12$p, ',', narfima_ind_12$q, ',', narfima_ind_12$size, ',', narfima_ind_12$skip,')')))

# 4- NNaive - Naive Error
naive_ind_12 <- naive(train_ind_12)
naive_er_ind_12 <-  residuals(naive_ind_12)
naive_er_ind_12[is.na(naive_er_ind_12)] <-  0
set.seed(100)
narfima_ind_12 <-  auto.narfima(train_ind_12, naive_er_ind_12, p = 1, q = 3, size = 4, skip = T, xreg = train_reg_ind_12, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_ind_12_pred <-  forecast.narfima(narfima_ind_12, PI = FALSE, h = n, xreg = test_reg_ind_12)
set.seed(100)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, narfima_ind_12_pred$mean, model = paste0('NNaive(', narfima_ind_12$p, ',', narfima_ind_12$q, ',', narfima_ind_12$size, ',', narfima_ind_12$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_ind_12 <- tbats(train_ind_12)
#tbats_er_ind_12 <-  residuals(tbats_ind_12)
#tbats_er_ind_12[is.na(tbats_er_ind_12)] <-  0
#set.seed(100)
#narfima_ind_12 <-  auto.narfima(train_ind_12, tbats_er_ind_12, p = 1, q = 3, size = 4, skip = T, xreg = train_reg_ind_12, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_ind_12_pred <-  forecast.narfima(narfima_ind_12, PI = FALSE, h = n, xreg = test_reg_ind_12)
#set.seed(100)
#model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, narfima_ind_12_pred$mean, model = paste0('NTBATS(', narfima_ind_12$p, ',', narfima_ind_12$q, ',', narfima_ind_12$size, ',', narfima_ind_12$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_ind_12 <- nnetar(train_ind_12, xreg = train_reg_ind_12)
arnn_ind_12_pred <- forecast(arnn_ind_12, h = n, xreg = test_reg_ind_12)
model_evaluate_ind_12 <- rbind(model_evaluate_ind_12, evaluate(test_ind_12, arnn_ind_12_pred$mean, model = paste0('ARNNx(', arnn_ind_12$p, ',', arnn_ind_12$size , ')')))


write.csv(model_evaluate_ind_12, 'India NARFIMA Variants 12.csv', row.names = FALSE)



##################################################### India 24 #####################################################

n = 24
set.seed(100)
train_ind_24 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_24 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_24 <- reg_ind[1:length(train_ind_24),]
test_reg_ind_24 <- reg_ind[1:n,]

model_evaluate_ind_24 <- tibble()  



# 1- NARFIMA - ARFIMA Error
arfima_ind_24 <- arfima(train_ind_24, xreg = train_reg_ind_24)
arfima_er_ind_24 <-  residuals(arfima_ind_24)
arfima_er_ind_24[is.na(arfima_er_ind_24)] <-  0
set.seed(100)
narfima_ind_24 <-  auto.narfima(train_ind_24, arfima_er_ind_24, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_ind_24, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_24_pred <-  forecast.narfima(narfima_ind_24, PI = FALSE, h = n, xreg = test_reg_ind_24)
set.seed(100)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, narfima_ind_24_pred$mean, model = paste0('NARFIMA(', narfima_ind_24$p, ',', narfima_ind_24$q, ',', narfima_ind_24$size, ',', narfima_ind_24$skip,')')))


# 2- NARIMA - ARIMA Error
arima_ind_24 <- auto.arima(train_ind_24)
arima_er_ind_24 <-  residuals(arima_ind_24)
arima_er_ind_24[is.na(arima_er_ind_24)] <-  0
set.seed(100)
narfima_ind_24 <-  auto.narfima(train_ind_24, arima_er_ind_24, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_ind_24, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_24_pred <-  forecast.narfima(narfima_ind_24, PI = FALSE, h = n, xreg = test_reg_ind_24)
set.seed(100)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, narfima_ind_24_pred$mean, model = paste0('NARIMA(', narfima_ind_24$p, ',', narfima_ind_24$q, ',', narfima_ind_24$size, ',', narfima_ind_24$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_ind_24)
ss <- AddSeasonal(ss, train_ind_24, nseasons = 12)
bsts_ind_24 <- bsts(train_ind_24 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_ind_24, train_ind_24))
bsts_er_ind_24 <-  residuals(bsts_ind_24)
bsts_er_ind_24[is.na(bsts_er_ind_24)] <-  0
set.seed(100)
narfima_ind_24 <-  auto.narfima(train_ind_24, bsts_er_ind_24, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_ind_24, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_24_pred <-  forecast.narfima(narfima_ind_24, PI = FALSE, h = n, xreg = test_reg_ind_24)
set.seed(100)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, narfima_ind_24_pred$mean, model = paste0('NBSTS(', narfima_ind_24$p, ',', narfima_ind_24$q, ',', narfima_ind_24$size, ',', narfima_ind_24$skip,')')))


# 4- NNaive - Naive Error
naive_ind_24 <- naive(train_ind_24)
naive_er_ind_24 <-  residuals(naive_ind_24)
naive_er_ind_24[is.na(naive_er_ind_24)] <-  0
set.seed(100)
narfima_ind_24 <-  auto.narfima(train_ind_24, naive_er_ind_24, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_ind_24, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_ind_24_pred <-  forecast.narfima(narfima_ind_24, PI = FALSE, h = n, xreg = test_reg_ind_24)
set.seed(100)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, narfima_ind_24_pred$mean, model = paste0('NNaive(', narfima_ind_24$p, ',', narfima_ind_24$q, ',', narfima_ind_24$size, ',', narfima_ind_24$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_ind_24 <- tbats(train_ind_24)
#tbats_er_ind_24 <-  residuals(tbats_ind_24)
#tbats_er_ind_24[is.na(tbats_er_ind_24)] <-  0
#set.seed(100)
#narfima_ind_24 <-  auto.narfima(train_ind_24, tbats_er_ind_24, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_ind_24, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_ind_24_pred <-  forecast.narfima(narfima_ind_24, PI = FALSE, h = n, xreg = test_reg_ind_24)
#set.seed(100)
#model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, narfima_ind_24_pred$mean, model = paste0('NTBATS(', narfima_ind_24$p, ',', narfima_ind_24$q, ',', narfima_ind_24$size, ',', narfima_ind_24$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_ind_24 <- nnetar(train_ind_24, xreg = train_reg_ind_24)
arnn_ind_24_pred <- forecast(arnn_ind_24, h = n, xreg = test_reg_ind_24)
model_evaluate_ind_24 <- rbind(model_evaluate_ind_24, evaluate(test_ind_24, arnn_ind_24_pred$mean, model = paste0('ARNNx(', arnn_ind_24$p, ',', arnn_ind_24$size , ')')))


write.csv(model_evaluate_ind_24, 'India NARFIMA Variants 24.csv', row.names = FALSE)



##################################################### India 48 #####################################################

n = 48
set.seed(100)
train_ind_48 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_48 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_48 <- reg_ind[1:length(train_ind_48),]
test_reg_ind_48 <- reg_ind[1:n,]

model_evaluate_ind_48 <- tibble()  



# 1- NARFIMA - ARFIMA Error
arfima_ind_48 <- arfima(train_ind_48, xreg = train_reg_ind_48)
arfima_er_ind_48 <-  residuals(arfima_ind_48)
arfima_er_ind_48[is.na(arfima_er_ind_48)] <-  0
set.seed(100)
narfima_ind_48 <-  auto.narfima(train_ind_48, arfima_er_ind_48, p = 2, q = 4, size = 4, skip = T, xreg = train_reg_ind_48, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_48_pred <-  forecast.narfima(narfima_ind_48, PI = FALSE, h = n, xreg = test_reg_ind_48)
set.seed(100)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, narfima_ind_48_pred$mean, model = paste0('NARFIMA(', narfima_ind_48$p, ',', narfima_ind_48$q, ',', narfima_ind_48$size, ',', narfima_ind_48$skip,')')))


# 2- NARIMA - ARIMA Error
arima_ind_48 <- auto.arima(train_ind_48)
arima_er_ind_48 <-  residuals(arima_ind_48)
arima_er_ind_48[is.na(arima_er_ind_48)] <-  0
set.seed(100)
narfima_ind_48 <-  auto.narfima(train_ind_48, arima_er_ind_48, p = 2, q = 4, size = 4, skip = T, xreg = train_reg_ind_48, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_48_pred <-  forecast.narfima(narfima_ind_48, PI = FALSE, h = n, xreg = test_reg_ind_48)
set.seed(100)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, narfima_ind_48_pred$mean, model = paste0('NARIMA(', narfima_ind_48$p, ',', narfima_ind_48$q, ',', narfima_ind_48$size, ',', narfima_ind_48$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_ind_48)
ss <- AddSeasonal(ss, train_ind_48, nseasons = 12)
bsts_ind_48 <- bsts(train_ind_48 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_ind_48, train_ind_48))
bsts_er_ind_48 <-  residuals(bsts_ind_48)
bsts_er_ind_48[is.na(bsts_er_ind_48)] <-  0
set.seed(100)
narfima_ind_48 <-  auto.narfima(train_ind_48, bsts_er_ind_48, p = 2, q = 4, size = 4, skip = T, xreg = train_reg_ind_48, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_ind_48_pred <-  forecast.narfima(narfima_ind_48, PI = FALSE, h = n, xreg = test_reg_ind_48)
set.seed(100)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, narfima_ind_48_pred$mean, model = paste0('NBSTS(', narfima_ind_48$p, ',', narfima_ind_48$q, ',', narfima_ind_48$size, ',', narfima_ind_48$skip,')')))


# 4- NNaive - Naive Error
naive_ind_48 <- naive(train_ind_48)
naive_er_ind_48 <-  residuals(naive_ind_48)
naive_er_ind_48[is.na(naive_er_ind_48)] <-  0
set.seed(100)
narfima_ind_48 <-  auto.narfima(train_ind_48, naive_er_ind_48, p = 2, q = 4, size = 4, skip = T, xreg = train_reg_ind_48, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_ind_48_pred <-  forecast.narfima(narfima_ind_48, PI = FALSE, h = n, xreg = test_reg_ind_48)
set.seed(100)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, narfima_ind_48_pred$mean, model = paste0('NNaive(', narfima_ind_48$p, ',', narfima_ind_48$q, ',', narfima_ind_48$size, ',', narfima_ind_48$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_ind_48 <- tbats(train_ind_48)
#tbats_er_ind_48 <-  residuals(tbats_ind_48)
#tbats_er_ind_48[is.na(tbats_er_ind_48)] <-  0
#set.seed(100)
#narfima_ind_48 <-  auto.narfima(train_ind_48, tbats_er_ind_48, p = 2, q = 4, size = 4, skip = T, xreg = train_reg_ind_48, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_ind_48_pred <-  forecast.narfima(narfima_ind_48, PI = FALSE, h = n, xreg = test_reg_ind_48)
#set.seed(100)
#model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, narfima_ind_48_pred$mean, model = paste0('NTBATS(', narfima_ind_48$p, ',', narfima_ind_48$q, ',', narfima_ind_48$size, ',', narfima_ind_48$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_ind_48 <- nnetar(train_ind_48, xreg = train_reg_ind_48)
arnn_ind_48_pred <- forecast(arnn_ind_48, h = n, xreg = test_reg_ind_48)
model_evaluate_ind_48 <- rbind(model_evaluate_ind_48, evaluate(test_ind_48, arnn_ind_48_pred$mean, model = paste0('ARNNx(', arnn_ind_48$p, ',', arnn_ind_48$size , ')')))



write.csv(model_evaluate_ind_48, 'India NARFIMA Variants 48.csv', row.names = FALSE)



##################################################### China 1 #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('China_Data.xlsx') %>% rename('Exchange_Rate_chn' = spot_ER_China)                   
exchange_rate_chn <- ts(data$Exchange_Rate_chn)
reg_chn <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)

n = 1
set.seed(100)
train_chn_1 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_1 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_1 <- reg_chn[1:length(train_chn_1),]
test_reg_chn_1 <- matrix(c(reg_chn[1,1], reg_chn[1,2], reg_chn[1,3], reg_chn[1,4], reg_chn[1,5]), ncol = 5, nrow = 1)
colnames(test_reg_chn_1) <- c( "Oil_price_growth_rate_WTI", "SR_Interest_rate_diff_C_U", "global_EPU(PPP)", "US_EMV" ,"US_MPU")

model_evaluate_chn_1 <- tibble()  



# 1- NARFIMA - ARFIMA Error
arfima_chn_1 <- arfima(train_chn_1, xreg = train_reg_chn_1)
arfima_er_chn_1 <-  residuals(arfima_chn_1)
arfima_er_chn_1[is.na(arfima_er_chn_1)] <-  0
set.seed(100)
narfima_chn_1 <-  auto.narfima(train_chn_1, arfima_er_chn_1, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_chn_1, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_1_pred <-  forecast.narfima(narfima_chn_1, PI = FALSE, h = n, xreg = test_reg_chn_1)
set.seed(100)
model_evaluate_chn_1 <- rbind(model_evaluate_chn_1, evaluate(test_chn_1, narfima_chn_1_pred$mean, model = paste0('NARFIMA(', narfima_chn_1$p, ',', narfima_chn_1$q, ',', narfima_chn_1$size, ',', narfima_chn_1$skip,')')))


# 2- NARIMA - ARIMA Error
arima_chn_1 <- auto.arima(train_chn_1)
arima_er_chn_1 <-  residuals(arima_chn_1)
arima_er_chn_1[is.na(arima_er_chn_1)] <-  0
set.seed(100)
narfima_chn_1 <-  auto.narfima(train_chn_1, arima_er_chn_1, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_chn_1, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_1_pred <-  forecast.narfima(narfima_chn_1, PI = FALSE, h = n, xreg = test_reg_chn_1)
set.seed(100)
model_evaluate_chn_1 <- rbind(model_evaluate_chn_1, evaluate(test_chn_1, narfima_chn_1_pred$mean, model = paste0('NARIMA(', narfima_chn_1$p, ',', narfima_chn_1$q, ',', narfima_chn_1$size, ',', narfima_chn_1$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_chn_1)
ss <- AddSeasonal(ss, train_chn_1, nseasons = 12)
bsts_chn_1 <- bsts(train_chn_1 ~ ., 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   data = cbind(train_reg_chn_1, train_chn_1))

bsts_er_chn_1 <-  residuals(bsts_chn_1)
bsts_er_chn_1[is.na(bsts_er_chn_1)] <-  0
set.seed(100)
narfima_chn_1 <-  auto.narfima(train_chn_1, bsts_er_chn_1, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_chn_1, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_1_pred <-  forecast.narfima(narfima_chn_1, PI = FALSE, h = n, xreg = test_reg_chn_1)
set.seed(100)
model_evaluate_chn_1 <- rbind(model_evaluate_chn_1, evaluate(test_chn_1, narfima_chn_1_pred$mean, model = paste0('NBSTS(', narfima_chn_1$p, ',', narfima_chn_1$q, ',', narfima_chn_1$size, ',', narfima_chn_1$skip,')')))


# 4- NNaive - Naive Error
naive_chn_1 <- naive(train_chn_1)
naive_er_chn_1 <-  residuals(naive_chn_1)
naive_er_chn_1[is.na(naive_er_chn_1)] <-  0
set.seed(100)
narfima_chn_1 <-  auto.narfima(train_chn_1, naive_er_chn_1, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_chn_1, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_chn_1_pred <-  forecast.narfima(narfima_chn_1, PI = FALSE, h = n, xreg = test_reg_chn_1)
set.seed(100)
model_evaluate_chn_1 <- rbind(model_evaluate_chn_1, evaluate(test_chn_1, narfima_chn_1_pred$mean, model = paste0('NNaive(', narfima_chn_1$p, ',', narfima_chn_1$q, ',', narfima_chn_1$size, ',', narfima_chn_1$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_chn_1 <- tbats(train_chn_1)
#tbats_er_chn_1 <-  residuals(tbats_chn_1)
#tbats_er_chn_1[is.na(tbats_er_chn_1)] <-  0
#set.seed(100)
#narfima_chn_1 <-  auto.narfima(train_chn_1, tbats_er_chn_1, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_chn_1, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_chn_1_pred <-  forecast.narfima(narfima_chn_1, PI = FALSE, h = n, xreg = test_reg_chn_1)
#set.seed(100)
#model_evaluate_chn_1 <- rbind(model_evaluate_chn_1, evaluate(test_chn_1, narfima_chn_1_pred$mean, model = paste0('NTBATS(', narfima_chn_1$p, ',', narfima_chn_1$q, ',', narfima_chn_1$size, ',', narfima_chn_1$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_chn_1 <- nnetar(train_chn_1, xreg = train_reg_chn_1)
arnn_chn_1_pred <- forecast(arnn_chn_1, h = n, xreg = test_reg_chn_1)
model_evaluate_chn_1 <- rbind(model_evaluate_chn_1, evaluate(test_chn_1, arnn_chn_1_pred$mean, model = paste0('ARNNx(', arnn_chn_1$p, ',', arnn_chn_1$size , ')')))


write.csv(model_evaluate_chn_1, 'China NARFIMA Variants 1.csv', row.names = FALSE)



##################################################### China 3 #####################################################

n = 3
set.seed(100)
train_chn_3 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_3 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_3 <- reg_chn[1:length(train_chn_3),]
test_reg_chn_3 <- reg_chn[1:n,]

model_evaluate_chn_3 <- tibble() 


# 1- NARFIMA
arfima_chn_3 <- arfima(train_chn_3, xreg = train_reg_chn_3)
arfima_er_chn_3 <-  residuals(arfima_chn_3)
arfima_er_chn_3[is.na(arfima_er_chn_3)] <-  0
set.seed(100)
narfima_chn_3 <-  auto.narfima(train_chn_3, arfima_er_chn_3, p = 1, q = 2, size = 1, skip = T, lambda = 0, lambdae = 0, repeats = 1000, xreg = train_reg_chn_3)
set.seed(100)
narfima_chn_3_pred <-  forecast.narfima(narfima_chn_3, PI = FALSE, h = n, xreg = test_reg_chn_3)
set.seed(100)
model_evaluate_chn_3 <- rbind(model_evaluate_chn_3, evaluate(test_chn_3, narfima_chn_3_pred$mean, model = paste0('NARFIMA(', narfima_chn_3$p, ',', narfima_chn_3$q, ',', narfima_chn_3$size, ',', narfima_chn_3$skip, ')')))


# 2- NARFIMA - ARIMA Error
arima_chn_3 <- auto.arima(train_chn_3)
arima_er_chn_3 <-  residuals(arima_chn_3)
arima_er_chn_3[is.na(arima_er_chn_3)] <-  0
set.seed(100)
narfima_chn_3 <-  auto.narfima(train_chn_3, arima_er_chn_3, p = 1, q = 2, size = 1, skip = T, xreg = train_reg_chn_3, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_3_pred <-  forecast.narfima(narfima_chn_3, PI = FALSE, h = n, xreg = test_reg_chn_3)
set.seed(100)
model_evaluate_chn_3 <- rbind(model_evaluate_chn_3, evaluate(test_chn_3, narfima_chn_3_pred$mean, model = paste0('NARIMA(', narfima_chn_3$p, ',', narfima_chn_3$q, ',', narfima_chn_3$size, ',', narfima_chn_3$skip,')')))


# 3- NBSTS - BSTS Error 
ss <- AddSemilocalLinearTrend(list(), train_chn_3)
ss <- AddSeasonal(ss, train_chn_3, nseasons = 12)
bsts_chn_3 <- bsts(train_chn_3 ~ ., 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   data = cbind(train_reg_chn_3, train_chn_3))

bsts_er_chn_3 <-  residuals(bsts_chn_3)
bsts_er_chn_3[is.na(bsts_er_chn_3)] <-  0
set.seed(100)
narfima_chn_3 <-  auto.narfima(train_chn_3, bsts_er_chn_3, p = 1, q = 2, size = 1, skip = T, xreg = train_reg_chn_3, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_3_pred <-  forecast.narfima(narfima_chn_3, PI = FALSE, h = n, xreg = test_reg_chn_3)
set.seed(100)
model_evaluate_chn_3 <- rbind(model_evaluate_chn_3, evaluate(test_chn_3, narfima_chn_3_pred$mean, model = paste0('NBSTS(', narfima_chn_3$p, ',', narfima_chn_3$q, ',', narfima_chn_3$size, ',', narfima_chn_3$skip,')')))


# 4- NNaive - Naive Error
naive_chn_3 <- naive(train_chn_3)
naive_er_chn_3 <-  residuals(naive_chn_3)
naive_er_chn_3[is.na(naive_er_chn_3)] <-  0
set.seed(100)
narfima_chn_3 <-  auto.narfima(train_chn_3, naive_er_chn_3, p = 1, q = 2, size = 1, skip = T, xreg = train_reg_chn_3, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_chn_3_pred <-  forecast.narfima(narfima_chn_3, PI = FALSE, h = n, xreg = test_reg_chn_3)
set.seed(100)
model_evaluate_chn_3 <- rbind(model_evaluate_chn_3, evaluate(test_chn_3, narfima_chn_3_pred$mean, model = paste0('NNaive(', narfima_chn_3$p, ',', narfima_chn_3$q, ',', narfima_chn_3$size, ',', narfima_chn_3$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_chn_3 <- tbats(train_chn_3)
#tbats_er_chn_3 <-  residuals(tbats_chn_3)
#tbats_er_chn_3[is.na(tbats_er_chn_3)] <-  0
#set.seed(100)
#narfima_chn_3 <-  auto.narfima(train_chn_3, tbats_er_chn_3, p = 1, q = 2, size = 1, skip = T, xreg = train_reg_chn_3, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_chn_3_pred <-  forecast.narfima(narfima_chn_3, PI = FALSE, h = n, xreg = test_reg_chn_3)
#set.seed(100)
#model_evaluate_chn_3 <- rbind(model_evaluate_chn_3, evaluate(test_chn_3, narfima_chn_3_pred$mean, model = paste0('NTBATS(', narfima_chn_3$p, ',', narfima_chn_3$q, ',', narfima_chn_3$size, ',', narfima_chn_3$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_chn_3 <- nnetar(train_chn_3, xreg = train_reg_chn_3)
arnn_chn_3_pred <- forecast(arnn_chn_3, h = n, xreg = test_reg_chn_3)
model_evaluate_chn_3 <- rbind(model_evaluate_chn_3, evaluate(test_chn_3, arnn_chn_3_pred$mean, model = paste0('ARNNx(', arnn_chn_3$p, ',', arnn_chn_3$size , ')')))

write.csv(model_evaluate_chn_3, 'China NARFIMA Variants 3.csv', row.names = FALSE)



##################################################### China 6 #####################################################

n = 6
set.seed(100)
train_chn_6 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_6 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_6 <- reg_chn[1:length(train_chn_6),]
test_reg_chn_6 <- reg_chn[1:n,]

model_evaluate_chn_6 <- tibble()



# 1- NARFIMA - ARFIMA Error
arfima_chn_6 <- arfima(train_chn_6, xreg = train_reg_chn_6)
arfima_er_chn_6 <-  residuals(arfima_chn_6)
arfima_er_chn_6[is.na(arfima_er_chn_6)] <-  0
set.seed(100)
narfima_chn_6 <-  auto.narfima(train_chn_6, arfima_er_chn_6, p = 1, q = 1, size = 2, skip = F, xreg = train_reg_chn_6, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_6_pred <-  forecast.narfima(narfima_chn_6, PI = FALSE, h = n, xreg = test_reg_chn_6)
set.seed(100)
model_evaluate_chn_6 <- rbind(model_evaluate_chn_6, evaluate(test_chn_6, narfima_chn_6_pred$mean, model = paste0('NARFIMA(', narfima_chn_6$p, ',', narfima_chn_6$q, ',', narfima_chn_6$size, ',', narfima_chn_6$skip,')')))


# 2- NARIMA - ARIMA Error
arima_chn_6 <- auto.arima(train_chn_6)
arima_er_chn_6 <-  residuals(arima_chn_6)
arima_er_chn_6[is.na(arima_er_chn_6)] <-  0
set.seed(100)
narfima_chn_6 <-  auto.narfima(train_chn_6, arima_er_chn_6, p = 1, q = 1, size = 2, skip = F, xreg = train_reg_chn_6, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_6_pred <-  forecast.narfima(narfima_chn_6, PI = FALSE, h = n, xreg = test_reg_chn_6)
set.seed(100)
model_evaluate_chn_6 <- rbind(model_evaluate_chn_6, evaluate(test_chn_6, narfima_chn_6_pred$mean, model = paste0('NARIMA(', narfima_chn_6$p, ',', narfima_chn_6$q, ',', narfima_chn_6$size, ',', narfima_chn_6$skip,')')))


# 3- NBSTS - BSTS Error
bsts_chn_6 <- bsts(train_chn_6 ~ ., 
                   state.specification = ss,
                   niter = 1000,
                   ping = 100,
                   seed = 100,
                   data = cbind(train_reg_chn_6, train_chn_6))
bsts_er_chn_6 <-  residuals(bsts_chn_6)
bsts_er_chn_6[is.na(bsts_er_chn_6)] <-  0
set.seed(100)
narfima_chn_6 <-  auto.narfima(train_chn_6, bsts_er_chn_6, p = 1, q = 1, size = 2, skip = F, xreg = train_reg_chn_6, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_6_pred <-  forecast.narfima(narfima_chn_6, PI = FALSE, h = n, xreg = test_reg_chn_6)
set.seed(100)
model_evaluate_chn_6 <- rbind(model_evaluate_chn_6, evaluate(test_chn_6, narfima_chn_6_pred$mean, model = paste0('NBSTS(', narfima_chn_6$p, ',', narfima_chn_6$q, ',', narfima_chn_6$size, ',', narfima_chn_6$skip,')')))


# 4- NNaive - Naive Error
naive_chn_6 <- naive(train_chn_6)
naive_er_chn_6 <-  residuals(naive_chn_6)
naive_er_chn_6[is.na(naive_er_chn_6)] <-  0
set.seed(100)
narfima_chn_6 <-  auto.narfima(train_chn_6, naive_er_chn_6, p = 1, q = 1, size = 2, skip = F, xreg = train_reg_chn_6, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_chn_6_pred <-  forecast.narfima(narfima_chn_6, PI = FALSE, h = n, xreg = test_reg_chn_6)
set.seed(100)
model_evaluate_chn_6 <- rbind(model_evaluate_chn_6, evaluate(test_chn_6, narfima_chn_6_pred$mean, model = paste0('NNaive(', narfima_chn_6$p, ',', narfima_chn_6$q, ',', narfima_chn_6$size, ',', narfima_chn_6$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_chn_6 <- tbats(train_chn_6)
#tbats_er_chn_6 <-  residuals(tbats_chn_6)
#tbats_er_chn_6[is.na(tbats_er_chn_6)] <-  0
#set.seed(100)
#narfima_chn_6 <-  auto.narfima(train_chn_6, tbats_er_chn_6, p = 1, q = 1, size = 2, skip = F, xreg = train_reg_chn_6, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_chn_6_pred <-  forecast.narfima(narfima_chn_6, PI = FALSE, h = n, xreg = test_reg_chn_6)
#set.seed(100)
#model_evaluate_chn_6 <- rbind(model_evaluate_chn_6, evaluate(test_chn_6, narfima_chn_6_pred$mean, model = paste0('NTBATS(', narfima_chn_6$p, ',', narfima_chn_6$q, ',', narfima_chn_6$size, ',', narfima_chn_6$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_chn_6 <- nnetar(train_chn_6, xreg = train_reg_chn_6)
arnn_chn_6_pred <- forecast(arnn_chn_6, h = n, xreg = test_reg_chn_6)
model_evaluate_chn_6 <- rbind(model_evaluate_chn_6, evaluate(test_chn_6, arnn_chn_6_pred$mean, model = paste0('ARNNx(', arnn_chn_6$p, ',', arnn_chn_6$size , ')')))


write.csv(model_evaluate_chn_6, 'China NARFIMA Variants 6.csv', row.names = FALSE)



##################################################### China 12 #####################################################

n = 12
set.seed(100)
train_chn_12 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_12 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_12 <- reg_chn[1:length(train_chn_12),]
test_reg_chn_12 <- reg_chn[1:n,]

model_evaluate_chn_12 <- tibble() 



# 1- NARFIMA - ARFIMA Error
arfima_chn_12 <- arfima(train_chn_12, xreg = train_reg_chn_12)
arfima_er_chn_12 <-  residuals(arfima_chn_12)
arfima_er_chn_12[is.na(arfima_er_chn_12)] <-  0
set.seed(100)
narfima_chn_12 <-  auto.narfima(train_chn_12, arfima_er_chn_12, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_chn_12, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_12_pred <-  forecast.narfima(narfima_chn_12, PI = FALSE, h = n, xreg = test_reg_chn_12)
set.seed(100)
model_evaluate_chn_12 <- rbind(model_evaluate_chn_12, evaluate(test_chn_12, narfima_chn_12_pred$mean, model = paste0('NARFIMA(', narfima_chn_12$p, ',', narfima_chn_12$q, ',', narfima_chn_12$size, ',', narfima_chn_12$skip,')')))


# 2- NARIMA - ARIMA Error
arima_chn_12 <- auto.arima(train_chn_12)
arima_er_chn_12 <-  residuals(arima_chn_12)
arima_er_chn_12[is.na(arima_er_chn_12)] <-  0
set.seed(100)
narfima_chn_12 <-  auto.narfima(train_chn_12, arima_er_chn_12, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_chn_12, lambda = 0, lambdae = 0.5, repeats = 1000)
set.seed(100)
narfima_chn_12_pred <-  forecast.narfima(narfima_chn_12, PI = FALSE, h = n, xreg = test_reg_chn_12)
set.seed(100)
model_evaluate_chn_12 <- rbind(model_evaluate_chn_12, evaluate(test_chn_12, narfima_chn_12_pred$mean, model = paste0('NARIMA(', narfima_chn_12$p, ',', narfima_chn_12$q, ',', narfima_chn_12$size, ',', narfima_chn_12$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_chn_12)
ss <- AddSeasonal(ss, train_chn_12, nseasons = 12)
bsts_chn_12 <- bsts(train_chn_12 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_chn_12, train_chn_12))
bsts_er_chn_12 <-  residuals(bsts_chn_12)
bsts_er_chn_12[is.na(bsts_er_chn_12)] <-  0
set.seed(100)
narfima_chn_12 <-  auto.narfima(train_chn_12, bsts_er_chn_12, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_chn_12, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_12_pred <-  forecast.narfima(narfima_chn_12, PI = FALSE, h = n, xreg = test_reg_chn_12)
set.seed(100)
model_evaluate_chn_12 <- rbind(model_evaluate_chn_12, evaluate(test_chn_12, narfima_chn_12_pred$mean, model = paste0('NBSTS(', narfima_chn_12$p, ',', narfima_chn_12$q, ',', narfima_chn_12$size, ',', narfima_chn_12$skip,')')))


# 4- NNaive - Naive Error
naive_chn_12 <- naive(train_chn_12)
naive_er_chn_12 <-  residuals(naive_chn_12)
naive_er_chn_12[is.na(naive_er_chn_12)] <-  0
set.seed(100)
narfima_chn_12 <-  auto.narfima(train_chn_12, naive_er_chn_12, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_chn_12, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_chn_12_pred <-  forecast.narfima(narfima_chn_12, PI = FALSE, h = n, xreg = test_reg_chn_12)
set.seed(100)
model_evaluate_chn_12 <- rbind(model_evaluate_chn_12, evaluate(test_chn_12, narfima_chn_12_pred$mean, model = paste0('NNaive(', narfima_chn_12$p, ',', narfima_chn_12$q, ',', narfima_chn_12$size, ',', narfima_chn_12$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_chn_12 <- tbats(train_chn_12)
#tbats_er_chn_12 <-  residuals(tbats_chn_12)
#tbats_er_chn_12[is.na(tbats_er_chn_12)] <-  0
#set.seed(100)
#narfima_chn_12 <-  auto.narfima(train_chn_12, tbats_er_chn_12, p = 5, q = 4, size = 1, skip = T, xreg = train_reg_chn_12, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_chn_12_pred <-  forecast.narfima(narfima_chn_12, PI = FALSE, h = n, xreg = test_reg_chn_12)
#set.seed(100)
#model_evaluate_chn_12 <- rbind(model_evaluate_chn_12, evaluate(test_chn_12, narfima_chn_12_pred$mean, model = paste0('NTBATS(', narfima_chn_12$p, ',', narfima_chn_12$q, ',', narfima_chn_12$size, ',', narfima_chn_12$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_chn_12 <- nnetar(train_chn_12, xreg = train_reg_chn_12)
arnn_chn_12_pred <- forecast(arnn_chn_12, h = n, xreg = test_reg_chn_12)
model_evaluate_chn_12 <- rbind(model_evaluate_chn_12, evaluate(test_chn_12, arnn_chn_12_pred$mean, model = paste0('ARNNx(', arnn_chn_12$p, ',', arnn_chn_12$size , ')')))


write.csv(model_evaluate_chn_12, 'China NARFIMA Variants 12.csv', row.names = FALSE)



##################################################### China 24 #####################################################

n = 24
set.seed(100)
train_chn_24 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_24 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_24 <- reg_chn[1:length(train_chn_24),]
test_reg_chn_24 <- reg_chn[1:n,]

model_evaluate_chn_24 <- tibble()  



# 1- NARFIMA - ARFIMA Error
arfima_chn_24 <- arfima(train_chn_24, xreg = train_reg_chn_24)
arfima_er_chn_24 <-  residuals(arfima_chn_24)
arfima_er_chn_24[is.na(arfima_er_chn_24)] <-  0
set.seed(100)
narfima_chn_24 <-  auto.narfima(train_chn_24, arfima_er_chn_24, p = 1, q = 2, size = 4, skip = T, xreg = train_reg_chn_24, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_24_pred <-  forecast.narfima(narfima_chn_24, PI = FALSE, h = n, xreg = test_reg_chn_24)
set.seed(100)
model_evaluate_chn_24 <- rbind(model_evaluate_chn_24, evaluate(test_chn_24, narfima_chn_24_pred$mean, model = paste0('NARFIMA(', narfima_chn_24$p, ',', narfima_chn_24$q, ',', narfima_chn_24$size, ',', narfima_chn_24$skip,')')))


# 2- NARIMA - ARIMA Error
arima_chn_24 <- auto.arima(train_chn_24)
arima_er_chn_24 <-  residuals(arima_chn_24)
arima_er_chn_24[is.na(arima_er_chn_24)] <-  0
set.seed(100)
narfima_chn_24 <-  auto.narfima(train_chn_24, arima_er_chn_24, p = 1, q = 2, size = 4, skip = T, xreg = train_reg_chn_24, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_24_pred <-  forecast.narfima(narfima_chn_24, PI = FALSE, h = n, xreg = test_reg_chn_24)
set.seed(100)
model_evaluate_chn_24 <- rbind(model_evaluate_chn_24, evaluate(test_chn_24, narfima_chn_24_pred$mean, model = paste0('NARIMA(', narfima_chn_24$p, ',', narfima_chn_24$q, ',', narfima_chn_24$size, ',', narfima_chn_24$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_chn_24)
ss <- AddSeasonal(ss, train_chn_24, nseasons = 12)
bsts_chn_24 <- bsts(train_chn_24 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_chn_24, train_chn_24))
bsts_er_chn_24 <-  residuals(bsts_chn_24)
bsts_er_chn_24[is.na(bsts_er_chn_24)] <-  0
set.seed(100)
narfima_chn_24 <-  auto.narfima(train_chn_24, bsts_er_chn_24, p = 1, q = 2, size = 4, skip = T, xreg = train_reg_chn_24, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_24_pred <-  forecast.narfima(narfima_chn_24, PI = FALSE, h = n, xreg = test_reg_chn_24)
set.seed(100)
model_evaluate_chn_24 <- rbind(model_evaluate_chn_24, evaluate(test_chn_24, narfima_chn_24_pred$mean, model = paste0('NBSTS(', narfima_chn_24$p, ',', narfima_chn_24$q, ',', narfima_chn_24$size, ',', narfima_chn_24$skip,')')))


# 4- NNaive - Naive Error
naive_chn_24 <- naive(train_chn_24)
naive_er_chn_24 <-  residuals(naive_chn_24)
naive_er_chn_24[is.na(naive_er_chn_24)] <-  0
set.seed(100)
narfima_chn_24 <-  auto.narfima(train_chn_24, naive_er_chn_24, p = 1, q = 2, size = 4, skip = T, xreg = train_reg_chn_24, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_chn_24_pred <-  forecast.narfima(narfima_chn_24, PI = FALSE, h = n, xreg = test_reg_chn_24)
set.seed(100)
model_evaluate_chn_24 <- rbind(model_evaluate_chn_24, evaluate(test_chn_24, narfima_chn_24_pred$mean, model = paste0('NNaive(', narfima_chn_24$p, ',', narfima_chn_24$q, ',', narfima_chn_24$size, ',', narfima_chn_24$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_chn_24 <- tbats(train_chn_24)
#tbats_er_chn_24 <-  residuals(tbats_chn_24)
#tbats_er_chn_24[is.na(tbats_er_chn_24)] <-  0
#set.seed(100)
#narfima_chn_24 <-  auto.narfima(train_chn_24, tbats_er_chn_24, p = 1, q = 2, size = 4, skip = T, xreg = train_reg_chn_24, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_chn_24_pred <-  forecast.narfima(narfima_chn_24, PI = FALSE, h = n, xreg = test_reg_chn_24)
#set.seed(100)
#model_evaluate_chn_24 <- rbind(model_evaluate_chn_24, evaluate(test_chn_24, narfima_chn_24_pred$mean, model = paste0('NTBATS(', narfima_chn_24$p, ',', narfima_chn_24$q, ',', narfima_chn_24$size, ',', narfima_chn_24$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_chn_24 <- nnetar(train_chn_24, xreg = train_reg_chn_24)
arnn_chn_24_pred <- forecast(arnn_chn_24, h = n, xreg = test_reg_chn_24)
model_evaluate_chn_24 <- rbind(model_evaluate_chn_24, evaluate(test_chn_24, arnn_chn_24_pred$mean, model = paste0('ARNNx(', arnn_chn_24$p, ',', arnn_chn_24$size , ')')))


write.csv(model_evaluate_chn_24, 'China NARFIMA Variants 24.csv', row.names = FALSE)



##################################################### China 48 #####################################################

n = 48
set.seed(100)
train_chn_48 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_48 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_48 <- reg_chn[1:length(train_chn_48),]
test_reg_chn_48 <- reg_chn[1:n,]

model_evaluate_chn_48 <- tibble()  



# 1- NARFIMA - ARFIMA Error
arfima_chn_48 <- arfima(train_chn_48, xreg = train_reg_chn_48)
arfima_er_chn_48 <-  residuals(arfima_chn_48)
arfima_er_chn_48[is.na(arfima_er_chn_48)] <-  0
set.seed(100)
narfima_chn_48 <-  auto.narfima(train_chn_48, arfima_er_chn_48, p = 4, q = 1, size = 2, skip = T, xreg = train_reg_chn_48, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_48_pred <-  forecast.narfima(narfima_chn_48, PI = FALSE, h = n, xreg = test_reg_chn_48)
set.seed(100)
model_evaluate_chn_48 <- rbind(model_evaluate_chn_48, evaluate(test_chn_48, narfima_chn_48_pred$mean, model = paste0('NARFIMA(', narfima_chn_48$p, ',', narfima_chn_48$q, ',', narfima_chn_48$size, ',', narfima_chn_48$skip,')')))


# 2- NARIMA - ARIMA Error
arima_chn_48 <- auto.arima(train_chn_48)
arima_er_chn_48 <-  residuals(arima_chn_48)
arima_er_chn_48[is.na(arima_er_chn_48)] <-  0
set.seed(100)
narfima_chn_48 <-  auto.narfima(train_chn_48, arima_er_chn_48, p = 4, q = 1, size = 2, skip = T, xreg = train_reg_chn_48, lambda = 0, lambdae = 0.5, repeats = 1000)
set.seed(100)
narfima_chn_48_pred <-  forecast.narfima(narfima_chn_48, PI = FALSE, h = n, xreg = test_reg_chn_48)
set.seed(100)
model_evaluate_chn_48 <- rbind(model_evaluate_chn_48, evaluate(test_chn_48, narfima_chn_48_pred$mean, model = paste0('NARIMA(', narfima_chn_48$p, ',', narfima_chn_48$q, ',', narfima_chn_48$size, ',', narfima_chn_48$skip,')')))


# 3- NBSTS - BSTS Error
ss <- AddSemilocalLinearTrend(list(), train_chn_48)
ss <- AddSeasonal(ss, train_chn_48, nseasons = 12)
bsts_chn_48 <- bsts(train_chn_48 ~ ., 
                    state.specification = ss,
                    niter = 1000,
                    ping = 100,
                    seed = 100,
                    data = cbind(train_reg_chn_48, train_chn_48))
bsts_er_chn_48 <-  residuals(bsts_chn_48)
bsts_er_chn_48[is.na(bsts_er_chn_48)] <-  0
set.seed(100)
narfima_chn_48 <-  auto.narfima(train_chn_48, bsts_er_chn_48, p = 4, q = 1, size = 2, skip = T, xreg = train_reg_chn_48, lambda = 0, lambdae = 0, repeats = 1000)
set.seed(100)
narfima_chn_48_pred <-  forecast.narfima(narfima_chn_48, PI = FALSE, h = n, xreg = test_reg_chn_48)
set.seed(100)
model_evaluate_chn_48 <- rbind(model_evaluate_chn_48, evaluate(test_chn_48, narfima_chn_48_pred$mean, model = paste0('NBSTS(', narfima_chn_48$p, ',', narfima_chn_48$q, ',', narfima_chn_48$size, ',', narfima_chn_48$skip,')')))


# 4- NNaive - Naive Error
naive_chn_48 <- naive(train_chn_48)
naive_er_chn_48 <-  residuals(naive_chn_48)
naive_er_chn_48[is.na(naive_er_chn_48)] <-  0
set.seed(100)
narfima_chn_48 <-  auto.narfima(train_chn_48, naive_er_chn_48, p = 4, q = 1, size = 2, skip = T, xreg = train_reg_chn_48, lambda = 0.00001, lambdae = 0.00001, repeats = 1000)
set.seed(100)
narfima_chn_48_pred <-  forecast.narfima(narfima_chn_48, PI = FALSE, h = n, xreg = test_reg_chn_48)
set.seed(100)
model_evaluate_chn_48 <- rbind(model_evaluate_chn_48, evaluate(test_chn_48, narfima_chn_48_pred$mean, model = paste0('NNaive(', narfima_chn_48$p, ',', narfima_chn_48$q, ',', narfima_chn_48$size, ',', narfima_chn_48$skip,')')))


# 4- NTBATS - TBATS Error
#tbats_chn_48 <- tbats(train_chn_48)
#tbats_er_chn_48 <-  residuals(tbats_chn_48)
#tbats_er_chn_48[is.na(tbats_er_chn_48)] <-  0
#set.seed(100)
#narfima_chn_48 <-  auto.narfima(train_chn_48, tbats_er_chn_48, p = 4, q = 1, size = 2, skip = T, xreg = train_reg_chn_48, lambda = 0, lambdae = 0, repeats = 1000)
#set.seed(100)
#narfima_chn_48_pred <-  forecast.narfima(narfima_chn_48, PI = FALSE, h = n, xreg = test_reg_chn_48)
#set.seed(100)
#model_evaluate_chn_48 <- rbind(model_evaluate_chn_48, evaluate(test_chn_48, narfima_chn_48_pred$mean, model = paste0('NTBATS(', narfima_chn_48$p, ',', narfima_chn_48$q, ',', narfima_chn_48$size, ',', narfima_chn_48$skip,')')))


# 5- ARNNx
set.seed(100)
arnn_chn_48 <- nnetar(train_chn_48, xreg = train_reg_chn_48)
arnn_chn_48_pred <- forecast(arnn_chn_48, h = n, xreg = test_reg_chn_48)
model_evaluate_chn_48 <- rbind(model_evaluate_chn_48, evaluate(test_chn_48, arnn_chn_48_pred$mean, model = paste0('ARNNx(', arnn_chn_48$p, ',', arnn_chn_48$size , ')')))



write.csv(model_evaluate_chn_48, 'China NARFIMA Variants 48.csv', row.names = FALSE)
