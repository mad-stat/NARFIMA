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
library(nonlinearTseries)


##################################################### Non-Linearity Function #####################################################


nonlinearity_tests_summary <- function(data) {
  
  if (!is.numeric(data)){
    stop("Data must be numeric")
  } 
  
  if(any(is.na(data))){
    data <- na.approx(data)
  }
  
  # Teraesvirta Neural Network Test for Nonlinearity
  if(terasvirta.test(data)$p.value < 0.05){
    terasvirta_nonlinearity_value <- 'Non-linear Residuals'
  }else{
    terasvirta_nonlinearity_value <- 'Linear Residuals'
  }
  
  
  # Brock, Dechert, and Scheinkman
  if(bds.test(data)$p.value[[1]] < 0.05){
    bds_nonlinearity_value <- 'Non-linear Residuals'
  }else{
    bds_nonlinearity_value <- 'Linear Residuals'
  }
  
  
  # Create a tibble with the results
  result <- tibble(
    Terasvirta_Test_p.value = terasvirta.test(data)$p.value,
    Terasvirta_Test = terasvirta_nonlinearity_value,
    BDS_Test_p.value = bds.test(data)$p.value[[1]],
    BDS_Test = bds_nonlinearity_value
  )
  
  return(result)
}




##################################################### Brazil #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('Brazil_Data.xlsx') %>% rename('Exchange_Rate_braz' = spot_ER_Brazil)                   
exchange_rate_braz <- ts(data$Exchange_Rate_braz)
reg_braz <- as.matrix(data[,c(3,4,5,6,7)], ncol = 5)

n = 1
set.seed(100)
train_braz_1 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_1 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_1 <- reg_braz[1:length(train_braz_1),]
test_reg_braz_1 <- matrix(c(reg_braz[1,1], reg_braz[1,2], reg_braz[1,3], reg_braz[1,4], reg_braz[1,5]), ncol = 5, nrow = 1)
colnames(test_reg_braz_1) <- c("SR_Interest_rate_diff_B_U", "Oil_price_growth_rate_WTI", "global_EPU(PPP)", "US_EMV" ,"US_MPU")


arfima_braz_1 <- arfima(train_braz_1, xreg = train_reg_braz_1)
arfima_braz_1_pred <- forecast(arfima_braz_1, h = n, xreg = test_reg_braz_1)
arfima_er_braz_1 <-  residuals(arfima_braz_1)
arfima_er_braz_1[is.na(arfima_er_braz_1)] <-  0

summ <- tibble()
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_braz_1))



n = 3
set.seed(100)
train_braz_3 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_3 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_3 <- reg_braz[1:length(train_braz_3),]
test_reg_braz_3 <- reg_braz[1:n,]

arfima_braz_3 <- arfima(train_braz_3, xreg = train_reg_braz_3)
arfima_braz_3_pred <- forecast(arfima_braz_3, h = n, xreg = test_reg_braz_3)
arfima_er_braz_3 <-  residuals(arfima_braz_3)
arfima_er_braz_3[is.na(arfima_er_braz_3)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_braz_3))



n = 6
set.seed(100)
train_braz_6 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_6 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_6 <- reg_braz[1:length(train_braz_6),]
test_reg_braz_6 <- reg_braz[1:n,]

arfima_braz_6 <- arfima(train_braz_6, xreg = train_reg_braz_6)
arfima_braz_6_pred <- forecast(arfima_braz_6, h = n, xreg = test_reg_braz_6)
arfima_er_braz_6 <-  residuals(arfima_braz_6)
arfima_er_braz_6[is.na(arfima_er_braz_6)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_braz_6))




n = 12
set.seed(100)
train_braz_12 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_12 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_12 <- reg_braz[1:length(train_braz_12),]
test_reg_braz_12 <- reg_braz[1:n,]

arfima_braz_12 <- arfima(train_braz_12, xreg = train_reg_braz_12)
arfima_braz_12_pred <- forecast(arfima_braz_12, h = n, xreg = test_reg_braz_12)
arfima_er_braz_12 <-  residuals(arfima_braz_12)
arfima_er_braz_12[is.na(arfima_er_braz_12)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_braz_12))




n = 24
set.seed(100)
train_braz_24 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_24 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_24 <- reg_braz[1:length(train_braz_24),]
test_reg_braz_24 <- reg_braz[1:n,]

arfima_braz_24 <- arfima(train_braz_24, xreg = train_reg_braz_24)
arfima_braz_24_pred <- forecast(arfima_braz_24, h = n, xreg = test_reg_braz_24)
arfima_er_braz_24 <-  residuals(arfima_braz_24)
arfima_er_braz_24[is.na(arfima_er_braz_24)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_braz_24))


n = 48
set.seed(100)
train_braz_48 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_48 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_48 <- reg_braz[1:length(train_braz_48),]
test_reg_braz_48 <- reg_braz[1:n,]

arfima_braz_48 <- arfima(train_braz_48, xreg = train_reg_braz_48)
arfima_braz_48_pred <- forecast(arfima_braz_48, h = n, xreg = test_reg_braz_48)
arfima_er_braz_48 <-  residuals(arfima_braz_48)
arfima_er_braz_48[is.na(arfima_er_braz_48)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_braz_48))



##################################################### Russia #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('Russia_Data.xlsx') %>% rename('Exchange_Rate_rus' = spot_ER_Russia)                   
exchange_rate_rus <- ts(data$Exchange_Rate_rus)
reg_rus <- as.matrix(data[,c(3,4,5,6,7)], ncol = 5)

n = 1
set.seed(100)
train_rus_1 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_1 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_1 <- reg_rus[1:length(train_rus_1),]
test_reg_rus_1 <- matrix(c(reg_rus[1,1], reg_rus[1,2], reg_rus[1,3], reg_rus[1,4], reg_rus[1,5]), ncol = 5, nrow = 1)
colnames(test_reg_rus_1) <- c("Oil_price_growth_rate_WTI", "SR_Interest_rate_diff_R_U", "global_EPU(PPP)", "US_EMV" ,"US_MPU")


arfima_rus_1 <- arfima(train_rus_1, xreg = train_reg_rus_1)
arfima_rus_1_pred <- forecast(arfima_rus_1, h = n, xreg = test_reg_rus_1)
arfima_er_rus_1 <-  residuals(arfima_rus_1)
arfima_er_rus_1[is.na(arfima_er_rus_1)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_rus_1))



n = 3
set.seed(100)
train_rus_3 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_3 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_3 <- reg_rus[1:length(train_rus_3),]
test_reg_rus_3 <- reg_rus[1:n,]

arfima_rus_3 <- arfima(train_rus_3, xreg = train_reg_rus_3)
arfima_rus_3_pred <- forecast(arfima_rus_3, h = n, xreg = test_reg_rus_3)
arfima_er_rus_3 <-  residuals(arfima_rus_3)
arfima_er_rus_3[is.na(arfima_er_rus_3)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_rus_3))



n = 6
set.seed(100)
train_rus_6 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_6 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_6 <- reg_rus[1:length(train_rus_6),]
test_reg_rus_6 <- reg_rus[1:n,]

arfima_rus_6 <- arfima(train_rus_6, xreg = train_reg_rus_6)
arfima_rus_6_pred <- forecast(arfima_rus_6, h = n, xreg = test_reg_rus_6)
arfima_er_rus_6 <-  residuals(arfima_rus_6)
arfima_er_rus_6[is.na(arfima_er_rus_6)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_rus_6))




n = 12
set.seed(100)
train_rus_12 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_12 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_12 <- reg_rus[1:length(train_rus_12),]
test_reg_rus_12 <- reg_rus[1:n,]

arfima_rus_12 <- arfima(train_rus_12, xreg = train_reg_rus_12)
arfima_rus_12_pred <- forecast(arfima_rus_12, h = n, xreg = test_reg_rus_12)
arfima_er_rus_12 <-  residuals(arfima_rus_12)
arfima_er_rus_12[is.na(arfima_er_rus_12)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_rus_12))




n = 24
set.seed(100)
train_rus_24 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_24 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_24 <- reg_rus[1:length(train_rus_24),]
test_reg_rus_24 <- reg_rus[1:n,]

arfima_rus_24 <- arfima(train_rus_24, xreg = train_reg_rus_24)
arfima_rus_24_pred <- forecast(arfima_rus_24, h = n, xreg = test_reg_rus_24)
arfima_er_rus_24 <-  residuals(arfima_rus_24)
arfima_er_rus_24[is.na(arfima_er_rus_24)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_rus_24))



n = 48
set.seed(100)
train_rus_48 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
test_rus_48 <- subset(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)
train_reg_rus_48 <- reg_rus[1:length(train_rus_48),]
test_reg_rus_48 <- reg_rus[1:n,]

arfima_rus_48 <- arfima(train_rus_48, xreg = train_reg_rus_48)
arfima_rus_48_pred <- forecast(arfima_rus_48, h = n, xreg = test_reg_rus_48)
arfima_er_rus_48 <-  residuals(arfima_rus_48)
arfima_er_rus_48[is.na(arfima_er_rus_48)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_rus_48))



##################################################### India #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('India_Data.xlsx') %>% rename('Exchange_Rate_ind' = spot_ER_India)                   
exchange_rate_ind <- ts(data$Exchange_Rate_ind)
reg_ind <- as.matrix(data[,c(3,4,5,6,7)], ncol = 5)

n = 1
set.seed(100)
train_ind_1 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_1 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_1 <- reg_ind[1:length(train_ind_1),]
test_reg_ind_1 <- matrix(c(reg_ind[1,1], reg_ind[1,2], reg_ind[1,3], reg_ind[1,4], reg_ind[1,5]), ncol = 5, nrow = 1)
colnames(test_reg_ind_1) <- c("Oil_price_growth_rate_WTI", "SR_Interest_rate_diff_I_U", "global_EPU(PPP)", "US_EMV" ,"US_MPU")


arfima_ind_1 <- arfima(train_ind_1, xreg = train_reg_ind_1)
arfima_ind_1_pred <- forecast(arfima_ind_1, h = n, xreg = test_reg_ind_1)
arfima_er_ind_1 <-  residuals(arfima_ind_1)
arfima_er_ind_1[is.na(arfima_er_ind_1)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_ind_1))



n = 3
set.seed(100)
train_ind_3 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_3 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_3 <- reg_ind[1:length(train_ind_3),]
test_reg_ind_3 <- reg_ind[1:n,]

arfima_ind_3 <- arfima(train_ind_3, xreg = train_reg_ind_3)
arfima_ind_3_pred <- forecast(arfima_ind_3, h = n, xreg = test_reg_ind_3)
arfima_er_ind_3 <-  residuals(arfima_ind_3)
arfima_er_ind_3[is.na(arfima_er_ind_3)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_ind_3))



n = 6
set.seed(100)
train_ind_6 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_6 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_6 <- reg_ind[1:length(train_ind_6),]
test_reg_ind_6 <- reg_ind[1:n,]

arfima_ind_6 <- arfima(train_ind_6, xreg = train_reg_ind_6)
arfima_ind_6_pred <- forecast(arfima_ind_6, h = n, xreg = test_reg_ind_6)
arfima_er_ind_6 <-  residuals(arfima_ind_6)
arfima_er_ind_6[is.na(arfima_er_ind_6)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_ind_6))




n = 12
set.seed(100)
train_ind_12 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_12 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_12 <- reg_ind[1:length(train_ind_12),]
test_reg_ind_12 <- reg_ind[1:n,]

arfima_ind_12 <- arfima(train_ind_12, xreg = train_reg_ind_12)
arfima_ind_12_pred <- forecast(arfima_ind_12, h = n, xreg = test_reg_ind_12)
arfima_er_ind_12 <-  residuals(arfima_ind_12)
arfima_er_ind_12[is.na(arfima_er_ind_12)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_ind_12))




n = 24
set.seed(100)
train_ind_24 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_24 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_24 <- reg_ind[1:length(train_ind_24),]
test_reg_ind_24 <- reg_ind[1:n,]

arfima_ind_24 <- arfima(train_ind_24, xreg = train_reg_ind_24)
arfima_ind_24_pred <- forecast(arfima_ind_24, h = n, xreg = test_reg_ind_24)
arfima_er_ind_24 <-  residuals(arfima_ind_24)
arfima_er_ind_24[is.na(arfima_er_ind_24)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_ind_24))


n = 48
set.seed(100)
train_ind_48 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
test_ind_48 <- subset(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)
train_reg_ind_48 <- reg_ind[1:length(train_ind_48),]
test_reg_ind_48 <- reg_ind[1:n,]

arfima_ind_48 <- arfima(train_ind_48, xreg = train_reg_ind_48)
arfima_ind_48_pred <- forecast(arfima_ind_48, h = n, xreg = test_reg_ind_48)
arfima_er_ind_48 <-  residuals(arfima_ind_48)
arfima_er_ind_48[is.na(arfima_er_ind_48)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_ind_48))



##################################################### China #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()
data <- read_excel('China_Data.xlsx') %>% rename('Exchange_Rate_chn' = spot_ER_China)                   
exchange_rate_chn <- ts(data$Exchange_Rate_chn)
reg_chn <- as.matrix(data[,c(3,4,5,6,7)], ncol = 5)

n = 1
set.seed(100)
train_chn_1 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_1 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_1 <- reg_chn[1:length(train_chn_1),]
test_reg_chn_1 <- matrix(c(reg_chn[1,1], reg_chn[1,2], reg_chn[1,3], reg_chn[1,4], reg_chn[1,5]), ncol = 5, nrow = 1)
colnames(test_reg_chn_1) <- c("Oil_price_growth_rate_WTI", "SR_Interest_rate_diff_C_U", "global_EPU(PPP)", "US_EMV" ,"US_MPU")


arfima_chn_1 <- arfima(train_chn_1, xreg = train_reg_chn_1)
arfima_chn_1_pred <- forecast(arfima_chn_1, h = n, xreg = test_reg_chn_1)
arfima_er_chn_1 <-  residuals(arfima_chn_1)
arfima_er_chn_1[is.na(arfima_er_chn_1)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_chn_1))



n = 3
set.seed(100)
train_chn_3 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_3 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_3 <- reg_chn[1:length(train_chn_3),]
test_reg_chn_3 <- reg_chn[1:n,]

arfima_chn_3 <- arfima(train_chn_3, xreg = train_reg_chn_3)
arfima_chn_3_pred <- forecast(arfima_chn_3, h = n, xreg = test_reg_chn_3)
arfima_er_chn_3 <-  residuals(arfima_chn_3)
arfima_er_chn_3[is.na(arfima_er_chn_3)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_chn_3))



n = 6
set.seed(100)
train_chn_6 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_6 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_6 <- reg_chn[1:length(train_chn_6),]
test_reg_chn_6 <- reg_chn[1:n,]

arfima_chn_6 <- arfima(train_chn_6, xreg = train_reg_chn_6)
arfima_chn_6_pred <- forecast(arfima_chn_6, h = n, xreg = test_reg_chn_6)
arfima_er_chn_6 <-  residuals(arfima_chn_6)
arfima_er_chn_6[is.na(arfima_er_chn_6)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_chn_6))




n = 12
set.seed(100)
train_chn_12 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_12 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_12 <- reg_chn[1:length(train_chn_12),]
test_reg_chn_12 <- reg_chn[1:n,]

arfima_chn_12 <- arfima(train_chn_12, xreg = train_reg_chn_12)
arfima_chn_12_pred <- forecast(arfima_chn_12, h = n, xreg = test_reg_chn_12)
arfima_er_chn_12 <-  residuals(arfima_chn_12)
arfima_er_chn_12[is.na(arfima_er_chn_12)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_chn_12))




n = 24
set.seed(100)
train_chn_24 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_24 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_24 <- reg_chn[1:length(train_chn_24),]
test_reg_chn_24 <- reg_chn[1:n,]

arfima_chn_24 <- arfima(train_chn_24, xreg = train_reg_chn_24)
arfima_chn_24_pred <- forecast(arfima_chn_24, h = n, xreg = test_reg_chn_24)
arfima_er_chn_24 <-  residuals(arfima_chn_24)
arfima_er_chn_24[is.na(arfima_er_chn_24)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_chn_24))



n = 48
set.seed(100)
train_chn_48 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
test_chn_48 <- subset(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)
train_reg_chn_48 <- reg_chn[1:length(train_chn_48),]
test_reg_chn_48 <- reg_chn[1:n,]

arfima_chn_48 <- arfima(train_chn_48, xreg = train_reg_chn_48)
arfima_chn_48_pred <- forecast(arfima_chn_48, h = n, xreg = test_reg_chn_48)
arfima_er_chn_48 <-  residuals(arfima_chn_48)
arfima_er_chn_48[is.na(arfima_er_chn_48)] <-  0
summ <- rbind(summ,nonlinearity_tests_summary(arfima_er_chn_48))
