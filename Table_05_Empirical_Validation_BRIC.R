library(forecast)
library(tidyverse)
library(Metrics)
library(tsDyn)
library(tseries)
library(bsts)
library(readxl)

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()

##################################################### Brazil 48 #####################################################

data <- read_excel('Brazil_Data.xlsx') %>% rename('Exchange_Rate_braz' = spot_ER_Brazil)                   
exchange_rate_braz <- ts(data$Exchange_Rate_braz)
reg_braz <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)

n = 48
set.seed(100)
train_braz_48 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
train_reg_braz_48 <- reg_braz[1:length(train_braz_48),]

set.seed(100)
arfima_braz_48 <- arfima(train_braz_48, xreg = train_reg_braz_48)
arfima_er_braz_48 <-  residuals(arfima_braz_48)
arfima_er_braz_48[is.na(arfima_er_braz_48)] <-  0

set.seed(100)
narfimaT_braz_48 <-  auto_narfima(train_braz_48, arfima_er_braz_48, p = 4, q = 2, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_braz_48)

# AR skip weights
sy = narfimaT_braz_48$model[[1]]$wts[14:17])
View(as.data.frame(sy)
# Error skip weights
ser = narfimaT_braz_48$model[[1]]$wts[18:19])
View(as.data.frame(ser)
# Assumption 3
sum(sy) + sum(ser)     
# Assumption 5
abs(sum(sy))


##################################################### Russia 12 #####################################################

data <- read_excel('Russia_Data.xlsx') %>% rename('Exchange_Rate_rus' = spot_ER_Russia)                   
exchange_rate_rus <- ts(data$Exchange_Rate_rus)
reg_rus <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)
     
n = 12
set.seed(100)
train_rus_12 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
train_reg_rus_12 <- reg_rus[1:length(train_rus_12),]

set.seed(100)
arfima_rus_12 <- arfima(train_rus_12, xreg = train_reg_rus_12)
arfima_er_rus_12 <-  residuals(arfima_rus_12)
arfima_er_rus_12[is.na(arfima_er_rus_12)] <-  0

set.seed(100)
narfimaT_rus_12 <-  auto_narfima(train_rus_12, arfima_er_rus_12, p = 5, q = 2, size = 5, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_rus_12)

# AR skip weights
sy = narfimaT_rus_12$model[[1]]$wts[71:75]
View(as.data.frame(sy))
# Error skip weights
ser = narfimaT_rus_12$model[[1]]$wts[76:77]
View(as.data.frame(ser))
# Assumption 3
sum(sy) + sum(ser)
# Assumption 5
abs(sum(sy))

     
##################################################### Russia 24 #####################################################
     
n = 24
set.seed(100)
train_rus_24 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
train_reg_rus_24 <- reg_rus[1:length(train_rus_24),]
     
set.seed(100)
arfima_rus_24 <- arfima(train_rus_24, xreg = train_reg_rus_24)
arfima_er_rus_24 <-  residuals(arfima_rus_24)
arfima_er_rus_24[is.na(arfima_er_rus_24)] <-  0

set.seed(100)
narfimaT_rus_24 <-  auto_narfima(train_rus_24, arfima_er_rus_24, p = 1, q = 1, size = 5, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_rus_24)

# AR skip weights
sy = narfimaT_rus_24$model[[1]]$wts[46]
View(as.data.frame(sy))
# Error skip weights
ser = narfimaT_rus_24$model[[1]]$wts[47]
View(as.data.frame(ser))
# Assumption 3
sum(sy) + sum(ser)
# Assumption 5
abs(sum(sy))


##################################################### India 12 #####################################################

data <- read_excel('India_Data.xlsx') %>% rename('Exchange_Rate_ind' = spot_ER_India)                   
exchange_rate_ind <- ts(data$Exchange_Rate_ind)
reg_ind <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5) 

n = 12
set.seed(100)
train_ind_12 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
train_reg_ind_12 <- reg_ind[1:length(train_ind_12),]

set.seed(100)
arfima_ind_12 <- arfima(train_ind_12, xreg = train_reg_ind_12)
arfima_er_ind_12 <-  residuals(arfima_ind_12)
arfima_er_ind_12[is.na(arfima_er_ind_12)] <-  0

set.seed(100)
narfimaT_ind_12 <-  auto_narfima(train_ind_12, arfima_er_ind_12, p = 1, q = 3, size = 4, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_ind_12)

# AR skip weights
sy = narfimaT_ind_12$model[[1]]$wts[45]
View(as.data.frame(sy))
# Error skip weights
ser = narfimaT_ind_12$model[[1]]$wts[46:48]
View(as.data.frame(ser))
# Assumption 3
sum(sy) + sum(ser)
# Assumption 5
abs(sum(sy))     


##################################################### India 24 #####################################################

n = 24
set.seed(100)
train_ind_24 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
train_reg_ind_24 <- reg_ind[1:length(train_ind_24),]    

set.seed(100)
arfima_ind_24 <- arfima(train_ind_24, xreg = train_reg_ind_24)
arfima_er_ind_24 <-  residuals(arfima_ind_24)
arfima_er_ind_24[is.na(arfima_er_ind_24)] <-  0

set.seed(100)
narfimaT_ind_24 <-  auto_narfima(train_ind_24, arfima_er_ind_24, p = 5, q = 4, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_ind_24)
     
# AR skip weights
sy = narfimaT_ind_24$model[[1]]$wts[17:21]
View(as.data.frame(sy))
# Error skip weights
ser = narfimaT_ind_24$model[[1]]$wts[22:25]
View(as.data.frame(ser))
# Assumption 3
sum(sy) + sum(ser)
# Assumption 5
abs(sum(sy))

     
##################################################### India 48 #####################################################

n = 48
set.seed(100)
train_ind_48 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n)
train_reg_ind_48 <- reg_ind[1:length(train_ind_48),]

set.seed(100)
arfima_ind_48 <- arfima(train_ind_48, xreg = train_reg_ind_48)
arfima_er_ind_48 <-  residuals(arfima_ind_48)
arfima_er_ind_48[is.na(arfima_er_ind_48)] <-  0

set.seed(100)
narfimaT_ind_48 <-  auto_narfima(train_ind_48, arfima_er_ind_48, p = 2, q = 4, size = 4, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_ind_48)

# AR skip weights
sy = narfimaT_ind_48$model[[1]]$wts[53:54]
View(as.data.frame(sy))
# Error skip weights
ser = narfimaT_ind_48$model[[1]]$wts[55:58]
View(as.data.frame(ser))
# Assumption 3
sum(sy) + sum(ser)
# Assumption 5
abs(sum(sy))

     
##################################################### China 12 #####################################################
     
data <- read_excel('China_Data.xlsx') %>% rename('Exchange_Rate_chn' = spot_ER_China)                   
exchange_rate_chn <- ts(data$Exchange_Rate_chn)
reg_chn <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)

n = 12
set.seed(100)
train_chn_12 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
train_reg_chn_12 <- reg_chn[1:length(train_chn_12),]   

set.seed(100)
arfima_chn_12 <- arfima(train_chn_12, xreg = train_reg_chn_12)
arfima_er_chn_12 <-  residuals(arfima_chn_12)
arfima_er_chn_12[is.na(arfima_er_chn_12)] <-  0

set.seed(100)
narfimaT_chn_12 <-  auto_narfima(train_chn_12, arfima_er_chn_12, p = 5, q = 4, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_chn_12)

# AR skip weights
sy = narfimaT_chn_12$model[[1]]$wts[17:21]
View(as.data.frame(sy))
# Error skip weights
ser = narfimaT_chn_12$model[[1]]$wts[22:25]
View(as.data.frame(ser))
# Assumption 3
sum(sy) + sum(ser)
# Assumption 5
abs(sum(sy))

     
##################################################### China 24 #####################################################

n = 24
set.seed(100)
train_chn_24 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
train_reg_chn_24 <- reg_chn[1:length(train_chn_24),]

set.seed(100)
arfima_chn_24 <- arfima(train_chn_24, xreg = train_reg_chn_24)
arfima_er_chn_24 <-  residuals(arfima_chn_24)
arfima_er_chn_24[is.na(arfima_er_chn_24)] <-  0

set.seed(100)
narfimaT_chn_24 <-  auto_narfima(train_chn_24, arfima_er_chn_24, p = 1, q = 2, size = 4, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_chn_24)

# AR skip weights
sy = narfimaT_chn_24$model[[1]]$wts[41]
View(as.data.frame(sy))
# Error skip weights
ser = narfimaT_chn_24$model[[1]]$wts[42:43]
View(as.data.frame(ser))
# Assumption 3
sum(sy) + sum(ser)
# Assumption 5
abs(sum(sy))


##################################################### China 48 #####################################################

n = 48
set.seed(100)
train_chn_48 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
train_reg_chn_48 <- reg_chn[1:length(train_chn_48),]

set.seed(100)
arfima_chn_48 <- arfima(train_chn_48, xreg = train_reg_chn_48)
arfima_er_chn_48 <-  residuals(arfima_chn_48)
arfima_er_chn_48[is.na(arfima_er_chn_48)] <-  0

set.seed(100)
narfimaT_chn_48 <-  auto_narfima(train_chn_48, arfima_er_chn_48, p = 4, q = 1, size = 2, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_chn_48)

# AR skip weights
sy = narfimaT_chn_48$model[[1]]$wts[25:28]
View(as.data.frame(sy))
# Error skip weights
ser = narfimaT_chn_48$model[[1]]$wts[29]
View(as.data.frame(ser))
# Assumption 3
sum(sy) + sum(ser)
# Assumption 5
abs(sum(sy))
