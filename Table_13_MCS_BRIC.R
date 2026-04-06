library(MCS)
library(readxl)
library(dplyr)
library(stats)
library(forecast)

par(mfrow = c(1,1), mfcol = c(1,1))
par(mgp = c(3, 1, 0))
par(mar = c(5.1, 4.1, 4.1, 2.1))


##################################################### Brazil #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
data <- read_excel('Brazil_Data.xlsx') %>% rename('Exchange_Rate_braz' = spot_ER_Brazil)                 
exchange_rate_braz <- ts(data$Exchange_Rate_braz)

n = 1
test_braz_1 <- window(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1) 
n = 3
test_braz_3 <- window(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1) 
n = 6
test_braz_6 <- window(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1) 
n = 12
test_braz_12 <- window(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)  
n = 24
test_braz_24 <- window(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1) 
n = 48
test_braz_48 <- window(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1) 

setwd('Dataset/Dataset_Model_Forecasts/Brazil')
braz_1 <- read.csv('Brazil Forecast 1.csv')
braz_3 <- read.csv('Brazil Forecast 3.csv')
braz_6 <- read.csv('Brazil Forecast 6.csv')
braz_12 <- read.csv('Brazil Forecast 12.csv')
braz_24 <- read.csv('Brazil Forecast 24.csv')
braz_48 <- read.csv('Brazil Forecast 48.csv')

# Combine forecasts
braz_forecast_list <- list(braz_1, braz_3, braz_6, braz_12, braz_24, braz_48)

# Make column names consistent
for(i in 2:length(braz_forecast_list)) {
  colnames(braz_forecast_list[[i]]) <- colnames(braz_forecast_list[[1]])
}

# Remove numbers and dots from column names
colnames(braz_forecast_list[[1]]) <- gsub("[0-9\\.]+", "", colnames(braz_forecast_list[[1]]))
for(i in 2:length(braz_forecast_list)) {
  colnames(braz_forecast_list[[i]]) <- colnames(braz_forecast_list[[1]])
}

braz_forecast_all <- do.call(rbind, braz_forecast_list)[-1]

# Combine test series
test_braz_list <- list(test_braz_1, test_braz_3, test_braz_6, test_braz_12, test_braz_24, test_braz_48)
test_braz_all <- unlist(test_braz_list)

# Compute squared errors
squared_errors_braz <- (as.matrix(braz_forecast_all) - matrix(test_braz_all, nrow = length(test_braz_all), ncol = ncol(braz_forecast_all), byrow = FALSE))^2

# MCS
mcs_result_braz <- MCSprocedure(squared_errors_braz, statistic = "TR", alpha = 0.05, B = 5000, seed = 100)
mcs_result_braz


##################################################### Russia #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
data <- read_excel('Russia_Data.xlsx') %>% rename('Exchange_Rate_rus' = spot_ER_Russia)                   
exchange_rate_rus <- ts(data$Exchange_Rate_rus)

n = 1
test_rus_1 <- window(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1) 
n = 3
test_rus_3 <- window(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1) 
n = 6
test_rus_6 <- window(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1) 
n = 12
test_rus_12 <- window(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1)  
n = 24
test_rus_24 <- window(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1) 
n = 48
test_rus_48 <- window(exchange_rate_rus, start = length(exchange_rate_rus) - n + 1) 


setwd('Dataset/Dataset_Model_Forecasts/Russia')
rus_1 <- read.csv('Russia Forecast 1.csv')
rus_3 <- read.csv('Russia Forecast 3.csv')
rus_6 <- read.csv('Russia Forecast 6.csv')
rus_12 <- read.csv('Russia Forecast 12.csv')
rus_24 <- read.csv('Russia Forecast 24.csv')
rus_48 <- read.csv('Russia Forecast 48.csv')

# Combine forecasts
rus_forecast_list <- list(rus_1, rus_3, rus_6, rus_12, rus_24, rus_48)

# Make column names consistent
for(i in 2:length(rus_forecast_list)) {
  colnames(rus_forecast_list[[i]]) <- colnames(rus_forecast_list[[1]])
}

# Remove numbers and dots from column names
colnames(rus_forecast_list[[1]]) <- gsub("[0-9\\.]+", "", colnames(rus_forecast_list[[1]]))
for(i in 2:length(rus_forecast_list)) {
  colnames(rus_forecast_list[[i]]) <- colnames(rus_forecast_list[[1]])
}

rus_forecast_all <- do.call(rbind, rus_forecast_list)[-1]

# Combine test series
test_rus_list <- list(test_rus_1, test_rus_3, test_rus_6, test_rus_12, test_rus_24, test_rus_48)
test_rus_all <- unlist(test_rus_list)

# Compute squared errors
squared_errors_rus <- (as.matrix(rus_forecast_all) - matrix(test_rus_all, nrow = length(test_rus_all), ncol = ncol(rus_forecast_all), byrow = FALSE))^2

# MCS
mcs_result_rus <- MCSprocedure(squared_errors_rus, statistic = "TR", B = 5000, seed = 100)
mcs_result_rus



##################################################### India #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
data <- read_excel('India_Data.xlsx') %>% rename('Exchange_Rate_ind' = spot_ER_India)                   
exchange_rate_ind <- ts(data$Exchange_Rate_ind)

n = 1
test_ind_1 <- window(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1) 
n = 3
test_ind_3 <- window(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1) 
n = 6
test_ind_6 <- window(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1) 
n = 12
test_ind_12 <- window(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1)  
n = 24
test_ind_24 <- window(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1) 
n = 48
test_ind_48 <- window(exchange_rate_ind, start = length(exchange_rate_ind) - n + 1) 


setwd('Dataset/Dataset_Model_Forecasts/India')
ind_1 <- read.csv('India Forecast 1.csv')
ind_3 <- read.csv('India Forecast 3.csv')
ind_6 <- read.csv('India Forecast 6.csv')
ind_12 <- read.csv('India Forecast 12.csv')
ind_24 <- read.csv('India Forecast 24.csv')
ind_48 <- read.csv('India Forecast 48.csv')

# Combine forecasts
ind_forecast_list <- list(ind_1, ind_3, ind_6, ind_12, ind_24, ind_48)

# Make column names consistent
for(i in 2:length(ind_forecast_list)) {
  colnames(ind_forecast_list[[i]]) <- colnames(ind_forecast_list[[1]])
}

# Remove numbers and dots from column names
colnames(ind_forecast_list[[1]]) <- gsub("[0-9\\.]+", "", colnames(ind_forecast_list[[1]]))
for(i in 2:length(ind_forecast_list)) {
  colnames(ind_forecast_list[[i]]) <- colnames(ind_forecast_list[[1]])
}

ind_forecast_all <- do.call(rbind, ind_forecast_list)[-1]

# Combine test series
test_ind_list <- list(test_ind_1, test_ind_3, test_ind_6, test_ind_12, test_ind_24, test_ind_48)
test_ind_all <- unlist(test_ind_list)

# Compute squared errors
squared_errors_ind <- (as.matrix(ind_forecast_all) - matrix(test_ind_all, nrow = length(test_ind_all), ncol = ncol(ind_forecast_all), byrow = FALSE))^2

# MCS
mcs_result_ind <- MCSprocedure(squared_errors_ind, statistic = "TR", B = 5000, seed = 100)
mcs_result_ind


##################################################### China #####################################################

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
data <- read_excel('China_Data.xlsx') %>% rename('Exchange_Rate_chn' = spot_ER_China)                   
exchange_rate_chn <- ts(data$Exchange_Rate_chn)

n = 1
test_chn_1 <- window(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1) 
n = 3
test_chn_3 <- window(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1) 
n = 6
test_chn_6 <- window(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1) 
n = 12
test_chn_12 <- window(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1)  
n = 24
test_chn_24 <- window(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1) 
n = 48
test_chn_48 <- window(exchange_rate_chn, start = length(exchange_rate_chn) - n + 1) 


setwd('Dataset/Dataset_Model_Forecasts/China')
chn_1 <- read.csv('China Forecast 1.csv')
chn_3 <- read.csv('China Forecast 3.csv')
chn_6 <- read.csv('China Forecast 6.csv')
chn_12 <- read.csv('China Forecast 12.csv')
chn_24 <- read.csv('China Forecast 24.csv')
chn_48 <- read.csv('China Forecast 48.csv')

# Combine forecasts
chn_forecast_list <- list(chn_1, chn_3, chn_6, chn_12, chn_24, chn_48)

# Make column names consistent
for(i in 2:length(chn_forecast_list)) {
  colnames(chn_forecast_list[[i]]) <- colnames(chn_forecast_list[[1]])
}

# Remove numbers and dots from column names
colnames(chn_forecast_list[[1]]) <- gsub("[0-9\\.]+", "", colnames(chn_forecast_list[[1]]))
for(i in 2:length(chn_forecast_list)) {
  colnames(chn_forecast_list[[i]]) <- colnames(chn_forecast_list[[1]])
}

chn_forecast_all <- do.call(rbind, chn_forecast_list)[-1]

# Combine test series
test_chn_list <- list(test_chn_1, test_chn_3, test_chn_6, test_chn_12, test_chn_24, test_chn_48)
test_chn_all <- unlist(test_chn_list)

# Compute squared errors
squared_errors_chn <- (as.matrix(chn_forecast_all) - matrix(test_chn_all, nrow = length(test_chn_all), ncol = ncol(chn_forecast_all), byrow = FALSE))^2

# MCS
mcs_result_chn <- MCSprocedure(squared_errors_chn, statistic = "TR", alpha = 0.05, B = 5000, seed = 100)
mcs_result_chn
