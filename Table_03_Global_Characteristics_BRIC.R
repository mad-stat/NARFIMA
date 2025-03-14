############################# Table 03: Global Characteristics - BRIC #############################
 
# For reproducibility, we are using this seed value
set.seed(100) 
 
# Load the necessary libraries
library(tidyverse)
library(readxl)
library(pracma)         
library(e1071)        
library(nonlinearTseries)  
library(seastests) 
library(tseries)
library(car)

 
##############################################################################################
# This function performs key statistical tests on a time series:
# 1. Skewness & Kurtosis: Measures asymmetry and tail heaviness of the distribution.
# 2. Non-Linearity Test: Determines if the series exhibits non-linear patterns.
# 3. Seasonality Test: Checks if the series follows a seasonal pattern.
# 4. Stationarity Test (KPSS): Identifies whether the series is stationary or not.
# 5. Long-Range Dependence (Hurst Exponent): Determines if the time series exhibits long-range dependence.  
 
statistical_tests_summary <- function(data) {
  
  if (!is.numeric(data)){
    stop("Data must be numeric")
  } 
  
  if(any(is.na(data))){
    data <- na.approx(data)
  }
  
  skewness_value <- skewness(data)
  kurtosis_value <- kurtosis(data)
  
  # Non-Linearity
  if(is.nan(nonlinearityTest(data, verbose = TRUE)$Tsay$p.value)){
    if(nonlinearityTest(data, verbose = TRUE)$Keenan$p.value <= 0.05){
      nonlinearity_value <- 'Non-linear'
    }else{
      nonlinearity_value <- 'Linear'
    }
  }else{
    if(nonlinearityTest(data, verbose = TRUE)$Tsay$p.value <= 0.05){
      nonlinearity_value <- 'Non-linear'
    }else{
      nonlinearity_value <- 'Linear'
    }
  }
  
  
  # Seasonality
  if(isSeasonal(data, test = "combined", freq = 12) == TRUE){
    seasonality_value <- 'Seasonal'
  } else{
    seasonality_value <- 'Non-seasonal'
  }
  
  
  # Stationarity
  if(kpss.test(data)$p.value > 0.05){
    stationarity_value <- 'Startionary'
  } else{
    stationarity_value <- 'Non-stationary'
  } 
  
  
  # Long-Range Dependence
  hurst_Hs_value <- hurstexp(data)[1]
  hurst_Hrs_value <- hurstexp(data)[2]
  hurst_He_value <- hurstexp(data)[3]
  hurst_Hal_value <- hurstexp(data)[4]
  hurst_Ht_value <- hurstexp(data)[5]
  
  
  # Create a tibble with the results
  result <- tibble(
    Skewness = skewness_value,
    Kurtosis = kurtosis_value,
    NonLinearity = nonlinearity_value,
    Seasonality = seasonality_value,
    Stationarity = stationarity_value,
    LongRangeDependence_Hs = hurst_Hs_value,
    LongRangeDependence_Hrs = hurst_Hrs_value,
    LongRangeDependence_He = hurst_He_value,
    LongRangeDependence_Hal = hurst_Hal_value,
    LongRangeDependence_Ht = hurst_Ht_value,
  )
  
  return(result)
}
 
##############################################################################################
 
 
########################################### Brazil ###########################################
# Set the working directory
setwd("NARFIMA/Dataset/Dataset_All_Exogenous")
getwd()
 
# Dataset
data <- read_excel('Brazil_Data.xlsx') %>% rename('Exchange_Rate_braz' = spot_ER_Brazil)                   
exchange_rate_braz <- ts(data$Exchange_Rate_braz)
ir_braz <- ts(data$SR_interest_rate_brazil)
ir_diff <- ts(data$SR_Interest_rate_diff_B_U)
cpi_braz <- ts(data$CPI_Inflation_Brazil)
cpi_diff <- ts(data$CPI_inflation_diff_B_U)
gprc_braz <- ts(data$gprc_brazil)

# Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
n = 48
train_braz_48 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
train_ir_braz_48 <- subset(ir_braz, end = length(ir_braz) - n) 
train_ir_diff_48 <- subset(ir_diff, end = length(ir_diff) - n) 
train_cpi_braz_48 <- subset(cpi_braz, end = length(cpi_braz) - n) 
train_cpi_diff_48 <- subset(cpi_diff, end = length(cpi_diff) - n) 
train_gprc_braz_48 <- subset(gprc_braz, end = length(gprc_braz) - n) 

# Perform key statistical tests:
summ_stat_braz_48 <- tibble()
summ_stat_braz_48 <- rbind(summ_stat_braz_48,statistical_tests_summary(train_braz_48))
summ_stat_braz_48 <- rbind(summ_stat_braz_48,statistical_tests_summary(train_ir_braz_48))
summ_stat_braz_48 <- rbind(summ_stat_braz_48,statistical_tests_summary(train_ir_diff_48))
summ_stat_braz_48 <- rbind(summ_stat_braz_48,statistical_tests_summary(train_cpi_braz_48))
summ_stat_braz_48 <- rbind(summ_stat_braz_48,statistical_tests_summary(train_cpi_diff_48))
summ_stat_braz_48 <- rbind(summ_stat_braz_48,statistical_tests_summary(train_gprc_braz_48))
View(summ_stat_braz_48)

# Outlier Detection 
time <- c(1:274)

outliers_exchange_rate_braz_test <- outlierTest(lm(train_braz_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_exchange_rate_braz_test)

outliers_ir_braz_test <- outlierTest(lm(train_ir_braz_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_ir_braz_test)

outliers_ir_diff_test <- outlierTest(lm(train_ir_diff_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_ir_diff_test)

outliers_cpi_braz_test <- outlierTest(lm(train_cpi_braz_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_cpi_braz_test)

outliers_cpi_diff_test <- outlierTest(lm(train_cpi_diff_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_cpi_diff_test)

outliers_gprc_braz_test <- outlierTest(lm(train_gprc_braz_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_gprc_braz_test)



########################################### Russia ###########################################
# Set the working directory
setwd("NARFIMA/Dataset/Dataset_All_Exogenous")
getwd()
 
# Dataset
data <- read_excel('Russia_Data.xlsx') %>% rename('Exchange_Rate_rus' = spot_ER_Russia)                   
exchange_rate_rus <- ts(data$Exchange_Rate_rus)
ir_rus <- ts(data$SR_interest_rate_russia)
ir_diff <- ts(data$SR_Interest_rate_diff_R_U)
cpi_rus <- ts(data$CPI_Inflation_Russia)
cpi_diff <- ts(data$CPI_inflation_diff_R_U)
gprc_rus <- ts(data$gprc_russia)

# Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
n = 48
train_rus_48 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
train_ir_rus_48 <- subset(ir_rus, end = length(ir_rus) - n) 
train_ir_diff_48 <- subset(ir_diff, end = length(ir_diff) - n) 
train_cpi_rus_48 <- subset(cpi_rus, end = length(cpi_rus) - n) 
train_cpi_diff_48 <- subset(cpi_diff, end = length(cpi_diff) - n) 
train_gprc_rus_48 <- subset(gprc_rus, end = length(gprc_rus) - n) 

# Perform key statistical tests:
summ_stat_rus_48 <- tibble()
summ_stat_rus_48 <- rbind(summ_stat_rus_48,statistical_tests_summary(train_rus_48))
summ_stat_rus_48 <- rbind(summ_stat_rus_48,statistical_tests_summary(train_ir_rus_48))
summ_stat_rus_48 <- rbind(summ_stat_rus_48,statistical_tests_summary(train_ir_diff_48))
summ_stat_rus_48 <- rbind(summ_stat_rus_48,statistical_tests_summary(train_cpi_rus_48))
summ_stat_rus_48 <- rbind(summ_stat_rus_48,statistical_tests_summary(train_cpi_diff_48))
summ_stat_rus_48 <- rbind(summ_stat_rus_48,statistical_tests_summary(train_gprc_rus_48))
View(summ_stat_rus_48)

# Outlier Detection 
time <- c(1:274)

outliers_exchange_rate_rus_test <- outlierTest(lm(train_rus_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_exchange_rate_rus_test)

outliers_ir_rus_test <- outlierTest(lm(train_ir_rus_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_ir_rus_test)

outliers_ir_diff_test <- outlierTest(lm(train_ir_diff_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_ir_diff_test)

outliers_cpi_rus_test <- outlierTest(lm(train_cpi_rus_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_cpi_rus_test)

outliers_cpi_diff_test <- outlierTest(lm(train_cpi_diff_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_cpi_diff_test)

outliers_gprc_rus_test <- outlierTest(lm(train_gprc_rus_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_gprc_rus_test)



########################################### India ###########################################
# Set the working directory
setwd("NARFIMA/Dataset/Dataset_All_Exogenous")
getwd()
 
# Dataset
data <- read_excel('India_Data.xlsx') %>% rename('Exchange_Rate_ind' = spot_ER_India)                   
exchange_rate_ind <- ts(data$Exchange_Rate_ind)
ir_ind <- ts(data$SR_interest_rate_india)
ir_diff <- ts(data$SR_Interest_rate_diff_I_U)
cpi_ind <- ts(data$CPI_Inflation_India)
cpi_diff <- ts(data$CPI_inflation_diff_I_U)
gprc_ind <- ts(data$gprc_india)

# Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
n = 48
train_ind_48 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
train_ir_ind_48 <- subset(ir_ind, end = length(ir_ind) - n) 
train_ir_diff_48 <- subset(ir_diff, end = length(ir_diff) - n) 
train_cpi_ind_48 <- subset(cpi_ind, end = length(cpi_ind) - n) 
train_cpi_diff_48 <- subset(cpi_diff, end = length(cpi_diff) - n) 
train_gprc_ind_48 <- subset(gprc_ind, end = length(gprc_ind) - n) 

# Perform key statistical tests:
summ_stat_ind_48 <- tibble()
summ_stat_ind_48 <- rbind(summ_stat_ind_48,statistical_tests_summary(train_ind_48))
summ_stat_ind_48 <- rbind(summ_stat_ind_48,statistical_tests_summary(train_ir_ind_48))
summ_stat_ind_48 <- rbind(summ_stat_ind_48,statistical_tests_summary(train_ir_diff_48))
summ_stat_ind_48 <- rbind(summ_stat_ind_48,statistical_tests_summary(train_cpi_ind_48))
summ_stat_ind_48 <- rbind(summ_stat_ind_48,statistical_tests_summary(train_cpi_diff_48))
summ_stat_ind_48 <- rbind(summ_stat_ind_48,statistical_tests_summary(train_gprc_ind_48))
View(summ_stat_ind_48)

# Outlier Detection 
time <- c(1:274)

outliers_exchange_rate_ind_test <- outlierTest(lm(train_ind_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_exchange_rate_ind_test)

outliers_ir_ind_test <- outlierTest(lm(train_ir_ind_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_ir_ind_test)

outliers_ir_diff_test <- outlierTest(lm(train_ir_diff_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_ir_diff_test)

outliers_cpi_ind_test <- outlierTest(lm(train_cpi_ind_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_cpi_ind_test)

outliers_cpi_diff_test <- outlierTest(lm(train_cpi_diff_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_cpi_diff_test)

outliers_gprc_ind_test <- outlierTest(lm(train_gprc_ind_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_gprc_ind_test)



########################################### China ###########################################
# Set the working directory
setwd("NARFIMA/Dataset/Dataset_All_Exogenous")
getwd()
 
# Dataset
data <- read_excel('China_Data.xlsx') %>% rename('Exchange_Rate_chn' = spot_ER_China)                   
exchange_rate_chn <- ts(data$Exchange_Rate_chn)
ir_chn <- ts(data$SR_interest_rate_china)
ir_diff <- ts(data$SR_Interest_rate_diff_C_U)
cpi_chn <- ts(data$CPI_Inflation_China)
cpi_diff <- ts(data$CPI_inflation_diff_C_U)
gprc_chn <- ts(data$gprc_china)

# Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
n = 48
train_chn_48 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
train_ir_chn_48 <- subset(ir_chn, end = length(ir_chn) - n) 
train_ir_diff_48 <- subset(ir_diff, end = length(ir_diff) - n) 
train_cpi_chn_48 <- subset(cpi_chn, end = length(cpi_chn) - n) 
train_cpi_diff_48 <- subset(cpi_diff, end = length(cpi_diff) - n) 
train_gprc_chn_48 <- subset(gprc_chn, end = length(gprc_chn) - n) 

# Perform key statistical tests:
summ_stat_chn_48 <- tibble()
summ_stat_chn_48 <- rbind(summ_stat_chn_48,statistical_tests_summary(train_chn_48))
summ_stat_chn_48 <- rbind(summ_stat_chn_48,statistical_tests_summary(train_ir_chn_48))
summ_stat_chn_48 <- rbind(summ_stat_chn_48,statistical_tests_summary(train_ir_diff_48))
summ_stat_chn_48 <- rbind(summ_stat_chn_48,statistical_tests_summary(train_cpi_chn_48))
summ_stat_chn_48 <- rbind(summ_stat_chn_48,statistical_tests_summary(train_cpi_diff_48))
summ_stat_chn_48 <- rbind(summ_stat_chn_48,statistical_tests_summary(train_gprc_chn_48))
View(summ_stat_chn_48)

# Outlier Detection 
time <- c(1:274)

outliers_exchange_rate_chn_test <- outlierTest(lm(train_chn_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_exchange_rate_chn_test)

outliers_ir_chn_test <- outlierTest(lm(train_ir_chn_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_ir_chn_test)

outliers_ir_diff_test <- outlierTest(lm(train_ir_diff_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_ir_diff_test)

outliers_cpi_chn_test <- outlierTest(lm(train_cpi_chn_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_cpi_chn_test)

outliers_cpi_diff_test <- outlierTest(lm(train_cpi_diff_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_cpi_diff_test)

outliers_gprc_chn_test <- outlierTest(lm(train_gprc_chn_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_gprc_chn_test)



########################################### Global ###########################################
# Set the working directory
setwd("NARFIMA/Dataset/Dataset_All_Exogenous")
getwd()
 
# Dataset
data <- read_excel('Brazil_Data.xlsx') %>% rename('Exchange_Rate_braz' = spot_ER_Brazil)                   
global_epu_braz <- ts(data$`global_EPU(PPP)`)
emv_us <- ts(data$US_EMV)
mpu_us <- ts(data$US_MPU)
oil_braz <- ts(data$Oil_price_growth_rate_WTI)
ir_us <- ts(data$SR_interest_rate_USA)
cpi_us <- ts(data$CPI_inflation_USA)

# Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
n = 48
train_global_epu_braz_48 <- subset(global_epu_braz, end = length(global_epu_braz) - n) 
train_emv_us_48 <- subset(emv_us, end = length(emv_us) - n) 
train_mpu_us_48 <- subset(mpu_us, end = length(mpu_us) - n) 
train_oil_braz_48 <- subset(oil_braz, end = length(oil_braz) - n) 
train_ir_us_48 <- subset(ir_us, end = length(ir_us) - n) 
train_cpi_us_48 <- subset(cpi_us, end = length(cpi_us) - n) 

# Perform key statistical tests:
summ_stat_global_48 <- tibble()
summ_stat_global_48 <- rbind(summ_stat_global_48,statistical_tests_summary(train_global_epu_braz_48))
summ_stat_global_48 <- rbind(summ_stat_global_48,statistical_tests_summary(train_emv_us_48))
summ_stat_global_48 <- rbind(summ_stat_global_48,statistical_tests_summary(train_mpu_us_48))
summ_stat_global_48 <- rbind(summ_stat_global_48,statistical_tests_summary(train_oil_braz_48))
summ_stat_global_48 <- rbind(summ_stat_global_48,statistical_tests_summary(train_ir_us_48))
summ_stat_global_48 <- rbind(summ_stat_global_48,statistical_tests_summary(train_cpi_us_48))
View(summ_stat_global_48)

# Outlier Detection 
time <- c(1:274)

outliers_global_epu_braz_test <- outlierTest(lm(train_global_epu_braz_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_global_epu_braz_test)

outliers_mpu_us_test <- outlierTest(lm(train_mpu_us_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_mpu_us_test)

outliers_emv_us_test <- outlierTest(lm(train_emv_us_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_emv_us_test)

outliers_oil_braz_test <- outlierTest(lm(train_oil_braz_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_oil_braz_test)

outliers_ir_us_test <- outlierTest(lm(train_ir_us_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_ir_us_test)

outliers_cpi_us_test <- outlierTest(lm(train_cpi_us_48 ~ time, cutoff = 0.05, n.max = 10, order = TRUE, labels = names(rstudent)))
print(outliers_cpi_us_test)
