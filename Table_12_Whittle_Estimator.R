############################# Table 12: Whittle Estimator - BRIC #############################
 
# For reproducibility, we are using this seed value
set.seed(100) 
 
# Load the necessary libraries
library(tidyverse)
library(readxl)
library(longmemo)
library(zoo)

 
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
train_braz_48 <- window(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
train_ir_braz_48 <- window(ir_braz, end = length(ir_braz) - n) 
train_ir_diff_48 <- window(ir_diff, end = length(ir_diff) - n) 
train_cpi_braz_48 <- window(cpi_braz, end = length(cpi_braz) - n) 
train_cpi_diff_48 <- window(cpi_diff, end = length(cpi_diff) - n) 
train_gprc_braz_48 <- window(gprc_braz, end = length(gprc_braz) - n) 

# Perform Whittle Estimator
WhittleEst(train_braz_48, model = "fARIMA")
WhittleEst(train_ir_braz_48, model = "fARIMA")
WhittleEst(train_ir_diff_48, model = "fARIMA")
WhittleEst(train_cpi_braz_48, model = "fARIMA")
WhittleEst(train_cpi_diff_48, model = "fARIMA")
WhittleEst(train_gprc_braz_48, model = "fARIMA")



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
train_rus_48 <- window(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
train_ir_rus_48 <- window(ir_rus, end = length(ir_rus) - n) 
train_ir_diff_48 <- window(ir_diff, end = length(ir_diff) - n) 
train_cpi_rus_48 <- window(cpi_rus, end = length(cpi_rus) - n) 
train_cpi_diff_48 <- window(cpi_diff, end = length(cpi_diff) - n) 
train_gprc_rus_48 <- window(gprc_rus, end = length(gprc_rus) - n) 

# Perform Whittle Estimator
WhittleEst(train_rus_48, model = "fARIMA")
WhittleEst(train_ir_rus_48, model = "fARIMA")
WhittleEst(train_ir_diff_48, model = "fARIMA")
WhittleEst(train_cpi_rus_48, model = "fARIMA")
WhittleEst(train_cpi_diff_48, model = "fARIMA")
WhittleEst(train_gprc_rus_48, model = "fARIMA")



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
train_ind_48 <- window(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
train_ir_ind_48 <- window(ir_ind, end = length(ir_ind) - n) 
train_ir_diff_48 <- window(ir_diff, end = length(ir_diff) - n) 
train_cpi_ind_48 <- window(cpi_ind, end = length(cpi_ind) - n) 
train_cpi_diff_48 <- window(cpi_diff, end = length(cpi_diff) - n) 
train_gprc_ind_48 <- window(gprc_ind, end = length(gprc_ind) - n) 

# Perform Whittle Estimator
WhittleEst(train_ind_48, model = "fARIMA")
WhittleEst(train_ir_ind_48, model = "fARIMA")
WhittleEst(train_ir_diff_48, model = "fARIMA")
WhittleEst(train_cpi_ind_48, model = "fARIMA")
WhittleEst(train_cpi_diff_48, model = "fARIMA")
WhittleEst(train_gprc_ind_48, model = "fARIMA")


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
train_chn_48 <- window(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
train_ir_chn_48 <- window(ir_chn, end = length(ir_chn) - n) 
train_ir_diff_48 <- window(ir_diff, end = length(ir_diff) - n) 
train_cpi_chn_48 <- window(cpi_chn, end = length(cpi_chn) - n) 
train_cpi_diff_48 <- window(cpi_diff, end = length(cpi_diff) - n) 
train_gprc_chn_48 <- window(gprc_chn, end = length(gprc_chn) - n) 

# Perform Whittle Estimator
WhittleEst(train_chn_48, model = "fARIMA")
WhittleEst(na.approx(train_ir_chn_48), model = "fARIMA")
WhittleEst(train_ir_diff_48, model = "fARIMA")
WhittleEst(train_cpi_chn_48, model = "fARIMA")
WhittleEst(train_cpi_diff_48, model = "fARIMA")
WhittleEst(train_gprc_chn_48, model = "fARIMA")



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
train_global_epu_braz_48 <- window(global_epu_braz, end = length(global_epu_braz) - n) 
train_emv_us_48 <- window(emv_us, end = length(emv_us) - n) 
train_mpu_us_48 <- window(mpu_us, end = length(mpu_us) - n) 
train_oil_braz_48 <- window(oil_braz, end = length(oil_braz) - n) 
train_ir_us_48 <- window(ir_us, end = length(ir_us) - n) 
train_cpi_us_48 <- window(cpi_us, end = length(cpi_us) - n) 

# Perform Whittle Estimator
WhittleEst(train_global_epu_braz_48, model = "fARIMA")
WhittleEst(train_emv_us_48, model = "fARIMA")
WhittleEst(train_mpu_us_48, model = "fARIMA")
WhittleEst(train_oil_braz_48, model = "fARIMA")
WhittleEst(train_ir_us_48, model = "fARIMA")
WhittleEst(train_cpi_us_48, model = "fARIMA")
