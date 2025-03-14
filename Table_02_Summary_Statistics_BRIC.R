############################# Table 02: Sumary Statistics - BRIC #############################
 
 # For reproducibility, we are using this seed value
 set.seed(100) 
 
 # Load the necessary libraries
library(tidyverse)
library(readxl)
 
 
##############################################################################################
# This function computes key statistical measures:  
# 1. Coefficient of Variation (CoV).
# 2. Entropy.  
# 3. SUMMARY: Provides Min, Q1 Value, Median, Mean, Q3 Value, Max, CoV, and Entropy. 
 
# Calculate Coefficient of Variation
CoV <- function(data) {
  return(sd(data) / mean(data) * 100)
}

# Calculate Entropy
Entropy <- function(data) {
  probs <- table(data) / length(data)
  return(-sum(probs * log(probs)))
}

# Summary Function
SUMMARY <- function(data) {
  if (!is.numeric(data)){
    stop("Data must be numeric")
  } 
  
  Min <- min(data)
  Q1 <- quantile(data, 0.25)
  Median <- median(data)
  Mean <- mean(data)
  Q3 <- quantile(data, 0.75)
  Max <- max(data)
  CoV_value <- CoV(data)
  Entropy_value <- Entropy(data)
  
  summ <- tibble(
    Min = Min,
    `1st Qu.` = Q1,
    Median = Median,
    Mean = Mean,
    `3rd Qu.` = Q3,
    Max = Max,
    CoV = CoV_value,
    Entropy = Entropy_value
  )
  
  return(summ)
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

# Find the Min, Q1 Value, Median, Mean, Q3 Value, Max, CoV, and Entropy.
summ_braz_48 <- tibble()
summ_braz_48 <- rbind(summ_braz_48,SUMMARY(train_braz_48))
summ_braz_48 <- rbind(summ_braz_48,SUMMARY(train_ir_braz_48))
summ_braz_48 <- rbind(summ_braz_48,SUMMARY(train_ir_diff_48))
summ_braz_48 <- rbind(summ_braz_48,SUMMARY(train_cpi_braz_48))
summ_braz_48 <- rbind(summ_braz_48,SUMMARY(train_cpi_diff_48))
summ_braz_48 <- rbind(summ_braz_48,SUMMARY(train_gprc_braz_48))
write.csv(summ_braz_48, "Summary_Statistics/Brazil_48.csv", row.names = FALSE)



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

# Find the Min, Q1 Value, Median, Mean, Q3 Value, Max, CoV, and Entropy.
summ_rus_48 <- tibble()
summ_rus_48 <- rbind(summ_rus_48,SUMMARY(train_rus_48))
summ_rus_48 <- rbind(summ_rus_48,SUMMARY(train_ir_rus_48))
summ_rus_48 <- rbind(summ_rus_48,SUMMARY(train_ir_diff_48))
summ_rus_48 <- rbind(summ_rus_48,SUMMARY(train_cpi_rus_48))
summ_rus_48 <- rbind(summ_rus_48,SUMMARY(train_cpi_diff_48))
summ_rus_48 <- rbind(summ_rus_48,SUMMARY(train_gprc_rus_48))
write.csv(summ_rus_48, "Summary_Statistics/Russia_48.csv", row.names = FALSE)



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

# Find the Min, Q1 Value, Median, Mean, Q3 Value, Max, CoV, and Entropy.
summ_ind_48 <- tibble()
summ_ind_48 <- rbind(summ_ind_48,SUMMARY(train_ind_48))
summ_ind_48 <- rbind(summ_ind_48,SUMMARY(train_ir_ind_48))
summ_ind_48 <- rbind(summ_ind_48,SUMMARY(train_ir_diff_48))
summ_ind_48 <- rbind(summ_ind_48,SUMMARY(train_cpi_ind_48))
summ_ind_48 <- rbind(summ_ind_48,SUMMARY(train_cpi_diff_48))
summ_ind_48 <- rbind(summ_ind_48,SUMMARY(train_gprc_ind_48))
write.csv(summ_ind_48, "Summary_Statistics/India_48.csv", row.names = FALSE)



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

# Find the Min, Q1 Value, Median, Mean, Q3 Value, Max, CoV, and Entropy.
summ_chn_48 <- tibble()
summ_chn_48 <- rbind(summ_chn_48,SUMMARY(train_chn_48))
summ_chn_48 <- rbind(summ_chn_48,SUMMARY(train_ir_chn_48))
summ_chn_48 <- rbind(summ_chn_48,SUMMARY(train_ir_diff_48))
summ_chn_48 <- rbind(summ_chn_48,SUMMARY(train_cpi_chn_48))
summ_chn_48 <- rbind(summ_chn_48,SUMMARY(train_cpi_diff_48))
summ_chn_48 <- rbind(summ_chn_48,SUMMARY(train_gprc_chn_48))
write.csv(summ_chn_48, "Summary_Statistics/China_48.csv", row.names = FALSE)



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

# Find the Min, Q1 Value, Median, Mean, Q3 Value, Max, CoV, and Entropy.
summ_global_48 <- tibble()
summ_global_48 <- rbind(summ_global_48,SUMMARY(train_global_epu_braz_48))
summ_global_48 <- rbind(summ_global_48,SUMMARY(train_emv_us_48))
summ_global_48 <- rbind(summ_global_48,SUMMARY(train_mpu_us_48))
summ_global_48 <- rbind(summ_global_48,SUMMARY(train_oil_braz_48))
summ_global_48 <- rbind(summ_global_48,SUMMARY(train_ir_us_48))
summ_global_48 <- rbind(summ_global_48,SUMMARY(train_cpi_us_48))
write.csv(summ_global_48, "Summary_Statistics/Global_48.csv", row.names = FALSE)
