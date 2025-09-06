############################# Table 02: Causality Analysis - BRIC #############################
 
# For reproducibility, we are using this seed value
set.seed(100) 
 
# Load the necessary libraries
library(tidyverse)
library(readxl)
library(RTransferEntropy)
library(NlinTS)


########################################### Brazil ###########################################
# Set the working directory
setwd("NARFIMA/Dataset/Dataset_All_Exogenous")
getwd()
 
# Dataset
data <- read_excel('Brazil_Data.xlsx') %>% rename('Exchange_Rate_braz' = spot_ER_Brazil)                   
exchange_rate_braz <- data$Exchange_Rate_braz
global_epu_braz <- data$`global_EPU(PPP)`
emv_us <- data$US_EMV
mpu_us <- data$US_MPU
gprc_braz <- data$gprc_brazil
oil_braz <- data$Oil_price_growth_rate_WTI
ir_braz <- data$SR_interest_rate_brazil
ir_us <- data$SR_interest_rate_USA
ir_diff <- data$SR_Interest_rate_diff_B_U
cpi_braz <- data$CPI_Inflation_Brazil
cpi_us <- data$CPI_inflation_USA
cpi_diff <- data$CPI_inflation_diff_B_U

# Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
n = 48
train_braz_1 <- exchange_rate_braz[1:(length(exchange_rate_braz) - n)]
train_braz_1 <- cbind(Date = 1:length(train_braz_1), exchange_rate_braz = train_braz_1)

train_global_epu_braz_1 <- global_epu_braz[1:(length(global_epu_braz) - n)]
train_global_epu_braz_1 <- cbind(Date = 1:length(train_global_epu_braz_1), Global_EPU_Brazil = train_global_epu_braz_1)

train_emv_us_1 <- emv_us[1:(length(emv_us) - n)]
train_emv_us_1 <- cbind(Date = 1:length(train_emv_us_1), US_EMV = train_emv_us_1)

train_mpu_us_1 <- mpu_us[1:(length(mpu_us) - n)]
train_mpu_us_1 <- cbind(Date = 1:length(train_mpu_us_1), US_MPU = train_mpu_us_1)

train_gprc_braz_1 <- gprc_braz[1:(length(gprc_braz) - n)]
train_gprc_braz_1 <- cbind(Date = 1:length(train_gprc_braz_1), Brazil_GPRC = train_gprc_braz_1)

train_oil_braz_1 <- oil_braz[1:(length(oil_braz) - n)]
train_oil_braz_1 <- cbind(Date = 1:length(train_oil_braz_1), Oil = train_oil_braz_1)

train_ir_braz_1 <- ir_braz[1:(length(ir_braz) - n)]
train_ir_braz_1 <- cbind(Date = 1:length(train_ir_braz_1), Interest_Brazil = train_ir_braz_1)

train_ir_us_1 <- ir_us[1:(length(ir_us) - n)]
train_ir_us_1 <- cbind(Date = 1:length(train_ir_us_1), Interest_US = train_ir_us_1)

train_ir_diff_1 <- ir_diff[1:(length(ir_diff) - n)]
train_ir_diff_1 <- cbind(Date = 1:length(train_ir_diff_1), Interest_Brazil_US_Difference = train_ir_diff_1)

train_cpi_braz_1 <- cpi_braz[1:(length(cpi_braz) - n)]
train_cpi_braz_1 <- cbind(Date = 1:length(train_cpi_braz_1), CPI_Inflation_Brazil = train_cpi_braz_1)

train_cpi_us_1 <- cpi_us[1:(length(cpi_us) - n)]
train_cpi_us_1 <- cbind(Date = 1:length(train_cpi_us_1), CPI_Inflation_US = train_cpi_us_1)

train_cpi_diff_1 <- cpi_diff[1:(length(cpi_diff) - n)]
train_cpi_diff_1 <- cbind(Date = 1:length(train_cpi_diff_1), CPI_Inflation_Brazil_US_Difference = train_cpi_diff_1)

# Non-linear Granger Test
nlin_causality.test(exchange_rate_braz, global_epu_braz,  1, c(3), c(2), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, emv_us,  1, c(3), c(2), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, mpu_us,  1, c(3), c(2), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, gprc_braz,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, oil_braz, 1, c(3), c(2), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, ir_braz,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, ir_us,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, ir_diff,  1, c(3), c(2), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, cpi_braz,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, cpi_us,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, cpi_diff,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()



########################################### Russia ###########################################
# Set the working directory
setwd("NARFIMA/Dataset/Dataset_All_Exogenous")
getwd()
 
# Dataset
data <- read_excel('Russia_Data.xlsx') %>% rename('Exchange_Rate_rus' = spot_ER_Russia)                   
exchange_rate_rus <- ts(data$Exchange_Rate_rus)
global_epu_rus <- data$`global_EPU(PPP)`
emv_us <- data$US_EMV
mpu_us <- data$US_MPU
gprc_rus <- data$gprc_russia
oil_rus <- data$Oil_price_growth_rate_WTI
ir_rus <- data$SR_interest_rate_russia
ir_us <- data$SR_interest_rate_USA
ir_diff <- data$SR_Interest_rate_diff_R_U
cpi_rus <- data$CPI_Inflation_Russia
cpi_us <- data$CPI_inflation_USA
cpi_diff <- data$CPI_inflation_diff_R_U

# Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
n = 48
train_rus_1 <- exchange_rate_rus[1:(length(exchange_rate_rus) - n)]
train_rus_1 <- cbind(Date = 1:length(train_rus_1), exchange_rate_rus = train_rus_1)

train_global_epu_rus_1 <- global_epu_rus[1:(length(global_epu_rus) - n)]
train_global_epu_rus_1 <- cbind(Date = 1:length(train_global_epu_rus_1), Global_EPU_russia = train_global_epu_rus_1)

train_emv_us_1 <- emv_us[1:(length(emv_us) - n)]
train_emv_us_1 <- cbind(Date = 1:length(train_emv_us_1), US_EMV = train_emv_us_1)

train_mpu_us_1 <- mpu_us[1:(length(mpu_us) - n)]
train_mpu_us_1 <- cbind(Date = 1:length(train_mpu_us_1), US_MPU = train_mpu_us_1)

train_gprc_rus_1 <- gprc_rus[1:(length(gprc_rus) - n)]
train_gprc_rus_1 <- cbind(Date = 1:length(train_gprc_rus_1), russia_GPRC = train_gprc_rus_1)

train_oil_rus_1 <- oil_rus[1:(length(oil_rus) - n)]
train_oil_rus_1 <- cbind(Date = 1:length(train_oil_rus_1), Oil = train_oil_rus_1)

train_ir_rus_1 <- ir_rus[1:(length(ir_rus) - n)]
train_ir_rus_1 <- cbind(Date = 1:length(train_ir_rus_1), Interest_russia = train_ir_rus_1)

train_ir_us_1 <- ir_us[1:(length(ir_us) - n)]
train_ir_us_1 <- cbind(Date = 1:length(train_ir_us_1), Interest_US = train_ir_us_1)

train_ir_diff_1 <- ir_diff[1:(length(ir_diff) - n)]
train_ir_diff_1 <- cbind(Date = 1:length(train_ir_diff_1), Interest_russia_US_Difference = train_ir_diff_1)

train_cpi_rus_1 <- cpi_rus[1:(length(cpi_rus) - n)]
train_cpi_rus_1 <- cbind(Date = 1:length(train_cpi_rus_1), CPI_Inflation_russia = train_cpi_rus_1)

train_cpi_us_1 <- cpi_us[1:(length(cpi_us) - n)]
train_cpi_us_1 <- cbind(Date = 1:length(train_cpi_us_1), CPI_Inflation_US = train_cpi_us_1)

train_cpi_diff_1 <- cpi_diff[1:(length(cpi_diff) - n)]
train_cpi_diff_1 <- cbind(Date = 1:length(train_cpi_diff_1), CPI_Inflation_russia_US_Difference = train_cpi_diff_1)

# Non-linear Granger Test
nlin_causality.test(exchange_rate_rus, global_epu_rus,  1, c(2), c(1), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_rus, emv_us,  1, c(2), c(1), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_rus, mpu_us,  1, c(2), c(1), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_rus, gprc_rus,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_rus, oil_rus, 1, c(2), c(1), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_rus, ir_rus,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_rus, ir_us,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_rus, ir_diff,  1, c(2), c(1), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_rus, cpi_rus,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_rus, cpi_us,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_rus, cpi_diff,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()



########################################### India ###########################################
# Set the working directory
setwd("NARFIMA/Dataset/Dataset_All_Exogenous")
getwd()
 
# Dataset
data <- read_excel('India_Data.xlsx') %>% rename('Exchange_Rate_ind' = spot_ER_India)                   
exchange_rate_ind <- ts(data$Exchange_Rate_ind)
global_epu_ind <- data$`global_EPU(PPP)`
emv_us <- data$US_EMV
mpu_us <- data$US_MPU
gprc_ind <- data$gprc_india
oil_ind <- data$Oil_price_growth_rate_WTI
ir_ind <- data$SR_interest_rate_india
ir_us <- data$SR_interest_rate_USA
ir_diff <- data$SR_Interest_rate_diff_I_U
cpi_ind <- data$CPI_Inflation_India
cpi_us <- data$CPI_inflation_USA
cpi_diff <- data$CPI_inflation_diff_I_U

# Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
n = 48
train_ind_1 <- exchange_rate_ind[1:(length(exchange_rate_ind) - n)]
train_ind_1 <- cbind(Date = 1:length(train_ind_1), exchange_rate_ind = train_ind_1)

train_global_epu_ind_1 <- global_epu_ind[1:(length(global_epu_ind) - n)]
train_global_epu_ind_1 <- cbind(Date = 1:length(train_global_epu_ind_1), Global_EPU_india = train_global_epu_ind_1)

train_emv_us_1 <- emv_us[1:(length(emv_us) - n)]
train_emv_us_1 <- cbind(Date = 1:length(train_emv_us_1), US_EMV = train_emv_us_1)

train_mpu_us_1 <- mpu_us[1:(length(mpu_us) - n)]
train_mpu_us_1 <- cbind(Date = 1:length(train_mpu_us_1), US_MPU = train_mpu_us_1)

train_gprc_ind_1 <- gprc_ind[1:(length(gprc_ind) - n)]
train_gprc_ind_1 <- cbind(Date = 1:length(train_gprc_ind_1), india_GPRC = train_gprc_ind_1)

train_oil_ind_1 <- oil_ind[1:(length(oil_ind) - n)]
train_oil_ind_1 <- cbind(Date = 1:length(train_oil_ind_1), Oil = train_oil_ind_1)

train_ir_ind_1 <- ir_ind[1:(length(ir_ind) - n)]
train_ir_ind_1 <- cbind(Date = 1:length(train_ir_ind_1), Interest_india = train_ir_ind_1)

train_ir_us_1 <- ir_us[1:(length(ir_us) - n)]
train_ir_us_1 <- cbind(Date = 1:length(train_ir_us_1), Interest_US = train_ir_us_1)

train_ir_diff_1 <- ir_diff[1:(length(ir_diff) - n)]
train_ir_diff_1 <- cbind(Date = 1:length(train_ir_diff_1), Interest_india_US_Difference = train_ir_diff_1)

train_cpi_ind_1 <- cpi_ind[1:(length(cpi_ind) - n)]
train_cpi_ind_1 <- cbind(Date = 1:length(train_cpi_ind_1), CPI_Inflation_india = train_cpi_ind_1)

train_cpi_us_1 <- cpi_us[1:(length(cpi_us) - n)]
train_cpi_us_1 <- cbind(Date = 1:length(train_cpi_us_1), CPI_Inflation_US = train_cpi_us_1)

train_cpi_diff_1 <- cpi_diff[1:(length(cpi_diff) - n)]
train_cpi_diff_1 <- cbind(Date = 1:length(train_cpi_diff_1), CPI_Inflation_india_US_Difference = train_cpi_diff_1)

# Non-linear Granger Test
nlin_causality.test(exchange_rate_braz, global_epu_braz,  1, c(3), c(2), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, emv_us,  1, c(3), c(2), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, mpu_us,  1, c(3), c(2), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, gprc_braz,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, oil_braz, 1, c(3), c(2), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, cpi_braz,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, cpi_us,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, cpi_diff,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, ir_braz,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, ir_us,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_braz, ir_diff,  1, c(3), c(2), 50, 0.01, seed = 11)$summary()



########################################### China ###########################################
# Set the working directory
setwd("NARFIMA/Dataset/Dataset_All_Exogenous")
getwd()
 
# Dataset
data <- read_excel('China_Data.xlsx') %>% rename('Exchange_Rate_chn' = spot_ER_China)                   
exchange_rate_chn <- ts(data$Exchange_Rate_chn)
global_epu_chn <- data$`global_EPU(PPP)`
emv_us <- data$US_EMV
mpu_us <- data$US_MPU
gprc_chn <- data$gprc_china
oil_chn <- data$Oil_price_growth_rate_WTI
ir_chn <- data$SR_interest_rate_china
ir_us <- data$SR_interest_rate_USA
ir_diff <- data$SR_Interest_rate_diff_C_U
cpi_chn <- data$CPI_Inflation_China
cpi_us <- data$CPI_inflation_USA
cpi_diff <- data$CPI_inflation_diff_C_U

for(i in 1:length(ir_chn)){
  if(is.na(ir_chn[i])){
    ir_chn[i] = mean(ir_chn, na.rm = TRUE)
  }
}

# Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
n = 48
train_chn_1 <- exchange_rate_chn[1:(length(exchange_rate_chn) - n)]
train_chn_1 <- cbind(Date = 1:length(train_chn_1), exchange_rate_chn = train_chn_1)

train_global_epu_chn_1 <- global_epu_chn[1:(length(global_epu_chn) - n)]
train_global_epu_chn_1 <- cbind(Date = 1:length(train_global_epu_chn_1), Global_EPU_china = train_global_epu_chn_1)

train_emv_us_1 <- emv_us[1:(length(emv_us) - n)]
train_emv_us_1 <- cbind(Date = 1:length(train_emv_us_1), US_EMV = train_emv_us_1)

train_mpu_us_1 <- mpu_us[1:(length(mpu_us) - n)]
train_mpu_us_1 <- cbind(Date = 1:length(train_mpu_us_1), US_MPU = train_mpu_us_1)

train_gprc_chn_1 <- gprc_chn[1:(length(gprc_chn) - n)]
train_gprc_chn_1 <- cbind(Date = 1:length(train_gprc_chn_1), china_GPRC = train_gprc_chn_1)

train_oil_chn_1 <- oil_chn[1:(length(oil_chn) - n)]
train_oil_chn_1 <- cbind(Date = 1:length(train_oil_chn_1), Oil = train_oil_chn_1)

train_ir_chn_1 <- ir_chn[1:(length(ir_chn) - n)]
train_ir_chn_1 <- cbind(Date = 1:length(train_ir_chn_1), Interest_china = train_ir_chn_1)

train_ir_us_1 <- ir_us[1:(length(ir_us) - n)]
train_ir_us_1 <- cbind(Date = 1:length(train_ir_us_1), Interest_US = train_ir_us_1)

train_ir_diff_1 <- ir_diff[1:(length(ir_diff) - n)]
train_ir_diff_1 <- cbind(Date = 1:length(train_ir_diff_1), Interest_china_US_Difference = train_ir_diff_1)

train_cpi_chn_1 <- cpi_chn[1:(length(cpi_chn) - n)]
train_cpi_chn_1 <- cbind(Date = 1:length(train_cpi_chn_1), CPI_Inflation_china = train_cpi_chn_1)

train_cpi_us_1 <- cpi_us[1:(length(cpi_us) - n)]
train_cpi_us_1 <- cbind(Date = 1:length(train_cpi_us_1), CPI_Inflation_US = train_cpi_us_1)

train_cpi_diff_1 <- cpi_diff[1:(length(cpi_diff) - n)]
train_cpi_diff_1 <- cbind(Date = 1:length(train_cpi_diff_1), CPI_Inflation_china_US_Difference = train_cpi_diff_1)

# Non-linear Granger Test
nlin_causality.test(exchange_rate_chn, global_epu_chn,  1, c(2), c(1), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_chn, emv_us,  1, c(2), c(1), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_chn, mpu_us,  1, c(2), c(1), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_chn, gprc_chn,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_chn, oil_chn, 1, c(2), c(1), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_chn, ir_chn,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_chn, ir_us,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_chn, ir_diff,  1, c(2), c(1), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_chn, cpi_chn,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_chn, cpi_us,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
nlin_causality.test(exchange_rate_chn, cpi_diff,  1, c(2), c(4), 50, 0.01, seed = 11)$summary()
