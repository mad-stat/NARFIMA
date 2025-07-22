############################### Figure 02: Model Illustration ###############################

# For reproducibility, we are using this seed value
 set.seed(100) 

# Load the necessary libraries
library(readxl)
library(forecast)
library(tidyverse)
library(lubridate)
library(ggplot2)

 
#############################################################################################


################## Linear impulse responses with local projections: Brazil ##################
# Set the working directory
setwd("NARFIMA/Dataset/Dataset_Selected_Exogenous")
getwd()


# Dataset
data <- read_excel('Brazil_Data.xlsx') %>% rename('Exchange_Rate_braz' = spot_ER_Brazil)    
exchange_rate_braz <- ts(data$Exchange_Rate_braz)
oil_braz <- ts(data$Oil_price_growth_rate_WTI)
ir_diff <- ts(data$SR_Interest_rate_diff_B_U)
global_epu_braz <- ts(data$`global_EPU(PPP)`)
emv_us <- ts(data$US_EMV)
mpu_us <- ts(data$US_MPU)


# Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
n = 48
train_braz_48 <- exchange_rate_braz[1:(nrow(data) - n)]
train_oil_braz_48 <- oil_braz[1:(nrow(data) - n)]
train_ir_diff_48 <- ir_diff[1:(nrow(data) - n)]
train_global_epu_braz_48 <- global_epu_braz[1:(nrow(data) - n)]
train_emv_us_48 <- emv_us[1:(nrow(data) - n)]
train_mpu_us_48 <- mpu_us[1:(nrow(data) - n)]


# Time Series Plot: Exchange Rate 
ggplot(data = data[1:(nrow(data) - n),], aes(x = as.Date(Date))) +
  geom_line(aes(y = train_braz_48), size = 0.9, color = 'red') +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y-%m") +
  theme_classic() +
  theme(
    legend.text = element_text(size = 30, face = 'bold'),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 20, face = 'bold'),
    axis.text.y = element_text(size = 30, face = 'bold'),
    axis.title.x =  element_text(size = 30, face = 'bold'),
    axis.title.y =  element_text(size = 30, face = 'bold')
  ) +
  labs(color = '') +
  xlab('Time') +
  ylab('Exchange Rate') +  
  theme(plot.title = element_text(hjust = 0.5))


# Time Series Plot: Covariates 
ggplot(data = data[1:(nrow(data) - n),], aes(x = as.Date(Date))) +
  geom_line(aes(y = train_oil_braz_48, color = 'Oil Price Growth Rate'), size = 0.9) +
  geom_line(aes(y = train_ir_diff_48, color = 'Interest Rate Difference'), size = 0.9) +
  geom_line(aes(y = train_global_epu_braz_48, color = 'Global EPU'), size = 0.9) +
  geom_line(aes(y = train_emv_us_48, color = 'US EMV'), size = 0.9) +
  geom_line(aes(y = train_mpu_us_48, color = 'US MPU'), size = 0.9) +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y-%m") +
  theme_classic() +
  theme(
    legend.text = element_text(size = 20, face = 'bold'),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 30, face = 'bold'),
    axis.text.y = element_text(size = 30, face = 'bold'),
    axis.title.x =  element_text(size = 30, face = 'bold'),
    legend.position = c(0.233, 0.999), 
    legend.justification = c(0.9, 1)
  ) +
  labs(color = '') +
  xlab('Time') +
  ylab('') +  
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c(
    'Oil Price Growth Rate' = '#1F75FE',     
    'Interest Rate Difference' = '#FF9933', 
    'Global EPU' = '#F56FA1',
    'US EMV' = '#7BB661', 
    'US MPU' = 'black'    
  ))


# Time Series Plot: ARFIMA Residuals 
exchange_rate_braz <- ts(data$Exchange_Rate_braz)
reg_braz <- as.matrix(data[,c(7,10,11,13,14)], ncol = 5)

n = 48
train_braz_48 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
test_braz_48 <- subset(exchange_rate_braz, start = length(exchange_rate_braz) - n + 1)
train_reg_braz_48 <- reg_braz[1:length(train_braz_48),]
test_reg_braz_48 <- reg_braz[1:n,]

set.seed(100)
arfima_braz_48 <- arfima(train_braz_48, xreg = train_reg_braz_48)
arfima_braz_48_pred <- forecast(arfima_braz_48, h = n, xreg = test_reg_braz_48)
arfima_er_braz_48 <-  residuals(arfima_braz_48)
arfima_er_braz_48[is.na(arfima_er_braz_48)] <-  0

ggplot(data = data[1:(nrow(data) - n),], aes(x = as.Date(Date))) +
  geom_line(aes(y = arfima_er_braz_48), size = 0.9, color = 'forestgreen') +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y-%m") +
  theme_classic() +
  theme(
    legend.text = element_text(size = 30, face = 'bold'),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 30, face = 'bold'),
    axis.text.y = element_text(size = 30, face = 'bold'),
    axis.title.x =  element_text(size = 30, face = 'bold')
  ) +
  labs(color = '') +
  xlab('Time') +
  ylab('') +  
  theme(plot.title = element_text(hjust = 0.5))


# Time Series Plot: NARFIMA Forecasts 
set.seed(100)
narfimaT_braz_48 <-  auto.narfima(train_braz_48, arfima_er_braz_48, p = 4, q = 2, size = 1, lambda = 0, lambdae = 0, repeats = 1000, skip = T, xreg = train_reg_braz_48)

set.seed(100)
narfimaT_braz_48_pred <-  forecast.narfima(narfimaT_braz_48, PI = FALSE, h = n, xreg = test_reg_braz_48)

df <- data.frame(Date = data$Date[275:322], Value = narfimaT_braz_48_pred$mean)

ggplot(data = df, aes(x = as.Date(Date))) +
  geom_line(aes(y = Value), size = 0.9, color = 'purple') +
  scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
  theme_classic() +
  theme(
    legend.text = element_text(size = 20, face = 'bold'),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 25, face = 'bold'),
    axis.text.y = element_text(size = 23, face = 'bold'),
    axis.title.x =  element_text(size = 30, face = 'bold'),
    axis.title.y =  element_text(size = 30, face = 'bold')
  ) +
  labs(color = '') +
  xlab('Time') +
  ylab('Exchange Rate') +  
  theme(plot.title = element_text(hjust = 0.5))
