library(forecast)
library(tidyverse)
library(ggplot2)
library(readxl)


##################################################### Brazil 24 #####################################################

setwd('Dataset/Dataset_Model_Forecasts/ConfPI')
braz_data <- read.csv("Brazil ConfPI 24.csv")

ggplot(data = braz_data, aes(x = as.Date(Date))) +
  geom_point(aes(y = Observed, color = 'Ground Truth')) +
  geom_line(aes(y = NARFIMAx, color = 'NARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARIMAx, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = BSTSx, color = 'BSTS'), linewidth = 2) +
  geom_ribbon(aes(ymin = Interval_L, ymax = Interval_U), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "5 month", date_labels = "%Y-%m-%d", limits = c(ymd("2021-11-01"), ymd("2023-10-01"))) +  scale_y_continuous(
    limits = round(c(min(braz_24$Observed) - 0.1, max(braz_24$Observed) + 0.45),1),
    breaks = function(limits) seq(limits[1], limits[2], length.out = 5)
  ) +
  ylab('Exchange Rate') +
  xlab('Time') +
  labs(color = 'Models') +
  ggtitle('Brazil Exchange Rate: 24 Month Holdout') +
  scale_color_manual(values = c(
    'ARIMA' = '#9A32CD',
    'BSTS' = '#7BB661', 
    'NARFIMA' = '#1F75FE',    
    'Ground Truth' = '#FF3800'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')



##################################################### Russia 24 #####################################################

setwd('Dataset/Dataset_Model_Forecasts/ConfPI')
rus_data <- read.csv("Russia ConfPI 24.csv")

ggplot(data = rus_24, aes(x = as.Date(Date))) +
  geom_point(aes(y = Observed, color = 'Ground Truth')) +
  geom_line(aes(y = NARFIMAx, color = 'NARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARIMAx, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = BSTSx, color = 'BSTS'), linewidth = 2) +
  geom_ribbon(aes(ymin = Interval_L, ymax = Interval_U), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "5 month", date_labels = "%Y-%m-%d", limits = c(ymd("2021-11-01"), ymd("2023-10-01"))) +  
  scale_y_continuous(
    limits = round(c(min(rus_24$Observed) - 0.1, max(rus_24$Interval_U) + 0.1),1),
    breaks = function(limits) seq(limits[1], limits[2], length.out = 5)
  ) +
  ylab('Exchange Rate') +
  xlab('Time') +
  labs(color = 'Models') +
  ggtitle('Russia Exchange Rate: 24 Month Holdout') +
  scale_color_manual(values = c(
    'ARIMA' = '#9A32CD',
    'BSTS' = '#7BB661', 
    'NARFIMA' = '#1F75FE',    
    'Ground Truth' = '#FF3800'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')



##################################################### India 24 #####################################################

setwd('Dataset/Dataset_Model_Forecasts/ConfPI')
ind_data <- read.csv("India ConfPI 24.csv")

ggplot(data = ind_data, aes(x = as.Date(Date))) +
  geom_point(aes(y = Observed, color = 'Ground Truth')) +
  geom_line(aes(y = NARFIMAx, color = 'NARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARIMAx, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = BSTSx, color = 'BSTS'), linewidth = 2) +
  geom_ribbon(aes(ymin = Interval_L, ymax = Interval_U), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "5 month", date_labels = "%Y-%m-%d", limits = c(ymd("2021-11-01"), ymd("2023-10-01"))) +  
  scale_y_continuous(
    limits = round(c(min(ind_data$Interval_L) - 0.1, max(ind_data$Interval_U) + 0.1),1),  
    breaks = function(limits) seq(limits[1], limits[2], length.out = 5)
  ) +
  ylab('Exchange Rate') +
  xlab('Time') +
  labs(color = 'Models') +
  ggtitle('India Exchange Rate: 24 Month Holdout') +
  scale_color_manual(values = c(
    'ARIMA' = '#9A32CD',
    'BSTS' = '#7BB661', 
    'NARFIMA' = '#1F75FE',    
    'Ground Truth' = '#FF3800'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')



##################################################### China 24 #####################################################

setwd('Dataset/Dataset_Model_Forecasts/ConfPI')
chn_data <- read.csv("China ConfPI 24.csv")

ggplot(data = chn_data, aes(x = as.Date(Date))) +
  geom_point(aes(y = Observed, color = 'Ground Truth')) +
  geom_line(aes(y = NARFIMAx, color = 'NARFIMA'), linewidth = 2) +
  geom_line(aes(y = ARIMAx, color = 'ARIMA'), linewidth = 2) +
  geom_line(aes(y = BSTSx, color = 'BSTS'), linewidth = 2) +
  geom_ribbon(aes(ymin = Interval_L, ymax = Interval_U), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "5 month", date_labels = "%Y-%m-%d", limits = c(ymd("2021-11-01"), ymd("2023-10-01"))) +  
  scale_y_continuous(
    limits = round(c(min(chn_data$Interval_L) - 0.1, max(chn_data$Interval_U) + 0.1),1),  
    breaks = function(limits) seq(limits[1], limits[2], length.out = 5)
  ) + 
  ylab('Exchange Rate') +
  xlab('Time') +
  labs(color = 'Models') +
  ggtitle('China Exchange Rate: 24 Month Holdout') +
  scale_color_manual(values = c(
    'ARIMA' = '#9A32CD',
    'BSTS' = '#7BB661', 
    'NARFIMA' = '#1F75FE',    
    'Ground Truth' = '#FF3800'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')
