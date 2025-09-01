################## Table 01: TS - ACF - PACF - OLS-based CUMSUM Plots: BRIC ##################
 
 # For reproducibility, we are using this seed value
 set.seed(100) 
 
 # Load the necessary libraries
 library(readxl)
 library(tidyverse)
 library(lubridate)
 library(ggplot2)
 library(forecast)
 library(strucchange)
 library(gridExtra)
 
 
 ##############################################################################################
 # This function automatically generates three key plots:
 # 1. Time Series Plot.
 # 2. ACF Plot: analyze the autocorrelations in the time series.
 # 3. OLS-based CUSUM Plot:  test for any structural changes in the series.
 # After running, you'll get a combined plot with all three visualizations side by side.
 
 ts_analysis_plots <- function(train_series, start_date) {
   
   # Automatically detect the date format
   if (grepl("\\d{4}-\\d{2}-\\d{2}", start_date)) {
     date_format <- "%Y-%m-%d"
   } else if (grepl("\\d{4}/\\d{2}/\\d{2}", start_date)) {
     date_format <- "%Y/%m/%d"  # Added condition for 'YYYY/MM/DD'
   } else if (grepl("\\d{2}/\\d{2}/\\d{4}", start_date)) {
     date_format <- "%m/%d/%Y"
   } else if (grepl("\\d{2}-\\d{2}-\\d{4}", start_date)) {
     date_format <- "%d-%m-%Y"
   } else if (grepl("\\d{2}\\.\\d{2}\\.\\d{4}", start_date)) {
     date_format <- "%d.%m.%Y"
   } else {
     stop("Unknown date format. Please provide a date in a recognized format.")
   }
   
   # Convert the start date to a Date object
   start_date <- as.Date(start_date, format = date_format)
   
   # Generate a sequence of dates for the training data
   train_dates <- seq.Date(from = start_date, by = paste(frequency(train_series), "month"), length.out = length(train_series))
   
   # Combine all the series in one data frame
   train_data <- data.frame(Date = train_dates, Value = train_series)
   
   
   # Time Series Plot
   time_series_plot <- ggplot(train_data, aes(x = Date, y = Value)) +
     geom_line(color = 'dodgerblue3', linewidth = 1.5) +
     labs(x = "Time", y = "Exchange Rate") +
     scale_x_date(date_breaks = "5 years", date_labels = "%Y-%m") +
     theme_classic() +
     theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
     theme(plot.title = element_text(hjust = 0.5)) +
     theme(
       axis.title.x = element_text(size = 20, face = 'bold'), 
       axis.title.y = element_text(size = 20, face = 'bold'), 
       axis.text.x = element_text(size = 18, face = 'bold'),  
       axis.text.y = element_text(size = 18, face = 'bold'))    
   
   # ACF Plot
   diff_train_series <- diff(train_series, differences = ndiffs(train_series), ci = 0.8)
   acf_plot <- ggAcf(diff_train_series, size = 1.5) +
     geom_point(color = 'navy blue', size = 1.5) +
     ggtitle(NULL) +
     theme_classic() +
     theme(plot.title = element_text(hjust = 0.5)) +
     theme(
       axis.title.x = element_text(size = 20, face = 'bold'), 
       axis.title.y = element_text(size = 20, face = 'bold'), 
       axis.text.x = element_text(size = 18, face = 'bold'),  
       axis.text.y = element_text(size = 18, face = 'bold'))       
   
   # CUSUM Plot
   cusum_test <- efp(train_series ~ 1, type = "OLS-CUSUM", dynamic = TRUE)
   
   # Define the Bounds
   cusum_stats <- cusum_test$process
   mean_cusum <- mean(cusum_stats)
   sd_cusum <- sd(cusum_stats)
   upper_bound <- mean_cusum + 3 * sd_cusum
   lower_bound <- mean_cusum - 3 * sd_cusum
   
   cusum_df <- data.frame(Index = 1:(cusum_test$datatsp[3] + cusum_test$datatsp[2] + cusum_test$datatsp[1]), Statistic = as.numeric(cusum_stats))
   bounds_df <- data.frame(
     Bound = c("Upper Bound", "Lower Bound", "Mean"),
     Value = c(upper_bound, lower_bound, mean_cusum)
   )
   
   cusum_plot <- ggplot(cusum_df, aes(x = Index, y = Statistic)) +
     geom_line(color = "black", size = 1.5) +
     geom_hline(data = bounds_df, aes(yintercept = Value, color = Bound), linetype = "dashed", size = 1.5) +
     scale_color_manual(values = c("Upper Bound" = "red", "Lower Bound" = "red", "Mean" = "blue")) +
     labs(x = "Time", y = "Empirical Fluctuation Process") +
     theme_classic() +
     theme(plot.title = element_text(hjust = 0.5)) +
     theme(
       axis.title.x = element_text(size = 20, face = 'bold'), 
       axis.title.y = element_text(size = 20, face = 'bold'), 
       axis.text.x = element_text(size = 18, face = 'bold'),  
       axis.text.y = element_text(size = 18, face = 'bold'),
       legend.position = "none")
   
   # Combine all plots
   combined_plot <- grid.arrange(time_series_plot, acf_plot, cusum_plot, ncol = 3)
   
   # Return the combined plot
   return(combined_plot)
 }
 
 ##############################################################################################
 
 
 ########################################### Brazil ###########################################
 # Set the working directory
 setwd("NARFIMA/Dataset/Dataset_All_Exogenous")
 getwd()
 
 # Dataset
 data <- read_excel('Brazil_Data.xlsx') %>% rename('Exchange_Rate_braz' = spot_ER_Brazil)                   
 exchange_rate_braz <- ts(data$Exchange_Rate_braz)
 
 # Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
 n = 48
 train_braz_48 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
 
 # Create the plots
 ts_analysis_plots(train_series = train_braz_48, start_date = '1997-01-01')
 
 
 ########################################### Russia ###########################################
 # Set the working directory
 setwd("NARFIMA/Dataset/Dataset_All_Exogenous")
 getwd()
 
 # Dataset
 data <- read_excel('Russia_Data.xlsx') %>% rename('Exchange_Rate_rus' = spot_ER_Russia)                   
 exchange_rate_rus <- ts(data$Exchange_Rate_rus)
 
 # Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
 n = 48
 train_rus_48 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
 
 # Create the plots
 ts_analysis_plots(train_series = train_rus_48, start_date = '1997-01-01')
 
 
 ########################################### India ###########################################
 # Set the working directory
 setwd("NARFIMA/Dataset/Dataset_All_Exogenous")
 getwd()
 
 # Dataset
 data <- read_excel('India_Data.xlsx') %>% rename('Exchange_Rate_ind' = spot_ER_India)                   
 exchange_rate_ind <- ts(data$Exchange_Rate_ind)
 
 # Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
 n = 48
 train_ind_48 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
 
 # Create the plots
 ts_analysis_plots(train_series = train_ind_48, start_date = '1997-01-01')
 
 
 ########################################### China ###########################################
 # Set the working directory
 setwd("NARFIMA/Dataset/Dataset_All_Exogenous")
 getwd()
 
 # Dataset
 data <- read_excel('China_Data.xlsx') %>% rename('Exchange_Rate_chn' = spot_ER_China)                   
 exchange_rate_chn <- ts(data$Exchange_Rate_chn)
 
 # Remove the last 48 observations (test data) -- Time frame considered: 1997M01 - 2019M10
 n = 48
 train_chn_48 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
 
 # Create the plots
 ts_analysis_plots(train_series = train_chn_48, start_date = '1997-01-01')
