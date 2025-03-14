####################### Figure 01: Impulse Response Functions - BRIC #######################
 
# Load the necessary libraries
library(lpirfs)
library(ggpubr)
library(gridExtra)

 
#############################################################################################


################## Linear impulse responses with local projections: Brazil ##################
# Set the working directory
setwd("NARFIMA/Dataset/Dataset_IRF")
getwd()
 
# Dataset
all_data_brazil <- read.csv("Brazil_Data_IRF.csv", header = TRUE)
str(all_data_brazil)

# Convert Date into Datetime Value
all_data_brazil$Date <- as.Date(all_data_brazil$Date)
str(all_data_brazil)

endog_data_brazil_all <- all_data_brazil[,c("spot_ER_Brazil", 
                                            "global_EPU_PPP",
                                            "gprc_brazil",
                                            "US_EMV",
                                            "US_MPU")]
str(endog_data_brazil_all)

results_lin_brazil <- lp_lin(endog_data_brazil_all, 
                             lags_endog_lin = NaN,     # Number of lags for endogenous data
                             lags_criterion = 'BIC', 
                             max_lags       = 12,
                             trend          = 1,       # 0 = no trend, 1 = trend, 2 = trend & trend^2    
                             shock_type     = 0,       # 0 = standard deviation shock, 1 = unit shock
                             confint        = 1.96,    # Width of confidence bands: # 1 = 68%, 1.67 = 90%, 1.96 = 95%
                             use_nw         = TRUE,
                             hor            = 24)      # Number of cores to use. When NULL, the number of cores 

# Is chosen automatically 
linear_plots <- plot_lin(results_lin_brazil)

# Show all plots 
lin_plots_all <- sapply(linear_plots, ggplotGrob)
marrangeGrob(lin_plots_all,
             nrow = ncol(endog_data_brazil_all), 
             ncol = ncol(endog_data_brazil_all),
             top = NULL)


################## Linear impulse responses with local projections: Russia ##################
# Set the working directory
setwd("NARFIMA/Dataset/Dataset_IRF")
getwd()
 
# Dataset
all_data_russia <- read.csv("Russia_Data_IRF.csv", header = TRUE)
str(all_data_russia)

# Convert Date into Datetime Value
all_data_russia$Date <- as.Date(all_data_russia$Date)
str(all_data_russia)

endog_data_russia_all <- all_data_russia[,c("spot_ER_Russia", 
                                            "global_EPU_PPP",
                                            "gprc_russia",
                                            "US_EMV",
                                            "US_MPU")]
str(endog_data_russia_all)

results_lin_russia <- lp_lin(endog_data_russia_all, 
                             lags_endog_lin = NaN,      # Number of lags for endogenous data
                             lags_criterion = 'BIC', 
                             max_lags       = 12,
                             trend          = 1,        # 0 = no trend, 1 = trend, 2 = trend & trend^2    
                             shock_type     = 0,        # 0 = standard deviation shock, 1 = unit shock
                             confint        = 1.96,     # Width of confidence bands: # 1 = 68%, 1.67 = 90%, 1.96 = 95%
                             use_nw         = TRUE,
                             hor            = 24)       # Number of cores to use. When NULL, the number of cores 

# Is chosen automatically 
linear_plots <- plot_lin(results_lin_russia)

# Show all plots 
lin_plots_all <- sapply(linear_plots, ggplotGrob)
marrangeGrob(lin_plots_all,
             nrow = ncol(endog_data_russia_all), 
             ncol = ncol(endog_data_russia_all),
             top = NULL)



################## Linear impulse responses with local projections: India ##################
# Set the working directory
setwd("NARFIMA/Dataset/Dataset_IRF")
getwd()
 
# Dataset
all_data_india <- read.csv("India_Data_IRF.csv", header = TRUE)
str(all_data_india)

# Convert Date into Datetime Value
all_data_india$Date <- as.Date(all_data_india$Date)
str(all_data_india)

endog_data_india_all <- all_data_india[,c("spot_ER_India", 
                                          "global_EPU_PPP",
                                          "gprc_india",
                                          "US_EMV",
                                          "US_MPU")]
str(endog_data_india_all)

results_lin_india <- lp_lin(endog_data_india_all, 
                            lags_endog_lin = NaN,      # Number of lags for endogenous data
                            lags_criterion = 'BIC', 
                            max_lags       = 12,
                            trend          = 1,        # 0 = no trend, 1 = trend, 2 = trend & trend^2    
                            shock_type     = 0,        # 0 = standard deviation shock, 1 = unit shock
                            confint        = 1.96,     # Width of confidence bands: # 1 = 68%, 1.67 = 90%, 1.96 = 95%
                            use_nw         = TRUE,
                            hor            = 24)       # Number of cores to use. When NULL, the number of cores 

# Is chosen automatically 
linear_plots <- plot_lin(results_lin_india)

# Show all plots 
lin_plots_all <- sapply(linear_plots, ggplotGrob)
marrangeGrob(lin_plots_all,
             nrow = ncol(endog_data_india_all), 
             ncol = ncol(endog_data_india_all),
             top = NULL)



################## Linear impulse responses with local projections: China ##################
# Set the working directory
setwd("/Exchange_Rate/Datasets")
getwd()
 
# Dataset
all_data_china <- read.csv("China_Data_IRF.csv", header = TRUE)
str(all_data_china)

# Convert Date into Datetime Value
all_data_china$Date <- as.Date(all_data_china$Date)
str(all_data_china)

endog_data_china_all <- all_data_china[,c("spot_ER_China", 
                                          "global_EPU_PPP",
                                          "gprc_china",
                                          "US_EMV",
                                          "US_MPU")]
str(endog_data_china_all)

results_lin_china <- lp_lin(endog_data_china_all, 
                            lags_endog_lin = NaN,      # Number of lags for endogenous data
                            lags_criterion = 'BIC', 
                            max_lags       = 12,
                            trend          = 1,        # 0 = no trend, 1 = trend, 2 = trend & trend^2    
                            shock_type     = 0,        # 0 = standard deviation shock, 1 = unit shock
                            confint        = 1.96,     # Width of confidence bands: # 1 = 68%, 1.67 = 90%, 1.96 = 95%
                            use_nw         = TRUE,
                            hor            = 24)       # Number of cores to use. When NULL, the number of cores 

# Is chosen automatically 
linear_plots <- plot_lin(results_lin_china)

# Show all plots 
lin_plots_all <- sapply(linear_plots, ggplotGrob)
marrangeGrob(lin_plots_all,
             nrow = ncol(endog_data_china_all), 
             ncol = ncol(endog_data_china_all),
             top = NULL)
