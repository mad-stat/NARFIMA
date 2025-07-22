library(tsutils)
library(PMCMRplus)
library(vioplot)
library(greybox)
library(stats)
library(MTS)
library(forecast)
library(murphydiagram)
library(readxl)
library(dplyr)

par(mfrow = c(1,1), mfcol = c(1,1))
par(mgp = c(3, 1, 0))
par(mar = c(5.1, 4.1, 4.1, 2.1))

setwd('NARFIMA/Dataset/MCB')
getwd()

##################################################### BRIC RMSE #####################################################

# Read the data
bric <- read_excel('BRIC MCB RMSE.xlsx')

# Extract the errors
abs_err_bric <- bric[-1]

# MCB Plot
tsutils::nemenyi(abs_err_bric, plottype = "mcb", main = 'BRIC MCB Plot for RMSE Metric')


##################################################### BRIC SMAPE #####################################################

# Read the data
bric <- read_excel('BRIC MCB SMAPE.xlsx')

# Extract the errors
abs_err_bric <- bric[-1]

# MCB Plot
tsutils::nemenyi(abs_err_bric, plottype = "mcb", main = 'BRIC MCB Plot for SMAPE Metric')



##################################################### BRIC MAE #####################################################

# Read the data
bric <- read_excel('BRIC MCB MAE.xlsx')

# Extract the errors
abs_err_bric <- bric[-1]

# MCB Plot
tsutils::nemenyi(abs_err_bric, plottype = "mcb", main = 'BRIC MCB Plot for MAE Metric')
