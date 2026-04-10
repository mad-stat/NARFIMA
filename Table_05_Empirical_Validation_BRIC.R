library(forecast)
library(tidyverse)
library(readxl)

setwd('NARFIMA/Dataset/Dataset_Selected_Exogenous')
getwd()



##################################################### Bootstrapping Function #####################################################

bootstrap_narfima_params <- function(train_data, error_data, p, q, size, skip = TRUE, xreg_data, sy_idx, ser_idx, iterations = 100, decay = 0, rang = 0.7) {
  
  sy_matrix <- matrix(NA, nrow = iterations, ncol = length(sy_idx))
  ser_matrix <- matrix(NA, nrow = iterations, ncol = length(ser_idx))
  
  for(i in 1:iterations) {
    current_seed <- (i * 123) + 456 
    set.seed(current_seed)
    
    fit <- auto_narfima(train_data, error_data, p = p, q = q, size = size, skip = skip, xreg = xreg_data, lambda = 0, lambdae = 0, repeats = 100, seed = current_seed, decay = decay, rang = rang)
    
    # Extract weights from model in the average ensemble
    sy_matrix[i, ] <- fit$model[[1]]$wts[sy_idx]
    ser_matrix[i, ] <- fit$model[[1]]$wts[ser_idx]
    
    if(i %% 100 == 0) message(paste("Processed iteration", i))
  }
  
  # Calculate Medians
  median_sy <- apply(sy_matrix, 2, median)
  median_ser <- apply(ser_matrix, 2, median)
  
  # Calculate Assumptions 
  assump_3 <- sum(median_sy) + sum(median_ser)
  assump_5 <- abs(sum(median_sy))
  
  sy_string <- paste0("(", paste(round(median_sy, 3), collapse = ","), ")")
  ser_string <- paste0("(", paste(round(median_ser, 3), collapse = ","), ")")
  
  return(list(
    AR_Weights = sy_string,
    Resid_Weights = ser_string,
    A3 = round(assump_3, 3),
    A5 = round(assump_5, 3)
  ))
}


auto_narfima <- function(y, er, xreg = NULL, p, q, P = 1, size, skip, repeats = 1000, lambda = 0.5, lambdae = 0.5, scale.inputs = TRUE, seed, ...) {
  
  yname <- deparse(substitute(y))
  
  if(missing(seed)){
    seed <- 100
  }
  
  if(missing(er)){
    arfima_model <- arfima(as.ts(y), xreg = if (!is.null(xreg)) as.ts(xreg) else NULL)
    arfima_er <-  residuals(arfima_model)
    arfima_er[is.na(arfima_er)] <-  0
    er <- arfima_er
    e <- er
  }
  
  e <- er
  x <- y
  
  # Check for NAs in y
  if (any(is.na(x))) {
    warning("Missing values in y, omitting rows")
  }
  
  
  # Transform data
  if (!is.null(lambda)) {
    xx <- BoxCox(x, lambda)
    lambda <- attr(xx, "lambda")
  } else {
    xx <- x
  }
  
  
  # Transform error
  if (!is.null(lambdae)) {
    ee <- BoxCox(e, lambdae)
    lambdae <- attr(ee, "lambdae")
  } else {
    ee <- e
  }
  
  
  # Scale series x and error
  scaley <- NULL
  if (scale.inputs) {
    
    tmpx <- scale(xx, center = TRUE, scale = TRUE)
    tmpe <- scale(ee, center = TRUE, scale = TRUE)
    scaley <- list(
      center = attr(tmpx, "scaled:center"),
      scale = attr(tmpx, "scaled:scale")
    )
    scalee <- list(
      center = attr(tmpe, "scaled:center"),
      scale = attr(tmpe, "scaled:scale")
    )
    
    xx <- scale(xx, center = scaley$center, scale = scaley$scale)
    xx <- xx[, 1]
    ee <- scale(ee, center = scalee$center, scale = scalee$scale)
    ee <- ee[, 1]
  }
  
  
  # Check xreg class & dim
  xxreg <- NULL
  scalexreg <- NULL
  
  if (!is.null(xreg)) {
    xxreg <- xreg <- as.matrix(xreg)
    if (length(x) != NROW(xreg)) {
      stop("Number of rows in xreg does not match series length")
    }
    
    # Check for NAs in xreg
    if (any(is.na(xreg))) {
      warning("Missing values in xreg, omitting rows")
    }
    
    # Scale xreg
    if (scale.inputs) {
      tmpx <- scale(xxreg, center = TRUE, scale = TRUE)
      scalexreg <- list(
        center = attr(tmpx, "scaled:center"),
        scale = attr(tmpx, "scaled:scale")
      )
      
      xxreg <- scale(xxreg, center = scalexreg$center, scale = scalexreg$scale)
    }
  }
  
  
  # Set up lagged matrix
  n <- length(xx)
  xx <- as.ts(xx)
  m <- max(round(frequency(xx)), 1L)
  
  
  if (m == 1) {
    
    if (missing(p)) {
      p <- max(length(ar(na.interp(xx))$ar), 1)
    }
    
    if (missing(q)) {
      if (!missing(er)){
        arfima_model <- arfima(as.ts(y), xreg = if (!is.null(xreg)) as.ts(xreg) else NULL)
        q <- max(order(arfima_model$ma),0)
      }
      
      else{
        q <- max(order(arfima_model$ma),0)
      }
    }
    
    if(missing(size)){
      size <- floor((q + p) / 2)
    }
    
    # For non-seasonal data also use default calculation for p if that argument is 0, but issue a warning
    if (p == 0){
      warning("Cannot set p = 0 for non-seasonal data; using default calculation for p")
      p <- max(length(ar(na.interp(xx))$ar), 1)
    }
    
    if (q == 0){
      warning("Cannot set q = 0; setting q to 1")
      q <- 1
    }
    
    if (p >= n) {
      warning("Reducing number of lagged inputs due to short series")
      p <- n - 1
    }
    
    lags <- seq_len(p)
    lagse <- seq_len(q)
    
    
    if (P > 1) {
      warning("Non-seasonal data, ignoring seasonal lags")
    }
    
    P <- 0
  }
  
  
  # Seasonal data
  else {
    
    if (missing(p)) {
      
      if (n > 2 * m) {
        x.sa <- seasadj(mstl(forecast::na.interp(xx)))
      }
      
      else {
        x.sa <- na.interp(xx)
      }
      
      p <- max(length(ar(x.sa)$ar), 1)
    }
    
    if (missing(q)) {
      q <- max(order(arfima_model$ma),0)
    }
    
    if(missing(size)){
      size <- floor((q + p) / 2)
    }
    
    
    if (p == 0 && P == 0){
      stop("'p' and 'P' cannot both be zero")
    }
    
    
    if (p >= n) {
      warning("Reducing number of lagged inputs due to short series")
      p <- n - 1
    }
    
    
    if (P > 0 && n >= m * P + 2) {
      lagse <- sort(unique(c(seq_len(q), m * (seq_len(P)))))
      lags <- seq_len(p)
    }
    
    else {
      lagse <- seq_len(q)
      lags <- seq_len(p)
      
      if (P > 0) {
        warning("Series too short for seasonal lagse")
        P <- 0
      }
    }
  }
  
  
  if(missing(skip)){
    skip <- TRUE
  }
  
  
  # Setting up lagged matrices for Y and Errors
  maxlage <- max(lagse)
  maxlage <- ifelse(maxlage < 0, 0, maxlage)
  nlage <- length(lagse)
  maxlag <- max(lags)
  nlag <- length(lags)
  
  er <- ee[-(1:(max(maxlag, maxlage)))]
  y <- xx[-(1:(max(maxlag, maxlage)))]
  
  
  # lagged matrix for x(y)
  lags.X <- matrix(NA_real_, ncol = nlag, nrow = n - maxlag)
  for (i in 1:nlag)
    lags.X[, i] <- xx[(maxlag - lags[i] + 1):(n - lags[i])]
  
  
  # lagged matrix for er
  lags.E <- matrix(NA_real_, ncol = nlage, nrow = n - maxlage)
  for (i in 1:nlage)
    lags.E[, i] <- ee[(maxlage - lagse[i] + 1):(n - lagse[i])]
  
  max_lag_all = max(maxlag, maxlage)
  
  if (p >= q){
    lags.E = lags.E[(p-q+1):nrow(lags.E),]
  }
  
  if(p < q){
    lags.X = lags.X[(q-p+1):nrow(lags.X),]
  }
  
  
  # Combining lags of y and errors
  lags.X <- cbind(lags.X, lags.E)
  
  # Add xreg into lagged matrix
  lags.X <- cbind(lags.X, xxreg[-(1:max_lag_all), , drop = FALSE])
  
  
  # Remove missing values if present
  j <- complete.cases(lags.X, y)
  
  
  # Stop if there's no data to fit (e.g. due to NAs or NaNs)
  if (NROW(lags.X[j,, drop=FALSE]) == 0) {
    stop("No data to fit (possibly due to NA or NaN)")
  }
  
  
  set.seed(seed)
  
  # Passing the value(y and err combine matrix) to average on nnet function
  if(skip == FALSE){
    fit <- avnnet(lags.X[j, , drop = FALSE], y[j], size = size, repeats = repeats, ...)
  }
  
  
  if(skip == TRUE){
    fit <- avnnet_T(lags.X[j, , drop = FALSE], y[j], size = size, repeats = repeats, ...)
  }
  
  set.seed(seed)
  
  # To return the output
  out <- list()
  out$y <- as.ts(x)
  out$e <- as.ts(e)
  out$m <- m
  out$p <- p
  out$q <- q
  out$P <- P
  out$scaley <- scaley
  out$scalee <- scalee
  out$scalexreg <- scalexreg
  out$size <- size
  out$xreg <- xreg
  out$skip <- skip
  out$seed <-  seed
  out$lambda <- lambda
  out$lambdae <- lambdae
  out$model <- fit
  out$nnetargs <- list(...)
  
  if (NROW(lags.X[j,, drop = FALSE]) == 1){
    print("coe")
    fits <- c(rep(NA_real_, maxlag), mean(sapply(fit, predict)))
  } else{
    fits <- c(rep(NA_real_, max(maxlag, maxlage)), rowMeans(sapply(fit, predict)))
  }
  
  
  if (scale.inputs) {
    fits <- fits * scaley$scale + scaley$center
  }
  
  
  fits <- ts(fits)
  
  
  if (!is.null(lambda)) {
    fits <- InvBoxCox(fits, lambda)
  }
  
  out$fitted <- ts(rep(NA_real_, length(out$y)))
  out$fitted[c(rep(TRUE, max(maxlag, maxlage)), j)] <- fits
  tsp(out$fitted) <- tsp(out$y)
  out$residuals <- out$y - out$fitted
  out$lags <- lags
  out$lagse <- lagse
  out$series <- yname
  out$method <- paste("NARFIMA(", p,",",q, sep = "")
  
  if (P > 0) {
    out$method <- paste(out$method, ",", P, sep = "")
  }
  out$method <- paste(out$method, ",", size, "," , skip,")", sep = "")
  if (P > 0) {
    out$method <- paste(out$method, "[", m, "]", sep = "")
  }
  out$call <- match.call()
  return(structure(out, class = c("narfima")))
}



# Aggregate several neural network models
avnnet <- function(x, y, repeats, linout = TRUE, trace = FALSE, ...) {
  mods <- list()
  for (i in 1:repeats) {
    # Ensure each repeat has a unique initialization based on the parent seed
    set.seed(runif(1, 0, 1e8) + i) 
    mods[[i]] <- nnet::nnet(x, y, linout = linout, trace = trace, ...)
  }
  return(structure(mods, class = "nnetarmodels"))
}

# Aggregate several neural network models 
avnnet_T <- function(x, y, repeats, linout = TRUE, trace = FALSE, ...) {
  mods <- list()
  for (i in 1:repeats) {
    # Ensure each repeat has a unique initialization based on the parent seed
    set.seed(runif(1, 0, 1e8) + i)
    mods[[i]] <- nnet::nnet(x, y, linout = linout, trace = trace, skip = TRUE, ...)
  }
  return(structure(mods, class = "nnetarmodels"))
}

                               
##################################################### Brazil 48 #####################################################

data <- read_excel('Brazil_Data.xlsx') %>% rename('Exchange_Rate_braz' = spot_ER_Brazil)                   
exchange_rate_braz <- ts(data$Exchange_Rate_braz)
reg_braz <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)

n = 48
set.seed(100)
train_braz_48 <- subset(exchange_rate_braz, end = length(exchange_rate_braz) - n) 
train_reg_braz_48 <- reg_braz[1:length(train_braz_48),]

set.seed(100)
arfima_braz_48 <- arfima(train_braz_48, xreg = train_reg_braz_48)
arfima_er_braz_48 <-  residuals(arfima_braz_48)
arfima_er_braz_48[is.na(arfima_er_braz_48)] <-  0

set.seed(100)
braz_48 <- bootstrap_narfima_params(train_data = train_braz_48, error_data = arfima_er_braz_48, p = 4, q = 2, size = 1, skip = TRUE, xreg = train_reg_braz_48, sy_idx = 14:17, ser_idx = 18:19, iterations = 1000, decay = 0.005, rang = 3)
braz_48
                          
##################################################### Russia 12 #####################################################

data <- read_excel('Russia_Data.xlsx') %>% rename('Exchange_Rate_rus' = spot_ER_Russia)                   
exchange_rate_rus <- ts(data$Exchange_Rate_rus)
reg_rus <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)
     
n = 12
set.seed(100)
train_rus_12 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
train_reg_rus_12 <- reg_rus[1:length(train_rus_12),]

set.seed(100)
arfima_rus_12 <- arfima(train_rus_12, xreg = train_reg_rus_12)
arfima_er_rus_12 <-  residuals(arfima_rus_12)
arfima_er_rus_12[is.na(arfima_er_rus_12)] <-  0

set.seed(100)
rus_12 <- bootstrap_narfima_params(train_data = train_rus_12, error_data = arfima_er_rus_12, p = 5, q = 2, size = 5, skip = TRUE, xreg = train_reg_rus_12, sy_idx = 71:75, ser_idx = 76:77, iterations = 1000, decay = 0, rang = 0.7)
rus_12
                      
##################################################### Russia 24 #####################################################
     
n = 24
set.seed(100)
train_rus_24 <- subset(exchange_rate_rus, end = length(exchange_rate_rus) - n) 
train_reg_rus_24 <- reg_rus[1:length(train_rus_24),]
     
set.seed(100)
arfima_rus_24 <- arfima(train_rus_24, xreg = train_reg_rus_24)
arfima_er_rus_24 <-  residuals(arfima_rus_24)
arfima_er_rus_24[is.na(arfima_er_rus_24)] <-  0

set.seed(100)
rus_24 <- bootstrap_narfima_params(train_data = train_rus_24, error_data = arfima_er_rus_24, p = 1, q = 1, size = 5, skip = TRUE, xreg = train_reg_rus_24, sy_idx = 46, ser_idx = 47, iterations = 1000, decay = 0, rang = 0.7)
rus_24

##################################################### India 12 #####################################################

data <- read_excel('India_Data.xlsx') %>% rename('Exchange_Rate_ind' = spot_ER_India)                   
exchange_rate_ind <- ts(data$Exchange_Rate_ind)
reg_ind <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5) 

n = 12
set.seed(100)
train_ind_12 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
train_reg_ind_12 <- reg_ind[1:length(train_ind_12),]

set.seed(100)
arfima_ind_12 <- arfima(train_ind_12, xreg = train_reg_ind_12)
arfima_er_ind_12 <-  residuals(arfima_ind_12)
arfima_er_ind_12[is.na(arfima_er_ind_12)] <-  0

set.seed(100)
ind_12 <- bootstrap_narfima_params(train_data = train_ind_12, error_data = arfima_er_ind_12, p = 1, q = 3, size = 4, skip = TRUE, xreg = train_reg_ind_12, sy_idx = 45, ser_idx = 46:48, iterations = 1000, decay = 0, rang = 0.7)
ind_12

##################################################### India 24 #####################################################

n = 24
set.seed(100)
train_ind_24 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n) 
train_reg_ind_24 <- reg_ind[1:length(train_ind_24),]    

set.seed(100)
arfima_ind_24 <- arfima(train_ind_24, xreg = train_reg_ind_24)
arfima_er_ind_24 <-  residuals(arfima_ind_24)
arfima_er_ind_24[is.na(arfima_er_ind_24)] <-  0

set.seed(100)
ind_24 <- bootstrap_narfima_params(train_data = train_ind_24, error_data = arfima_er_ind_24, p = 5, q = 4, size = 1, skip = TRUE, xreg = train_reg_ind_24, sy_idx = 17:21, ser_idx = 22:25, iterations = 1000, decay = 0, rang = 0.7)
ind_24
                               
##################################################### India 48 #####################################################

n = 48
set.seed(100)
train_ind_48 <- subset(exchange_rate_ind, end = length(exchange_rate_ind) - n)
train_reg_ind_48 <- reg_ind[1:length(train_ind_48),]

set.seed(100)
arfima_ind_48 <- arfima(train_ind_48, xreg = train_reg_ind_48)
arfima_er_ind_48 <-  residuals(arfima_ind_48)
arfima_er_ind_48[is.na(arfima_er_ind_48)] <-  0

set.seed(100)
ind_48 <- bootstrap_narfima_params(train_data = train_ind_48, error_data = arfima_er_ind_48, p = 2, q = 4, size = 4, skip = TRUE, xreg = train_reg_ind_48, sy_idx = 53:54, ser_idx = 55:58, iterations = 1000, decay = 0, rang = 0.7)
ind_48
     
##################################################### China 12 #####################################################
     
data <- read_excel('China_Data.xlsx') %>% rename('Exchange_Rate_chn' = spot_ER_China)                   
exchange_rate_chn <- ts(data$Exchange_Rate_chn)
reg_chn <- as.matrix(data[,c(4,3,5,6,7)], ncol = 5)

n = 12
set.seed(100)
train_chn_12 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
train_reg_chn_12 <- reg_chn[1:length(train_chn_12),]   

set.seed(100)
arfima_chn_12 <- arfima(train_chn_12, xreg = train_reg_chn_12)
arfima_er_chn_12 <-  residuals(arfima_chn_12)
arfima_er_chn_12[is.na(arfima_er_chn_12)] <-  0

set.seed(100)
chn_12 <- bootstrap_narfima_params(train_data = train_chn_12, error_data = arfima_er_chn_12, p = 5, q = 4, size = 1, skip = TRUE, xreg = train_reg_chn_12, sy_idx = 17:21, ser_idx = 22:25, iterations = 1000, decay = 0, rang = 0.7)
chn_12
     
##################################################### China 24 #####################################################

n = 24
set.seed(100)
train_chn_24 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
train_reg_chn_24 <- reg_chn[1:length(train_chn_24),]

set.seed(100)
arfima_chn_24 <- arfima(train_chn_24, xreg = train_reg_chn_24)
arfima_er_chn_24 <-  residuals(arfima_chn_24)
arfima_er_chn_24[is.na(arfima_er_chn_24)] <-  0

set.seed(100)
chn_24 <- bootstrap_narfima_params(train_data = train_chn_24, error_data = arfima_er_chn_24, p = 1, q = 2, size = 4, skip = TRUE, xreg = train_reg_chn_24, sy_idx = 41, ser_idx = 42:43, iterations = 1000, decay = 0, rang = 0.7)
chn_24

##################################################### China 48 #####################################################

n = 48
set.seed(100)
train_chn_48 <- subset(exchange_rate_chn, end = length(exchange_rate_chn) - n) 
train_reg_chn_48 <- reg_chn[1:length(train_chn_48),]

set.seed(100)
arfima_chn_48 <- arfima(train_chn_48, xreg = train_reg_chn_48)
arfima_er_chn_48 <-  residuals(arfima_chn_48)
arfima_er_chn_48[is.na(arfima_er_chn_48)] <-  0

set.seed(100)
chn_48 <- bootstrap_narfima_params(train_data = train_chn_48, error_data = arfima_er_chn_48, p = 4, q = 1, size = 2, skip = TRUE, xreg = train_reg_chn_48, sy_idx = 25:28, ser_idx = 29, iterations = 1000, decay = 0, rang = 0.7)
chn_48
