library(forecast)
library(tidyverse)
library(Metrics)
library(ggplot2)
library(tsDyn)
library(forecastHybrid)
library(WaveletArima)
library(FinTS)
library(tseries)
library(rugarch)
library(bsts)
library(readxl)
library(pracma)
library(e1071)
library(tseries)
library(nonlinearTseries)
library(seastests)
library(car)


simulate.narfima <- function(object, nsim = length(object$x), seed = NULL, xreg = NULL, future = TRUE, bootstrap = FALSE, innov = NULL, lambda = object$lambda, lambdae = object$lambdae...) {
  
  if (is.null(innov)) {
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      runif(1)
    }
    if (is.null(seed)) {
      RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    } else {
      R.seed <- get(".Random.seed", envir = .GlobalEnv)
      set.seed(seed)
      RNGstate <- structure(seed, kind = as.list(RNGkind()))
      on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
  } else {
    nsim <- length(innov)
  }
  
  if (is.null(object$x)) {
    future <- FALSE
  }
  
  
  ## only future currently implemented
  if (!future) {
    warning("simulate.nnetar() currently only supports future=TRUE")
  }
  
  
  ## set simulation innovations
  if (bootstrap) {
    res <- na.omit(c(residuals(object, type = "innovation")))
    res <- res - mean(res)
    
    ## scale if appropriate
    if (!is.null(object$scalex$scale)) {
      res <- res / object$scalex$scale
    }
    e <- sample(res, nsim, replace = TRUE)
  } else if (is.null(innov)) {
    res <- na.omit(c(residuals(object, type = "innovation")))
    
    ## scale if appropriate
    if (!is.null(object$scalex$scale)) {
      res <- res / object$scalex$scale
    }
    e <- rnorm(nsim, 0, sd(res, na.rm = TRUE))
  } else if (length(innov) == nsim) {
    e <- innov
    if (!is.null(object$scalex$scale)) {
      e <- e / object$scalex$scale
    }
  } else if (isTRUE(innov == 0L)) {
    ## to pass innov=0 so simulation equals mean forecast
    e <- rep(innov, nsim)
  } else {
    stop("Length of innov must be equal to nsim")
  }
  
  tspx <- tsp(object$x)
  
  
  # Check if xreg was used in fitted model
  if (is.null(object$xreg)) {
    if (!is.null(xreg)) {
      warning("External regressors were not used in fitted model, xreg will be ignored")
    }
    xreg <- NULL
  } else {
    if (is.null(xreg)) {
      stop("No external regressors provided")
    }
    xreg <- as.matrix(xreg)
    if (NCOL(xreg) != NCOL(object$xreg)) {
      stop("Number of external regressors does not match fitted model")
    }
    if (NROW(xreg) != nsim) {
      stop("Number of rows in xreg does not match nsim")
    }
  }
  
  
  xx <- object$x
  if (!is.null(lambda)) {
    xx <- BoxCox(xx, lambda)
    lambda <- attr(xx, "lambda")
  }
  
  
  ee <- object$e 
  if (!is.null(lambdae)) {
    ee <- BoxCox(ee, lambdae)
    lambdae <- attr(ee, "lambdae")
  }
  
  
  # Check and apply scaling of fitted model
  if (!is.null(object$scalex)) {
    xx <- scale(xx, center = object$scalex$center, scale = object$scalex$scale)
    if (!is.null(xreg)) {
      xreg <- scale(xreg, center = object$scalexreg$center, scale = object$scalexreg$scale)
    }
  }
  
  
  if (!is.null(object$scalee)) {
    ee <- scale(ee, center = object$scalee$center, scale = object$scalee$scale)
  } 
  
  
  # Get lags used in fitted model
  lags <- object$lags
  maxlag <- max(lags)
  flag <- rev(tail(xx, n = maxlag))
  
  lagse <- object$lagse
  maxlagse <- max(lagse)
  flage <- rev(tail(ee, n = maxlagse)) 
  
  
  ## Simulate by iteratively forecasting and adding innovation
  path <- numeric(nsim)
  
  for (i in 1:nsim) {
    newdata <- c(flag[lags],flage[lagse], xreg[i, ])
    if (any(is.na(newdata))) {
      stop("I can't simulate when there are missing values near the end of the series.")
    }
    path[i] <- mean(sapply(object$model, predict, newdata = newdata)) + e[i]
    flag <- c(path[i], flag[-maxlag])
    flage <- c(path[i], flage) 
  }
  
  
  ## Re-scale simulated points
  if (!is.null(object$scalex)) {
    path <- path * object$scalex$scale + object$scalex$center
  }
  
  
  ## Add ts properties
  path <- ts(path, start = tspx[2] + 1 / tspx[3], frequency = tspx[3])
  
  
  ## Back-transform simulated points
  if (!is.null(lambda)) {
    path <- InvBoxCox(path, lambda)
  }
  return(path)
}



future_msts <- function(x, y) {
  if(NCOL(y) > 1) {
    class(y) <- c("mts", "ts", "matrix")
  } else {
    class(y) <- "ts"
  }
  
  
  if("msts" %in% class(x))
    class(y) <- c("msts", class(y))
  attr <- attributes(x)
  attr$tsp[1:2] <- attr$tsp[2] + c(1,NROW(y))/attr$tsp[3]
  attributes(y)$tsp <- attr$tsp
  attributes(y)$msts <- attr$msts
  return(y)
}



auto.narfima <- function(y, er, p, q, P = 1, size = NULL, skip, seed, repeats = 1000, xreg = NULL, variable_selection = FALSE, te_threshold = NULL, lambda = 0.5, lambdae = 0.5, model = NULL, subset = NULL, scale.inputs = TRUE, e = er, x = y, ...) {
  
  yname <- deparse(substitute(y))
  
  if(missing(er)){
    arfima_model <- arfima(y, xreg = xreg)
    arfima_er <-  residuals(arfima_model)
    arfima_er[is.na(arfima_er)] <-  0
    er <- arfima_er
    e <- er
  }
  
  if(variable_selection == TRUE && is.null(xreg) == FALSE){
    if(is.null(te_threshold)){
      te_threshold <- .01
      if (!is.null(xreg)) {
        for (i in ncol(xreg):1) {  
          te_value <- calc_te(xreg[, i],y,1,1)  
          if (te_value < te_threshold) {
            xreg <- xreg[, -i, drop = FALSE]  
          }
        }
      }
    }
  }
  
  
  # Check for NAs in x
  if (any(is.na(x))) {
    warning("Missing values in x, omitting rows")
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
  
  
  # Scale series X and error
  scalex <- NULL
  if (scale.inputs) {
    
    tmpx <- scale(xx, center = TRUE, scale = TRUE)
    tmpe <- scale(ee, center = TRUE, scale = TRUE)
    scalex <- list(
      center = attr(tmpx, "scaled:center"),
      scale = attr(tmpx, "scaled:scale")
    )
    scalee <- list(
      center = attr(tmpe, "scaled:center"),
      scale = attr(tmpe, "scaled:scale")
    )
    
    xx <- scale(xx, center = scalex$center, scale = scalex$scale)
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
      #p <- ifelse(p > 5, 1, p)
    }
    
    if (missing(q)) {
      q <- max(order(arfima_model$ma),0)
      #q <- ifelse(q > 5 | q <= 0, 1, q)
    }
    
      if(is.null(size)){
        size <- floor((q + p) / 2)
    }
    
    # For non-seasonal data also use default calculation for p if that argument is 0, but issue a warning
    if (p == 0){
      warning("Cannot set p = 0 for non-seasonal data; using default calculation for p")
      p <- max(length(ar(na.interp(xx))$ar), 1)
      #p <- ifelse(p > 5, 1, p)
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
        x.sa <- seasadj(mstl(na.interp(xx)))
      } 
      
      else {
        x.sa <- na.interp(xx)
      }
      
      p <- max(length(ar(x.sa)$ar), 1)
      #p <- ifelse(p > 5, 1, p)
    }
    
    if (missing(q)) {
      q <- max(order(arfima_model$ma),0)
      #q <- ifelse(q > 5 | q <= 0, 1, q)
    }
    
    if(is.null(size)){
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
  
  if(missing(seed)){
    seed <- 100
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
    fit <- avnnet(lags.X[j, , drop = FALSE], y[j], size = size, repeats = repeats)
  }
  
  
  if(skip == TRUE){
    fit <- avnnet_T(lags.X[j, , drop = FALSE], y[j], size = size, repeats = repeats)
  }
  
  set.seed(seed)
  
  # To return the output
  out <- list()
  out$x <- as.ts(x)
  out$e <- as.ts(e)
  out$m <- m
  out$p <- p
  out$q <- q
  out$P <- P
  out$scalex <- scalex
  out$scalee <- scalee
  out$scalexreg <- scalexreg
  out$size <- size
  out$xreg <- xreg
  out$skip <- skip
  out$seed <-  seed
  out$lambda <- lambda
  out$lambdae <- lambdae
  out$VariableSelection <- variable_selection
  out$te_threshold <- te_threshold
  out$model <- fit
  out$nnetargs <- list(...)
  
  if (NROW(lags.X[j,, drop = FALSE]) == 1){
    print("coe")
    fits <- c(rep(NA_real_, maxlag), mean(sapply(fit, predict)))
  } else{
    fits <- c(rep(NA_real_, max(maxlag, maxlage)), rowMeans(sapply(fit, predict)))
  }
  
  
  if (scale.inputs) {
    fits <- fits * scalex$scale + scalex$center
  }
  
  
  fits <- ts(fits)
  
  
  if (!is.null(lambda)) {
    fits <- InvBoxCox(fits, lambda)
  }
  
  out$fitted <- ts(rep(NA_real_, length(out$x)))
  out$fitted[c(rep(TRUE, max(maxlag, maxlage)), j)] <- fits
  tsp(out$fitted) <- tsp(out$x)
  out$residuals <- out$x - out$fitted
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
  return(structure(out, class = c("nnetar")))
}


# Aggregate several neural network models
avnnet <- function(x, y, repeats, linout = TRUE, trace = FALSE, ...) {
  mods <- list()
  for (i in 1:repeats)
    mods[[i]] <- nnet::nnet(x, y, linout = linout, trace = trace, ...)
  return(structure(mods, class = "nnetarmodels"))
}



# Aggregate several neural network models
avnnet_T <- function(x, y, repeats, linout = TRUE, trace = FALSE, ...) {
  mods <- list()
  for (i in 1:repeats)
    mods[[i]] <- nnet::nnet(x, y, linout = linout, trace = trace, skip = TRUE, ...)
  return(structure(mods, class = "nnetarmodels"))
}



#' @export
print.nnetarmodels <- function(x, ...) {
  cat(paste("\nAverage of", length(x), "networks, each of which is\n"))
  print(x[[1]])
}



forecast.narfima <- function(object, h = ifelse(object$m > 1, 2 * object$m, 10), PI = FALSE, level = 80, fan = FALSE, xreg = NULL, lambda = object$lambda, lambdae = object$lambdae, bootstrap = FALSE, npaths = 1000, innov = NULL, ...) {
  
  out <- object
  tspx <- tsp(out$x)
  
  if (!is.null(xreg) && !is.null(out$xreg)) {
    selected_columns <- intersect(colnames(xreg), colnames(out$xreg))
    xreg <- xreg[, selected_columns, drop = FALSE]
  }
  
  
  if (fan) {
    level <- seq(51, 99, by = 3)
  } else {
    if (min(level) > 0 && max(level) < 1) {
      level <- 100 * level
    } else if (min(level) < 0 || max(level) > 99.99) {
      stop("Confidence limit out of range")
    }
  }
  
  
  # Check if xreg was used in fitted model
  if (is.null(object$xreg)) {
    if (!is.null(xreg)) {
      warning("External regressors were not used in fitted model, xreg will be ignored")
    }
    xreg <- NULL
  }
  
  else {
    if (is.null(xreg)) {
      stop("No external regressors provided")
    }
    
    xreg <- as.matrix(xreg)
    
    if (NCOL(xreg) != NCOL(object$xreg)) {
      stop("Number of external regressors does not match fitted model")
    }
    
    if(!identical(colnames(xreg), colnames(object$xreg))){
      warning("xreg contains different column names from the xreg used in training. Please check that the regressors are in the same order.")
    }
    
    h <- NROW(xreg)
  }
  
  
  fcast <- numeric(h)
  xx <- object$x
  ee <- object$e
  xxreg <- xreg
  
  
  if (!is.null(lambda)) {
    xx <- BoxCox(xx, lambda)
    lambda <- attr(xx, "lambda")
  }
  
  
  if (!is.null(lambdae)) {
    ee <- BoxCox(ee, lambdae)
    lambdae <- attr(ee, "lambdae")
  }
  
  
  # Check and apply scaling of fitted model
  if (!is.null(object$scalex)) {
    xx <- scale(xx, center = object$scalex$center, scale = object$scalex$scale)
    if (!is.null(xreg)) {
      xxreg <- scale(xreg, center = object$scalexreg$center, scale = object$scalexreg$scale)
    }
  }
  
  
  if (!is.null(object$scalee)) {
    ee <- scale(ee, center = object$scalee$center, scale = object$scalee$scale)
    if (!is.null(xreg)) {
      xxreg <- scale(xreg, center = object$scalexreg$center, scale = object$scalexreg$scale)
    }
  }
  
  
  # Get lags used in fitted model
  lags <- object$lags
  lagse <- object$lagse
  maxlag <- max(lags)
  maxlage <- max(lagse)
  flag <- rev(tail(xx, n = maxlag))
  flage <- rev(tail(ee, n = maxlage))
  
  
  # Iterative 1-step forecast
  for (i in 1:h){
    newdata <- c(flag[lags], flage[lagse], xxreg[i, ])
    if (any(is.na(newdata))) {
      stop("I can't forecast when there are missing values near the end of the series.")
    }
    fcast[i] <- mean(sapply(object$model, predict, newdata = newdata))
    flag <- c(fcast[i], flag[-maxlag])
    flage<- c(0, flage[-maxlage])
  }
  
  
  # Re-scale point forecasts
  if (!is.null(object$scalex)) {
    fcast <- fcast * object$scalex$scale + object$scalex$center
  }
  
  
  # Add ts properties
  fcast <- ts(fcast, start = tspx[2] + 1 / tspx[3], frequency = tspx[3])
  
  
  # Back-transform point forecasts
  if (!is.null(lambda)) {
    fcast <- InvBoxCox(fcast, lambda)
  }
  
  
  # Compute prediction intervals using simulations
  if (isTRUE(PI)) {
    nint <- length(level)
    sim <- matrix(NA, nrow = npaths, ncol = h)
    
    if (!is.null(innov)) {
      if (length(innov) != h * npaths) {
        stop("Incorrect number of innovations, need h*npaths values")
      }
      innov <- matrix(innov, nrow = h, ncol = npaths)
      bootstrap <- FALSE
    }
    
    for (i in 1:npaths)
      sim[i, ] <- simulate.narfima(object, nsim = h, bootstrap = bootstrap, xreg = xreg, lambda = lambda, innov = innov[, i], ...)
    lower <- apply(sim, 2, quantile, 0.5 - level / 200, type = 8, na.rm = TRUE)
    upper <- apply(sim, 2, quantile, 0.5 + level / 200, type = 8, na.rm = TRUE)
    
    if (nint > 1L) {
      lower <- ts(t(lower))
      upper <- ts(t(upper))
    }
    
    else {
      lower <- ts(matrix(lower, ncol = 1L))
      upper <- ts(matrix(upper, ncol = 1L))
    }
    out$lower <- future_msts(out$x, lower)
    out$upper <- future_msts(out$x, upper)
  }
  
  
  else {
    level <- NULL
    lower <- NULL
    upper <- NULL
  }
  
  out$mean <- future_msts(out$x, fcast)
  out$level <- level
  
  return(structure(out, class = "forecast"))
} 



#' @rdname fitted.Arima
#' @export
fitted.nnetar <- function(object, h=1, ...) {
  if (h == 1) {
    return(object$fitted)
  }
  else {
    return(hfitted(object = object, h = h, FUN = "nnetar", ...))
  }
}



#' @export
print.nnetar <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Series:", x$series, "\n")
  cat("Model: ", x$method, "\n")
  # cat("  one hidden layer with",x$size,"nodes\n")
  cat("Call:   ")
  print(x$call)
  print(x$model)
  cat(
    "\nsigma^2 estimated as ", format(mean(residuals(x) ^ 2, na.rm = TRUE), digits = digits),
    "\n", sep = ""
  )
  invisible(x)
}



#' @rdname is.ets
#' @export
is.nnetar <- function(x) {
  inherits(x, "nnetar")
}



#' @rdname is.ets
#' @export
is.nnetarmodels <- function(x) {
  inherits(x, "nnetarmodels")
}



# Scale a univariate time series
scale.ts <- function(x, center=TRUE, scale=TRUE) {
  tspx <- tsp(x)
  x <- as.ts(scale.default(x, center = center, scale = scale))
  tsp(x) <- tspx
  return(x)
}
