################################################################################################
## Long-memory estimators and tests used in the SNDE revision (reviewer Comment 2a)
## Written from the original papers; validated by Monte Carlo in long_memory_montecarlo.R.
##
##  periodogram()      I(lambda_j) = |sum_t x_t exp(-i t lambda_j)|^2 / (2 pi n), lambda_j = 2 pi j / n
##  frac_diff()        (1 - L)^d x_t with x_s = 0 for s <= 0 (type-II), fast FFT version
##                     (Jensen & Nielsen, 2014)
##  gph()              log-periodogram regression (Geweke & Porter-Hudak, 1983; Robinson, 1995b):
##                     log I_j = c - d log(4 sin^2(lambda_j / 2)) + u_j,  j = 1..m;  s.e. = pi / sqrt(24 m)
##  gph_bias_reduced() Andrews & Guggenberger (2003): adds lambda_j^(2k), k = 1..r, as regressors;
##                     asymptotic variance inflated by c_r (c_1 = 2.25)
##  local_whittle()    Robinson (1995a): R(d) = log( mean_j lambda_j^(2d) I_j ) - 2 d mean_j log lambda_j;
##                     s.e. = 1 / (2 sqrt(m))
##  elw()              exact local Whittle (Shimotsu & Phillips, 2005):
##                     R(d) = log( mean_j I_{Delta^d x}(lambda_j) ) - 2 d mean_j log lambda_j
##                     applied to x_t - x_1 (initial-value correction); s.e. = 1 / (2 sqrt(m))
##  felw()             feasible ELW with unknown mean (Shimotsu, 2010): x_t - mu(d),
##                     mu(d) = w(d) mean(x) + (1 - w(d)) x_1, w(d) = 1 (d <= 1/2),
##                     (1 + cos(4 pi d)) / 2 (1/2 < d < 3/4), 0 (d >= 3/4); minimised by a fine grid
##                     search followed by local refinement, which returns the global minimiser.
##  modified_rs()      Lo's (1991) modified rescaled range, Andrews (1991) data-dependent lag
##  qu_test()          Qu (2011) test of true long memory against spurious long memory
##                     (local Whittle score process, trimming epsilon)
##  whittle_bounded()  re-run of the original longmemo::WhittleEst call used for the published table
################################################################################################

periodogram <- function(x) {
  n <- length(x)
  f <- fft(x)
  j <- 1:floor(n / 2)
  list(lambda = 2 * pi * j / n, I = (Mod(f[j + 1])^2) / (2 * pi * n))
}

frac_diff <- function(x, d) {
  n <- length(x)
  np2 <- nextn(2 * n - 1, 2)
  k <- 1:(n - 1)
  b <- c(1, cumprod((k - d - 1) / k))                 # coefficients of (1 - L)^d
  dx <- fft(fft(c(b, rep(0, np2 - n))) * fft(c(x, rep(0, np2 - n))), inverse = TRUE) / np2
  Re(dx[1:n])
}

gph <- function(x, m) {
  p <- periodogram(x)
  lam <- p$lambda[1:m]
  reg <- -log(4 * sin(lam / 2)^2)
  fit <- lm(log(p$I[1:m]) ~ reg)
  list(d = unname(coef(fit)[2]), se = pi / sqrt(24 * m),
       se_ols = unname(summary(fit)$coefficients[2, 2]), m = m)
}

gph_bias_reduced <- function(x, m, r = 1) {
  c_r <- c(1, 2.25, 3.52, 4.79)[r + 1]                 # Andrews & Guggenberger (2003), variance inflation
  p <- periodogram(x)
  lam <- p$lambda[1:m]
  X <- cbind(-log(4 * sin(lam / 2)^2), sapply(seq_len(r), function(k) lam^(2 * k)))
  fit <- lm(log(p$I[1:m]) ~ X)
  list(d = unname(coef(fit)[2]), se = sqrt(c_r * pi^2 / (24 * m)), m = m)
}

.grid_min <- function(obj, lower, upper, step = 0.005) {
  g <- seq(lower, upper, by = step)
  v <- vapply(g, obj, numeric(1))
  i <- which.min(v)
  lo <- g[max(1, i - 1)]; hi <- g[min(length(g), i + 1)]
  optimize(obj, c(lo, hi), tol = 1e-6)$minimum
}

local_whittle <- function(x, m, lower = -0.49, upper = 1.5) {
  p <- periodogram(x)
  lam <- p$lambda[1:m]; I <- p$I[1:m]
  obj <- function(d) log(mean(lam^(2 * d) * I)) - 2 * d * mean(log(lam))
  list(d = .grid_min(obj, lower, upper), se = 1 / (2 * sqrt(m)), m = m)
}

elw <- function(x, m, lower = -0.49, upper = 2.5) {
  x <- x - x[1]
  n <- length(x)
  lam <- 2 * pi * (1:m) / n
  obj <- function(d) log(mean(periodogram(frac_diff(x, d))$I[1:m])) - 2 * d * mean(log(lam))
  list(d = .grid_min(obj, lower, upper), se = 1 / (2 * sqrt(m)), m = m)
}

felw_weight <- function(d) ifelse(d <= 0.5, 1, ifelse(d >= 0.75, 0, (1 + cos(4 * pi * d)) / 2))

felw <- function(x, m, lower = -0.49, upper = 2.0) {
  n <- length(x)
  lam <- 2 * pi * (1:m) / n
  xbar <- mean(x); x1 <- x[1]
  obj <- function(d) {
    mu <- felw_weight(d) * xbar + (1 - felw_weight(d)) * x1
    log(mean(periodogram(frac_diff(x - mu, d))$I[1:m])) - 2 * d * mean(log(lam))
  }
  list(d = .grid_min(obj, lower, upper), se = 1 / (2 * sqrt(m)), m = m)
}

## Lo (1991) modified R/S; V = Q_n(q) / sqrt(n). 95% acceptance region of H0 (short memory): [0.809, 1.862]
modified_rs <- function(x, q = NULL) {
  n <- length(x)
  xc <- x - mean(x)
  if (is.null(q)) {
    rho <- acf(x, lag.max = 1, plot = FALSE)$acf[2]
    q <- floor((3 * n / 2)^(1 / 3) * (2 * rho / (1 - rho^2))^(2 / 3))
    if (!is.finite(q) || q < 0) q <- 0
    q <- min(q, n - 1)
  }
  s <- cumsum(xc)
  gam <- function(j) sum(xc[(j + 1):n] * xc[1:(n - j)]) / n
  s2 <- sum(xc^2) / n + if (q > 0) 2 * sum(sapply(1:q, function(j) (1 - j / (q + 1)) * gam(j))) else 0
  V <- (max(s) - min(s)) / sqrt(s2) / sqrt(n)
  list(V = V, q = q, reject_5pct = V < 0.809 | V > 1.862)
}

## Qu (2011): W = sup_{r in [eps, 1]} (sum_j nu_j^2)^(-1/2) | sum_{j <= mr} nu_j (I_j / (G lambda_j^(-2d)) - 1) |
## nu_j = log lambda_j - mean(log lambda_j); critical values (eps = 0.05): 10% 1.022, 5% 1.155, 1% 1.426
qu_test <- function(x, m, eps = 0.05) {
  p <- periodogram(x)
  lam <- p$lambda[1:m]; I <- p$I[1:m]
  d <- local_whittle(x, m)$d
  G <- mean(lam^(2 * d) * I)
  nu <- log(lam) - mean(log(lam))
  terms <- nu * (I / (G * lam^(-2 * d)) - 1)
  ks <- seq(max(1, floor(eps * m)), m)
  W <- max(abs(cumsum(terms))[ks]) / sqrt(sum(nu^2))
  list(W = W, d_lw = d, m = m, crit = c(`10%` = 1.022, `5%` = 1.155, `1%` = 1.426),
       reject_5pct = W > 1.155)
}

whittle_bounded <- function(x) {
  w <- longmemo::WhittleEst(ts(x), model = "fARIMA")
  H <- unname(w$coefficients[1, "Estimate"]); se <- unname(w$coefficients[1, "Std. Error"])
  list(H = H, d = H - 0.5, se = se, z_reported = H / se, z_d0 = (H - 0.5) / se, at_bound = H >= 0.989)
}

## simulate ARFIMA(0, d, 0) of length n for any d > -1/2: stationary part simulated with fracdiff.sim,
## integrated (cumulated) once when d >= 1/2
sim_arfima <- function(n, d, burn = 500) {
  if (d < 0.5) {
    as.numeric(fracdiff::fracdiff.sim(n + burn, d = d)$series)[-(1:burn)]
  } else {
    cumsum(as.numeric(fracdiff::fracdiff.sim(n + burn, d = d - 1)$series)[-(1:burn)])
  }
}
