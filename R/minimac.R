# Main Function: minimac
#' Testing for Asset Pricing Errors (minimac)
#'
#' This function calculates the minimum autocovariances over multiple
#' horizons to test for the presence of pricing errors.
#'
#' @param y A numeric vector representing the log prices of a financial asset.
#' @param T A numeric number representing sample period (T=1/252 represents 1 day)
#' @return A list containing:
#'   \describe{
#'     \item{h}{A numeric vector of testing statistics for kns = 1, 1:2, 1:4, and 1:8}
#'     \item{sig_h}{A 6x4 matrix of significance results:
#'                   Columns correspond to the four groups of kns
#'                   Rows 1-2: 1%  level (1-sided, 2-sided)
#'                   Rows 3-4: 5%  level (1-sided, 2-sided)
#'                   Rows 5-6: 10% level (1-sided, 2-sided)
#'                   1 indicates rejection, 0 indicates acceptance}
#'   }
#' @examples
#' stat <- minimac(y,T)
#' Reference:
#'   Li, Z. Merrick and Xiye Yang (2025), "Multi-Horizon Test for Market Frictions"
minimac <- function(y, T) {
  # Import libraries
  if (!require("dplyr")) {install.packages("dplyr")}
  library(dplyr)
  
  # Load the critical values
  cvs <- read.csv("cvs.csv", header = FALSE)
  cvs <- as.matrix(cvs)
  
  # Calculate h values
  h <- scovks(y, 1:8, T)
  hs <- numeric(4)
  sig_h <- matrix(0, nrow = 6, ncol = 4)
  
  for (L in 0:3) {
    ks <- 1:(2^L)
    # get the min stats
    mh <- min(h[ks])
    # store the min stats
    hs[L + 1] <- mh
    
    for (j in 1:3) {
      sig_h[1 + 2 * (j - 1), L + 1] <- as.integer(mh < cvs[1 + 3 * (j - 1), L + 1])  # 1-sided test
      sig_h[2 + 2 * (j - 1), L + 1] <- as.integer(mh > cvs[2 + 3 * (j - 1), L + 1] | 
                                                    mh < cvs[3 + 3 * (j - 1), L + 1])  # 2-sided test
    }
  }
  
  stat <- list(h = hs, sig_h = sig_h)
  stat
}


# Supplementary Function: scovks
#' Calculate Standardized Testing Statistics for Multiple Horizons
#'
#' @param y A numeric vector representing the log prices of a financial asset.
#' @param ks A numeric vector of tuning parameters (horizons) for testing statistics.
#' @param T A numeric number representing the sample period (T=1/252 represents 1 day)
#' @return A numeric vector of standardized testing statistics for each horizon.
#' @examples
#' Hs <- scovks(y, ks, T)
#' Reference:
#'   Li, Z. Merrick and Xiye Yang (2025), "Multi-Horizon Test for Market Efficiency."
scovks <- function(y, ks, T) {
  # Number of horizons to process
  numKs <- length(ks)
  
  # Preallocate arrays
  Hs <- numeric(numKs)
  
  # Loop over each horizon
  for (idx in 1:numKs) {
    kn <- ks[idx]
    H <- scovk(y, kn, T)
    Hs[idx] <- H
  }
  
  Hs
}


# Supplementary Function: scovk
#' Calculate Standardized Covariances Over a Single k-Horizon
#'
#' @param y A numeric vector representing the log prices of a financial asset.
#' @param kn A numeric value representing the horizon parameter.
#' @param T A numeric number representing the sample period (T=1/252 represents 1 day)
#' @return A numeric value of the standardized testing statistic.
#' #' @examples
#' H <- scovk(y, ks, T)
#' Reference:
#'   Li, Z. Merrick and Xiye Yang (2025), "Multi-Horizon Test for Market Efficiency."
scovk <- function(y, kn, T) {
  # Calculate log returns
  dy <- diff(y)
  n <- length(y)
  
  # Jumps truncations
  dn <- 1 / n
  BPV <- (pi / 2 / T) * sum(abs(dy[1:(length(dy) - 1)]) * abs(dy[2:length(dy)]))
  threshold <- 3 * (T*dn)^(0.48) * sqrt(BPV)
  Jidx <- abs(dy) > threshold
  dyt <- dy * (!Jidx)
  
  # Variance estimation
  var_cont4 <- sum((dyt[1:(length(dyt) - 1)] * dyt[2:length(dyt)])^2) * kn / 2
  factor_kn <- 1 / 3 + 1 / (6 * kn^2)
  U <- factor_kn * var_cont4
  
  # F-bar statistic for truncated values
  y_trunc <- cumsum(c(0, dyt))
  dykn <- y_trunc[(1 + kn):length(y_trunc)] - y_trunc[1:(length(y_trunc) - kn)]
  Fbar <- sum(dykn[(1 + kn):length(dykn)] * dykn[1:(length(dykn) - kn)]) / (2 * kn)
  
  # Calculate the standardized testing statistic
  if (Fbar == 0 || U == 0) {
    H <- 0
  } else {
    H <- Fbar / sqrt(U)
  }
  
  H
}

summary_minimac <- function(stat) {
  # This function prints output formally for minimac function.
  #
  # Args:
  #   stat: output from minimac(y,T) function
  
  cat('================ minimac testing result ================\n')
  cat('>> Testing Statistics:\n')
  h <- as.data.frame(t(stat$h))
  colnames(h) <- c('K_0', 'K_1', 'K_2', 'K_3')
  rownames(h) <- 'H'
  print(h)
  
  cat('>> Rejection Status:\n')
  sig_h <- as.data.frame(stat$sig_h)
  colnames(sig_h) <- c('K_0', 'K_1', 'K_2', 'K_3')
  rownames(sig_h) <- c('1%, one-sided', '1%, two-sided', '5%, one-sided', '5%, two-sided', '10%, one-sided', '10%, two-sided')
  print(sig_h)
}