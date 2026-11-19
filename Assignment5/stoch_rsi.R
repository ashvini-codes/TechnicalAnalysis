# stoch_rsi.R - Stochastic RSI (base R only, no libraries)
# Author: Ashviniben Tailor | BDA400 Assignment 5
#
# stoch_rsi(data, period, k_period, d_period)
# From the assignment pseudocode:
#   stoch  = (RSI - lowest RSI) / (highest RSI - lowest RSI)   (over all RSI values)
#   k_line = sma(stoch, k_period)
#   d_line = sma(k_line, d_period)
# Returns list(k_line, d_line). sma() does not pad, so k_line has
# length(data) - k_period + 1 values and d_line is shorter again. Values that
# depend on the NA warm-up of the RSI are NA.
# Needs rsi() and sma(); if they are not loaded, rsi.R and sma.R are sourced
# from the working folder (keep the files together).

if (!exists("sma", mode = "function")) source("sma.R")
if (!exists("rsi", mode = "function")) source("rsi.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  # Validate the input
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)

  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Lowest and highest RSI (ignoring the NA warm-up)
  lowest  <- suppressWarnings(min(rsi_values, na.rm = TRUE))
  highest <- suppressWarnings(max(rsi_values, na.rm = TRUE))

  # Rescale to 0..1; if the RSI never changes (or does not exist) there is nothing to rescale
  if (!is.finite(lowest) || highest == lowest) {
    stoch <- rep(NA_real_, length(rsi_values))
  } else {
    stoch <- (rsi_values - lowest) / (highest - lowest)
  }

  # Smooth into %K and then %D
  k_line <- sma(stoch, k_period)
  d_line <- sma(k_line, d_period)

  return(list(k_line = k_line, d_line = d_line))
}
