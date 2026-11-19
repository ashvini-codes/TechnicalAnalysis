# macd.R - Moving Average Convergence Divergence (base R only, no libraries)
# Author: Ashviniben Tailor | BDA400 Assignment 5
#
# macd(data, short_period, long_period, signal_period)
# Returns a list of three vectors, each as long as `data`:
#   macd_line   : short EMA - long EMA
#   signal_line : EMA of the MACD line
#   histogram   : macd_line - signal_line
# Needs ema(); if it is not loaded, ema.R is sourced from the working folder
# (keep the files together).

if (!exists("ema", mode = "function")) source("ema.R")

macd <- function(data, short_period, long_period, signal_period) {
  # Validate the inputs
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  all_periods <- c(short_period, long_period, signal_period)
  if (!is.numeric(all_periods) || anyNA(all_periods) || min(all_periods) < 1) {
    stop("All periods must be positive numbers", call. = FALSE)
  }

  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema  <- ema(data, long_period)

  # Calculate the MACD line
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  return(list(macd_line = macd_line, signal_line = signal_line, histogram = histogram))
}
