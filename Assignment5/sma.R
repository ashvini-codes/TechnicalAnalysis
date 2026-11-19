# sma.R - Simple Moving Average (base R only, no libraries)
# Author: Ashviniben Tailor | BDA400 Assignment 5
#
# sma(data, period)
#   data   : numeric vector
#   period : number of points in each window
# Returns length(data) - period + 1 values: the average of each window of
# `period` consecutive points. Nothing is padded, as in the pseudocode.

sma <- function(data, period) {
  # Validate the inputs
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  if (length(period) != 1 || !is.numeric(period) || is.na(period) ||
      period < 1 || period != floor(period)) {
    stop("`period` must be a single positive whole number", call. = FALSE)
  }

  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period", call. = FALSE)
  }

  # Row i of this matrix holds the positions of window i
  n_windows <- length(data) - period + 1
  positions <- outer(seq_len(n_windows), 0:(period - 1), "+")

  # Pick out the values of every window and average each row
  windows <- matrix(data[positions], nrow = n_windows)
  return(rowSums(windows) / period)
}
