# ema.R - Exponential Moving Average (base R only, no libraries)
# Author: Ashviniben Tailor | BDA400 Assignment 5
#
# ema(data, period)
# Returns a vector the same length as `data`. The first value is the first data
# point. Each later value moves a fraction `multiplier` of the way from the
# previous EMA towards the new data point, where multiplier = 2 / (period + 1).

ema <- function(data, period) {
  # Validate the inputs
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  if (length(period) != 1 || !is.numeric(period) || is.na(period) ||
      period < 1 || period != floor(period)) {
    stop("`period` must be a single positive whole number", call. = FALSE)
  }
  n <- length(data)
  if (n == 0) return(numeric(0))

  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # The first EMA value is the first data point
  result <- numeric(n)
  result[1] <- data[1]

  # Walk through the remaining points
  i <- 2
  while (i <= n) {
    gap <- data[i] - result[i - 1]                   # how far the new point is from the EMA
    result[i] <- result[i - 1] + gap * multiplier    # move part of the way towards it
    i <- i + 1
  }

  return(result)
}
