# rsi.R - Relative Strength Index (base R only, no libraries)
# Author: Ashviniben Tailor | BDA400 Assignment 5
#
# rsi(data, period)
# Returns a vector as long as `data`. The first `period` values are NA. Later
# values are 100 - 100 / (1 + RS), with RS = average gain / average loss, using
# Wilder smoothing that starts at position period + 1 (as in the pseudocode).
# Special cases: no losses -> 100; no gains and no losses -> 50.
# With fewer than period + 1 points, NA values are returned with a warning.

rsi <- function(data, period) {
  # Validate the inputs
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  if (length(period) != 1 || !is.numeric(period) || is.na(period) ||
      period < 1 || period != floor(period)) {
    stop("`period` must be a single positive whole number", call. = FALSE)
  }

  n <- length(data)
  if (n < period + 1) {
    warning("Need at least period + 1 data points; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }

  # Gains and losses between consecutive points (losses as positive numbers)
  moves  <- data[-1] - data[-n]
  gains  <- pmax(moves, 0)
  losses <- pmax(-moves, 0)

  # Starting averages over the first `period` moves
  avg_gain <- mean(gains[seq_len(period)])
  avg_loss <- mean(losses[seq_len(period)])

  result <- rep(NA_real_, n)
  for (pos in (period + 1):n) {
    # Wilder smoothing: keep (period - 1)/period of the old average, add 1/period of the new move
    avg_gain <- (avg_gain * (period - 1) + gains[pos - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[pos - 1]) / period

    if (avg_gain == 0 && avg_loss == 0) {
      result[pos] <- 50                                   # flat prices
    } else if (avg_loss == 0) {
      result[pos] <- 100                                  # only gains
    } else {
      relative_strength <- avg_gain / avg_loss
      result[pos] <- 100 - 100 / (1 + relative_strength)
    }
  }

  return(result)
}
