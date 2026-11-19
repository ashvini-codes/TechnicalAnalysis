# stdev.R - Standard Deviation (base R only, no libraries)
# Author: Ashviniben Tailor | BDA400 Assignment 5
#
# stdev(data)
# Returns the POPULATION standard deviation: sqrt(sum((x - mean)^2) / n).
# Base R's sd() divides by n - 1, so stdev(x) = sd(x) * sqrt((n - 1) / n).

stdev <- function(data) {
  # Validate the input
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  n <- length(data)
  if (n == 0) stop("`data` must contain at least one value", call. = FALSE)

  # Step 1: the mean
  mean_value <- sum(data) / n

  # Step 2: each point's distance from the mean
  deviations <- data - mean_value

  # Step 3: the variance (mean squared distance), using a dot product
  variance <- sum(deviations * deviations) / n

  # Step 4: the standard deviation
  return(variance^0.5)
}
