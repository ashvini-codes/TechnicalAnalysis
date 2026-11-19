# crossover.R - Crossover signals (base R only, no libraries)
# Author: Ashviniben Tailor | BDA400 Assignment 5
#
# crossover(arr1, arr2)
# Returns a character vector as long as the inputs:
#   "Up"   : arr1 crosses above arr2 at this position
#            (arr1[i] > arr2[i] and arr1[i-1] <= arr2[i-1])
#   "Down" : arr1 crosses below arr2 at this position
#            (arr1[i] < arr2[i] and arr1[i-1] >= arr2[i-1])
#   "None" : no crossing (always the case for the first position)
# Missing values never create a signal.

crossover <- function(arr1, arr2) {
  # Check that both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length", call. = FALSE)
  }

  n <- length(arr1)
  result <- rep("None", n)
  if (n < 2) return(result)

  # Is arr1 above, below or level with arr2 at each position?
  above <- arr1 > arr2
  below <- arr1 < arr2

  # Compare each position with the one before it
  for (i in 2:n) {
    if (isTRUE(above[i]) && isTRUE(!above[i - 1] && !is.na(above[i - 1]))) {
      result[i] <- "Up"          # was level or below, now above
    } else if (isTRUE(below[i]) && isTRUE(!below[i - 1] && !is.na(below[i - 1]))) {
      result[i] <- "Down"        # was level or above, now below
    }
  }

  return(result)
}
