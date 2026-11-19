# crossunder.R - Crossunder signals (base R only, no libraries)
# Author: Ashviniben Tailor | BDA400 Assignment 5
#
# crossunder(arr1, arr2)
# Returns a character vector as long as the inputs. The first element is "None".
# Every later element is "True" when arr1 crosses under arr2
# (arr1[i] < arr2[i] and arr1[i-1] >= arr2[i-1]) and "False" otherwise.
# Missing values never create a signal.

crossunder <- function(arr1, arr2) {
  # Check that both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length", call. = FALSE)
  }

  n <- length(arr1)
  result <- rep("None", n)
  if (n < 2) return(result)

  below <- arr1 < arr2                                   # arr1 under arr2 at each position
  at_or_above <- arr1 >= arr2                            # arr1 level with or over arr2

  for (i in 2:n) {
    crossed <- isTRUE(below[i]) && isTRUE(at_or_above[i - 1])
    result[i] <- if (crossed) "True" else "False"
  }

  return(result)
}
