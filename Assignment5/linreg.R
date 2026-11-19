# linreg.R - Simple Linear Regression (base R only, no libraries)
# Author: Ashviniben Tailor | BDA400 Assignment 5
#
# linreg(regressionSource, regressionLength, regressionOffset)
#   regressionSource : numeric vector of data points
#   regressionLength : how many points back the regression looks
#   regressionOffset : points trimmed from the end (0 = up to the last point)
# Window (from the assignment pseudocode):
#   start_index = max(1, n - regressionLength + regressionOffset)
#   end_index   = min(n, n - regressionOffset)
# x is 1, 2, 3, ... inside the window. The line y = intercept + slope * x is
# found with the normal equations, solve(t(X) %*% X, t(X) %*% y), where X has a
# column of ones and a column of x values.
# Returns list(slope, intercept, predicted_values).

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Validate the inputs
  if (!is.numeric(regressionSource)) {
    stop("`regressionSource` must be a numeric vector", call. = FALSE)
  }
  if (length(regressionLength) != 1 || length(regressionOffset) != 1 ||
      !is.numeric(regressionLength) || !is.numeric(regressionOffset) ||
      regressionOffset < 0) {
    stop("`regressionLength` and `regressionOffset` must be single numbers; offset >= 0",
         call. = FALSE)
  }

  n <- length(regressionSource)
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource",
         call. = FALSE)
  }
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength", call. = FALSE)
  }

  # The window of points
  from <- max(1, n - regressionLength + regressionOffset)
  to   <- min(n, n - regressionOffset)
  y <- regressionSource[from:to]
  if (length(y) < 2) stop("At least two data points are needed for a regression", call. = FALSE)

  # Design matrix: a column of ones and the positions 1, 2, 3, ...
  X <- cbind(1, seq_along(y))

  # Normal equations give the intercept and the slope together
  coefficients <- solve(crossprod(X), crossprod(X, y))
  intercept <- as.numeric(coefficients[1])
  slope     <- as.numeric(coefficients[2])

  # Fitted values on the line
  predicted_values <- as.numeric(X %*% coefficients)

  return(list(slope = slope, intercept = intercept, predicted_values = predicted_values))
}
