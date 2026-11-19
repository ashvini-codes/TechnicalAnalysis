# run_all_tests.R - tests for all Assignment 5 indicator functions
# Author: Ashviniben Tailor | BDA400 Assignment 5
#
# HOW TO RUN
#   1. Keep this file and the nine indicator scripts in one folder.
#   2. In RStudio: Session > Set Working Directory > To Source File Location.
#   3. Click Source (or run source("run_all_tests.R")).
# Expected values were calculated independently in Python from the assignment
# pseudocode. Base R helpers (sd, mean) appear only in this test file as
# cross-checks; the indicator scripts themselves use no libraries.

for (f in c("sma", "ema", "macd", "stdev", "linreg", "rsi", "stoch_rsi",
            "crossover", "crossunder")) source(paste0(f, ".R"))

passed <- 0; failed <- 0
check <- function(label, ok) {
  ok <- isTRUE(ok)
  if (ok) passed <<- passed + 1 else failed <<- failed + 1
  cat(sprintf("%-64s %s\n", label, if (ok) "PASS" else "FAIL"))
}
same <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-8))
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

# My own price series (15 closing prices) and two short series for the crossovers
prices <- c(210, 214, 209, 218, 222, 219, 227, 231, 226, 235, 232, 238, 233, 241, 244)
a <- c(3, 5, 4, 6, 5, 3, 2, 4, 6, 7, 5)
b <- c(4, 4, 5, 5, 6, 4, 3, 3, 5, 6, 6)

cat("\n--- SMA ---\n"); print(sma(prices, 5))
check("sma: period 5 on my series", same(sma(prices, 5), c(214.6, 216.4, 219.0, 223.4, 225.0, 227.6, 230.2, 232.4, 232.8, 235.8, 237.6)))
check("sma: 11 values for 15 points and period 5", length(sma(prices, 5)) == 11)
check("sma: first value = mean of first 5 points", same(sma(prices, 5)[1], mean(prices[1:5])))
check("sma: period 1 returns the data", same(sma(prices, 1), prices))
check("sma: period = length gives the mean", same(sma(prices, 15), mean(prices)))
check("sma: hand calculation c(2, 4, 6, 8), period 2", same(sma(c(2, 4, 6, 8), 2), c(3, 5, 7)))
check("sma: error if data is shorter than the period", fails(sma(c(1, 2), 3)))
check("sma: error for text data", fails(sma("a", 1)))
check("sma: error for period 0", fails(sma(prices, 0)))

cat("\n--- EMA ---\n"); print(ema(prices, 4))
check("ema: period 4 on my series", same(ema(prices, 4), c(210, 211.6, 210.56, 213.536, 216.9216, 217.75296, 221.451776, 225.2710656, 225.56263936, 229.337583616, 230.4025501696, 233.4415301018, 233.2649180611, 236.3589508366, 239.415370502)))
check("ema: first value is the first data point", ema(prices, 4)[1] == prices[1])
check("ema: same length as the data", length(ema(prices, 4)) == length(prices))
check("ema: hand calculation c(10, 20), period 3 gives 10, 15", same(ema(c(10, 20), 3), c(10, 15)))
check("ema: period 1 returns the data", same(ema(prices, 1), prices))
check("ema: empty data gives an empty vector", length(ema(numeric(0), 3)) == 0)
check("ema: error for period 1.5", fails(ema(prices, 1.5)))

cat("\n--- MACD ---\n"); m <- macd(prices, 3, 6, 3); print(m$macd_line)
check("macd: MACD line (3, 6, 3)", same(m$macd_line, c(0, 0.8571428571, -0.0306122449, 1.5852769679, 2.7930549771, 2.1825392694, 3.3669923353, 4.1661552395, 2.7849769568, 3.8224165763, 3.0040140723, 3.568296882, 2.188641188, 3.0975296991, 3.6224856388)))
check("macd: signal line (3, 6, 3)", same(m$signal_line, c(0, 0.4285714286, 0.1989795918, 0.8921282799, 1.8425916285, 2.0125654489, 2.6897788921, 3.4279670658, 3.1064720113, 3.4644442938, 3.234229183, 3.4012630325, 2.7949521103, 2.9462409047, 3.2843632718)))
check("macd: histogram = MACD line - signal line", same(m$histogram, m$macd_line - m$signal_line))
check("macd: MACD line = short EMA - long EMA", same(m$macd_line, ema(prices, 3) - ema(prices, 6)))
check("macd: three named vectors", identical(names(m), c("macd_line", "signal_line", "histogram")))
check("macd: every vector as long as the data", all(lengths(m) == length(prices)))
check("macd: error for a negative period", fails(macd(prices, -3, 6, 3)))

cat("\n--- Standard deviation ---\n"); print(stdev(prices))
check("stdev: my series", same(stdev(prices), 10.6382956028))
check("stdev: textbook example 2,4,4,4,5,5,7,9 gives 2", same(stdev(c(2, 4, 4, 4, 5, 5, 7, 9)), 2))
check("stdev: equals sd() * sqrt((n-1)/n)", same(stdev(prices), sd(prices) * sqrt(14 / 15)))
check("stdev: constant data gives 0", stdev(rep(3, 6)) == 0)
check("stdev: single value gives 0", stdev(42) == 0)
check("stdev: error for empty data", fails(stdev(numeric(0))))

cat("\n--- Linear regression ---\n")
l1 <- linreg(prices, 7, 0); print(c(l1$slope, l1$intercept))
check("linreg: (7, 0) slope", same(l1$slope, 1.9761904762))
check("linreg: (7, 0) intercept", same(l1$intercept, 226.1071428571))
check("linreg: (7, 0) predicted values", same(l1$predicted_values, c(228.0833333333, 230.0595238095, 232.0357142857, 234.0119047619, 235.9880952381, 237.9642857143, 239.9404761905, 241.9166666667)))
l2 <- linreg(prices, 9, 3)
check("linreg: (9, 3) predicted values", same(l2$predicted_values, c(227.8, 231.1, 234.4, 237.7)))
check("linreg: perfect line y = 3x + 1 gives slope 3 and intercept 1",
      same(unname(unlist(linreg(3 * (1:9) + 1, 9, 1)[c("slope", "intercept")])), c(3, 1)))
check("linreg: error if length is more than the data", fails(linreg(1:5, 6, 0)))
check("linreg: error if offset >= length", fails(linreg(1:5, 3, 3)))
check("linreg: error for text data", fails(linreg(c("a", "b", "c"), 2, 0)))

cat("\n--- RSI ---\n"); r <- rsi(prices, 5); print(r)
check("rsi: period 5, positions 6 to 15", same(r[6:15], c(59.1304347826, 71.5151515152, 76.050955414, 60.8989480395, 73.0017058273, 64.6622881304, 72.5124520755, 58.8860425545, 70.117117555, 73.5092138526)))
check("rsi: first 5 values are NA, the rest are not", all(is.na(r[1:5])) && !anyNA(r[6:15]))
check("rsi: always between 0 and 100", all(r[6:15] >= 0 & r[6:15] <= 100))
check("rsi: only rises gives 100", all(rsi(1:12, 5)[6:12] == 100))
check("rsi: only falls gives 0", all(rsi(12:1, 5)[6:12] == 0))
check("rsi: flat prices give 50", all(rsi(rep(8, 12), 5)[6:12] == 50))
check("rsi: too little data gives NA and a warning",
      all(is.na(suppressWarnings(rsi(1:3, 5)))) && inherits(tryCatch(rsi(1:3, 5), warning = function(w) w), "warning"))

cat("\n--- Stochastic RSI ---\n"); s <- stoch_rsi(prices, 5, 3, 3); print(s$k_line)
check("stoch_rsi: %K line from position 6", same(s$k_line[6:13], c(0.5833297707, 0.6176733426, 0.6465414242, 0.4253796589, 0.6509076324, 0.3767890785, 0.4827189186, 0.5020754938)))
check("stoch_rsi: %D line from position 6", same(s$d_line[6:11], c(0.6158481792, 0.5631981419, 0.5742762385, 0.48435879, 0.5034718765, 0.4538611636)))
check("stoch_rsi: lengths are 13 and 11", length(s$k_line) == 13 && length(s$d_line) == 11)
ok_vals <- c(s$k_line, s$d_line); ok_vals <- ok_vals[!is.na(ok_vals)]
check("stoch_rsi: values stay between 0 and 1", all(ok_vals >= 0 & ok_vals <= 1))
check("stoch_rsi: flat prices give only NA", all(is.na(unlist(stoch_rsi(rep(8, 20), 5, 3, 3)))))

cat("\n--- Crossover and crossunder ---\n"); print(crossover(a, b)); print(crossunder(a, b))
check("crossover: my arrays", identical(crossover(a, b), c("None", "Up", "Down", "Up", "Down", "None", "None", "Up", "None", "None", "Down")))
check("crossunder: my arrays", identical(crossunder(a, b), c("None", "False", "True", "False", "True", "False", "False", "False", "False", "False", "True")))
check("crossover: first position is always None", crossover(a, b)[1] == "None")
check("crossover: equal arrays never cross", all(crossover(1:6, 1:6) == "None"))
check("crossover: NA values give no signal", all(crossover(c(1, NA, 3), c(2, 2, 2)) == "None"))
check("crossunder: NA values give no signal", all(crossunder(c(1, NA, 3), c(2, 2, 2))[-1] == "False"))
check("crossover: error for different lengths", fails(crossover(1:3, 1:2)))
check("crossunder: error for different lengths", fails(crossunder(1:3, 1:2)))

cat(sprintf("\nTests passed: %d | failed: %d\n", passed, failed))
if (failed == 0) cat("All tests passed.\n") else stop("Some tests failed.")
