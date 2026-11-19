# stock_functions.R
# BDA400 Assignment 2 - Technical Analysis using R (Preliminary Stage)
# Author: Ashviniben Tailor
# Functions that read the portfolio, import prices from Yahoo Finance and
# calculate descriptive statistics.

required_packages <- c("quantmod", "TTR", "ggplot2")

# Install whatever is missing, then load everything.
prepare_packages <- function(pkgs = required_packages) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) > 0) install.packages(missing)
  invisible(lapply(pkgs, library, character.only = TRUE))
}

# ---- Part 1: import ---------------------------------------------------------

# Read the ticker symbols from portfolio.txt (one per line, # starts a comment).
read_portfolio <- function(file = "portfolio.txt") {
  if (!file.exists(file)) stop("Portfolio file not found: ", file, call. = FALSE)
  tickers <- toupper(trimws(readLines(file, warn = FALSE)))
  tickers <- tickers[nzchar(tickers) & substr(tickers, 1, 1) != "#"]
  if (length(tickers) == 0) stop("No ticker symbols found in ", file, call. = FALSE)
  unique(tickers)
}

# Download one ticker and return it as a plain data frame.
fetch_one <- function(ticker, from, to) {
  xt <- getSymbols(ticker, src = "yahoo", from = from, to = to, auto.assign = FALSE)
  prices <- data.frame(Date = as.Date(zoo::index(xt)), zoo::coredata(xt))
  names(prices) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  prices[complete.cases(prices), ]
}

# Import every ticker listed in portfolio.txt.
# Returns a named list of data frames (names are the tickers). With
# assign_vars = TRUE each data frame is also created as a variable with the
# ticker's name (META, V, HD, ...) in `env`, as the assignment asks.
import_portfolio <- function(file = "portfolio.txt", from = "2024-01-01", to = Sys.Date(),
                             assign_vars = TRUE, env = globalenv()) {
  tickers <- read_portfolio(file)
  stocks <- list()
  for (tk in tickers) {
    result <- tryCatch(fetch_one(tk, from, to), error = function(e) {
      warning("Could not import ", tk, ": ", conditionMessage(e), call. = FALSE)
      NULL
    })
    if (!is.null(result)) {
      stocks[[tk]] <- result
      if (assign_vars) assign(tk, result, envir = env)
    }
  }
  if (length(stocks) == 0) stop("No stock could be imported. Check your internet connection.", call. = FALSE)
  stocks
}

# ---- Part 2: statistics -------------------------------------------------------

# Most frequent value (prices are rounded to the nearest cent first).
price_mode <- function(x) {
  counts <- table(round(x, 2))
  as.numeric(names(counts)[which.max(counts)])
}

# Mean, median, mode, standard deviation and moving average of the Close price.
describe_prices <- function(prices, window = 30) {
  close <- prices$Close
  moving_avg <- SMA(close, n = window)                  # TTR simple moving average
  list(
    stats = data.frame(
      Observations = length(close),
      Mean         = mean(close),
      Median       = median(close),
      Mode         = price_mode(close),
      Std_Dev      = sd(close),
      Last_MA      = moving_avg[length(moving_avg)],
      MA_Window    = window
    ),
    series = data.frame(Date = prices$Date, Close = close, Moving_Avg = moving_avg)
  )
}

# Statistics for the whole portfolio: a table plus the moving-average series.
analyse_portfolio <- function(stocks, window = 30) {
  parts <- lapply(stocks, describe_prices, window = window)
  table_all <- do.call(rbind, lapply(names(parts), function(tk) cbind(Ticker = tk, parts[[tk]]$stats)))
  rownames(table_all) <- NULL
  series_all <- do.call(rbind, lapply(names(parts), function(tk) cbind(Ticker = tk, parts[[tk]]$series)))
  rownames(series_all) <- NULL
  list(table = table_all, series = series_all)
}
