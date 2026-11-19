# stock_display.R
# BDA400 Assignment 2 - functions to display the imported data and statistics.
# Author: Ashviniben Tailor. Run stock_functions.R first.

# One-block summary of the latest trading day, similar to a Yahoo Finance quote.
print_snapshot <- function(prices, ticker) {
  n <- nrow(prices); last <- prices[n, ]; prev_close <- prices$Close[n - 1]
  year <- prices[prices$Date > last$Date - 365, ]
  cat(sprintf("\n%s | %s\n", ticker, format(last$Date)))
  cat(sprintf("  Close %.2f (%+.2f / %+.2f%%) | Open %.2f\n", last$Close,
              last$Close - prev_close, 100 * (last$Close / prev_close - 1), last$Open))
  cat(sprintf("  Day %.2f - %.2f | 52 weeks %.2f - %.2f | Volume %s\n", last$Low, last$High,
              min(year$Low), max(year$High), format(last$Volume, big.mark = ",")))
}

# Show the quote, the first and last rows, and a summary for every stock.
show_portfolio <- function(stocks, n_rows = 4) {
  for (tk in names(stocks)) {
    print_snapshot(stocks[[tk]], tk)
    cat("\n  First rows:\n"); print(head(stocks[[tk]], n_rows), row.names = FALSE)
    cat("\n  Last rows:\n");  print(tail(stocks[[tk]], n_rows), row.names = FALSE)
    cat("\n  Summary:\n");    print(summary(stocks[[tk]][, c("Open", "High", "Low", "Close", "Volume")]))
  }
  invisible(stocks)
}

# Close price and moving average for every stock, one panel each.
plot_moving_averages <- function(series, window = 30) {
  ggplot(series, aes(Date)) +
    geom_line(aes(y = Close), colour = "grey40") +
    geom_line(aes(y = Moving_Avg), colour = "#c2410c", linewidth = 0.9, na.rm = TRUE) +
    facet_wrap(~ Ticker, scales = "free_y", ncol = 2) +
    labs(title = paste0("Close price (grey) and ", window, "-day moving average (orange)"),
         x = NULL, y = "USD") +
    theme_bw()
}

# Daily volume for every stock, one panel each.
plot_volumes <- function(stocks) {
  long <- do.call(rbind, lapply(names(stocks), function(tk) data.frame(Ticker = tk, stocks[[tk]][, c("Date", "Volume")])))
  ggplot(long, aes(Date, Volume / 1e6)) +
    geom_area(fill = "#0f766e", alpha = 0.7) +
    facet_wrap(~ Ticker, scales = "free_y", ncol = 2) +
    labs(title = "Daily trading volume", x = NULL, y = "Millions of shares") +
    theme_bw()
}

# Percentage growth since the first day, all stocks on one chart.
plot_growth <- function(stocks) {
  long <- do.call(rbind, lapply(names(stocks), function(tk) {
    p <- stocks[[tk]]
    data.frame(Ticker = tk, Date = p$Date, Growth = 100 * (p$Close / p$Close[1] - 1))
  }))
  ggplot(long, aes(Date, Growth, colour = Ticker)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    labs(title = "Growth since the first day (%)", x = NULL, y = "% change") +
    theme_bw()
}

# Mean and median close price side by side (the mode is shown in the table).
plot_centre <- function(table_all) {
  long <- rbind(data.frame(Ticker = table_all$Ticker, Measure = "Mean", Value = table_all$Mean),
                data.frame(Ticker = table_all$Ticker, Measure = "Median", Value = table_all$Median))
  ggplot(long, aes(Ticker, Value, fill = Measure)) +
    geom_col(position = position_dodge(0.8), width = 0.7) +
    scale_fill_manual(values = c(Mean = "#1d4ed8", Median = "#f59e0b")) +
    labs(title = "Mean and median close price", x = NULL, y = "USD") +
    theme_bw()
}
