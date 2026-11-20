# =============================================================================
# app.R - Stock Portfolio Dashboard (R Shiny)
# BDA400 - Assignment 6: Technical Analysis using R, Visualization Phase
# Author: Ashviniben Tailor
#
# Interactive dashboard: Yahoo Finance prices (or offline demo data), line /
# candlestick / area charts, indicator overlays (Moving Averages, Bollinger
# Bands, RSI, MACD) built from the Assignment 5 functions, a Buy / Sell / Hold
# signal for every bar from a selectable trading rule, and a portfolio view.
#
# HOW TO RUN
#   1. Open this file in RStudio and click "Run App" (or shiny::runApp("app.R")).
#   2. The first run installs any missing packages (shiny, ggplot2, quantmod).
#   3. Pick "Yahoo Finance" (needs internet) or "Demo data (offline)".
#
# SECTIONS (matching the assignment tasks)
#   Step 1  Data collection and setup
#   Step 2  Visualizing stock data (Shiny skeleton, widgets, charts)
#   Step 3  Overlay technical indicators
#   Step 4  Trading rules and annotations
# =============================================================================


# =============================================================================
# STEP 1: DATA COLLECTION AND SETUP
# =============================================================================

# ---- 1a. Install (if missing) and load the packages -------------------------
app_packages <- c("shiny", "ggplot2", "quantmod")
absent <- app_packages[!app_packages %in% rownames(installed.packages())]
if (length(absent) > 0) install.packages(absent)

library(shiny)
library(ggplot2)
library(quantmod)       # only used to download Yahoo Finance data

# ---- 1b. Indicators from Assignment 5 (base R, no libraries) -----------------
sma <- function(data, period) {
  if (length(data) < period) stop("Data length should be greater than or equal to the period")
  n_windows <- length(data) - period + 1
  positions <- outer(seq_len(n_windows), 0:(period - 1), "+")
  rowSums(matrix(data[positions], nrow = n_windows)) / period
}

ema <- function(data, period) {
  multiplier <- 2 / (period + 1)
  result <- numeric(length(data)); result[1] <- data[1]
  i <- 2
  while (i <= length(data)) {
    result[i] <- result[i - 1] + (data[i] - result[i - 1]) * multiplier
    i <- i + 1
  }
  result
}

stdev <- function(data) {
  deviations <- data - sum(data) / length(data)
  (sum(deviations * deviations) / length(data))^0.5
}

macd <- function(data, short_period, long_period, signal_period) {
  macd_line   <- ema(data, short_period) - ema(data, long_period)
  signal_line <- ema(macd_line, signal_period)
  list(macd_line = macd_line, signal_line = signal_line, histogram = macd_line - signal_line)
}

rsi <- function(data, period) {
  n <- length(data)
  if (n < period + 1) return(rep(NA_real_, n))
  moves <- data[-1] - data[-n]
  gains <- pmax(moves, 0); losses <- pmax(-moves, 0)
  avg_gain <- mean(gains[seq_len(period)]); avg_loss <- mean(losses[seq_len(period)])
  result <- rep(NA_real_, n)
  for (pos in (period + 1):n) {
    avg_gain <- (avg_gain * (period - 1) + gains[pos - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[pos - 1]) / period
    result[pos] <- if (avg_gain == 0 && avg_loss == 0) 50
                   else if (avg_loss == 0) 100
                   else 100 - 100 / (1 + avg_gain / avg_loss)
  }
  result
}

# sma() returns fewer values than it is given: pad the front with NA to match dates
rolling <- function(x, period, fun) {
  if (length(x) < period) return(rep(NA_real_, length(x)))
  c(rep(NA_real_, period - 1), vapply(seq_len(length(x) - period + 1),
      function(s) fun(x[s:(s + period - 1)]), numeric(1)))
}
moving_average <- function(x, period, type = "SMA") {
  if (length(x) < period) return(rep(NA_real_, length(x)))
  if (type == "EMA") {
    out <- ema(x, period); out[seq_len(period - 1)] <- NA_real_; out
  } else {
    c(rep(NA_real_, period - 1), sma(x, period))
  }
}

# ---- 1c. Data sources ----------------------------------------------------------
HISTORY_DAYS <- 365 * 8

portfolio_symbols <- local({
  default <- c("META", "V", "HD", "PEP", "AMD")
  if (!file.exists("portfolio.txt")) return(default)
  s <- toupper(trimws(readLines("portfolio.txt", warn = FALSE)))
  s <- s[nzchar(s)]
  if (length(s) == 0) default else s
})

fetch_yahoo <- function(symbol, from, to) {
  xt <- tryCatch(
    suppressWarnings(getSymbols(symbol, src = "yahoo", from = from, to = to, auto.assign = FALSE)),
    error = function(e) stop("Could not download '", symbol, "' from Yahoo Finance (",
                             conditionMessage(e), "). Check the symbol and your internet ",
                             "connection, or choose 'Demo data'.", call. = FALSE))
  d <- data.frame(Date = as.Date(index(xt)), Open = as.numeric(Op(xt)), High = as.numeric(Hi(xt)),
                  Low = as.numeric(Lo(xt)), Close = as.numeric(Cl(xt)), Volume = as.numeric(Vo(xt)))
  d[complete.cases(d), ]
}

# Offline demo prices: a random walk that is the same every time for the same symbol
fetch_demo <- function(symbol, from, to) {
  days <- seq(as.Date(from), as.Date(to), by = "day")
  days <- days[!format(days, "%u") %in% c("6", "7")]
  seed <- sum(utf8ToInt(symbol)) * 7; set.seed(seed)
  n <- length(days)
  close <- (80 + 9 * (seed %% 13)) * exp(cumsum(rnorm(n, 0.0003, 0.015)))
  open  <- c(close[1], close[-n]) * exp(rnorm(n, 0, 0.004))
  data.frame(Date = days, Open = open,
             High = pmax(open, close) * (1 + abs(rnorm(n, 0, 0.006))),
             Low  = pmin(open, close) * (1 - abs(rnorm(n, 0, 0.006))),
             Close = close, Volume = round(runif(n, 5e6, 5e7)))
}

# Make sure the data can be charted
validate_prices <- function(d) {
  if (is.null(d) || nrow(d) < 5) stop("Not enough price data was returned.", call. = FALSE)
  if (any(d$High < d$Low, na.rm = TRUE)) stop("Data problem: some High values are below Low.", call. = FALSE)
  d
}

# Daily bars -> weekly or monthly bars
to_time_frame <- function(d, frame) {
  if (frame == "Daily") return(d)
  groups <- split(d, cut(d$Date, if (frame == "Weekly") "week" else "month"))
  bars <- lapply(groups[vapply(groups, nrow, integer(1)) > 0], function(g)
    data.frame(Date = max(g$Date), Open = g$Open[1], High = max(g$High), Low = min(g$Low),
               Close = g$Close[nrow(g)], Volume = sum(g$Volume)))
  out <- do.call(rbind, bars); rownames(out) <- NULL; out
}


# =============================================================================
# STEP 4 (helper): TRADING RULES
# Every bar gets a Signal of "Buy", "Sell" or "Hold" (as in the assignment text).
# =============================================================================
trading_signal <- function(d, rule, s) {
  signal <- switch(rule,
    "MA trend (short vs long)" = {
      short <- moving_average(d$Close, s$ma_short, s$ma_type)
      long  <- moving_average(d$Close, s$ma_long,  s$ma_type)
      ifelse(is.na(short) | is.na(long), "Hold", ifelse(short > long, "Buy", ifelse(short < long, "Sell", "Hold")))
    },
    "RSI zones" = {
      r <- rsi(d$Close, s$rsi_period)
      ifelse(is.na(r), "Hold", ifelse(r < s$rsi_low, "Buy", ifelse(r > s$rsi_high, "Sell", "Hold")))
    },
    "MACD vs signal line" = {
      m <- macd(d$Close, s$macd_fast, s$macd_slow, s$macd_signal)
      state <- ifelse(m$macd_line > m$signal_line, "Buy", ifelse(m$macd_line < m$signal_line, "Sell", "Hold"))
      state[seq_len(min(s$macd_slow, length(state)))] <- "Hold"      # ignore the EMA warm-up
      state
    })
  signal
}

# Bars where the signal changes to Buy or Sell (the moments to act)
signal_changes <- function(signal) {
  previous <- c("Hold", signal[-length(signal)])
  signal != previous & signal %in% c("Buy", "Sell")
}


# =============================================================================
# STEP 2: SHINY SKELETON - USER INTERFACE
# =============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background: #f5f7f7; }
    .navbar-brand, h2 { font-weight: 600; }
    .well { background: #ffffff; border: 1px solid #d8dfdf; }
    .kpi { display: inline-block; min-width: 130px; margin: 0 8px 10px 0; padding: 8px 14px;
           background: #fff; border: 1px solid #d8dfdf; border-left: 4px solid #0f766e; border-radius: 4px; }
    .kpi .k { font-size: 11px; color: #5b6b6b; text-transform: uppercase; }
    .kpi .v { font-size: 19px; font-weight: 600; }
    .buy { color: #15803d; } .sell { color: #b91c1c; }
  "))),
  titlePanel("Stock Portfolio Dashboard"),

  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      h4("Data"),
      selectInput("source", "Data source", c("Yahoo Finance", "Demo data (offline)")),
      selectizeInput("symbol", "Stock symbol", choices = portfolio_symbols,
                     selected = portfolio_symbols[1], options = list(create = TRUE)),
      # ---- interactive widgets: date range and time frame ----
      dateRangeInput("date_range", "Select Date Range:", start = Sys.Date() - 365, end = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:", c("Daily", "Weekly", "Monthly")),
      hr(), h4("Chart"),
      radioButtons("chart_type", "Chart type", c("Candlestick", "Line", "Area"), inline = TRUE),
      checkboxGroupInput("indicators", "Technical indicators",
                         c("Moving Averages", "Bollinger Bands", "RSI", "MACD"),
                         selected = "Moving Averages"),
      conditionalPanel("input.indicators.indexOf('Moving Averages') > -1 || input.rule == 'MA trend (short vs long)'",
        selectInput("ma_type", "Moving average type", c("SMA", "EMA")),
        numericInput("ma_short", "Short MA period", 20, min = 2, max = 200),
        numericInput("ma_long", "Long MA period", 50, min = 3, max = 400)),
      conditionalPanel("input.indicators.indexOf('Bollinger Bands') > -1",
        numericInput("bb_period", "Bollinger period", 20, min = 3, max = 200),
        numericInput("bb_mult", "Bollinger width (std devs)", 2, min = 0.5, max = 4, step = 0.5)),
      conditionalPanel("input.indicators.indexOf('RSI') > -1 || input.rule == 'RSI zones'",
        numericInput("rsi_period", "RSI period", 14, min = 2, max = 100),
        numericInput("rsi_low", "RSI buy below", 30, min = 1, max = 49),
        numericInput("rsi_high", "RSI sell above", 70, min = 51, max = 99)),
      conditionalPanel("input.indicators.indexOf('MACD') > -1 || input.rule == 'MACD vs signal line'",
        numericInput("macd_fast", "MACD fast", 12, min = 2, max = 100),
        numericInput("macd_slow", "MACD slow", 26, min = 3, max = 200),
        numericInput("macd_signal", "MACD signal", 9, min = 2, max = 100)),
      hr(), h4("Trading rule"),
      selectInput("rule", NULL, c("MA trend (short vs long)", "RSI zones", "MACD vs signal line")),
      checkboxInput("show_ribbon", "Colour each bar by its signal", TRUE),
      checkboxInput("show_marks", "Mark the bars where the signal flips", TRUE)
    ),

    mainPanel(
      width = 9,
      uiOutput("kpis"),
      tabsetPanel(
        tabPanel("Chart", plotOutput("main_plot", height = "auto")),
        tabPanel("Signal log", br(), uiOutput("signal_summary"), br(), tableOutput("signal_log")),
        tabPanel("Portfolio", br(),
                 p("Growth since the start of the selected date range and the current signal for each symbol in portfolio.txt."),
                 actionButton("load_portfolio", "Load portfolio"), br(), br(),
                 plotOutput("growth_plot", height = "340px"), tableOutput("portfolio_table")),
        tabPanel("Data", br(), tableOutput("data_table")),
        tabPanel("About", br(),
          p("Data: Yahoo Finance through quantmod, or synthetic demo data offline."),
          p("Indicators (SMA, EMA, standard deviation, MACD, RSI) are the base-R functions from Assignment 5."),
          p("Each bar gets a signal: Buy, Sell or Hold, from the selected trading rule."),
          p(strong("For learning only. This is not investment advice.")))
      )
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  store <- new.env()                                   # downloaded data, kept per symbol

  get_prices <- function(sym, source) {
    key <- paste(source, sym)
    if (is.null(store[[key]])) {
      to <- Sys.Date(); from <- to - HISTORY_DAYS
      d <- if (source == "Yahoo Finance") fetch_yahoo(sym, from, to) else fetch_demo(sym, from, to)
      store[[key]] <- validate_prices(d)
    }
    store[[key]]
  }

  # ---- Step 1: fetch the data ---------------------------------------------------
  all_prices <- reactive({
    sym <- toupper(trimws(input$symbol))
    validate(need(nzchar(sym), "Enter a stock symbol."))
    d <- tryCatch(
      withProgress(message = paste("Loading", sym), value = 0.5, get_prices(sym, input$source)),
      error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 12); NULL })
    validate(need(!is.null(d), "No data to show. See the message, or try 'Demo data'."))
    d
  })

  # ---- Step 2: filter by date range and time frame ------------------------------
  bars <- reactive({
    d <- all_prices()
    validate(need(input$date_range[1] < input$date_range[2], "The start date must be before the end date."))
    d <- d[d$Date >= input$date_range[1] & d$Date <= input$date_range[2], ]
    validate(need(nrow(d) >= 5, "Fewer than 5 trading days in this range. Widen the date range."))
    to_time_frame(d, input$time_frame)
  })

  params <- reactive(list(
    ma_type = input$ma_type, ma_short = input$ma_short, ma_long = input$ma_long,
    rsi_period = input$rsi_period, rsi_low = input$rsi_low, rsi_high = input$rsi_high,
    macd_fast = input$macd_fast, macd_slow = input$macd_slow, macd_signal = input$macd_signal,
    bb_period = input$bb_period, bb_mult = input$bb_mult))

  # ---- Steps 3 and 4: indicators and signals --------------------------------------
  analysed <- reactive({
    d <- bars(); p <- params()
    validate(
      need(p$ma_short < p$ma_long, "The short MA period must be smaller than the long MA period."),
      need(p$macd_fast < p$macd_slow, "MACD fast must be smaller than MACD slow."),
      need(p$rsi_low < p$rsi_high, "RSI buy level must be below the RSI sell level."))
    d$MA_short <- moving_average(d$Close, p$ma_short, p$ma_type)
    d$MA_long  <- moving_average(d$Close, p$ma_long,  p$ma_type)
    d$BB_mid   <- moving_average(d$Close, p$bb_period, "SMA")
    d$BB_sd    <- rolling(d$Close, p$bb_period, stdev)
    d$BB_up    <- d$BB_mid + p$bb_mult * d$BB_sd
    d$BB_low   <- d$BB_mid - p$bb_mult * d$BB_sd
    d$RSI      <- rsi(d$Close, p$rsi_period)
    m <- macd(d$Close, p$macd_fast, p$macd_slow, p$macd_signal)
    d$MACD <- m$macd_line; d$MACD_signal <- m$signal_line; d$MACD_hist <- m$histogram
    d$Signal <- trading_signal(d, input$rule, p)
    d$Flip   <- signal_changes(d$Signal)
    d
  })

  # ---- key numbers ----------------------------------------------------------------
  output$kpis <- renderUI({
    d <- analysed(); n <- nrow(d)
    change <- if (n > 1) 100 * (d$Close[n] / d$Close[n - 1] - 1) else 0
    now <- d$Signal[n]
    kpi <- function(label, value, cls = "") div(class = "kpi", div(class = "k", label), div(class = paste("v", cls), value))
    tagList(
      kpi("Symbol", toupper(trimws(input$symbol))),
      kpi("Last close", sprintf("$%.2f", d$Close[n])),
      kpi("Last change", sprintf("%+.2f%%", change), if (change >= 0) "buy" else "sell"),
      kpi("Period high", sprintf("$%.2f", max(d$High))),
      kpi("Period low", sprintf("$%.2f", min(d$Low))),
      kpi("Current signal", now, switch(now, Buy = "buy", Sell = "sell", "")))
  })

  # ---- chart panels ---------------------------------------------------------------
  half_bar <- function() switch(input$time_frame, Daily = 0.4, Weekly = 2.2, Monthly = 10)

  price_panel <- function(d) {
    span <- diff(range(c(d$Low, d$High)))
    low_edge <- min(d$Low)
    w <- half_bar()
    p <- ggplot(d, aes(x = Date))

    # Step 2: chart type (candle colours are fixed values, so no legend is created for them)
    if (input$chart_type == "Candlestick") {
      up <- d[d$Close >= d$Open, ]; down <- d[d$Close < d$Open, ]
      p <- p +
        geom_segment(data = up, aes(xend = Date, y = Low, yend = High), colour = "#15803d") +
        geom_segment(data = down, aes(xend = Date, y = Low, yend = High), colour = "#b91c1c") +
        geom_rect(data = up, aes(xmin = Date - w, xmax = Date + w, ymin = Open, ymax = Close), fill = "#15803d") +
        geom_rect(data = down, aes(xmin = Date - w, xmax = Date + w, ymin = Close, ymax = Open), fill = "#b91c1c")
    } else if (input$chart_type == "Area") {
      p <- p + geom_ribbon(aes(ymin = low_edge - 0.02 * span, ymax = Close), fill = "#99f6e4", alpha = 0.6) +
               geom_line(aes(y = Close), colour = "#0f766e", linewidth = 0.8)
    } else {
      p <- p + geom_line(aes(y = Close), colour = "#0f766e", linewidth = 0.8)
    }

    # Step 3: overlays on the price chart
    line_names <- character(0)
    if ("Moving Averages" %in% input$indicators) {
      p <- p + geom_line(aes(y = MA_short, colour = "Short MA"), na.rm = TRUE, linewidth = 0.8) +
               geom_line(aes(y = MA_long, colour = "Long MA"), na.rm = TRUE, linewidth = 0.8)
      line_names <- c(line_names, "Short MA", "Long MA")
    }
    if ("Bollinger Bands" %in% input$indicators) {
      p <- p + geom_ribbon(aes(ymin = BB_low, ymax = BB_up), fill = "#94a3b8", alpha = 0.18, na.rm = TRUE) +
               geom_line(aes(y = BB_up, colour = "Bollinger band"), na.rm = TRUE, linewidth = 0.4) +
               geom_line(aes(y = BB_low, colour = "Bollinger band"), na.rm = TRUE, linewidth = 0.4)
      line_names <- c(line_names, "Bollinger band")
    }

    # Step 4: annotate the bars with their signals
    fills <- c(Buy = "#16a34a", Sell = "#dc2626", Hold = "#cbd5e1")
    if (isTRUE(input$show_ribbon)) {
      p <- p + geom_rect(aes(xmin = Date - w, xmax = Date + w, ymin = low_edge - 0.10 * span,
                             ymax = low_edge - 0.05 * span, fill = Signal)) +
               scale_fill_manual(values = fills, name = "Signal", drop = FALSE)
    }
    if (isTRUE(input$show_marks)) {
      flips <- d[d$Flip, ]
      if (nrow(flips) > 0) {
        top <- flips[flips$Signal == "Sell", ]; bottom <- flips[flips$Signal == "Buy", ]
        if (nrow(bottom) > 0) p <- p +
          geom_point(data = bottom, aes(y = Low - 0.015 * span), shape = 17, size = 3.6, colour = "#15803d") +
          geom_text(data = bottom, aes(y = Low - 0.05 * span - (input$show_ribbon * 0.06 * span), label = "BUY"),
                    colour = "#15803d", size = 3.2, fontface = "bold")
        if (nrow(top) > 0) p <- p +
          geom_point(data = top, aes(y = High + 0.015 * span), shape = 25, size = 3.6, fill = "#b91c1c", colour = "#7f1d1d") +
          geom_text(data = top, aes(y = High + 0.06 * span, label = "SELL"),
                    colour = "#b91c1c", size = 3.2, fontface = "bold")
      }
    }

    line_colours <- c("Short MA" = "#2563eb", "Long MA" = "#f59e0b", "Bollinger band" = "#64748b")
    p + scale_colour_manual(values = line_colours, breaks = line_names, name = NULL) +
      scale_y_continuous(labels = function(v) paste0("$", format(v, big.mark = ","))) +
      labs(title = paste0(toupper(trimws(input$symbol)), " - ", input$chart_type, " chart, ", tolower(input$time_frame)),
           subtitle = paste("Trading rule:", input$rule), x = NULL, y = "Price") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", plot.title = element_text(face = "bold"))
  }

  rsi_panel <- function(d) {
    lo <- input$rsi_low; hi <- input$rsi_high
    ggplot(d, aes(Date, RSI)) +
      annotate("rect", xmin = min(d$Date), xmax = max(d$Date), ymin = hi, ymax = 100, fill = "#dc2626", alpha = 0.08) +
      annotate("rect", xmin = min(d$Date), xmax = max(d$Date), ymin = 0, ymax = lo, fill = "#16a34a", alpha = 0.08) +
      geom_hline(yintercept = c(lo, hi), linetype = "dashed", colour = "grey55") +
      geom_line(colour = "#7c3aed", na.rm = TRUE, linewidth = 0.8) +
      scale_y_continuous(limits = c(0, 100), breaks = c(0, lo, 50, hi, 100)) +
      labs(x = NULL, y = paste0("RSI (", input$rsi_period, ")")) + theme_minimal(base_size = 13)
  }

  macd_panel <- function(d) {
    d$sign <- ifelse(d$MACD_hist >= 0, "up", "down")
    ggplot(d, aes(Date)) +
      geom_col(aes(y = MACD_hist, fill = sign), width = half_bar() * 2, alpha = 0.5, position = "identity") +
      scale_fill_manual(values = c(up = "#16a34a", down = "#dc2626"), guide = "none") +
      geom_line(aes(y = MACD), colour = "#2563eb", linewidth = 0.8) +
      geom_line(aes(y = MACD_signal), colour = "#f59e0b", linewidth = 0.8) +
      geom_hline(yintercept = 0, colour = "grey50") +
      labs(x = NULL, y = "MACD") + theme_minimal(base_size = 13)
  }

  # Stack the panels so the date axes line up (price panel is the tallest)
  stack_panels <- function(panels, xlim) {
    panels <- lapply(panels, function(p) p + coord_cartesian(xlim = xlim))
    if (length(panels) == 1) return(print(panels[[1]]))
    for (i in seq_len(length(panels) - 1)) panels[[i]] <- panels[[i]] + theme(axis.text.x = element_blank())
    stacked <- do.call(rbind, c(lapply(panels, ggplotGrob), list(size = "max")))
    rows <- unique(stacked$layout$t[grepl("^panel", stacked$layout$name)])
    stacked$heights[rows] <- grid::unit(c(4, 1.5, 1.7)[seq_along(panels)], "null")
    grid::grid.newpage(); grid::grid.draw(stacked)
  }

  # ---- Steps 2 and 3: the chart with the selected overlays --------------------------
  output$main_plot <- renderPlot({
    d <- analysed(); on <- input$indicators
    panels <- list(price_panel(d))
    if ("RSI" %in% on)  panels[[length(panels) + 1]] <- rsi_panel(d)
    if ("MACD" %in% on) panels[[length(panels) + 1]] <- macd_panel(d)
    stack_panels(panels, range(d$Date))
  }, height = function() 440 + 170 * ("RSI" %in% input$indicators) + 190 * ("MACD" %in% input$indicators), res = 96)

  # ---- Step 4: signal log -------------------------------------------------------------
  output$signal_summary <- renderUI({
    d <- analysed(); counts <- table(factor(d$Signal, levels = c("Buy", "Sell", "Hold")))
    flips <- sum(d$Flip)
    div(strong("Rule: "), input$rule, br(),
        strong("Bars in this period: "), span(class = "buy", paste(counts["Buy"], "Buy")), " | ",
        span(class = "sell", paste(counts["Sell"], "Sell")), " | ", paste(counts["Hold"], "Hold"), br(),
        strong("Signal flips to Buy or Sell: "), flips,
        if (counts["Buy"] + counts["Sell"] == 0)
          p(class = "sell", "Every bar is Hold: there are too few bars for this rule at this time frame. Widen the date range or use a shorter time frame."))
  })

  output$signal_log <- renderTable({
    d <- analysed(); f <- d[d$Flip, c("Date", "Signal", "Close")]
    validate(need(nrow(f) > 0, "No Buy or Sell flips with these settings. Try other parameters or a longer date range."))
    f$Date <- format(f$Date); f$Close <- sprintf("$%.2f", f$Close)
    f[order(f$Date, decreasing = TRUE), ]
  }, striped = TRUE, spacing = "s")

  # ---- Portfolio tab ----------------------------------------------------------------
  portfolio_data <- eventReactive(input$load_portfolio, {
    p <- params(); from <- input$date_range[1]; to <- input$date_range[2]
    out <- lapply(portfolio_symbols, function(sym) {
      d <- tryCatch(get_prices(sym, input$source), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      d <- d[d$Date >= from & d$Date <= to, ]
      if (nrow(d) < 5) return(NULL)
      d$Signal <- trading_signal(d, input$rule, p)
      d$Growth <- 100 * (d$Close / d$Close[1] - 1)
      cbind(Symbol = sym, d)
    })
    out <- Filter(Negate(is.null), out)
    validate(need(length(out) > 0, "No portfolio data could be loaded."))
    do.call(rbind, out)
  })

  output$growth_plot <- renderPlot({
    ggplot(portfolio_data(), aes(Date, Growth, colour = Symbol)) +
      geom_line(linewidth = 0.8) + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
      labs(title = "Growth since the start of the date range (%)", x = NULL, y = "% change") +
      theme_minimal(base_size = 13)
  })

  output$portfolio_table <- renderTable({
    all <- portfolio_data()
    do.call(rbind, lapply(split(all, all$Symbol), function(g) {
      n <- nrow(g)
      data.frame(Symbol = g$Symbol[1], Close = sprintf("$%.2f", g$Close[n]),
                 Growth = sprintf("%+.1f%%", g$Growth[n]), Signal = g$Signal[n])
    }))
  }, striped = TRUE, spacing = "s")

  output$data_table <- renderTable({
    out <- tail(analysed()[, c("Date", "Open", "High", "Low", "Close", "Volume", "Signal")], 50)
    out$Date <- format(out$Date); out$Volume <- format(round(out$Volume), big.mark = ",", trim = TRUE)
    out[order(out$Date, decreasing = TRUE), ]
  }, striped = TRUE, spacing = "s", digits = 2)
}

# =============================================================================
# Launch the app
# =============================================================================
shinyApp(ui, server)
