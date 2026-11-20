# Assignment 6 - Stock Portfolio Dashboard (R Shiny)

Author: Ashviniben Tailor - BDA400 Data Science Tools and Techniques

## How to run
1. Open `app.R` in RStudio.
2. Click **Run App** (or run `shiny::runApp("Assignment6")`). Missing packages (shiny, ggplot2, quantmod) are installed on first run.
3. Choose **Yahoo Finance** as the data source (needs internet) or **Demo data** to work offline.

## Features
- Step 1: symbol, date range and time frame (Daily / Weekly / Monthly) controls; candlestick, line and area charts.
- Step 2: Moving Averages (SMA/EMA), Bollinger Bands, RSI and MACD, calculated in base R from the Assignment 5 functions.
- Step 3: Trading rules (MA trend, RSI zones, MACD vs signal line) give a Buy / Hold / Sell signal per bar, shown as a coloured ribbon and BUY/SELL markers, plus a signal log.
- Step 4: Portfolio tab reads `portfolio.txt` (META, V, HD, PEP, AMD), plots growth since the start of the date range and lists each symbol's current signal.

## Files
- `app.R` - the dashboard
- `portfolio.txt` - portfolio symbols, one per line
