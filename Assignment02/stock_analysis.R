# BDA400 Assignment 2: Technical Analysis
# Author: Saravanan Gnanasekaran

library(quantmod)
library(TTR)

portfolio_file <- "portfolio.txt"

start_date <- as.Date("2025-01-01")
end_date <- as.Date("2026-01-01")

price_field <- "Close"
ma_window <- 20

# Read portfolio symbols and return one data frame per stock
load_stock_data <- function(portfolio_file, start_date, end_date) {
  
  symbols <- unique(trimws(readLines(portfolio_file)))
  symbols <- symbols[nzchar(symbols)]
  
  if (length(symbols) == 0) {
    stop("The portfolio file contains no stock symbols.")
  }
  
  stock_data <- list()
  
  for (symbol in symbols) {
    
    message("Loading: ", symbol)
    
    prices <- quantmod::getSymbols(
      Symbols = symbol,
      src = "yahoo",
      from = start_date,
      to = end_date,
      auto.assign = FALSE
    )
    
    if (NROW(prices) == 0) {
      stop("No data returned for: ", symbol)
    }
    
    stock_df <- data.frame(
      Date = as.Date(zoo::index(prices)),
      zoo::coredata(prices),
      row.names = NULL
    )
    
    # Standardize names: AAPL.Close becomes Close
    names(stock_df)[-1] <- sub(
      paste0(symbol, "."),
      "",
      names(stock_df)[-1],
      fixed = TRUE
    )
    
    stock_data[[symbol]] <- stock_df
  }
  
  return(stock_data)
}

# Download data for the portfolio
stock_data <- load_stock_data(
  portfolio_file,
  start_date,
  end_date
)

# Check which stocks were loaded
names(stock_data)

# Confirm each stock is stored as a data frame
sapply(stock_data, is.data.frame)

# Check the number of records per stock
sapply(stock_data, nrow)

# Check the actual date range for each stock
lapply(stock_data, function(x) range(x$Date))

# Preview Apple's records and column names
head(stock_data[["AAPL"]])
names(stock_data[["AAPL"]])

calculate_statistics <- function(stock_df,
                                 price_field = "Close",
                                 ma_window = 20) {
  
  # Keep records in chronological order
  stock_df <- stock_df[order(stock_df$Date), ]
  
  prices <- stock_df[[price_field]]
  
  if (!is.numeric(prices)) {
    stop("The selected price column must be numeric.")
  }
  
  if (any(!is.finite(prices))) {
    stop("Prices contain missing or non-finite values.")
  }
  
  if (length(prices) < max(2, ma_window)) {
    stop("Not enough records to calculate the statistics.")
  }
  
  # Statistical mode: retain all values tied for highest frequency
  unique_prices <- unique(prices)
  counts <- tabulate(match(prices, unique_prices))
  highest_count <- max(counts)
  
  mode_values <- if (highest_count == 1) {
    numeric(0)
  } else {
    sort(unique_prices[counts == highest_count])
  }
  
  moving_average <- as.numeric(
    TTR::SMA(prices, n = ma_window)
  )
  
  summary <- data.frame(
    Price_Field = price_field,
    Observations = length(prices),
    Mean = mean(prices),
    Mode = if (length(mode_values) == 0) {
      "No repeated values"
    } else {
      paste(mode_values, collapse = ", ")
    },
    Mode_Frequency = highest_count,
    Median = median(prices),
    Sample_SD = sd(prices),
    MA_Window = ma_window,
    Latest_SMA = tail(moving_average, 1)
  )
  
  moving_average_data <- data.frame(
    Date = stock_df$Date,
    Price = prices,
    SMA = moving_average
  )
  
  return(list(
    summary = summary,
    moving_average = moving_average_data,
    mode_values = mode_values
  ))
}

stock_statistics <- lapply(
  stock_data,
  calculate_statistics,
  price_field = price_field,
  ma_window = ma_window
)

# Display each stock's summary
lapply(stock_statistics, function(x) x$summary)

apple_prices <- stock_data[["AAPL"]]$Close
apple_summary <- stock_statistics[["AAPL"]]$summary

# Independently express mean and sample standard deviation
manual_mean <- sum(apple_prices) / length(apple_prices)

manual_sd <- sqrt(
  sum((apple_prices - manual_mean)^2) /
    (length(apple_prices) - 1)
)

# Median from the two middle sorted values
sorted_prices <- sort(apple_prices)
n <- length(sorted_prices)
manual_median <- (
  sorted_prices[n / 2] + sorted_prices[n / 2 + 1]
) / 2

# Latest moving average from the last 20 observations
manual_sma <- mean(tail(apple_prices, ma_window))

all.equal(apple_summary$Mean, manual_mean)
all.equal(apple_summary$Median, manual_median)
all.equal(apple_summary$Sample_SD, manual_sd)
all.equal(apple_summary$Latest_SMA, manual_sma)

# Most frequent Apple prices
apple_frequencies <- sort(table(apple_prices), decreasing = TRUE)
head(apple_frequencies, 10)

# Compare with the function's mode
stock_statistics[["AAPL"]]$mode_values

# Confirm initial SMA values are unavailable
head(stock_statistics[["AAPL"]]$moving_average, 21)

# Check missing SMA counts for every stock
sapply(stock_statistics, function(x) {
  sum(is.na(x$moving_average$SMA))
})

lapply(stock_statistics, function(x) x$summary)

#Function to display stock records and statistics

display_stock_data <- function(stock_data, stock_statistics,
                               preview_rows = 6) {
  
  for (symbol in names(stock_data)) {
    
    cat("\n========================================\n")
    cat("Stock:", symbol, "\n")
    cat("Records:", nrow(stock_data[[symbol]]), "\n")
    
    cat("\nFirst records:\n")
    print(head(stock_data[[symbol]], preview_rows))
    
    cat("\nLast records:\n")
    print(tail(stock_data[[symbol]], preview_rows))
    
    cat("\nStatistics:\n")
    print(stock_statistics[[symbol]]$summary)
  }
}

#Function to plot closing prices and moving averages

plot_stock_data <- function(stock_statistics) {
  
  for (symbol in names(stock_statistics)) {
    
    chart_data <- stock_statistics[[symbol]]$moving_average
    window <- stock_statistics[[symbol]]$summary$MA_Window
    
    plot(
      chart_data$Date,
      chart_data$Price,
      type = "l",
      col = "steelblue",
      xlab = "Date",
      ylab = "Closing Price (USD)",
      main = paste(symbol, "- Closing Price and SMA"),
      ylim = range(
        c(chart_data$Price, chart_data$SMA),
        na.rm = TRUE
      )
    )
    
    lines(
      chart_data$Date,
      chart_data$SMA,
      col = "darkorange",
      lwd = 2
    )
    
    legend(
      "topleft",
      legend = c("Close", paste(window, "observation SMA")),
      col = c("steelblue", "darkorange"),
      lty = 1,
      lwd = c(1, 2),
      bty = "n"
    )
  }
}

display_stock_data(stock_data, stock_statistics)
plot_stock_data(stock_statistics)