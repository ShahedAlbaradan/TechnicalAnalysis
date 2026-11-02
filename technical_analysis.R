# BDA400 Assignment 2 - Technical Analysis (Preliminary Stage)
# Student: Shahed Albaradan
# Run this script from the folder containing portfolio.txt.

required_packages <- c("quantmod", "TTR")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) install.packages(missing_packages)
library(quantmod)
library(TTR)

load_stock_data <- function(portfolio_file = "portfolio.txt", from = "2024-01-01") {
  if (!file.exists(portfolio_file)) stop("Missing portfolio file: ", portfolio_file)
  symbols <- unique(trimws(readLines(portfolio_file, warn = FALSE)))
  symbols <- symbols[nzchar(symbols)]
  if (!length(symbols)) stop("No stock symbols found")
  results <- list()
  for (symbol in symbols) {
    stock <- tryCatch(getSymbols(symbol, src = "yahoo", from = from,
                                 auto.assign = FALSE), error = function(e) {
      warning(sprintf("Could not load %s: %s", symbol, conditionMessage(e)))
      NULL
    })
    if (!is.null(stock) && NROW(stock) > 0) {
      frame <- data.frame(Date = as.Date(index(stock)), coredata(stock),
                          row.names = NULL, check.names = FALSE)
      results[[symbol]] <- list(xts = stock, data = frame)
    }
  }
  results
}

statistical_mode <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  tab <- table(x)
  if (max(tab) == 1L) return(NA_real_) # no repeated value / unique mode
  as.numeric(names(tab)[which.max(tab)])
}

calculate_statistics <- function(stock_data, window = 20) {
  if (!length(stock_data)) return(data.frame())
  do.call(rbind, lapply(names(stock_data), function(symbol) {
    stock <- stock_data[[symbol]]$xts
    close <- as.numeric(Cl(stock))
    valid <- close[is.finite(close)]
    ma <- TTR::SMA(close, n = window)
    data.frame(Symbol = symbol, Observations = length(valid),
               Latest_Close = if (length(valid)) tail(valid, 1) else NA_real_,
               Mean = if (length(valid)) mean(valid) else NA_real_,
               Mode = statistical_mode(valid),
               Median = if (length(valid)) median(valid) else NA_real_,
               Std_Dev = if (length(valid) > 1) sd(valid) else NA_real_,
               Moving_Average_20 = if (length(ma)) as.numeric(tail(ma, 1)) else NA_real_)
  }))
}

display_stock_data <- function(stock_data, stats) {
  for (symbol in names(stock_data)) {
    cat("\n---", symbol, "---\n")
    print(utils::head(stock_data[[symbol]]$data))
    print(utils::tail(stock_data[[symbol]]$data))
    print(stats[stats$Symbol == symbol, , drop = FALSE])
  }
}

plot_stock_data <- function(stock_data, window = 20) {
  if (!length(stock_data)) return(invisible(NULL))
  for (symbol in names(stock_data)) {
    stock <- stock_data[[symbol]]$xts
    close <- Cl(stock)
    ma <- TTR::SMA(close, n = window)
    plot(index(close), as.numeric(close), type = "l", col = "navy",
         xlab = "Date", ylab = "Closing price", main = paste(symbol, "Close and 20-day SMA"))
    lines(index(ma), as.numeric(ma), col = "firebrick", lwd = 2)
    legend("topleft", legend = c("Close", "20-day SMA"),
           col = c("navy", "firebrick"), lty = 1, bty = "n")
  }
}

stock_data <- load_stock_data()
if (!length(stock_data)) stop("No data downloaded; check internet connection and stock symbols")
statistics <- calculate_statistics(stock_data)
print(statistics)
display_stock_data(stock_data, statistics)
# Save plots to a PDF for convenient review.
pdf("stock_charts.pdf", width = 10, height = 6)
plot_stock_data(stock_data)
dev.off()
write.csv(statistics, "stock_statistics.csv", row.names = FALSE)
