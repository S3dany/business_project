install.packages("readxl")
install.packages("lubridate")
library(readxl)
library(lubridate)

# Possible correlations:
# NOMINAL GDP to Inflation 

setwd("~/Desktop/BUSINESS_DATA")
# list all dataframes for simplicity, needed later
data_frames <- list("nasdaq", "personal_saving", "bank_credit", "gdp", "inflation_rates", "vix")

####### IMPORT Y DATASET #########
nasdaq <- read.csv("NASDAQ.csv")
nasdaq <- subset(nasdaq, select = c(Date, Price, Vol.))
colnames(nasdaq) <- c("DATE", "PRICE", "VOL")
nasdaq$DATE <- as.Date(nasdaq$DATE, format = "%m/%d/%Y")
nasdaq$PRICE <- as.numeric(gsub(",", "", nasdaq$PRICE))

# Function to parse 1.04B or 900.3M (volume) to value in Billions
convert_numeric <- function(value) {
  suffix <- substr(value, nchar(value), nchar(value))
  number <- as.numeric(substr(value, 1, nchar(value) - 1))
  
  if (suffix == "B") {
    converted_value <- number  
  } else if (suffix == "M") {
    converted_value <- number * 0.001
  } else {
    converted_value <- NA  # Invalid suffix
  }
  
  return(converted_value)
}

nasdaq$VOL <- sapply(nasdaq$VOL, convert_numeric)

####### IMPORT PREDICTORS DATASET AND PREPARE #######

personal_saving <- read.csv("Personal Saving Rate.csv")

bank_credit <- read.csv("Bank Credit All Commercial Banks.csv")

# SOURCE https://news.ihsmarkit.com/
gdp <- read_excel("US-Monthly-GDP-History-Data.xlsx", sheet = "Data")
colnames(gdp) <- c("DATE", "NOMINAL", "REAL")

# SOURCE: https://www.usinflationcalculator.com/inflation/current-inflation-rates/
# inflation_rates <- read.csv("inflation_rates.csv")
# monthly is seasonally adjusted
# yearly is NOT seasonally adjusted
#colnames(inflation_rates) <- c("DATE", "MONTHLY", "YEARLY")

# Read the excel file
data <- read_excel("inflation_rates_larger.xlsx")


# Convert all columns to numeric
data[,-1] <- sapply(data[,-1], as.numeric)

# Initialize an empty data frame to store the reshaped data
data_long <- data.frame(DATE = as.Date(NA), YEARLY = as.numeric(NA))

# Loop over all rows in the data
for (i in 1:nrow(data)) {
  # Loop over all months (columns 2 to 13)
  for (j in 13:2) {
    # Check if data is NA
    if (!is.na(data[i, j])) {
      # Append a new row to data_long
      date_string <- paste(as.integer(data[i, 1]), names(data)[j])
      date1 <- parse_date_time(date_string, orders = "YB")
      formatted_date1 <- format(date1, "%Y-%m-%d")
  
      data_long <- rbind(data_long, data.frame(DATE = formatted_date1, YEARLY = as.numeric(data[i, j])))
    }
  }
}

# Remove the first row of data_long (which contains the NA values used for initialization)
inflation_rates <- data_long[-1, ]

inflation_rates



# SOURCE https:/finance.yahoo.com/quote/%5EVIX/history?period1=1454284800&period2=1685577600&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true
vix <- read.csv("VIX.csv")
vix <- vix[, -7]
vix <- vix[, -6]
colnames(vix) <- c("DATE", "OPEN", "HIGH", "LOW", "CLOSE")
vix$DATE = as.Date(vix$DATE)
vix

personal_saving$DATE <- as.Date(personal_saving$DATE)
bank_credit$DATE <- as.Date(bank_credit$DATE)
gdp$DATE <- as.Date(paste0(gdp$DATE, "-01"), format = "%Y - %b-%d")
inflation_rates$DATE <- as.Date(inflation_rates$DATE)

# View the data
head(personal_saving)
head(bank_credit)
head(gdp)
head(inflation_rates)
head(vix)

# PICK LAST 5 YEARS

# Lets choose the smallest of the max as our max to avoid empty data
print(max(personal_saving$DATE))
print(max(bank_credit$DATE))
print(max(gdp$DATE))
print(max(inflation_rates$DATE))
print(max(vix$DATE))



data_frames_temp <- lapply(data_frames, function(df) {
  df = get(df)
  df$DATE <- as.Date(df$DATE)
  return(df)
})

calculate_end_date <- function(data_frames) {
  maxes <- sapply(data_frames, function(df) max(df$DATE))
  print(maxes)
  end_date <- min(maxes)
  print(end_date)
  return(end_date)
}

end_date <- calculate_end_date(data_frames_temp)
end_date <- as.Date(end_date, origin = "1970-01-01")


years_ago <- 20
start_date <- as.Date(end_date - years(years_ago))



subset_by_date <- function(data, start_date, end_date) {
  subset_data <- data[data$DATE >= start_date & data$DATE <= end_date, ]
  return(subset_data)
}

nasdaq <- subset_by_date(nasdaq, start_date, end_date)

personal_saving <- subset_by_date(personal_saving, start_date, end_date)
bank_credit <- subset_by_date(bank_credit, start_date, end_date)
gdp <- subset_by_date(gdp, start_date, end_date)
inflation_rates <- subset_by_date(inflation_rates, start_date, end_date)
vix <- subset_by_date(vix, start_date, end_date)


######## TREAT MORE ROWS FOR THE SAME MONTH ########
# in this example we take the first entry of the month and count it as it was on the first day of the month
#library(dplyr)

extract_monthly_average <- function(data, date_column, value_column) {
  # Convert the date column to Date type
  data[[date_column]] <- as.Date(data[[date_column]])
  
  # Create a new column with the first day of the month
  data$MONTH_START <- as.Date(format(data[[date_column]], "%Y-%m-01"))
  
  # Find unique MONTH_START values
  unique_dates <- unique(data$MONTH_START)
  
  # Initialize an empty data frame to store results
  average_per_month <- data.frame()
  
  # Iterate over unique dates
  for (date in unique_dates) {
    # Subset the data for the current date
    subset_data <- data[data$MONTH_START == date, ]
    
    # Compute average for the value column
    average_value <- mean(subset_data[[value_column]], na.rm = TRUE)
    
    # Add other columns to the current row
    current_row <- subset_data[1, ]
    current_row[[value_column]] <- average_value
    current_row[["DATE"]] <- date
    
    # Add current row to the results data frame
    average_per_month <- rbind(average_per_month, current_row)
  }
  
  # Reorder the columns to make "DATE" the first column
  average_per_month <- average_per_month[, c("DATE", setdiff(names(average_per_month), "DATE"))]
  average_per_month$DATE <- average_per_month$MONTH_START
  average_per_month$MONTH_START <- NULL
  return(average_per_month)
}





##### FIX DATASETS WHERE WE HAVE MORE ENTRIES PER MONTH #####
bank_credit <- extract_monthly_average(bank_credit, "DATE", "TOTBKCR")

nasdaq <- extract_monthly_average(nasdaq, "DATE", "PRICE")




######## EDA ##########
customized_plot <- function(data, y_col, ylab) {
  x_col <- "DATE"
  xlab <- "Date"
  plot(data[[x_col]], data[[y_col]], type = "l", xlab = xlab, ylab = ylab, xaxt = "n")
  axis(1, at = data[[x_col]], labels = format(data[[x_col]], "%b-%Y"), las = 2)
}


# Plotting personal savings
customized_plot(personal_saving, "PSAVERT", "Personal Savings")

# Plotting bank credit
customized_plot(bank_credit, "TOTBKCR", "Bank Credit Total")

# Plotting nominal gdp
customized_plot(gdp, "NOMINAL", "Nominal GDP")

# Plotting real gdp
customized_plot(gdp, "REAL", "Real GDP")

# Plotting monthly inflation
customized_plot(inflation_rates, "MONTHLY", "Monthly Inflation Rate")

# Plotting yearly inflation
customized_plot(inflation_rates, "YEARLY", "Monthly Inflation Rate")

#VIX
customized_plot(vix, "CLOSE", "VIX Volatility Index")


###### BUILD DATAFRAME #######


# Initialize the merged_data variable with the first data frame
merged_data <- get(data_frames[[1]])
colnames(merged_data)[-1] <- paste(data_frames[[1]], colnames(merged_data)[-1], sep = "_")

# Loop through the remaining data frames and merge them with the merged_data
for (i in 2:length(data_frames)) {
  current_df <- get(data_frames[[i]])
  colnames(current_df)[-1] <- paste(data_frames[[i]], colnames(current_df)[-1], sep = "_")
  merged_data <- merge(merged_data, current_df, by = "DATE", all = TRUE)
}

# Print the merged data frame
print(merged_data)

df <- merged_data


######## CORRELATION ########
# Load the corrplot package
library(corrplot)

# Calculate the correlation matrix
cor_matrix <- cor(subset(df, select = -c(DATE)))

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle")

##### COLLINEARITY ######
# We iteratively remove the variable with highest VIF until we are satisfied

library(car)

calculate_plot_vif <- function(data, response_variable) {
  # Fit a linear regression model
  model <- lm(paste(response_variable, "~ ."), data = data)
  
  # Calculate VIF values
  vif_values <- vif(model)
  
  # Plot VIF values
  barplot(vif_values, main = "Variance Inflation Factors", 
          ylab = "VIF Value", xlab = "Variable",
          names.arg = names(vif_values), las = 2, col = "steelblue")
  
  # Add a horizontal line at VIF = 10
  abline(h = 10, col = "red", lwd = 2, lty = 2)
}

response_var <- "nasdaq_PRICE"
calculate_plot_vif(subset(df, select = -c(DATE)), response_var)

## TODO: remove the high VIF variables

library(forecast)


########### LM TREND+SEASON ###########
# We convert the NASDAQ index to a timeseries data

df$DATE <- as.Date(df$DATE)
# Sort dataframe by date
df <- df[order(df$DATE),]

# Create the time series object
nasdaq_ts <- ts(df$nasdaq_PRICE, start=c(year(min(df$DATE)), month(min(df$DATE))), frequency=12)

plot(nasdaq_ts, main="NASDAQ Price over Time", ylab="Price", xlab="Time")

# Decompose the time series into its trend, seasonal, and random components
nasdaq_ts_decomposed <- stl(nasdaq_ts, s.window="periodic")
# Four panels: 
# 1 the original series
# 2 the estimated trend component
# 3 the estimated seasonal component
# 4 the estimated irregular component
plot(nasdaq_ts_decomposed)
# Seems like there is seasonality,
# since there is a repeating pattern with constant amplitude and frequency

# Fit the Time Series Linear Model
model <- tslm(nasdaq_ts ~ trend + season)
summary(model)

# Plot LM predictions
fit<- fitted(model)
plot(nasdaq_ts, main="NASDAQ Price over Time", ylab="Price", xlab="Time")
lines(fitted(model), col=2)

# Plot forecast
forecast_lm <- forecast(model)
plot(forecast_lm)
lines(fitted(model), col=2)

# Analysis of residuals
res<- residuals(model) 
plot(res) 
Acf(res)
# The plot seems to indicate a strong positive autocorrelation

# The Durbin-Watson test statistic ranges from 0 to 4. 
# A value of 2 means there is no autocorrelation in the sample, values < 2 suggest positive autocorrelation, and values > 2 suggest negative autocorrelation.
dwtest(model, alt="two.sided")

########### ARIMA #############
Acf(nasdaq_ts)
# Gradual decrease in ACF indicates that the series is stationary and thus needs differencing
Pacf(nasdaq_ts)

# AR(p) = 1 since in PACF the cut off is at 1
# Every price is heavily dependent on the one next to it, lag = 1

# MA(q) unknown since all lags overflow the confidence interval.
# We thus need to do differencing i.e. remove from each entry, the previous entry

diff_nasdaq <- diff(nasdaq_ts)
plot(diff_nasdaq)

Acf(diff_nasdaq)
Pacf(diff_nasdaq)
# AR(p) = 1
# I(d) = 1
# MA(q) = 1

arima_model <- Arima(nasdaq_ts, order=c(1,1,1))
summary(arima_model)

fit_arima <- fitted(arima_model)

plot(nasdaq_ts)
lines(fit_arima, col=2)

forecast_arima <- forecast(arima_model)
plot(forecast_arima)

residuals_arima <- residuals(arima_model)
tsdisplay(residuals_arima) 


# Auto ARIMA

# no seasonality
arima_auto <- auto.arima(nasdaq_ts, seasonal = FALSE)
summary(arima_auto)

# Seasonality seems to be 12

fit_auto_arima <- fitted(arima_auto)

plot(nasdaq_ts)
lines(fit_auto_arima, col=2)

forecast_arima_auto <- forecast(arima_auto)
plot(forecast_arima_auto)


# with seasonality
arima_auto <- auto.arima(nasdaq_ts, seasonal = TRUE)
summary(arima_auto)

fit_auto_arima <- fitted(arima_auto)

plot(nasdaq_ts)
lines(fit_auto_arima, col=2)

forecast_arima_auto <- forecast(arima_auto)
plot(forecast_arima_auto)



########### ARIMAX #############
# Select the eXogenous variables 
# TODO: fix once figured out collinearity and added other vars
exogenous_df = subset(df, select = c(DATE, inflation_rates_YEARLY, bank_credit_TOTBKCR, personal_saving_PSAVERT, gdp_REAL))

arimax <- auto.arima(nasdaq_ts, xreg = as.matrix(subset(exogenous_df, select = -c(DATE))))
summary(arimax)

fit_arimax <- fitted(arimax)

plot(nasdaq_ts)
lines(fit_arimax, col=2)


########## ARIMA AND ARIMAX FOR MARKET CRASH FORECAST ##########

# Get the date of the peak
max_price_index <- which.max(df$nasdaq_PRICE)
breakpoint <- df$DATE[max_price_index]
print(breakpoint)

max_price <- df$nasdaq_PRICE[max_price_index]
price_breakpoint <- which(nasdaq_ts == max_price)

# Split the dataframes into two
exogenous_train <- subset(exogenous_df, DATE <= breakpoint)
exogenous_test <- subset(exogenous_df, DATE > breakpoint)

nasdaq_train <- head(nasdaq_ts, price_breakpoint)
nasdaq_test <- tail(nasdaq_ts, length(nasdaq_ts) - price_breakpoint)


# Fit ARIMA
arima_crash <- auto.arima(nasdaq_train, seasonal = TRUE)
summary(arima_crash)

fit_arima_crash <- fitted(arima_crash)

plot(nasdaq_train)
lines(fit_arima_crash, col=2)

forecast_arima_crash <- forecast(arima_crash)
plot(forecast_arima_crash)

lines(nasdaq_test, col="red")

# Fit ARIMAX

arimax_crash <- auto.arima(nasdaq_train, xreg = as.matrix(subset(exogenous_train, select = -c(DATE))))
summary(arimax_crash)

fit_arimax_crash <- fitted(arimax_crash)

plot(nasdaq_train)
lines(fit_arimax_crash, col=2)

forecast_arimax_crash <- forecast(arimax_crash, xreg = as.matrix(subset(exogenous_test, select = -c(DATE))))
plot(forecast_arimax_crash)

lines(nasdaq_test, col="red")

########### GAM #############
