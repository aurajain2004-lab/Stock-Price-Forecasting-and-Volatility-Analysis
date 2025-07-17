install.packages("tidyverse")    
install.packages("forecast")     
install.packages("lubridate")    
install.packages("tseries")     
library(tidyverse)
library(forecast)
library(lubridate)
library(tseries)
library(dplyr)

stock_data <- read.csv("C:/Users/skyline/OneDrive/Desktop/top_10_stocks_17-24.csv", stringsAsFactors = FALSE)
str(stock_data)
stock_data$date <- as.Date(stock_data$date)
colSums(is.na(stock_data))


stock_data <- stock_data %>%
  select(date, symbol, close)

stock_data <- stock_data %>%
  arrange(symbol, date)

unique(stock_data$symbol)

library(ggplot2)

ggplot(filter(stock_data, symbol == "AI"), aes(x = date, y = close)) +
  geom_line(color = "steelblue") +
  labs(title = "AI Stock Price Trend (2017–2024)",
       x = "Date", y = "Closing Price") +
  theme_minimal()

ggplot(filter(stock_data, symbol == "AMD"), aes(x = date, y = close)) +
  geom_line(color = "red") +
  labs(title = "AMD Stock Price Trend (2017–2024)",
       x = "Date", y = "Closing Price") +
  theme_minimal()

ggplot(filter(stock_data, symbol == "AMZN"), aes(x = date, y = close)) +
  geom_line(color = "green") +
  labs(title = "AMZN Stock Price Trend (2017–2024)",
       x = "Date", y = "Closing Price") +
  theme_minimal()

ggplot(filter(stock_data, symbol == "ANET"), aes(x = date, y = close)) +
  geom_line(color = "yellow") +
  labs(title = "ANET Stock Price Trend (2017–2024)",
       x = "Date", y = "Closing Price") +
  theme_minimal()

ggplot(filter(stock_data, symbol == "META"), aes(x = date, y = close)) +
  geom_line(color = "purple") +
  labs(title = "META Stock Price Trend (2017–2024)",
       x = "Date", y = "Closing Price") +
  theme_minimal()

ggplot(filter(stock_data, symbol == "NOW"), aes(x = date, y = close)) +
  geom_line(color = "orange") +
  labs(title = "NOW Stock Price Trend (2017–2024)",
       x = "Date", y = "Closing Price") +
  theme_minimal()

ggplot(filter(stock_data, symbol == "NVDA"), aes(x = date, y = close)) +
  geom_line(color = "blue") +
  labs(title = "NVDA Stock Price Trend (2017–2024)",
       x = "Date", y = "Closing Price") +
  theme_minimal()

ggplot(filter(stock_data, symbol == "PANW"), aes(x = date, y = close)) +
  geom_line(color = "brown") +
  labs(title = "PANW Stock Price Trend (2017–2024)",
       x = "Date", y = "Closing Price") +
  theme_minimal()

ggplot(filter(stock_data, symbol == "PATH"), aes(x = date, y = close)) +
  geom_line(color = "pink") +
  labs(title = "PATH Stock Price Trend (2017–2024)",
       x = "Date", y = "Closing Price") +
  theme_minimal()

ggplot(filter(stock_data, symbol == "TSLA"), aes(x = date, y = close)) +
  geom_line(color = "black") +
  labs(title = "TESLA Stock Price Trend (2017–2024)",
       x = "Date", y = "Closing Price") +
  theme_minimal()

library(ggplot2)

ggplot(stock_data, aes(x = date, y = close, color = symbol)) +
  geom_line() +
  labs(title = "Stock Price Trends (2017–2024)",
       x = "Date", y = "Closing Price",
       color = "Stock Symbol") +
  theme_minimal()



summary_stats <- stock_data %>%
  group_by(symbol) %>%
  summarise(
    Min = min(close, na.rm = TRUE),
    Max = max(close, na.rm = TRUE),
    Mean = mean(close, na.rm = TRUE),
    Median = median(close, na.rm = TRUE),
    SD = sd(close, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)

ggplot(summary_stats, aes(x = reorder(symbol, -Mean), y = Mean, fill = symbol)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Average Closing Price by Stock (2017–2024)",
       x = "Stock Symbol", y = "Average Price") +
  theme_minimal()

colSums(is.na(stock_data))
stock_data <- na.omit(stock_data)

install.packages("psych") 
library(psych)

describeBy(stock_data$close, group = stock_data$symbol)

library(ggplot2)

ggplot(stock_data, aes(x = close, fill = symbol, color = symbol)) +
  geom_density(alpha = 0.4) +
  labs(title = "Density Plot of Closing Prices per Stock",
       x = "Closing Price",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")

library(tidyr)
install.packages("reshape2")
library(reshape2)
library(ggplot2)
install.packages("viridis")
library(viridis)

wide_data <- stock_data %>%
  select(date, symbol, close) %>%
  pivot_wider(names_from = symbol, values_from = close)

wide_data_clean <- na.omit(wide_data)

# Convert all columns (except date) to numeric
wide_data_clean[-1] <- lapply(wide_data_clean[-1], function(x) as.numeric(as.character(x)))
str(wide_data_clean)
cor_matrix <- cor(wide_data_clean[-1])

install.packages("corrplot")
library(corrplot)

corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7)

wide_data_clean[-1] <- lapply(wide_data_clean[-1], function(x) as.numeric(as.character(x)))
cor_matrix <- cor(wide_data_clean[-1], use = "complete.obs")
corrplot(cor_matrix, 
         method = "color",        # color blocks
         type = "upper",          # only upper triangle
         col = colorRampPalette(c("red", "white", "blue"))(200),
         tl.col = "black",        # text color
         tl.srt = 45,             # text rotation
         addCoef.col = "black",   # show correlation values
         number.cex = 0.7,        # font size
         mar = c(0, 0, 1, 0),     # margin
         title = "Correlation Heatmap of Top 10 Stocks")

wide_data_numeric <- wide_data_clean
wide_data_numeric <- wide_data_numeric[, colSums(is.na(wide_data_numeric)) < nrow(wide_data_numeric)]
cor_matrix <- cor(wide_data_numeric[-1], use = "complete.obs")
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         col = colorRampPalette(c("red", "white", "blue"))(200),
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         mar = c(0, 0, 1, 0),
         title = "Correlation Heatmap of Top 10 Stocks")
colSums(is.na(wide_data_clean))
str(wide_data_clean)
cor_matrix <- cor(wide_data_clean[-1], use = "pairwise.complete.obs")
corrplot::corrplot(cor_matrix, method = "color", 
                   type = "upper", 
                   tl.col = "black", 
                   tl.srt = 45,
                   col = viridis::viridis(200),
                   addCoef.col = "black")

wide_data_filtered <- wide_data_clean %>%
  filter(rowSums(is.na(across(-date))) < 8)

cor_matrix <- cor(wide_data_filtered[-1], use = "pairwise.complete.obs")

corrplot::corrplot(cor_matrix, method = "color", type = "lower", 
                   col = viridis::viridis(200), 
                   tl.col = "black", tl.srt = 45,
                   addCoef.col = "black", number.cex = 0.7)

head(wide_data_clean)

library(readr)
raw_data <- read_csv("C:/Users/skyline/OneDrive/Desktop/top_10_stocks_17-24.csv")

head(raw_data)

str(raw_data)
library(dplyr)
library(tidyr)
library(lubridate)

raw_data <- raw_data %>%
  mutate(date = dmy(date))

selected_data <- raw_data %>%
  select(symbol, date, close)

wide_data <- selected_data %>%
  pivot_wider(names_from = symbol, values_from = close)

wide_data <- wide_data %>%
  arrange(date)

wide_data_clean <- wide_data %>%
  filter(if_any(-date, ~ !is.na(.)))

library(ggplot2)
library(reshape2)
library(viridis)
cor_matrix <- cor(wide_data_clean[-1], use = "complete.obs")
melted_corr <- melt(cor_matrix)
ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Correlation", limits = c(-1, 1)) +
  labs(
    title = "Correlation Heatmap of Stock Closing Prices",
    x = "Stock Symbol",
    y = "Stock Symbol"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

library(forecast)
library(ggplot2)
nvda_data <- raw_data %>%
  filter(symbol == "NVDA") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  arrange(date)
nvda_ts <- ts(nvda_data$close, frequency = 365, start = c(2017, 1))
nvda_decomp <- stl(nvda_ts, s.window = "periodic")
autoplot(nvda_decomp) +
  labs(title = "Time Series Decomposition: NVDA", x = "Time")

ai_data <- raw_data %>%
  filter(symbol == "AI") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  arrange(date)
ai_ts <- ts(ai_data$close, frequency = 365, start = c(2017, 1))
ai_decomp <- stl(ai_ts, s.window = "periodic")
autoplot(ai_decomp) +
  labs(title = "Time Series Decomposition: AI", x = "Time")



amd_data <- raw_data %>%
  filter(symbol == "AMD") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  arrange(date)
amd_ts <- ts(amd_data$close, frequency = 365, start = c(2017, 1))
amd_decomp <- stl(amd_ts, s.window = "periodic")
autoplot(amd_decomp) +
  labs(title = "Time Series Decomposition: AMD", x = "Time")

tsla_data <- raw_data %>%
  filter(symbol == "TSLA") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  arrange(date)
tsla_ts <- ts(tsla_data$close, frequency = 365, start = c(2017, 1))
tsla_decomp <- stl(tsla_ts, s.window = "periodic")
autoplot(tsla_decomp) +
  labs(title = "Time Series Decomposition: TSLA", x = "Time")

panw_data <- raw_data %>%
  filter(symbol == "PANW") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  arrange(date)
panw_ts <- ts(panw_data$close, frequency = 365, start = c(2017, 1))
panw_decomp <- stl(panw_ts, s.window = "periodic")
autoplot(panw_decomp) +
  labs(title = "Time Series Decomposition: PANW", x = "Time")

path_data <- raw_data %>%
  filter(symbol == "PATH") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  arrange(date)
path_ts <- ts(path_data$close, frequency = 365, start = c(2017, 1))
path_decomp <- stl(path_ts, s.window = "periodic")
autoplot(path_decomp) +
  labs(title = "Time Series Decomposition: PATH", x = "Time")

anet_data <- raw_data %>%
  filter(symbol == "ANET") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  arrange(date)

anet_ts <- ts(anet_data$close, frequency = 365, start = c(2017, 1))
anet_decomp <- stl(anet_ts, s.window = "periodic")
autoplot(anet_decomp) +
  labs(title = "Time Series Decomposition: ANET", x = "Time")

meta_data <- raw_data %>%
  filter(symbol == "META") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  arrange(date)

meta_ts <- ts(meta_data$close, frequency = 365, start = c(2017, 1))
meta_decomp <- stl(meta_ts, s.window = "periodic")
autoplot(meta_decomp) +
  labs(title = "Time Series Decomposition: META", x = "Time")

amzn_data <- raw_data %>%
  filter(symbol == "AMZN") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  arrange(date)

amzn_ts <- ts(amzn_data$close, frequency = 365, start = c(2017, 1))
amzn_decomp <- stl(amzn_ts, s.window = "periodic")
autoplot(amzn_decomp) +
  labs(title = "Time Series Decomposition: AMZN", x = "Time")

now_data <- raw_data %>%
  filter(symbol == "NOW") %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  arrange(date)

now_ts <- ts(now_data$close, frequency = 365, start = c(2017, 1))
now_decomp <- stl(now_ts, s.window = "periodic")
autoplot(now_decomp) +
  labs(title = "Time Series Decomposition: NOW", x = "Time")

library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)

raw_data <- raw_data %>%
  mutate(date = as.Date(date, format = "%d-%m-%Y"))
stock_list <- unique(raw_data$symbol)

forecast_list <- list()
for (stock in stock_list) {
  stock_data <- raw_data %>%
    filter(symbol == stock) %>%
    arrange(date)
  ts_data <- ts(stock_data$close, frequency = 365)
  model <- auto.arima(ts_data)
  forecast_result <- forecast(model, h = 30)
    last_date <- max(stock_data$date)
  forecast_dates <- seq.Date(from = last_date + 1, by = "day", length.out = 30)
  forecast_df <- data.frame(
    date = forecast_dates,
    close = as.numeric(forecast_result$mean),
    symbol = stock,
    type = "Forecast"
  )
  actual_df <- stock_data %>%
    select(date, close) %>%
    mutate(symbol = stock, type = "Actual")
  
  forecast_list[[stock]] <- rbind(actual_df, forecast_df)
}

all_forecast_df <- do.call(rbind, forecast_list)
ggplot(all_forecast_df, aes(x = date, y = close, color = symbol, linetype = type)) +
  geom_line(size = 1) +
  labs(title = "ARIMA Forecasts for All Stocks", x = "Date", y = "Closing Price") +
  theme_minimal()

library(zoo)

# Calculate 30-day rolling standard deviation for each stock
rolling_volatility <- raw_data %>%
  arrange(symbol, date) %>%
  group_by(symbol) %>%
  mutate(rolling_sd = rollapply(close, width = 30, FUN = sd, fill = NA, align = "right"))

# Plot all stocks' volatility together
ggplot(rolling_volatility, aes(x = date, y = rolling_sd, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "30-Day Rolling Volatility of Stocks",
       x = "Date",
       y = "Rolling Std Dev (Volatility)") +
  theme_minimal()

install.packages("torch")
library(torch)
torch::install_torch()

library(torch)

# Extract Close prices
stock_prices <- raw_data$close

# Normalize the data
price_min <- min(stock_prices, na.rm = TRUE)
price_max <- max(stock_prices, na.rm = TRUE)
normalized_prices <- (stock_prices - price_min) / (price_max - price_min)

# Create sequences for LSTM
create_sequences <- function(data, seq_length = 30) {
  x <- list()
  y <- list()
  for (i in 1:(length(data) - seq_length)) {
    x[[i]] <- data[i:(i + seq_length - 1)]
    y[[i]] <- data[i + seq_length]
  }
  list(x = x, y = y)
}

# Create sequences
seq_length <- 30
sequences <- create_sequences(normalized_prices, seq_length)

# Convert to tensors
x_tensor <- torch_tensor(do.call(rbind, sequences$x), dtype = torch_float())
y_tensor <- torch_tensor(unlist(sequences$y), dtype = torch_float())

# Reshape input to (samples, time steps, features)
x_tensor <- x_tensor$unsqueeze(3)  # Add feature dimension

# Split into training and test sets (80-20 split)
train_size <- floor(0.8 * x_tensor$size(1))
x_train <- x_tensor[1:train_size,,]
y_train <- y_tensor[1:train_size]

x_test <- x_tensor[(train_size+1):x_tensor$size(1),,]
y_test <- y_tensor[(train_size+1):y_tensor$size(1)]


lstm_model <- nn_module(
  "StockLSTM",
  
  initialize = function(input_size, hidden_size, num_layers, output_size) {
    self$lstm <- nn_lstm(
      input_size = input_size,
      hidden_size = hidden_size,
      num_layers = num_layers,
      batch_first = TRUE
    )
    self$fc <- nn_linear(hidden_size, output_size)
  },
  
  forward = function(x) {
    # x: [batch_size, seq_len, input_size]
    out <- self$lstm(x)
    out <- out[[1]][, -1, ]  # Take output of the last time step
    out <- self$fc(out)
    out
  }
)

# Model parameters
input_size <- 1       # one feature: Close price
hidden_size <- 50     # number of LSTM units
num_layers <- 1       # one LSTM layer
output_size <- 1      # predicting one value

model <- lstm_model(input_size, hidden_size, num_layers, output_size)



# Loss and optimizer
criterion <- nn_mse_loss()
optimizer <- optim_adam(model$parameters, lr = 0.001)

# Training loop
num_epochs <- 50

for (epoch in 1:num_epochs) {
  model$train()
  optimizer$zero_grad()
  
  # Forward pass
  output <- model(x_train)
  loss <- criterion(output, y_train)
  
  # Backward pass
  loss$backward()
  optimizer$step()
  
  if (epoch %% 10 == 0 || epoch == 1) {
    cat(sprintf("Epoch [%d/%d], Loss: %.6f\n", epoch, num_epochs, loss$item()))
  }
}

# Set model to training mode
model$train()

# Loss and optimizer
criterion <- nn_mse_loss()
optimizer <- optim_adam(model$parameters, lr = 0.001)

# Number of epochs
num_epochs <- 50

for (epoch in 1:num_epochs) {
  optimizer$zero_grad()
  
  # Forward pass
  output <- model(x_train)
  loss <- criterion(output, y_train)
  
  # Backward and optimize
  loss$backward()
  optimizer$step()
  
  # Print loss every few epochs
  if (epoch %% 10 == 0 || epoch == 1) {
    cat(sprintf("Epoch [%d/%d], Loss: %.6f\n", epoch, num_epochs, loss$item()))
  }
}

model$eval()
predicted <- model(x_test)

# Convert to numeric
predicted_vals <- as.numeric(predicted)
actual_vals <- as.numeric(y_test)

# Plot
library(ggplot2)
results <- data.frame(
  Actual = actual_vals,
  Predicted = predicted_vals
)

ggplot(results, aes(x = 1:nrow(results))) +
  geom_line(aes(y = Actual, colour = "Actual")) +
  geom_line(aes(y = Predicted, colour = "Predicted")) +
  labs(title = "LSTM Stock Price Prediction",
       x = "Time Step",
       y = "Stock Price") +
  scale_color_manual(values = c("Actual" = "red", "Predicted" = "brown")) +
  theme_minimal()



# Improved LSTM model
model <- nn_module(
  initialize = function() {
    self$lstm <- nn_lstm(input_size = 1, hidden_size = 50, num_layers = 2, batch_first = TRUE)
    self$dropout <- nn_dropout(p = 0.2)
    self$fc <- nn_linear(50, 1)
  },
  
  forward = function(x) {
    lstm_out <- self$lstm(x)
    out <- lstm_out[[1]][, -1, ]   # take the last time step's output
    out <- self$dropout(out)
    out <- self$fc(out)
    out
  }
)

# Train again with more epochs
num_epochs <- 100
model <- model()
optimizer <- optim_adam(model$parameters, lr = 0.001)
loss_fn <- nn_mse_loss()

for (epoch in 1:num_epochs) {
  model$train()
  optimizer$zero_grad()
  output <- model(x_train)
  loss <- loss_fn(output, y_train)
  loss$backward()
  optimizer$step()
  
  if (epoch %% 10 == 0) {
    cat("Epoch:", epoch, "Loss:", loss$item(), "\n")
  }
}

model$eval()
predicted <- model(x_test)

# Denormalize predictions and actual values
predicted_vals <- as.numeric(predicted$to(device = "cpu"))
actual_vals <- as.numeric(y_test$to(device = "cpu"))

# If you used min-max normalization:
predicted_vals <- predicted_vals * (max_val - min_val) + min_val
actual_vals <- actual_vals * (max_val - min_val) + min_val

# Assuming raw_data is your original dataset
min_val <- min(raw_data$close, na.rm = TRUE)
max_val <- max(raw_data$close, na.rm = TRUE)

predicted_vals <- predicted_vals * (max_val - min_val) + min_val
actual_vals <- actual_vals * (max_val - min_val) + min_val


# Mean Absolute Error
mae <- mean(abs(predicted_vals - actual_vals))

# Root Mean Squared Error
rmse <- sqrt(mean((predicted_vals - actual_vals)^2))

# Mean Absolute Percentage Error
mape <- mean(abs((predicted_vals - actual_vals) / actual_vals)) * 100

cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")

library(ggplot2)
plot_df <- data.frame(
  Time = 1:length(actual_vals),
  Actual = as.numeric(actual_vals),
  Predicted = as.numeric(predicted_vals)
)

ggplot(plot_df, aes(x = Time)) +
  geom_line(aes(y = Actual), color = "blue", size = 1) +
  geom_line(aes(y = Predicted), color = "red", size = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Stock Prices",
       y = "Price",
       x = "Time") +
  theme_minimal()

summary(actual_vals)
smape <- mean(2 * abs(predicted_vals - actual_vals) / 
                (abs(actual_vals) + abs(predicted_vals))) * 100
cat("SMAPE:", smape, "%\n")

model <- nn_module(
  initialize = function() {
    self$lstm1 <- nn_lstm(input_size = 1, hidden_size = 32, batch_first = TRUE)
    self$dropout1 <- nn_dropout(0.2)
    self$lstm2 <- nn_lstm(input_size = 32, hidden_size = 16, batch_first = TRUE)
    self$fc <- nn_linear(16, 1)
  },
  forward = function(x) {
    x <- self$lstm1(x)[[1]]
    x <- self$dropout1(x)
    x <- self$lstm2(x)[[1]]
    x <- x[ , dim(x)[2], ]
    self$fc(x)
  }
)

model <- model()
model$to(device = cpu)

optimizer <- optim_adam(model$parameters, lr = 0.001)
loss_fn <- nn_mse_loss()


MyLSTMModel <- nn_module(
  initialize = function() {
    self$lstm <- nn_lstm(input_size = 1, hidden_size = 50, batch_first = TRUE)
    self$fc <- nn_linear(50, 1)
  },
  forward = function(x) {
    out <- self$lstm(x)
    out <- out[[1]][ , -1, ]
    self$fc(out)
  }
)

# Now create a fresh model object
model <- MyLSTMModel()
model$to(device = device)



library(torch)

# 1. Set device (GPU if available, else CPU)
device <- if (cuda_is_available()) torch_device("cuda") else torch_device("cpu")

# 2. Define your LSTM model class
MyLSTMModel <- nn_module(
  "MyLSTMModel",
  initialize = function(input_size, hidden_size, num_layers, output_size) {
    self$lstm <- nn_lstm(input_size = input_size, hidden_size = hidden_size, 
                         num_layers = num_layers, batch_first = TRUE)
    self$fc <- nn_linear(hidden_size, output_size)
  },
  forward = function(x) {
    out <- self$lstm(x)
    out <- out[[1]][ , dim(out[[1]])[2], ]
    out <- self$fc(out)
    out
  }
)

# 3. Prepare your data (assuming 'raw_data' is loaded and normalized)
# Take one column (say, the 2nd column)
series <- raw_data[, 2]

# Normalize (min-max)
min_val <- min(series)
max_val <- max(series)
series <- (series - min_val) / (max_val - min_val)

# Create sequences
create_sequences <- function(data, seq_len) {
  x <- list()
  y <- list()
  for (i in 1:(length(data) - seq_len)) {
    x[[i]] <- data[i:(i + seq_len - 1)]
    y[[i]] <- data[i + seq_len]
  }
  x <- torch_tensor(do.call(rbind, lapply(x, function(a) array(a, dim = c(1, length(a))))), dtype = torch_float())
  y <- torch_tensor(do.call(rbind, y), dtype = torch_float())
  list(x = x, y = y)
}

seq_len <- 10
split <- 0.8
split_idx <- floor(length(series) * split)

train_series <- series[1:split_idx]
test_series <- series[(split_idx - seq_len):length(series)]

train_data <- create_sequences(train_series, seq_len)
test_data <- create_sequences(test_series, seq_len)

x_train <- train_data$x$to(device = device)
y_train <- train_data$y$to(device = device)

x_test <- test_data$x$to(device = device)
y_test <- test_data$y$to(device = device)

# 4. Initialize model
model <- MyLSTMModel(input_size = 1, hidden_size = 16, num_layers = 1, output_size = 1)
model$to(device = device)

# 5. Loss and optimizer
criterion <- nn_mse_loss()
optimizer <- optim_adam(model$parameters, lr = 0.01)

# 6. Train
epochs <- 50
for (epoch in 1:epochs) {
  model$train()
  optimizer$zero_grad()
  output <- model(x_train$unsqueeze(3))
  loss <- criterion(output, y_train)
  loss$backward()
  optimizer$step()
  cat("Epoch:", epoch, "Loss:", loss$item(), "\n")
}

# 7. Predict
model$eval()
predicted <- with_no_grad({
  model(x_test$unsqueeze(3))
})

# 8. Convert back to original scale
predicted_vals <- as_array(predicted$squeeze()) * (max_val - min_val) + min_val
actual_vals <- as_array(y_test$squeeze()) * (max_val - min_val) + min_val

# 9. Evaluation
mae <- mean(abs(actual_vals - predicted_vals))
rmse <- sqrt(mean((actual_vals - predicted_vals)^2))
mape <- mean(abs((actual_vals - predicted_vals) / actual_vals)) * 100
smape <- mean(2 * abs(actual_vals - predicted_vals) / (abs(actual_vals) + abs(predicted_vals))) * 100

cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")
cat("SMAPE:", smape, "%\n")

# 10. Plot
plot(actual_vals, type = "l", col = "blue", lwd = 2, ylim = range(c(actual_vals, predicted_vals)), main = "Actual vs Predicted", ylab = "Value", xlab = "Time")
lines(predicted_vals, col = "red", lwd = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1, lwd = 2)


str(raw_data)


library(torch)
library(dplyr)

# STEP 1: Use the 'close' column
series <- raw_data$close

# STEP 2: Normalize the series using min-max
min_val <- min(series, na.rm = TRUE)
max_val <- max(series, na.rm = TRUE)
series_norm <- (series - min_val) / (max_val - min_val)

# STEP 3: Create sliding window for LSTM input
create_sequences <- function(series, seq_length = 10) {
  x <- list()
  y <- list()
  for (i in seq_len(length(series) - seq_length)) {
    x[[i]] <- series[i:(i + seq_length - 1)]
    y[[i]] <- series[i + seq_length]
  }
  x_tensor <- torch_tensor(do.call(rbind, x))$unsqueeze(3)
  y_tensor <- torch_tensor(unlist(y))
  list(x = x_tensor, y = y_tensor)
}

# STEP 4: Prepare data
seq_length <- 20
data <- create_sequences(series_norm, seq_length)

# Train-test split
train_size <- floor(0.8 * data$x$size(1))
x_train <- data$x[1:train_size,,]
y_train <- data$y[1:train_size]
x_test <- data$x[(train_size+1):data$x$size(1),,]
y_test <- data$y[(train_size+1):data$y$size(1)]

# STEP 5: Define LSTM Model
lstm_model <- nn_module(
  initialize = function(input_size = 1, hidden_size = 64, num_layers = 2) {
    self$lstm <- nn_lstm(
      input_size = input_size,
      hidden_size = hidden_size,
      num_layers = num_layers,
      batch_first = TRUE
    )
    self$output <- nn_linear(hidden_size, 1)
  },
  forward = function(x) {
    out <- self$lstm(x)
    hidden <- out[[1]][ , dim(out[[1]])[2], ]
    self$output(hidden)$squeeze(2)
  }
)

model <- lstm_model()

# STEP 6: Train the model
optimizer <- optim_adam(model$parameters, lr = 0.001)
loss_fn <- nn_mse_loss()

num_epochs <- 30
for (epoch in 1:num_epochs) {
  model$train()
  optimizer$zero_grad()
  output <- model(x_train)
  loss <- loss_fn(output, y_train)
  loss$backward()
  optimizer$step()
  cat("Epoch:", epoch, "Loss:", loss$item(), "\n")
}

# STEP 7: Predict
model$eval()
predicted <- model(x_test)

# STEP 8: Denormalize
predicted_vals <- as.numeric(predicted) * (max_val - min_val) + min_val
actual_vals <- as.numeric(y_test) * (max_val - min_val) + min_val

# STEP 9: Evaluation metrics
mae <- mean(abs(predicted_vals - actual_vals))
rmse <- sqrt(mean((predicted_vals - actual_vals)^2))
mape <- mean(abs((predicted_vals - actual_vals) / actual_vals)) * 100
smape <- mean(2 * abs(predicted_vals - actual_vals) / (abs(predicted_vals) + abs(actual_vals))) * 100

cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")
cat("SMAPE:", smape, "%\n")

# STEP 10: Plot
plot(actual_vals, type = "l", col = "blue", ylab = "Stock Price", main = "Actual vs Predicted")
lines(predicted_vals, col = "red")
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)


# Helper: Create sequences for LSTM
create_sequences <- function(series, n_timesteps) {
  x <- list()
  y <- list()
  for (i in seq_len(length(series) - n_timesteps)) {
    x[[i]] <- series[i:(i + n_timesteps - 1)]
    y[[i]] <- series[i + n_timesteps]
  }
  x <- torch_tensor(do.call(rbind, x))$unsqueeze(3)
  y <- torch_tensor(unlist(y))$unsqueeze(2)
  list(x = x, y = y)
}

# Model class
model_class <- nn_module(
  "StockLSTM",
  initialize = function(input_size, hidden_size, num_layers) {
    self$lstm <- nn_lstm(input_size = input_size, hidden_size = hidden_size, num_layers = num_layers, batch_first = TRUE)
    self$fc <- nn_linear(hidden_size, 1)
  },
  forward = function(x) {
    out <- self$lstm(x)
    h_n <- out[[1]][ , dim(out[[1]])[2], ]
    self$fc(h_n)
  }
)

# Loop through each stock
stocks <- unique(raw_data$symbol)
n_timesteps <- 20
num_epochs <- 30

for (sym in stocks) {
  cat("\n🔁 Processing:", sym, "\n")
  
  stock_df <- raw_data %>%
    filter(symbol == sym) %>%
    arrange(date)
  
  series <- stock_df$adjusted
  
  # Normalize
  min_val <- min(series)
  max_val <- max(series)
  norm_series <- (series - min_val) / (max_val - min_val)
  
  # Prepare sequences
  seqs <- create_sequences(norm_series, n_timesteps)
  x_all <- seqs$x
  y_all <- seqs$y
  
  # Split 80-20
  train_size <- floor(0.8 * x_all$size(1))
  x_train <- x_all[1:train_size,,]
  y_train <- y_all[1:train_size,]
  x_test <- x_all[(train_size + 1):x_all$size(1),,]
  y_test <- y_all[(train_size + 1):y_all$size(1),]
  
  # Initialize model
  model <- model_class(input_size = 1, hidden_size = 32, num_layers = 2)
  optimizer <- optim_adam(model$parameters, lr = 0.001)
  loss_fn <- nn_mse_loss()
  
  # Train
  for (epoch in 1:num_epochs) {
    model$train()
    optimizer$zero_grad()
    output <- model(x_train)
    loss <- loss_fn(output, y_train)
    loss$backward()
    optimizer$step()
  }
  
  # Predict
  model$eval()
  predicted <- model(x_test)
  predicted_vals <- as.numeric(predicted$squeeze())
  actual_vals <- as.numeric(y_test$squeeze())
  
  # Denormalize
  predicted_vals <- predicted_vals * (max_val - min_val) + min_val
  actual_vals <- actual_vals * (max_val - min_val) + min_val
  
  # Plot
  df_plot <- data.frame(
    Actual = actual_vals,
    Predicted = predicted_vals,
    Index = seq_along(actual_vals)
  )
  
  p <- ggplot(df_plot, aes(x = Index)) +
    geom_line(aes(y = Actual), color = "blue", linewidth = 1) +
    geom_line(aes(y = Predicted), color = "red", linewidth = 1, linetype = "dashed") +
    labs(title = paste("📈 Stock Prediction:", sym),
         y = "Price", x = "Time") +
    theme_minimal()
  print(p)
}


library(torch)
library(ggplot2)
library(dplyr)

symbols <- unique(raw_data$symbol)

for (sym in symbols) {
  cat("\n-----------------------------\n")
  cat("Processing:", sym, "\n")
  
  # Filter data for current symbol
  stock_data <- raw_data %>% filter(symbol == sym)
  
  # Use closing price for prediction
  series <- stock_data$close
  
  # Normalize
  min_val <- min(series, na.rm = TRUE)
  max_val <- max(series, na.rm = TRUE)
  norm_series <- (series - min_val) / (max_val - min_val)
  
  # Prepare sequences
  create_sequences <- function(data, seq_len) {
    n <- length(data)
    x <- list()
    y <- list()
    for (i in 1:(n - seq_len)) {
      x[[i]] <- data[i:(i + seq_len - 1)]
      y[[i]] <- data[i + seq_len]
    }
    list(
      x = torch_tensor(do.call(rbind, x))$unsqueeze(3),
      y = torch_tensor(unlist(y))
    )
  }
  
  seq_length <- 20
  split_index <- floor(0.8 * length(norm_series))
  train_seq <- create_sequences(norm_series[1:split_index], seq_length)
  test_seq <- create_sequences(norm_series[(split_index + 1 - seq_length):length(norm_series)], seq_length)
  
  x_train <- train_seq$x
  y_train <- train_seq$y
  x_test <- test_seq$x
  y_test <- test_seq$y
  
  # Define model
  model <- nn_module(
    "LSTMModel",
    initialize = function() {
      self$lstm <- nn_lstm(input_size = 1, hidden_size = 64, num_layers = 2, batch_first = TRUE)
      self$fc <- nn_linear(64, 1)
    },
    forward = function(x) {
      out <- self$lstm(x)
      h_n <- out[[1]][ , dim(out[[1]])[2], ]  # last time step
      self$fc(h_n)$squeeze(2)
    }
  )
  
  net <- model()
  optimizer <- optim_adam(net$parameters, lr = 0.001)
  loss_fn <- nn_mse_loss()
  
  # Train
  epochs <- 25
  for (epoch in 1:epochs) {
    net$train()
    optimizer$zero_grad()
    output <- net(x_train)
    loss <- loss_fn(output, y_train)
    loss$backward()
    optimizer$step()
    if (epoch %% 5 == 0) cat("Epoch", epoch, "Loss:", loss$item(), "\n")
  }
  
  # Predict
  net$eval()
  predicted <- net(x_test)
  
  actual_vals <- as.numeric(y_test) * (max_val - min_val) + min_val
  predicted_vals <- as.numeric(predicted) * (max_val - min_val) + min_val
  
  # Plot
  plot_df <- data.frame(
    Index = 1:length(actual_vals),
    Actual = actual_vals,
    Predicted = predicted_vals
  )
  
  p <- ggplot(plot_df, aes(x = Index)) +
    geom_line(aes(y = Actual), color = "blue", size = 1) +
    geom_line(aes(y = Predicted), color = "red", size = 1, linetype = "dashed") +
    ggtitle(paste("Prediction for", sym)) +
    theme_minimal()
  
  print(p)
  
  # Evaluation
  mae <- mean(abs(actual_vals - predicted_vals))
  rmse <- sqrt(mean((actual_vals - predicted_vals)^2))
  mape <- mean(abs((actual_vals - predicted_vals) / actual_vals)) * 100
  smape <- mean(2 * abs(predicted_vals - actual_vals) / (abs(predicted_vals) + abs(actual_vals))) * 100
  
  cat("\n📊 Evaluation for", sym, ":\n")
  cat("MAE  :", round(mae, 5), "\n")
  cat("RMSE :", round(rmse, 5), "\n")
  cat("MAPE :", round(mape, 2), "%\n")
  cat("SMAPE:", round(smape, 2), "%\n")
}


library(torch)
library(dplyr)
library(ggplot2)
library(zoo)

# Parameters
seq_length <- 50
epochs <- 50
learning_rate <- 0.001

# Loop through each unique stock
unique_stocks <- unique(raw_data$symbol)

for (stock in unique_stocks) {
  cat("Training for stock:", stock, "\n")
  
  stock_data <- raw_data %>% filter(symbol == stock)
  if (nrow(stock_data) < 200) {
    cat("Skipping", stock, "- not enough data\n")
    next
  }
  
  # Smooth close prices using rolling average
  series <- zoo::rollmean(stock_data$close, k = 5, fill = NA, align = "right")
  series <- na.omit(series)
  
  # Normalize
  min_val <- min(series)
  max_val <- max(series)
  series_scaled <- (series - min_val) / (max_val - min_val)
  series_tensor <- torch_tensor(as.numeric(series_scaled), dtype = torch_float())
  
  # Create sequences
  x <- list()
  y <- list()
  for (i in 1:(length(series_tensor) - seq_length)) {
    x[[i]] <- series_tensor[i:(i + seq_length - 1)]
    y[[i]] <- series_tensor[i + seq_length]
  }
  
  x <- torch_stack(x) %>% torch_unsqueeze(3)
  y <- torch_stack(y)
  
  # Train-test split (80-20)
  n <- x$size(1)
  train_size <- floor(0.8 * n)
  x_train <- x[1:train_size,,]
  y_train <- y[1:train_size]
  x_test <- x[(train_size+1):n,,]
  y_test <- y[(train_size+1):n]
  
  # Model
  model <- nn_module(
    "LSTMModel",
    initialize = function() {
      self$lstm <- nn_lstm(input_size = 1, hidden_size = 64, num_layers = 2, batch_first = TRUE)
      self$dropout <- nn_dropout(p = 0.2)
      self$fc <- nn_linear(64, 1)
    },
    forward = function(x) {
      out <- self$lstm(x)
      h_n <- out[[1]][ , dim(out[[1]])[2], ]
      h_n <- self$dropout(h_n)
      self$fc(h_n)$squeeze(2)
    }
  )
  
  net <- model()
  net$train()
  optimizer <- optim_adam(net$parameters, lr = learning_rate)
  loss_fn <- nn_mse_loss()
  
  for (epoch in 1:epochs) {
    optimizer$zero_grad()
    output <- net(x_train)
    loss <- loss_fn(output, y_train)
    loss$backward()
    optimizer$step()
    if (epoch %% 10 == 0) {
      cat("Epoch:", epoch, "Loss:", loss$item(), "\n")
    }
  }
  
  # Evaluation
  net$eval()
  predicted <- with_no_grad({
    net(x_test)
  })
  
  predicted_vals <- as.numeric(predicted) * (max_val - min_val) + min_val
  actual_vals <- as.numeric(y_test) * (max_val - min_val) + min_val
  
  # Plot
  df <- data.frame(
    Time = 1:length(predicted_vals),
    Predicted = predicted_vals,
    Actual = actual_vals
  )
  
  plot <- ggplot(df, aes(x = Time)) +
    geom_line(aes(y = Actual), color = "blue", size = 1.1, alpha = 0.7) +
    geom_line(aes(y = Predicted), color = "red", size = 1.1, alpha = 0.7) +
    labs(title = paste("Prediction vs Actual for", stock), y = "Price", x = "Time") +
    theme_minimal()
  
  print(plot)
  
  # Error metrics
  mae <- mean(abs(actual_vals - predicted_vals))
  rmse <- sqrt(mean((actual_vals - predicted_vals)^2))
  mape <- mean(abs((actual_vals - predicted_vals) / actual_vals)) * 100
  smape <- mean(200 * abs(predicted_vals - actual_vals) / (abs(actual_vals) + abs(predicted_vals))) * 100
  
  cat("Stock:", stock, "\n")
  cat("MAE:", round(mae, 5), "\n")
  cat("RMSE:", round(rmse, 5), "\n")
  cat("MAPE:", round(mape, 2), "%\n")
  cat("SMAPE:", round(smape, 2), "%\n\n")
}





library(ggplot2)
library(gridExtra)

# List of unique stock symbols
symbols <- unique(raw_data$symbol)

# Store all plots in a list
plot_list <- list()

# Loop through each stock symbol
for (i in seq_along(symbols)) {
  symbol <- symbols[i]
  stock_data <- raw_data[raw_data$symbol == symbol, ]
  
  # Extract close price and normalize
  series <- stock_data$close
  min_val <- min(series)
  max_val <- max(series)
  norm_series <- (series - min_val) / (max_val - min_val)
  
  # Convert to tensor and create sequence
  series_tensor <- torch_tensor(as.numeric(norm_series), dtype = torch_float())
  seq_len <- 20
  create_sequences <- function(data, seq_len) {
    n <- length(data)
    x <- list()
    y <- list()
    for (i in 1:(n - seq_len)) {
      x[[i]] <- data[i:(i + seq_len - 1)]
      y[[i]] <- data[i + seq_len]
    }
    x_tensor <- torch_stack(x)
    y_tensor <- torch_stack(y)
    list(x = x_tensor$unsqueeze(3), y = y_tensor)
  }
  
  sequences <- create_sequences(series_tensor, seq_len)
  x_data <- sequences$x
  y_data <- sequences$y
  
  # Train-test split (90-10)
  split_idx <- floor(0.9 * x_data$size(1))
  x_train <- x_data[1:split_idx, , ]
  y_train <- y_data[1:split_idx]
  x_test <- x_data[(split_idx + 1):x_data$size(1), , ]
  y_test <- y_data[(split_idx + 1):y_data$size(1)]
  
  # Define LSTM Model
  model <- nn_module(
    "LSTMModel",
    initialize = function() {
      self$lstm <- nn_lstm(input_size = 1, hidden_size = 32, num_layers = 2, batch_first = TRUE)
      self$fc <- nn_linear(32, 1)
    },
    forward = function(x) {
      out <- self$lstm(x)
      h <- out[[1]][, -1, ]
      self$fc(h)
    }
  )
  
  net <- model()
  optimizer <- optim_adam(net$parameters, lr = 0.001)
  loss_fn <- nnf_mse_loss
  
  # Training
  for (epoch in 1:25) {
    net$train()
    optimizer$zero_grad()
    output <- net(x_train)
    loss <- loss_fn(output$squeeze(), y_train)
    loss$backward()
    optimizer$step()
  }
  
  # Prediction
  net$eval()
  predicted <- net(x_test)$squeeze()
  
  predicted_vals <- as.numeric(predicted) * (max_val - min_val) + min_val
  actual_vals <- as.numeric(y_test) * (max_val - min_val) + min_val
  
  # Build plot for current stock
  df <- data.frame(
    Time = 1:length(actual_vals),
    Actual = actual_vals,
    Predicted = predicted_vals
  )
  
  p <- ggplot(df, aes(x = Time)) +
    geom_line(aes(y = Actual), color = "blue") +
    geom_line(aes(y = Predicted), color = "red", linetype = "dashed") +
    labs(title = symbol, y = "Close Price") +
    theme_minimal(base_size = 10)
  
  plot_list[[i]] <- p
}

# Combine all plots into a grid (adjust ncol/nrow as per your screen)
grid.arrange(grobs = plot_list, ncol = 2)