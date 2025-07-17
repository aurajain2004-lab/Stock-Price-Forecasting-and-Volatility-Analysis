# Stock Price Forecasting Using ARIMA and LSTM (2017–2024)

This project focuses on forecasting stock prices and analyzing volatility trends for top AI-related companies using both traditional statistical techniques and modern deep learning approaches.

---

## Overview

- **Goal:** Predict future stock prices and assess volatility using historical data from 2017–2024.
- **Models Used:**
  - ARIMA (AutoRegressive Integrated Moving Average)
  - LSTM (Long Short-Term Memory networks using `torch` in R)
- **Tech Stack:** R, tidyverse, ggplot2, forecast, zoo, torch

---

## Dataset

- **Source:** Custom CSV file containing historical stock data for AI-related companies
- **Duration:** June 2017 – May 2024
- **Stocks:** NVDA, TSLA, AMZN, META, PANW, PATH, NOW, AI, ANET, AMD
- **Columns:**
  - `date` – Trading date
  - `symbol` – Stock ticker
  - `close`, `adjusted` – Closing prices
  - `open`, `high`, `low`, `volume`

---

## Methods

### 1. Exploratory Data Analysis (EDA)
- Line plots, density plots, and correlation heatmaps
- Summary statistics (mean, SD, median)
- Time Series Decomposition using STL

### 2. Forecasting Models
- **ARIMA:** Automatic model selection using `auto.arima()`
- **LSTM:** Implemented with `torch` in R
  - 2-layer LSTM with dropout
  - Sliding window sequence generation
  - Performance evaluated using MAE, RMSE, MAPE, SMAPE

### 3. Volatility Analysis
- 30-day rolling standard deviation to assess price stability

---



