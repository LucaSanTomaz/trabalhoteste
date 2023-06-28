
#Bibliotecas
library(dplyr)
library(tidyverse)
library(yfR)
library(rugarch)
library(tseries)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(nortest)
# library(GAS)

#Importação dos Dados
nome_acao1 <- "UGPA3.SA"   # Código no Yahoo Finance
data_ini1  <- "2018-01-01" # Data de inicio
data_fim1  <- Sys.Date() # Data de fim
UGPA <- yf_get(tickers = nome_acao1, first_date = data_ini1, last_date = data_fim1)
UGPA <- UGPA[-1,]

centrada1 <- na.omit(UGPA$ret_adjusted_prices) - mean(UGPA$ret_adjusted_prices, na.rm = T)

data_filtered <- UGPA %>%
  filter(ref_date >= data_ini1, ref_date <= data_fim1) %>%
  select(ret_adjusted_prices) %>%
  na.omit()

# EGarch(1,1) std
spec1 <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                    variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                    distribution = 'std')

fit1 <- ugarchfit(spec1, centrada1, solver = 'hybrid')

fore <- ugarchforecast(fit1, n.ahead = 1)
fore

forecasted_values <- fore@forecast$seriesFor[,1]
sigma_values <- fore@forecast$sigmaFor[,1]

forecast_data <- data.frame(
  Date = seq(from = length(data_filtered$ret_adjusted_prices) + 1, to = length(data_filtered$ret_adjusted_prices) + 1),
  Value = forecasted_values,
  Upper = forecasted_values + 2 * sigma_values,
  Lower = forecasted_values - 2 * sigma_values,
  Type = "Forecast"
)

original_data <- data.frame(
  Date = 1:length(data_filtered$ret_adjusted_prices),
  Value = data_filtered$ret_adjusted_prices,
  Upper = NA,
  Lower = NA,
  Type = "Original"
)

total_data <- rbind(original_data, forecast_data)
total_data <- total_data[(nrow(total_data) - 20):nrow(total_data),]


write.csv(total_data, file = "R/Previsao.csv")
