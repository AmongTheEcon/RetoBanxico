###
###   Seasonal mensual inflation forecast for Mexico
###   Emiliano Elias Dena
###

###   Libraries
library(readxl)
library(dplyr)
library(tseries)
library(ggfortify)
library(forecast)

###   Data
INPC <- read_excel("Data/Indicadores20200319035937.xls", 
                   range = "B5:B247") %>% 
          ts(start = c(2000, 1), frequency = 12)

###   Plot
autoplot(INPC)
Acf(diff(INPC))
Pacf(diff(INPC))
### Test
adf.test(INPC)
adf.test(diff(INPC))
### SARIMA
SARIMA <- auto.arima(y = INPC, approximation = FALSE, trace = TRUE, method = "ML")
summary(SARIMA)

checkresiduals(SARIMA)
Acf(residuals(SARIMA))
Pacf(residuals(SARIMA))

F_S <- forecast(SARIMA, 12,level=95)
F_S2 <- forecast(SARIMA, 12,level=90)
autoplot(F_S)    

summary(F_S)
