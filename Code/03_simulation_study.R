setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fit <- auto.arima(WWWusage)
plot(forecast(fit,h=20))
fit
