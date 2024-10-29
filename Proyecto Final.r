dev.new()
library(astsa)
library(quantmod)
library(forecast)
library(ggplot2)
library(tseries)


#A <-getSymbols("INTC", src = "yahoo", from = "2024-01-01", to = "2024-07-01")
#B <-getSymbols("INTC", src = "yahoo", from = "2024-01-01", to = "2024-08-15")
#C <- getSymbols("INTC", src = "yahoo", from = "2024-01-01", to = "2024-10-24")
#getSymbols("INTC", src = "yahoo", from = "2024-06-01", to = "2024-8-08")#despues caida
getSymbols("INTC", src = "yahoo", from = "2024-03-01", to = "2024-6-30")

chartSeries(INTC)  # Plot the stock price chart for Apple

#getSymbols("NVDA", src = "yahoo", from = "2024-04-01", to = "2024-10-01")
#chartSeries(NVDA)

values <- INTC[,6]
fit <- auto.arima(values)
summary(fit)
#plot(forecast(fit,h=10))

resultado_adf <- adf.test(log(values))
print(resultado_adf)

###########################
# PRIMRO VEAMOS LAMBDA POSITIVOS
###########################
lambda_list <- list()
p_list <- list()
for(i in 1:110){
  lambda <- i
  lambda_list[i]<- i
  values_2 <- (values^i-1)/i
  
  test <- adf.test(values_2)
  p_list[i] <- test$p.value
}

plot(lambda_list,p_list)
abline(h=0.05, col="red", lty=2) 

#############################
# Lambda negaticos
##############################

lambda_list <- list()
p_list <- list()
for(i in 1:110){
  lambda <- -i
  lambda_list[i]<- i
  values_2 <- (values^lambda-1)/lambda
  
  test <- adf.test(values_2)
  p_list[i] <- test$p.value
}

plot(lambda_list,p_list)
abline(h=0.05, col="red", lty=2) 

