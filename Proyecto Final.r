
library(astsa)
library(quantmod)
library(forecast)
library(ggplot2)
library(tseries)


#A <-getSymbols("INTC", src = "yahoo", from = "2024-01-01", to = "2024-07-01")

#B <-getSymbols("INTC", src = "yahoo", from = "2024-01-01", to = "2024-08-15")
#C <- getSymbols("INTC", src = "yahoo", from = "2024-01-01", to = "2024-10-24")
#getSymbols("INTC", src = "yahoo", from = "2024-06-01", to = "2024-8-08")#despues caida mal portado
getSymbols("INTC", src = "yahoo", from = "2024-03-01", to = "2024-6-30") # Antes de la caida, bien portado

chartSeries(INTC, theme = chartTheme("white"), # Tema
            bar.type = "hlc",  # Alto-bajo-cierre  
            up.col = "green",  # Vela alza
            dn.col = "red")   # Vela baja)  # Plot the stock price chart for Apple

#getSymbols("NVDA", src = "yahoo", from = "2024-04-01", to = "2024-10-01")
#chartSeries(NVDA)

values <- INTC[,6]
fit <- auto.arima(values,seasonal = FALSE)
summary(fit)
plot(forecast(fit,h=10))
# Esto es para inspeccionar el acf2
series <- na.exclude(values)
series <- diff(log(series),diferences = 1)
series <- na.omit(series)
adf.test(series)
acf2(series,main="Diagnóstico Intel") 
## Stocastic volatility

y   =  as.ts(log(series^2))
num = length(y)

# Initial Parameters
phi0=0; phi1=.95; sQ=.2; alpha=mean(y); sR0=1; mu1=-3; sR1=2
init.par = c(phi0,phi1,sQ,alpha,sR0,mu1,sR1)

# Innovations Likelihood 
Linn <- function(para){
  phi0=para[1]; phi1=para[2]; sQ=para[3]; alpha=para[4]
  sR0=para[5]; mu1=para[6]; sR1=para[7]
  sv = xSVfilter(num,y,phi0,phi1,sQ,alpha,sR0,mu1,sR1)
  return(sv$like)    
}

# Estimation  
(est = optim(init.par, Linn, NULL, method="BFGS", hessian=TRUE, control=list(trace=1,REPORT=1)))
SE = sqrt(diag(solve(est$hessian)))
u = cbind(estimates=est$par, SE)  
rownames(u)=c("phi0","phi1","sQ","alpha","sigv0","mu1","sigv1"); 

# Graphics   (need filters at the estimated parameters)
phi0=est$par[1]; phi1=est$par[2]; sQ=est$par[3]; alpha=est$par[4]
sR0=est$par[5]; mu1=est$par[6]; sR1=est$par[7]
sv = xSVfilter(num,y,phi0,phi1,sQ,alpha,sR0,mu1,sR1) 

# densities plot (f is chi-sq, fm is fitted mixture)
x = seq(-15,6,by=.01) 
f = exp(-.5*(exp(x)-x))/(sqrt(2*pi))
f0 = exp(-.5*(x^2)/sR0^2)/(sR0*sqrt(2*pi))
f1 = exp(-.5*(x-mu1)^2/sR1^2)/(sR1*sqrt(2*pi))
fm = (f0+f1)/2
tsplot(x, f, xlab='x')
lines(x, fm, lty=2, lwd=2)
legend('topleft', legend=c('log chi-square', 'normal mixture'), lty=1:2)



##############################################################################
dev.new()
Time = 1:82
tsplot(Time, y/10, type='l', ,col=4, lwd=2, ylab='', xlab='',ylim=c(-2,2))
lines(Time, sv$xp[Time], lwd=2, col=6)#Volatibidad.
legend('topleft', legend=c('Intel', 'Volatilidad'), col = c(4, 6), lty=1)
