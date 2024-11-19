library(astsa)
library(quantmod)
library(forecast)
library(ggplot2)
library(tseries)
library(readr)
library(xts)
setwd("C:/Users/kzep/Tareas Series Temporales/Proyecto Final") 

df <- read.csv("BTC_DATA.csv")
time <- as.POSIXct(df$Open.Time)
X<- df$Open
ts <- xts(X, order.by = time)
plot(ts, main = "BTC", xlab = "Fecha y Hora", ylab = "Precio $USD")
ts.plot(ts)

## Stocastic volatility



#z<- list()
#z[1]<-1
#for(i in 2:length(ts)){
#  z[i] <- X[i]-X[i-1]/X[i-1]

#}
#ts1<- unlist(z)

#y   =  as.ts(log((ts1)))

#num = length(ts1)
#y<-z
#y<- as.ts(log(ts^2))
y<-as.ts(ts)
num = length(ts)
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
(est = optim(init.par, Linn, NULL, method="L-BFGS-B", hessian=TRUE, control=list(trace=1,REPORT=1)))
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
Time = 1:72
tsplot(Time, y/250, type='l', ,col=4, lwd=2, ylab='', xlab='',ylim=c(-.1,.1))
lines(Time, sv$xp[Time], lwd=2, col=6)#Volatibidad.
legend('topleft', legend=c('BTC', 'Volatilidad'), col = c(4, 6), lty=1)