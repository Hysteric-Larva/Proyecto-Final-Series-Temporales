dev.new()
library(astsa)
library(fGarch)

plot(gnp)
acf2(gnp, 50)
gnpgr = diff(log(gnp)) # growth rate
plot(gnpgr)
acf2(gnpgr, 24)
sarima(gnpgr, 1, 0, 0) # AR(1)
sarima(gnpgr, 0, 0, 2) # MA(2)
ARMAtoMA(ar=.35, ma=0, 10) # prints psi-weights



u = sarima(diff(log(gnp)), 1, 0, 0)
acf2(resid(u$fit), 20)

summary(garchFit(~arma(1,0)+garch(1,0), diff(log(gnp))))
