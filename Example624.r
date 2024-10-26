dev.new();
library(astsa)
library(tstools)

n.boot = 500 # number of bootstrap replicates
tol = sqrt(.Machine$double.eps) # convergence tolerance
gnpgr = diff(log(gnp))
fit = arima(gnpgr, order=c(1,0,0))
y = as.matrix(log(resid(fit)^2))
num = length(y)
plot.ts(y, ylab='')
# Initial Parameters
phi1 = .9; sQ = .5; alpha = mean(y); sR0 = 1; mu1 = -3; sR1 = 2.5
init.par = c(phi1, sQ, alpha, sR0, mu1, sR1)
# Innovations Likelihood
Linn = function(para, y.data){
phi1 = para[1]; sQ = para[2]; alpha = para[3]
sR0 = para[4]; mu1 = para[5]; sR1 = para[6]
sv = xSVfilter(num, y.data, 0, phi1, sQ, alpha, sR0, mu1, sR1)
return(sv$like) }
# Estimation
(est = optim(init.par, Linn, NULL, y.data=y, method='BFGS', hessian=TRUE,
control=list(trace=1,REPORT=1)))
SE = sqrt(diag(solve(est$hessian)))
u = rbind(estimates=est$par, SE)
colnames(u)=c('phi1','sQ','alpha','sig0','mu1','sig1'); round(u, 3)

# Bootstrap
para.star = matrix(0, n.boot, 6) # to store parameter estimates
for (jb in 1:n.boot){
    cat('iteration:', jb, '\n')
phi1 = est$par[1]; sQ = est$par[2]; alpha = est$par[3]
sR0 = est$par[4]; mu1 = est$par[5]; sR1 = est$par[6]
Q = sQ^2; R0 = sR0^2; R1 = sR1^2
sv = xSVfilter(num, y, 0, phi1, sQ, alpha, sR0, mu1, sR1)
sig0 = sv$Pp+R0; sig1 = sv$Pp+R1;
K0 = sv$Pp/sig0; K1 = sv$Pp/sig1
inn0 = y-sv$xp-alpha; inn1 = y-sv$xp-mu1-alpha
den1 = (1/sqrt(sig1))*exp(-.5*inn1^2/sig1)
den0 = (1/sqrt(sig0))*exp(-.5*inn0^2/sig0)
fpi1 = den1/(den0+den1)
# start resampling at t=4
e0 = inn0/sqrt(sig0); e1 = inn1/sqrt(sig1)
indx = sample(4:num, replace=TRUE)
sinn = cbind(c(e0[1:3], e0[indx]), c(e1[1:3], e1[indx]))
eF = matrix(c(phi1, 1, 0, 0), 2, 2)
xi = cbind(sv$xp,y) # initialize
for (i in 4:num){ # generate boot sample
G = matrix(c(0, alpha+fpi1[i]*mu1), 2, 1)
h21 = (1-fpi1[i])*sqrt(sig0[i]); h11 = h21*K0[i]
h22 = fpi1[i]*sqrt(sig1[i]); h12 = h22*K1[i]
H = matrix(c(h11,h21,h12,h22),2,2)
xi[i,] = t(eF%*%as.matrix(xi[i-1,],2) + G + H%*%as.matrix(sinn[i,],2))}
# Estimates from boot data
y.star = xi[,2]
phi1=.9; sQ=.5; alpha=mean(y.star); sR0=1; mu1=-3; sR1=2.5
init.par = c(phi1, sQ, alpha, sR0, mu1, sR1) # same as for data
est.star = optim(init.par, Linn, NULL, y.data=y.star, method='BFGS',
control=list(reltol=tol))
para.star[jb,] = cbind(est.star$par[1], abs(est.star$par[2]),
est.star$par[3], abs(est.star$par[4]), est.star$par[5],
abs(est.star$par[6])) }
# Some summary statistics and graphics
rmse = rep(NA,6) # SEs from the bootstrap
for(i in 1:6){
rmse[i] = sqrt(sum((para.star[,i]-est$par[i])^2)/n.boot)
cat(i, rmse[i],'\n') }
phi = para.star[,1]
hist(phi, 15, prob=TRUE, main='', xlim=c(.4,1.2), xlab='')
xx = seq(.4, 1.2, by=.01)
lines(xx, dnorm(xx, mean=u[1,1], sd=u[2,1]), lty='dashed', lwd=2)