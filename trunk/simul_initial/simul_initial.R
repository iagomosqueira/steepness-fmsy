# simul_initial - «Short one line description»
# simul_initial

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Wed 29 Dec 2010 12:05:06 PM CET IM:

library(FLH)

# SIM 1:

# Simulate population with h = seq(0.5, 0.95, by=0.05), sr$model='bevholt'

out <- data.frame(h=seq(0.5, 0.95, by=0.05), msy=NA, crash=NA)

for (h in out$h) {

  res <- genBRP(age=1:25, Linf=100, k=exp(0.5235792+log(100)*-0.4540248),
    a1=1, sL=0.5, sR=150, mat95=3, s=h, v=1e3)

  dimnames(refpts(res))$refpt[5]<-'crash'

  res <- brp(res)

  out$msy[out$h==h] <- refpts(res)['msy','harvest',]
  out$crash[out$h==h] <- refpts(res)['crash','harvest',]
}


# Ratio FMSY/Fcrash vs. h

out$ratio<-out$msy/out$crash

plot(out$h, out$ratio, ylim=c(0,.5), type='b', pch=19)


# SIM 2:

# Based on given s & v, create SR curve

h <- 0.6
v <- 1e3
res <- genBRP(age=1:25, Linf=100, k=exp(0.5235792+log(100)*-0.4540248),
  a1=1, sL=0.5, sR=150, mat95=3, s=h, v=v)
spr0 <- spr0(res)

ssb <- FLQuant(seq(0.1, 5000, length=500))
rec <- eval(as.list(bevholtSV()$model)[[3]], list(s=h, v=v, spr0=spr0, ssb=ssb))

# Add variability with fixed CV at each SSB value, env. stochasticity
resid <- FLQuant(exp(rnorm(99*100,0,.5)), dimnames=list(age=0,year=2:99,iter=1:100))

# Better use vcov to generate correlated values of h & v
# http://www.sitmo.com/doc/Generating_Correlated_Random_Numbers
rho <- -0.89
cv <- 0.3
x <- rnorm(2000, h, h*cv)
x <- rnorm(2000, 1, 2)
x2 <- rnorm(2000, 1, 2)
x <- x[x<1 & x > 0.2][1:1000]
x2 <- rnorm(2000, v, v*cv)
x2 <- x2[x2 > 0][1:1000]
y <- rho*x + sqrt(1-(rho^2))*x2

plot(x, x2, type='p', col='black', main=rho)
points(x, y, col='red', pch=19)

myDraws <- mvrnorm(1e5, mu=c(h, v), Sigma=matrix(c((h*cv)^2,
  rho*sqrt((h*cv)^2*(v*cv)^2), rho*sqrt((h*cv)^2*(v*cv)^2), (v*cv)^2), 2, 2))

# Estimate s & v

# Distribution of FMSY/Fcrash ratio for each value of h

# Repeat for different levels of noise (CV)

# Repeat for different values of h

# Check CV(SR) vs. CV(h)


# SIM 3:

# Create FLStock

# fwd() @ FMSY_original w/ SR noise

# FMSY/Fcrash
