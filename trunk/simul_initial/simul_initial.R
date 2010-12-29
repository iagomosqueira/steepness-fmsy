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
res <- genBRP(age=1:25, Linf=100, k=exp(0.5235792+log(100)*-0.4540248),
  a1=1, sL=0.5, sR=150, mat95=3, s=h, v=1e3)
v <- 1e3
spr0 <- spr0(res)

ssb <- seq(0.1, 5000, length=500)
rec <- eval(as.list(bevholtSV()$model)[[3]], list(s=h, v=v, spr0=spr0, ssb=ssb))

# Add variability with fixed CV at each SSB value, env. stochasticity

# Estimate s & v

# Distribution of FMSY/Fcrash ratio for each value of h

# Repeat for different levels of noise (CV)

# Repeat for different values of h

# Check CV(SR) vs. CV(h)


# SIM 3:

# Create FLStock

# fwd() @ FMSY_original w/ SR noise

# FMSY/Fcrash
