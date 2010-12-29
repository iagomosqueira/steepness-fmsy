# test - «Short one line description»
# test

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Wed 29 Dec 2010 12:40:00 PM CET IM:

out <- data.frame(h=seq(0.5, 0.95, by=0.05), msy=NA, crash=NA)

for (h in out$h) {

  res <- genBRP(age=1:25, Linf=100, k=exp(0.5235792+log(100)*-0.4540248),
    a1=1, sL=0.5, sR=150, mat95=3, s=h, v=1e3)

  dimnames(refpts(res))$refpt[5]<-'crash'

  res <- brp(res)

  out$msy[out$h==h] <- refpts(res)['msy','harvest',]
  out$crash[out$h==h] <- refpts(res)['crash','harvest',]
}


out$ratio<-out$msy/out$crash

plot(out$h, out$ratio, ylim=c(0,.5), type='b', pch=19)
