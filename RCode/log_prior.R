# How to calculate the log prior
# this is just a record of some distribution simulations

summary(log(runif(1000,.05,.5)) - log(runif(1000,.01,.5)))
exp(-2.15)
summary(log(runif(1000,.01,.5)) - log(runif(1000,.01,.5)))
exp(-3.51)
hist(log(runif(1000,.05,.5)) - log(runif(1000,.01,.5)))
.05/.01
.04/.01
.01/.01
hist(log(runif(1000,.01,.5)) - log(runif(1000,.01,.5)))
x1 <- log(runif(1000,.01,.5)) - log(runif(1000,.01,.5))
dnorm(x1)
sum(dnorm(x1,log = T))
exp(-1556)
sum(dnorm(x1,0,2,log = T))
exp(-15)
exp(-1)

exp(-2)
sum(dnorm(x1,0,2))
sum(dnorm(x1,0,1))
sum(dnorm(x1,0,1,log=T))
install.packages("LaplacesDemon")
sum(dst(x1,0,1,log=T))
require(LaplacesDemon)
sum(dst(x1,0,1,log=T))
sum(dst(x1,0,1,nu=1,log=T))
sum(dst(x1,0,1,nu=2,log=T))
sum(dst(x1,0,1,nu=3,log=T))
sum(dst(x1,0,1,nu=4,log=T))
quantile(rnorm(1000),probs = seq(0,1,by=.01))
exp(-1.6)
exp(-2.34)
exp(2.91)
