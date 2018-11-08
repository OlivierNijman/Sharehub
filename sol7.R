setwd("D:/University/Num Methods/week 7 and exam")

library(tidyverse)
library(readxl)
library(tidyr)
library(purrr)
library(ggplot2)
library(mvtnorm)
library(purrr)
library(boot)
library(MASS)
#library(doParallel)

rm(list = ls())

load("aex-dax(2).Rdata")
set.seed(45)

loglik <- function(p, z) {
  sum(dnorm(z, mean = p[1], sd = p[2], log = TRUE))
}

n <- 30
x <- rnorm(n, mean = 1, sd = sqrt(2.25))
p0 <- c(mu = 0, sd  = 1)
optim(p0, loglik, control = list(fnscale = -1), z = x, hessian = TRUE )

# bootstrap
B <- 10000
bootstrap.results <- matrix(NA, nrow = B, ncol = 3)
colnames(bootstrap.results) <- c("mu","sigma","convergence")
for (b in 1:B) {
  sample.b <- rnorm(n, mean = 1, sd = 1.5)
  m.b <- optim(p0, loglik, control = list(fnscale = -1), hessian =TRUE, z= sample.b)
  bootstrap.results[b,] <- c(m.b$par, m.b$convergence)
}
head(bootstrap.results)


ggplot(data.frame(bootstrap.results),aes(x=sigma)) +
  geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density')+
  stat_function(fun = dnorm, args=list(mean=mean(1.5,na.rm=TRUE),sd=1.5/(sqrt(2*30))),
                aes(colour = 'Normal')) +
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) +
  theme(legend.position = c(0.85, 0.85))+xlab("sigma")


# now the same for larger n = 3000

set.seed(45)

n <- 3000
x <- rnorm(n,mean=1,sd=1.5)
optim(c(mu=0,sd=1),loglik,control=list(fnscale=-1),z=x, hessian = TRUE)


B <- 10000
bootstrap.results <- matrix(NA,nrow=B,ncol=3)
colnames(bootstrap.results) <- c("mu","sigma","convergence")
for (b in 1:B){
  sample.b <- rnorm(n,mean=1,sd=1.5)
  m.b <- optim(c(mu=0,sd=1),loglik,control=list(fnscale=-1),z=sample.b)
  bootstrap.results[b,] <- c(m.b$par,m.b$convergence)
}
library(ggplot2)
ggplot(data.frame(bootstrap.results),aes(x=sigma)) +
  geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density')+
  stat_function(fun = dnorm, args=list(mean=mean(1.5,na.rm=TRUE),sd=1.5/(sqrt(2*3000))),
                aes(colour = 'Normal')) +
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) +
  theme(legend.position = c(0.85, 0.85))+xlab("sigma")


# optional, speed up computiations by using al cores from the computer.

# n.cores <- detectCores()
# set.seed(45)
# n <- 3000
# x <- rnorm(n,mean=1,sd=1.5)
# optim(c(mu=0,sd=1),loglik,control=list(fnscale=-1),z=x)
# 
# B <- 100000
# bootstrap.results <- matrix(NA,nrow=B,ncol=3)
# colnames(bootstrap.results) <- c("mu","sigma","convergence")
# cl <- makePSOCKcluster(n.cores-1)
# registerDoParallel(cl)
# bootstrap.results <- foreach(b=1:B,.combine=rbind) %dopar% {
#   sample.b <- rnorm(n,mean=1,sd=1.5)
#   m.b <- optim(c(mu=0,sd=1),loglik,control=list(fnscale=-1),z=sample.b)
#   c(m.b$par,m.b$convergence)
# }
# stopCluster(cl)

################## PROBLEM 2 exponbential ditr to wait time
w.time <- c(3,5,7,18,43,85,91,98,100,130,230,487)

m <- fitdistr(w.time,"exponential")
m

# nonparametric bootstrap
B <- 9999

lambda.B <- rep(NA, B)
n <- length(w.time)

for (b in 1:B) {
  b.sample <- sample(1:n, n, replace = TRUE)
  lambda.B[b] <- 1/mean(w.time[b.sample])
}

bias <- mean(lambda.B - m$estimate)
sd(lambda.B)

ggplot(data=data.frame(lambda.B),aes(x=lambda.B), geom = 'blank') +
  stat_function(fun = dnorm, args=list(mean=m$estimate,sd=m$sd),
                aes(colour = 'ML, normality')) +
  xlab("lambda")+ylab("density")+
  geom_density(aes(y = ..density..,colour="Nonarametric bootstrap"), alpha = 0.4) +
  theme(legend.title=element_blank())

# make confidence interval for mean time between failures
# mean  = mean and  and variance is variance/n
n <- length(w.time)
m <- mean(w.time)
se <- sd(w.time)/sqrt(n)
interval.1 <- m + se * qnorm(c(0.025,0.975))
interval.1

# mean  = mean and var mean  = mean squared /n from the exponential distr
sd.m <- sqrt(m^2/n)
interval.2 <- m + sd.m * qnorm(c(0.025,0.975))
interval.2

# bootstrap procedure
B <- 9999
m.star <- rep(NA, B)
for (b in 1:B) {
  m.star[b] <- mean(sample(w.time, replace = TRUE))
}

sd.m.star <- sd(m.star)
interval.3 <- m+ sd.m.star*qnorm(c(0.025, 0.975))
interval.3

# quantile method
interval.4 <- quantile(m.star, probs = c(0.025, 0.975))
interval.4


# boot library
bootstrap.example <- boot(w.time,R=9999,statistic=function(x,i){mean(x[i])})
mean.fun <- function(d, i)
{ m <- mean(d[i])
n <- length(i)
v <- (n-1)*var(d[i])/n^2
c(m, v)
}
w.boot <- boot(w.time, mean.fun, R = 999)
boot.ci(w.boot, type = c("all"))




