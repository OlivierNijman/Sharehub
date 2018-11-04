setwd("C:/Users/oli4n/University/Num Methods/Week 3")

library(MASS)
library(stats)
library(dplyr)
library(tidyverse)
library(purrr)
library(ggplot2)


rm(list = ls())
load("aex-dax(1).Rdata")

aex <- financial.data$d.return.aex
aex <- aex[complete.cases(aex)]

aex_normal <- fitdistr(aex, "normal"); aex_normal
aex_t <- fitdistr(aex, "t"); aex_t

loglik <- function(p, x) {
  mean <- p[1]
  s <- p[2]
  df <- p[3]
  ll <- dt((x-mean)/s, df = df)/s
  sum(log(ll))
}

p0 <- c(m  = mean(aex), s  = sd(aex), df = 4)
aex_t2 <- optim(p0, loglik, x = aex, control = list(fnscale = -1), hessian = TRUE); aex_t2
rbind(aex_t2$par, sqrt(diag(solve(-aex_t2$hessian)))); aex_t
#they do slightly agree but not completely.

############# Problem 2
ggplot(geyser) + geom_histogram(aes(x=waiting, stat(density)), binwidth = 4)

# fit a bimodal normal distribution to the data

loglikgeyser <- function(p,x) {
  mu1 <- p[1]
  s1 <- p[2]
  mu2 <- p[3]
  s2 <- p[4]
  lambda <- 1/(1+exp(-p[5]))
  
  ll <- lambda*dnorm((x-mu1)/s1)/s1 + (1-lambda)*dnorm((x-mu2)/s2)/s2
  sum(log(ll))
}

#eyeballing the data for these starting values
p00 <- c(mu1 = 50, s1 = 10, mu2 = 80, s2 = 10, psi = 0)
waiting_m <- optim(p00, loglikgeyser, x = geyser$waiting, control = list(fnscale = -1, maxit = 10000), hessian = TRUE); waiting_m
rbind(waiting_m$par, sqrt(diag(solve(-waiting_m$hessian))))

# make another model but now use the estimated psi as bgin value for model

loglikgeyser2 <- function(p,x) {
  mu1 <- p[1]
  s1 <- p[2]
  mu2 <- p[3]
  s2 <- p[4]
  lambda <- p[5]
  
  ll <- lambda*dnorm((x-mu1)/s1)/s1 + (1-lambda)*dnorm((x-mu2)/s2)/s2
  sum(log(ll))
}

p000 <- c(waiting_m$par[1:4], lambda = 1/(1+exp(-waiting_m$par[5])))
waiting_m2 <- optim(p000, loglikgeyser2, x = geyser$waiting, control = list(fnscale = -1), hessian = TRUE); waiting_m2
rbind(waiting_m2$par, sqrt(diag(solve(-waiting_m2$hessian))))

par <- waiting_m2$par
mu1 <- par[1]
s1 <- par[2]
mu2 <- par[3]
s2 <- par[4]
lambda <- par[5]
x_values <- seq(from = 40, to = 110, by = 0.01)
estimated_density <- lambda*dnorm((x_values-mu1)/s1)/s1 + (1-lambda)*dnorm((x_values-mu2)/s2)/s2

df <- data.frame(x_values, estimated_density)

ggplot(geyser) + geom_histogram(aes(x=waiting, stat(density)), binwidth = 4) + geom_line(data = df, aes(x=x_values, y = estimated_density), color = "red")

#now a likelihood ratio test to see if the sigmas are statistically speaking the same.

loglik_unrestricted <- function(p, x) {
  mu1 <- p[1]
  s1 <- p[2]
  mu2 <- p[3]
  s2 <- p[4]
  lambda <- p[5]
  
  ll <- lambda*dnorm((x-mu1)/s1)/s1 + (1-lambda)*dnorm((x-mu2)/s2)/s2
  sum(log(ll))
}

loglik_restricted <- function(p,x) {
  mu1 <- p[1]
  mu2 <- p[2]
  s <- p[3]
  lambda <- p[4]
  
  ll <- lambda*dnorm((x-mu1)/s)/s + (1-lambda)*dnorm((x-mu2)/s)/s
  sum(log(ll))
}

p1 <- par
model_unrestricted <- optim(p1, loglik_unrestricted, x = geyser$waiting, control = list(fnscale = -1), hessian = TRUE)

p2 <- par[c(1,3,4,5)]
model_restricted <- optim(p2, loglik_restricted, x = geyser$waiting, control = list(fnscale = -1), hessian = TRUE)
model_unrestricted;model_restricted

1-pchisq(2*(model_unrestricted$value - model_restricted$value), df = 1)

################################# Problem 3

f1 <- function(a) {
  x1 <- 10^(-a) - 0.502
  x2 <- -1*(10^(-a))*log(10)
  x <- c(x1,x2)
  return(x)
}

f2 <- function(a) {
  x1 <- 40^(-a) - 0.301
  x2 <- -1*40^(-a)*log(40)
  x <- c(x1,x2)
  return(x)
}

newtonraph <- function(x0, ftn, tol = 1e-9, max.iter = 100) {
  x <- x0
  iter <- 0
  fx <- ftn(x)
  
  while(abs(fx[1])>tol && iter < max.iter) {
    x <- x - fx[1]/fx[2]
    fx <- ftn(x)
    iter <- iter + 1
    cat("At iteration", iter, "x is ", x, "\n")
  }
  if (abs(fx[1])>tol) {
    cat("failed to converge")
  } else {
    cat("After", iter, "iterations, x ended up at value", x, "\n")
    return(x)
  }
}

newtonraph(0.1, f1)
newtonraph(0.1, f2)



