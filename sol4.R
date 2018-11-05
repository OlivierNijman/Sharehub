setwd("D:/University/Num Methods/week 4")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(stats)
library(purrr)
library(wooldridge)
library(MASS)

rm(list = ls())

?hprice1

data <- as.matrix(hprice1[, c("lassess", "bdrms", "lotsize", "sqrft", "colonial")])
dfdata <- as.data.frame(hprice1[, c("lassess", "bdrms", "lotsize", "sqrft", "colonial")])
  
loglik <- function(p, x) {
 beta <- p[1:5]
 sigma <- p[6]
 y <- x[,1]
 eps <- (y - beta[1] - x[,2:5] %*% beta[2:5]) 
 
 -nrow(x)*log(sigma)-0.5*sum((eps/sigma)^2)
}

p0 <- c(intercept = 5, bedrooms = 0, lotsize = 0, sqrft = 0, colonial = 0, sigma = 2)
m <- optim(p0, loglik, method = "BFGS", control = list(fnscale = -1),hessian = TRUE, x = data)
m

rbind(m$par, sqrt(diag(solve(-m$hessian))))

# lm should give the same if the thing is normal
m.ols <- lm(lassess~bdrms+lotsize+sqrft+colonial, data = dfdata)
summary(m.ols)


#elasticity of assessed value wrt lotsize  = 
elasticity_mean <- mean(hprice1$lotsize) * m$par[3]
var_coefficient <- solve(-m$hessian)[3,3]
var_elasticity <- mean(hprice1$lotsize)^2 * var_coefficient

# upper bound
elasticity_mean + qnorm(0.975)*sqrt(var_elasticity)
#lower bound
elasticity_mean + qnorm(0.025)*sqrt(var_elasticity)

################################### problem 2 find median of distr function.


loglik2 <- function(p,x) {
  mu1 <- p[1]
  s1 <- p[2]
  mu2 <- p[3]
  s2 <- p[4]
  lambda <- p[5]
  
  ll <- lambda*dnorm((x-mu1)/s1)/s1 + (1 - lambda)*dnorm((x - mu2)/s2)/s2
  sum(log(ll))
}

p0 <- c(mu1=54.2005172, sigma1=4.9464740, mu2=80.3467316, sigma2=7.5063385, lambda=0.30)
m2 <- optim(p0, loglik2, hessian = TRUE, control = list(fnscale = -1), x = geyser$waiting)
m2
rbind(m2$par, sqrt(diag(solve(-m2$hessian))))

distribution <- function(m, p) {
  mu1 <- p[1]
  mu2 <- p[3]
  s1 <- p[2]
  s2 <- p[4]
  lambda <- p[5]
  
  distr_ftn <- lambda*pnorm((m-mu1)/s1) + (1-lambda)*pnorm((m-mu2)/s2)
  distr_ftn
}

objective <- function(m,p,q) {
  (distribution(m, p) - q)^2
}

# need to give optimize a quadratic equation
Me <- optimize(f = objective, p=m2$par, q = 0.5, interval = c(30,90))
Me

# the secantmehtod gives the same median, so that way itworks to
# secantmethod <- function(xn, xnmin1, ftn, tol = 1e-6, max.iter = 150) {
#   iter <- 1
#   while (abs(xn - xnmin1) > tol && iter < max.iter) {
#     xnplus1 <- xn - ftn(xn)* ((xn - xnmin1)/(ftn(xn) - ftn(xnmin1)))
#     xnmin1 <- xn
#     xn <- xnplus1
#     iter <- iter + 1
#     cat("At iteration", iter, "x_n+1 is: ", xnplus1, "\n")
#   }
#   if (abs(xn -xnmin1)> tol) {
#     cat("failed to convergen\n")
#   } else {
#     cat("At iteration", iter, "value of x_n+1 is:", xnplus1, "and the function value is", ftn(xnplus1),"\n")
#     return(xnplus1)
#   }
# }
# 
# secantdistribution <- function(m) {
#   mu1 <- m2$par[1]
#   mu2 <- m2$par[3]
#   s1 <- m2$par[2]
#   s2 <- m2$par[4]
#   lambda <- m2$par[5]
#   
#   lambda*pnorm((m-mu1)/s1) + (1-lambda)*pnorm((m-mu2)/s2) - 0.5
#   
# }
# 
# secantmethod(80, 90, secantdistribution)




# for calculating the dserivatives and then 
p0 <- m2$par
# derivative with respect to mu1
p.plus.eps <- p0
p.plus.eps[1] <- p.plus.eps[1] + 0.001
m.plus.eps <- optimize(f=objective,interval=c(30,90),p=p.plus.eps,q=0.50)
p.minus.eps <- p0
p.minus.eps[1] <- p.minus.eps[1] - 0.001
m.minus.eps <- optimize(f=objective,interval=c(30,90),p=p.minus.eps,q=0.50)
derivative.mu1 <- (m.plus.eps$minimum-m.minus.eps$minimum)/(2*0.001)

# derivative with respect to sigma1
p.plus.eps <- p0
p.plus.eps[2] <- p.plus.eps[2] + 0.001
m.plus.eps <- optimize(f=objective,interval=c(30,90),p=p.plus.eps,q=0.50)
p.minus.eps <- p0
p.minus.eps[2] <- p.minus.eps[2] - 0.001
m.minus.eps <- optimize(f=objective,interval=c(30,90),p=p.minus.eps,q=0.50)
derivative.sigma1 <- (m.plus.eps$minimum-m.minus.eps$minimum)/(2*0.001)
# derivative with respect to mu2
p.plus.eps <- p0
p.plus.eps[3] <- p.plus.eps[3] + 0.001
m.plus.eps <- optimize(f=objective,interval=c(30,90),p=p.plus.eps,q=0.50)
p.minus.eps <- p0
p.minus.eps[3] <- p.minus.eps[3] - 0.001
m.minus.eps <- optimize(f=objective,interval=c(30,90),p=p.minus.eps,q=0.50)
derivative.mu2 <- (m.plus.eps$minimum-m.minus.eps$minimum)/(2*0.001)
# derivative with respect to sigma2
p.plus.eps <- p0
p.plus.eps[4] <- p.plus.eps[4] + 0.001
m.plus.eps <- optimize(f=objective,interval=c(30,90),p=p.plus.eps,q=0.50)
p.minus.eps <- p0
p.minus.eps[4] <- p.minus.eps[4] - 0.001
m.minus.eps <- optimize(f=objective,interval=c(30,90),p=p.minus.eps,q=0.50)
derivative.sigma2 <- (m.plus.eps$minimum-m.minus.eps$minimum)/(2*0.001)
# derivative with respect to lambda
p.plus.eps <- p0
p.plus.eps[5] <- p.plus.eps[5] + 0.00001
m.plus.eps <- optimize(f=objective,interval=c(30,90),p=p.plus.eps,q=0.50)
p.minus.eps <- p0
p.minus.eps[5] <- p.minus.eps[5] - 0.00001
m.minus.eps <- optimize(f=objective,interval=c(30,90),p=p.minus.eps,q=0.50)
derivative.lambda <- (m.plus.eps$minimum-m.minus.eps$minimum)/(2*0.00001)


derivative <- c(derivative.mu1,derivative.sigma1,derivative.mu2,
                derivative.sigma2,derivative.lambda)
var.p <- solve(-m2$hessian)
var.Me <- t(derivative) %*% var.p %*% derivative

# upperbound
Me$minimum + qnorm(0.975)* sqrt(var.Me)

# lowerbound
Me$minimum + qnorm(0.025)* sqrt(var.Me)

# and some stuff i dont yet get: 

eps <- 10^(seq(-1,-15,by=-0.025))
p0 <- m2$par
Me0 <- Me$minimum
derivatives <- data.frame(mu1=NA,sigma1=NA,mu2=NA,sigma2=NA,lambda=NA,eps=eps)
for (i in 1:nrow(derivatives)){
  for (j in 1:(ncol(derivatives)-1)){
    p.plus.eps <- p0
    p.minus.eps <- p0
    p.plus.eps[j] <- p0[j] + eps[i]
    p.minus.eps[j] <- p0[j] -eps[i]
    m.plus.eps <- optimize(f=objective,interval=c(30,90),p=p.plus.eps,q=0.50)
    m.minus.eps <- optimize(f=objective,interval=c(30,90),p=p.minus.eps,q=0.50)
    derivatives[i,j] <- (m.plus.eps$minimum-m.minus.eps$minimum)/(2*eps[i])
  }
}

ggplot(derivatives) + geom_line(aes(x=log10(eps),y=mu1))
ggplot(derivatives) + geom_line(aes(x=log10(eps),y=mu2))
ggplot(derivatives) + geom_line(aes(x=log10(eps),y=sigma1))
ggplot(derivatives) + geom_line(aes(x=log10(eps),y=sigma2))
ggplot(derivatives) + geom_line(aes(x=log10(eps),y=lambda))