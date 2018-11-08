setwd("D:/University/Num Methods/week 6")

library(tidyverse)
library(readxl)
library(tidyr)
library(purrr)
library(ggplot2)
library(mvtnorm)
library(purrr)

rm(list = ls())
set.seed(458)

table2.10 <- cbind(lower=c(0,2.5,7.5,12.5,17.5,22.5,32.5,47.5,67.5,87.5,125,225,300),
                   upper=c(2.5,7.5,12.5,17.5,22.5,32.5,47.5,67.5,87.5,125,225,300,Inf),
                   freq=c(41,48,24,18,15,14,16,12,6,11,5,4,3))

loglik <- function(p,x) {
  lower <- x[, 1]
  upper <- x[, 2]
  freq <- x[, 3]
  
  ll <- freq*log(ifelse(upper < Inf, pgamma(upper, p[1], p[2]), 1) - pgamma(lower, p[1], p[2]))
    sum(ll)
}

p0 <- c(alpha = 0.47, beta = 0.014)
m <- optim(p0, loglik, x = table2.10, hessian = TRUE, control = list(fnscale = -1)); m
m_par <- rbind(m$par, sqrt(diag(solve(-m$hessian))))
q95 <- qgamma(0.95, m$par[1], m$par[2]); q95

p <- m$par
var_p <- solve(-m$hessian)

# the derivative approach, calculated derivative and multiply with varance to get variance of 95 percentile confidence interval
eps <- c(1e-5,1e-6,1e-7)
d.alpha <- 0*eps
d.beta <- 0*eps
for (i in 1:3){
  d.alpha[i] <- (qgamma(0.95,p[1]+eps[i],p[2])-qgamma(0.95,p[1]-eps[i],p[2]))/(2*eps[i])
  d.beta[i] <- (qgamma(0.95,p[1],p[2]+eps[i])-qgamma(0.95,p[1],p[2]-eps[i]))/(2*eps[i])
}
d.alpha
d.beta

var.q95 <- t(c(d.alpha[2],d.beta[2])) %*% var_p %*% c(d.alpha[2],d.beta[2])
ci_der <- qgamma(0.95,p[1],p[2]) + qnorm(c(0.025,0.975))*sqrt(c(var.q95)); ci_der

# now the bootstrap method, parametric bootstrap
B <- 10000
q.b <- rep(NA,B)
for (b in 1:B){
  p.b <- rmvnorm(1, p, var_p)
  if (!any(p.b<0)) q.b[b] <- qgamma(0.95,p.b[1],p.b[2])
}
# check for NA's due to negative draws
mean(is.na(q.b))

q.pb <- q.b
# 95% confidence interval
quantile(q.b,c(0.025,0.975))

# non parametric bootstrap, get the sample with replacement

line.numbers <- rep(1:13,table2.10[,"freq"])
q.b <- rep(NA,B)
table2.10b <- table2.10
for (b in 1:B){
  line.numbers.b <- sample(line.numbers,size=217,replace=TRUE)
  table2.10b[,"freq"] <- table(factor(line.numbers.b,levels=1:13))
  
  m.b <- optim(m$par, loglik, hessian = T, control = list(fnscale=-1), x = table2.10b)
  
  q.b[b] <- qgamma(0.95,m.b$par[1],m.b$par[2])
}
q.npb <- q.b
quantile(q.b,c(0.025,0.975))

q.normal <- rnorm(B,mean=qgamma(0.95,p[1],p[2]),sd=sqrt(var.q95))
q.bootstrap <- data.frame(quantile=c(q.pb,q.npb,q.normal),
                          type=rep(c("parametric bootstrap","nonparametric bootstrap","delta method"),c(B,B,B)))
ggplot(q.bootstrap) + geom_density(aes(x=quantile,color=type))


###################################### Problem 2

penalties <- read_excel("penalties.xlsx"); head(penalties)
penalties$id <- NULL

map(penalties, summary)
table(penalties$score, penalties$home)

penalties$score_logit <- ifelse(penalties$score=="score", 1, 0)

loglik2 <- function(beta, x, y) {
  z <- beta[1] + x %*% beta[2:3]
  p <- 1/(1 + exp(-z))
  prob <- ifelse(y==1, p, 1-p)
  sum(log(prob))
}

beta0 <- c(intercept = 0, home = 0, goal.difference = 0)
m2 <- optim(beta0, loglik2, hessian = TRUE, control = list(fnscale = -1), y = penalties$score_logit,
            x = as.matrix(penalties[, c("home", "goal.difference")]))
m2

vcov <- solve(-m2$hessian)
results <- cbind(beta = m2$par, sd = sqrt(diag(vcov)))
t_stat <- results[,1]/results[,2]
p_value <- 2*pnorm(-abs(t_stat))
results <- cbind(results, t_stat, p_value); results


m_glm <- glm(score_logit~home+goal.difference, data = penalties, family = "binomial")
summary(m_glm) # resulsts are the same as the maximum likelihood model.

# subquestion c 
gd <- seq(-2,2,1) #pick the range for goal difference
b0 <- m2$par["intercept"]
b1 <- m2$par["home"]
b2 <- m2$par["goal.difference"]

home.score.prob <- 1/(1+exp(-b0-b1-b2*gd))
away.score.prob <- 1/(1+exp(-b0-b2*gd))
effect <- cbind(goal.difference=gd,effect=home.score.prob-away.score.prob)
effect

# calculate 95 confidence interval of effect of playuing at home
sd.difference <- rep(NA,5)
for (i in 1:length(gd)){
  x.home <- c(1,1,gd[i])
  x.away <- c(1,0,gd[i])
  derivative.of.difference <- x.home * home.score.prob[i] * (1-home.score.prob[i]) -
    x.away * away.score.prob[i] * (1-away.score.prob[i])
  var.difference <- t(derivative.of.difference) %*% vcov %*% derivative.of.difference
  sd.difference[i] <- sqrt(var.difference)
}
effect <- cbind(effect,lower_bound=effect[,2]+qnorm(0.025)*sd.difference,
                upper_bound=effect[,2]+qnorm(0.975)*sd.difference)
effect


# now effect of playing at home with zero goal differnce using bootstrap
effect.at.0 <- function(b){
  1/(1+exp(-b[1]-b[2])) - 1/(1+exp(-b[1]))
}
effect.at.0(m2$par)

B <- 9999
bootstrap.effects <- rep(NA,B)
for (b in 1:B){
  sample.b <- sample(1:402,replace = TRUE)
  glm.m <- glm(score_logit~home+goal.difference,data=penalties[sample.b,],family=binomial)
  bootstrap.effects[b] <- effect.at.0(coef(glm.m))
}

hat.psi <- effect.at.0(m2$par)
basic.bootstrap <- c(2*hat.psi-quantile(bootstrap.effects,prob=0.975),
                     2*hat.psi-quantile(bootstrap.effects,prob=0.025))
basic.bootstrap

perc.bootstrap <- c(quantile(bootstrap.effects,prob=0.025),
                    quantile(bootstrap.effects,prob=0.975))
perc.bootstrap

normal.interval <- c(hat.psi + sd(bootstrap.effects)*qnorm(0.025),
                     hat.psi + sd(bootstrap.effects)*qnorm(0.975))
normal.interval



library(boot)
effect.at.0.boot <- function(dataset,bootstrap.sample){
  glm.m <- glm(score_logit~home+goal.difference,data=dataset[bootstrap.sample,],
               family=binomial)
  b <- coef(glm.m)
  1/(1+exp(-b[1]-b[2])) - 1/(1+exp(-b[1]))
}
bb <- boot(penalties,effect.at.0.boot,R=9999)
boot.ci(bb)
