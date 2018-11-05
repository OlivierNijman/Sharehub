setwd("D:/University/Num Methods/week 5")
  
library(tidyverse)
library(readxl)
library(tidyr)
library(purrr)
library(ggplot2)
library(Hmisc)
  

rm(list = ls())
  
  
table2.10 <- cbind(lower=c(0,2.5,7.5,12.5,17.5,22.5,32.5,47.5,67.5,87.5,125,225,300),
                   upper=c(2.5,7.5,12.5,17.5,22.5,32.5,47.5,67.5,87.5,125,225,300,Inf),
                   freq=c(41,48,24,18,15,14,16,12,6,11,5,4,3))
  
loglik <- function(p,x) {
  lower <- x[,1]
  upper <- x[,2]
  n <- x[,3]
  ll <- n*log(ifelse(upper < Inf, pgamma(upper, p[1], p[2]),1) - pgamma(lower, p[1], p[2]))
  sum(ll)
}

loglik_all_data <- function(p,x) {
  sum(log(dgamma(x,p[1], p[2])))
}
  
p0 <- c(alpha = 0.5, beta = 0.014)
m <- optim(p0, loglik, hessian = TRUE, control = list(fnscale = -1), x = table2.10);
m_par <- rbind(value = m$par, sd = sqrt(diag(solve(-m$hessian))))

set.seed(645)
x400 <- rgamma(400, m$par[1], m$par[2])

# # sort the observations in the same kind of table
# x400_1 <- x400[x400>0 & x400 < 2.5]
# x400_2 <- x400[x400 > 2.5 & x400 < 7.5]
# x400_3 <- x400[x400 > 7.5 & x400 < 12.5]
# x400_4 <- x400[x400 > 12.5 & x400 < 17.5]
# x400_5 <- x400[x400 > 17.5 & x400 < 22.5]
# x400_6 <- x400[x400 > 22.5 & x400 < 32.5]
# x400_7 <- x400[x400 > 32.5 & x400 < 47.5]
# x400_8 <- x400[x400 > 47.5 & x400 < 67.5]
# x400_9 <- x400[x400 > 67.5 & x400 < 87.5]
# x400_10 <- x400[x400 > 87.5 & x400 < 125]
# x400_11 <- x400[x400 > 125 & x400 < 225]
# x400_12 <- x400[x400 > 225 & x400 < 300]
# x400_13 <- x400[x400 > 300 & x400 < Inf]
# 
# table3 <- cbind(lower=c(0,2.5,7.5,12.5,17.5,22.5,32.5,47.5,67.5,87.5,125,225,300),
#                    upper=c(2.5,7.5,12.5,17.5,22.5,32.5,47.5,67.5,87.5,125,225,300,Inf),
#                    freq=c(length(x400_1),length(x400_2),length(x400_3),length(x400_4),length(x400_5),length(x400_6),length(x400_7),length(x400_8),length(x400_9),length(x400_10),length(x400_11),length(x400_12),length(x400_13)))

# or 
table3.10 <- table2.10

x.table <- cut2(x400,cuts=c(0,table2.10[,"upper"]))
table(x.table)
table3.10[, "freq"] <- table(x.table)

# fit gamma to the sample of the estimations

m2 <- optim(p0, loglik, hessian = TRUE, control = list(fnscale = -1), x  = table3.10); m2
m2_par <- rbind(value = m2$par, sd =  sqrt(diag(solve(-m2$hessian))))


# same but for 40000 observations
table4.10 <- table2.10
x40000 <- rgamma(40000, m$par[1], m$par[2])
x.table2 <- cut2(x40000, cuts = c(0, table2.10[, 'upper']))
table(x.table2)
table4.10[, "freq"] <- table(x.table2)

m3 <- optim(p0, loglik, hessian = TRUE, control = list(fnscale = -1), x = table4.10); m3
m3_par <- rbind(value = m3$par, sd = sqrt(diag(solve(-m3$hessian))))

# display the results, it does differ by a factor 10 (sqrt of 100, 400 times 100 = 40000)
m_par;m2_par;m3_par

cbind(original=m$par,sd.original=sqrt(diag(solve(-m$hessian))),
      n400=m2$par,sd.n400=sqrt(diag(solve(-m2$hessian))),
      n40000=m3$par,sd.n40000=sqrt(diag(solve(-m3$hessian))))

midpoints <- c(0, table2.10[, "upper"][1:12]) + c(0, table2.10[, "upper"][2:13])/2
cuts <- sort(c(table2.10[, "upper"], midpoints))[-26]; cuts


# make the two groupings and optim again
x.table3 <- cut2(x400, cuts = cuts)
table3.10_s <- cbind(lower = cuts[1:24], upper = cuts[2:25], freq = table(x.table3))
m2_s <- optim(p0, loglik, hessian = TRUE, control = list(fnscale = -1), x = table3.10_s); m2_s
m2_s_par <- rbind(value = m2_s$par, sd = sqrt(diag(solve(-m2_s$hessian))))

x.table4 <- cut2(x40000, cuts = cuts)
table4.10_s <- cbind(lower = cuts[1:24], upper = cuts[2:25], freq = table(x.table4))
m3_s <- optim(p0, loglik, hessian = TRUE, control = list(fnscale = -1), x = table4.10_s); m3_s
m3_s_par <- rbind(value = m3_s$par, sd = sqrt(diag(solve(-m3_s$hessian))))

m2_par;m2_s_par
m3_par; m3_s_par


###################### problem 2

set.seed(536)
n <- 10000
u <- runif(n)
x <- -log(u)
y <- runif(n,max = u*sqrt(2*exp(1)/pi))
z <- ifelse(y < dnorm(x)/2, -x, NA)
z <- ifelse(dnorm(x)/2 < y & y < dnorm(x), x, z)
z <- z[!is.na(z)]
mean(z)
var(z)

normal <- rnorm(length(z))
# not totally equal to a normal distribution, but only 10000 observations were tested

ggplot(data = data.frame(z=z), aes(sample = z)) + stat_qq() + stat_qq(data = data.frame(normal = normal), aes(sample = normal), col = "blue") + geom_abline(intercept = 0, slope = 1, col = "red")
