setwd("C:/Users/oli4n/University/Num Methods/Week 2")

library(dplyr)
rm(list = ls())


#the square root is not what the machine says it is
a <- sqrt(2)
a^2==2
near(a^2,2)

log.root.2 <- 0.5*log(2)
b <- exp(log.root.2)
b^2==2

exp(2*log.root.2) == 2

# dividing to find the machines precision
c <- 1
eps <- 1
c.plus.eps <- c+eps
while (c != c.plus.eps) {
  eps <- eps/2
  c.plus.eps <- c+eps
}
c;c.plus.eps;eps

################# the fixed point, secant and bisection method.

fx <- function(x) {
  (log(x))/(1+x)
}

derfx <- function(x) {
  (1+1/x - log(x))/((1+x)^2)
}

derfx0 <- function(x) {
  1+1/x - log(x)
}

gx <- function(x) {
  4*((1+1/x - log(x))/((1+x)^2))+x
}

fixedpoint <- function(ftn, x0, tol = 1e-6, max.iter = 100) {
  xold <- x0
  xnew <- ftn(xold)
  iter <- 1
  cat("At iteration 1 value of x is:", xnew, "\n")
  
  while (abs(xold - xnew) > tol && iter < max.iter) {
    xold <- xnew
    xnew <- ftn(xold)
    iter <- iter + 1
    cat("At iteration", iter, "value of x is:", xnew, "\n")
  }
  if (abs(xold - xnew) > tol) {
    cat("the algorithm failed to converge\n")
  } else {
    cat("the algorithm converged at iteration", iter, "with x is", xnew, "\n")
    return(xnew)
  }
}

fixpt_own <- function(x0, tol = 1e-6, max.iter = 100) {
  xold <- x0
  xnew <- gx(xold)
  iter <- 1
  cat("At iteration 1 value of x is:", xnew, "\n")
  
  while (abs(xold - xnew) > tol && iter < max.iter) {
    xold <- xnew
    xnew <- gx(xold)
    iter <- iter + 1
    cat("At iteration", iter, "value of x is:", xnew, "\n")
  }
  if (abs(xold - xnew) > tol) {
    cat("the algorithm failed to converge\n")
  } else {
    cat("the algorithm converged at iteration", iter, "with x is", xnew, "\n")
    return(xnew)
  }
}



bisectionmethod <- function(xl, xr, fctn, tol = 1e-6, max.iter = 150) {
  if (fctn(xl)*fctn(xr) >= 0 ) stop("no good starting values")
  iter <- 1
  while (abs(xr -xl) > tol && iter < max.iter) {
    xm <- (xl +xr)/2
    if (fctn(xm) == 0) {
      return(xm)
    } else if (fctn(xm)*fctn(xr) < 0) {
      xl <- xm
    } else {
      xr <- xm
    }
    iter <- iter + 1
    cat("at iteration", iter, "value of xmid is:", xm, "\n")
    
  }
  if (abs(xr - xl) > tol) {
    cat("failed to converge\n")
  } else {
    cat("At iteration", iter, "value of x.mid is:", xm, "and the function value is",
         fctn(xm),"\n")
    return(xm)
    
  }
}

secantmethod <- function(xn, xnmin1, ftn, tol = 1e-6, max.iter = 150) {
  iter <- 1
  while (abs(xn - xnmin1) > tol && iter < max.iter) {
    xnplus1 <- xn - ftn(xn)* ((xn - xnmin1)/(ftn(xn) - ftn(xnmin1)))
    xnmin1 <- xn
    xn <- xnplus1
    iter <- iter + 1
    cat("At iteration", iter, "x_n+1 is: ", xnplus1, "\n")
  }
  if (abs(xn -xnmin1)> tol) {
    cat("failed to convergen\n")
  } else {
    cat("At iteration", iter, "value of x_n+1 is:", xnplus1, "and the function value is", ftn(xnplus1),"\n")
    return(xnplus1)
  }
}

# bisectionmethod(0.5, 6, derfx0)
# fixedpoint(gx, 3)
# fixpt_own(3, max.iter = 150)
# secantmethod(6, 0.5, derfx0)

maxbisect <- bisectionmethod(0.5, 6, derfx0)
maxfixpt <- fixpt_own(3, max.iter = 150)
maxsecant <- secantmethod(6, 0.5, derfx0)

################ Problem 3

nine_a <- function(x) {
  y1 <- cos(x)-x
  y2 <- -sin(x)-1
  y3 <- -cos(x)
  y <- c(y1, y2, y3)
  return(y)
}

nine_b <- function(x) {
  
}

