setwd("C:/Users/oli4n/University/Num Methods/Week 2")

library(dplyr)
library(ggplot2)
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
  y1 <- log(x) - exp(-x)
  y2 <- 1/x + exp(-x)
  y3 <- -1/(x^2) - exp(-x)
  y <- c(y1, y2, y3)
  return(y)
}

nine_c <- function(x) {
  y1 <- x^3 - x - 3
  y2 <- 3*x^2 -1
  y3 <- 6*x
  y <- c(y1, y2, y3)
  return(y)
}

nine_d <- function(x) {
  y1 <- x^3 -7*x^2 + 14*x - 8
  y2 <- 3*x^2 -14*x + 14
  y3 <- 6*x -14
  y <- c(y1, y2, y3)
  return(y)
}

nine_e <- function(x) {
  y1 <- log(x)*exp(-x)
  y2 <- exp(-x)/x - log(x)*exp(-x)
  y3 <- log(x)*exp(-x) - exp(-x)*(2/x + 1/x^2);
  y = c(y1,y2, y3)
  return(y)
}

newtonraphsonquadratic <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
  x <- x0
  fx <- ftn(x)
  iter <- 0
  
  while (abs(fx[1]) > tol && iter < max.iter) {
    a <- fx[3]/2
    b <- fx[2] - x*fx[3]
    c <- fx[1] - x*fx[2] + ((x^2)*fx[3])/2
    discr <- b^2 - 4*a*c
    cat("the discriminant is equal to", discr, "\n")
    if (a==0) {
      x <- -c/b
    } else {
      if (discr > 0) {
        x1 <- (-b + sqrt(discr))/(2*a)
        x2 <- (-b - sqrt(discr))/(2*a)
        d1 <- abs(x - x1)
        d2 <- abs(x - x2)
        if (d1 < d2) {
          x <- x1
        } else {
          x <- x2
        }
      } else if (discr == 0) {
        x <- -b/(2*a)
      } else {
        x <- x - fx[2]/fx[3]
      }
    }
    fx <- ftn(x)
    iter <- iter +1
    cat("At iteration", iter, "x is ", x , "\n")
  }
  if (abs(fx[1])>tol) {
    cat("convergence failed\n")
    return(NULL)
  } else {
    cat("algorithm converged at iter", iter, "with x value of", x, "\n")
    return(x)
  }
}

newtonraphsonquadratic(nine_a, 1)
newtonraphsonquadratic(nine_b, 2)
newtonraphsonquadratic(nine_c, 0)
newtonraphsonquadratic(nine_d, 1.5)
newtonraphsonquadratic(nine_e, 2)


################# Problem 4
S <- 100
K <- 105
t <- 20
r <- 0.01/365

Blackscholes <- function(S = 100, K = 105, r = 0.01/365, t = 20,  sigma) {
  d1 <- (log(S/K) + (r + 0.5*sigma^2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma*sqrt(t)
  S*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
}

sigma_vector <- seq(from = 0, to = 0.3, by = 0.001)
Blackscholes_values <- Blackscholes(sigma = sigma_vector)

df <- data.frame(sigma = sigma_vector, BS  = Blackscholes_values)
ggplot(df, aes(sigma, BS)) + geom_line()

sigmaftn <- function(sigma) {
  Blackscholes(sigma = sigma) - 1.70
}

impliedsigma <- secantmethod(0.05, 0.1, sigmaftn)

Blackscholes(sigma = impliedsigma)
