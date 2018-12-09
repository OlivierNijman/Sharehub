
rm(list = ls())

library(purrr)
library(ggplot2)
library(polynom)

############################## a
polvec <- c()

polyn <- function(x = 1, a = c(1,1,1), m = 2) {
  if (length(a) != m+1){
    stop("non compatible length of order and coefficients")
  }
  m <- seq(from = 0, to = m, by = 1)
  for (i in 1:length(a)) {
    c <- a[i]*x^m[i]
    polvec <- c(polvec, c)
  }
  return(polvec)
}

polynsum <- function(x = 1, a = c(1,1,1), m = 2) {
  if (length(a) != m+1){
    stop("non compatible length of order and coefficients")
  }
  m <- seq(from = 0, to = m, by = 1)
  for (i in 1:length(a)) {
    c <- a[i]*x^m[i]
    polvec <- c(polvec, c)
  }
  return(sum(polvec))
}

poldf <- data.frame()

polyn2 <- function(a, m) {
  if (length(a) != m+1){
    stop("non compatible length of order and coefficients")
  }
  m <- seq(from = 0, to = m, by = 1)
  poldf <- data.frame(rbind(m, a), row.names = c("x_power", "coeff"))
  return(poldf)
}

polyn(2, c(1,1,1,1,1,1), m = 5)
polyn2(c(1,1,1,1,1,1), m = 5)

############################## b

primitive <- function(a) {
  prim_vec <- c()
  neworder <- seq(1, length(a), by = 1)
  for (i in neworder) {
    prim_vec[1] <- 0
    prim_vec[i+1] <- a[i]/i
  }
  return(prim_vec)
}

primitive(c(-3,0,6))

############## c

picard1 <- function(a, x0) {
  inval <- c()
  # for (i in 1:length(a)+1) {
  #   if (i ==1) {
  #     inval[i]  <- x0
  #   } else {
  #     inval[i] <- 0
  #   }
  # }
  iter <- primitive(polynsum(x = x0, a = a, m = length(a)-1))
  iter[1] <- iter[1] + x0
  return(iter)
}

picard2 <- function(a,x0) {
  
}

picard1(a = c(-3,0,6), x0 = -1)

############################## d



############################## e


############################## f



















