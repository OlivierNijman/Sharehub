
rm(list = ls())

library(purrr)
library(ggplot2)
library(polynom)
library(cowplot)

############################## a
polvec <- c()


polyn <- function() {
  cat("please only input numbers in the following prompts \n the polynomial will be of the form a1 + a2x + a3x^2... etc.")
  m <- as.numeric(readline(prompt = "the degree of the polynomial: "))
  a <- c()
  for (i in 1:(m+1)) {
    a[i] <- as.numeric(readline(prompt = paste("the ", i, "coefficient of the polynomial: ")))
  }
  poly <- polynomial(coef = a)
  cat("The polynomial: ")
  return(poly)
}

polynsum_ordi <- function(x = 1, a = c(-3, 0, 6), m = 2) {
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


polynsum <- function(x , a , m) {
  polvec <- polylist()
  if (length(a) != m+1){
    stop("non compatible length of order and coefficients")
  }
  m <- seq(from = 0, to = m, by = 1)
  for (i in 1:length(a)) {
    polvec[[i]] <- a[i]*(x^m[i])
  }
  return(sum(polvec))
}


############################## b

primitive <- function(a) {
  prim_vec <- c()
  neworder <- seq(1, length(a), by = 1)
  for (i in neworder) {
    prim_vec[1] <- 0
    prim_vec[i+1] <- a[i]/i
  }
  return(as.polynomial(prim_vec))
}

primitive(c(-3,0,6))

############## c



picarditeration <- function(a, xk, x0) {
  xold <- as.polynomial(xk)
  xnew <- primitive(polynsum(x = xold, a, m = length(a)-1))
  xnew[1] <- xnew[1] + x0
  return(xnew)
}




picarditeration(a = c(-3, 0, 6), xk = -1, x0 = -1)
picarditeration(a = c(-3, 0, 6), xk = c(-1, 3), x0 = -1)

polynsum_ordi(x = -1, a = c(-3, 0, 6), m = 2)
polynsum(x = as.polynomial(-1), a = c(-3, 0, 6), m = 2)

############################## d


picardmethod_input <- function() {
  cat("please only input numbers in the following prompts \n the polynomial will be of the form a1 + a2x + a3x^2... etc.\n")
  m <- as.numeric(readline(prompt = "the degree of the polynomial: "))
  a <- c()
  for (i in 1:(m+1)) {
    a[i] <- as.numeric(readline(prompt = paste("the ", i, "coefficient of the polynomial: ")))
  }
  x0 <- as.numeric(readline(prompt = "the initial value: "))
  max.iter <- as.numeric(readline(prompt = "the number of iterations: "))
  
  xnew <- polynomial(x0)
  iter <- 0
  while (iter < max.iter) {
    xold <- xnew
    xnew <- primitive(polynsum(x = xold, a, m = length(a) - 1))
    xnew[1] <- xnew[1] + x0
    iter <- iter + 1
#    cat("at iteration", iter, "the approximate polynomial solution coefficents are", xnew, "\n")
  }
  cat("iterations:", iter, "\n")
  return(xnew)
}



############################## e
picardmethod <- function(a, x0, max.iter) {
  xnew <- polynomial(x0)
  iter <- 0
  while (iter < max.iter) {
    xold <- xnew
    xnew <- primitive(polynsum(x = xold, a, m = length(a) - 1))
    xnew[1] <- xnew[1] + x0
    iter <- iter + 1
    #    cat("at iteration", iter, "the approximate polynomial solution coefficents are", xnew, "\n")
  }
  cat("iterations:", iter, "\n")
  return(xnew)
}


p3 <- picardmethod(a = c(0,2,-1), x0 = 1, max.iter = 3);p3
p4 <- picardmethod(a = c(0,2,-1), x0 = 1, max.iter = 4);p4
p5 <- picardmethod(a = c(0,2,-1), x0 = 1, max.iter = 5);p5


t <- seq(0, 5, 0.1)
p3vec <- c(); p4vec <- c(); p5vec <- c()

for (i in 1:length(t)) {
  p3vec[i] <- polynsum_ordi(x = t[i], a = p3, length(p3)-1) 
  p4vec[i] <- polynsum_ordi(x = t[i], a = p4, length(p4)-1) 
  p5vec[i] <- polynsum_ordi(x = t[i], a = p5, length(p5)-1) 
}


results <- data.frame(t, p3vec, p4vec, p5vec)


plot1 <- ggplot(data = results, aes(x = t)) + 
  geom_line(aes(y = p3vec, colour = "p3vec")) + 
  geom_line(aes(y = p4vec, colour = "p4vec")) + 
  geom_line(aes(y = p5vec, colour = "p5vec")) + 
  scale_colour_manual("", 
                      breaks = c("p3vec", "p4vec", "p5vec"),
                      values = c("red", "green", "blue")) +
  ylab("") + 
  coord_cartesian(xlim = c(0,5),ylim = c(-2.25e5, 5)) + 
  labs(title = "Result")

plot2 <- ggplot(data = results, aes(x = t)) + 
  geom_line(aes(y = p3vec, colour = "p3vec")) + 
  geom_line(aes(y = p4vec, colour = "p4vec")) + 
  geom_line(aes(y = p5vec, colour = "p5vec")) + 
  scale_colour_manual("", 
                      breaks = c("p3vec", "p4vec", "p5vec"),
                      values = c("red", "green", "blue")) +
  ylab("") + 
  coord_cartesian(xlim = c(0,5),ylim = c(-15, 5)) + 
  labs(title = "Result")

plot3 <- ggplot(data = results, aes(x = t)) + 
  geom_line(aes(y = p3vec, colour = "p3vec")) + 
  geom_line(aes(y = p4vec, colour = "p4vec")) + 
  geom_line(aes(y = p5vec, colour = "p5vec")) + 
  scale_colour_manual("", 
                      breaks = c("p3vec", "p4vec", "p5vec"),
                      values = c("red", "green", "blue")) +
  ylab("") + 
  coord_cartesian(xlim = c(0,5),ylim = c(-1e2, 5)) + 
  labs(title = "Result")

plot4 <- ggplot(data = results, aes(x = t)) + 
  geom_line(aes(y = p3vec, colour = "p3vec")) + 
  geom_line(aes(y = p4vec, colour = "p4vec")) + 
  geom_line(aes(y = p5vec, colour = "p5vec")) + 
  scale_colour_manual("", 
                      breaks = c("p3vec", "p4vec", "p5vec"),
                      values = c("red", "green", "blue")) +
  ylab("") + 
  coord_cartesian(xlim = c(0,5),ylim = c(-1e3, 5)) + 
  labs(title = "Result")

plot_grid(plot2, plot3, plot4, plot1, nrow = 2, ncol = 2 , align = "hv")

############################## f



################################ notes












