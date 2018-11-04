# handy in r
install.packages(c("ggplot2", "dplyr", "ggseas", "tidyr", "readxl", "xts", "zoo", "quantmod", "Quandl", "astsa", "vars"))
library(ggplot2)
library(readxl)
library(dplyr)
library(ggseas)
library(tidyr)
library(xts)
#library(zoo)
library(quantmod)
library(Quandl)
library(astsa)
library(purrr)
library(lubridate)

#updater
# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); 
  require(installr)
} #load / install+load installr

# using the package:
updateR()