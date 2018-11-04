setwd("C:/Users/oli4n/University/Num Methods/Week 1")

library(tidyverse)
library(readxl)
library(tidyr)
library(purrr)
library(lubridate)

file_names <- list.files("data")
female <- read_excel(file_names[1])
male <- read_excel(file_names[2])
class(female)
head(female)

force.type <- function(d) {
  nm <- names(d)
  if (is.element("WRank", nm)) d$WRank <- as.numeric(d$WRank)
  if (is.element("LRank", nm)) d$LRank <- as.numeric(d$LRank)
  if (is.element("B365W", nm)) d$B365W <- as.numeric(d$B365W)
  if (is.element("B365L", nm)) d$B365L <- as.numeric(d$B365L)
  if (is.element("WPts", nm))  d$WPts  <- as.numeric(d$WPts)
  if (is.element("LPts", nm))  d$LPts  <- as.numeric(d$LPts)
  if (is.element("LBW", nm))   d$LBW   <- as.numeric(d$LBW)
  if (is.element("LBL", nm))   d$LBL   <- as.numeric(d$LBL)
  d
}

female <- force.type(female)
male <- force.type(male)

# for (i in 2:length(m.files)){
#   f <- read_excel(m.files[i])
#   f <- force.type(f)
#   m.tennis <- bind_rows(m.tennis,f)
# }

male$Sex <- "male"
head(male)
female$Sex <- "female"
head(female)

tennisdata <- bind_rows(male, female)
save(tennisdata, file = "tennisdata.Rda")

#############################
set.seed(662288)
e <- sample(1:nrow(tennisdata),floor(nrow(tennisdata)/2))

sample1 <- tennisdata[e,]
sample2 <- tennisdata[-e,]

table(tennisdata$Comment)
tennisdata <- filter(tennisdata, Comment=="Completed")
map(tennisdata, summary)
tennisdata$Date <- as.Date(tennisdata$Date)


tennisdata %>% filter(Series == "Grand Slam") %>% group_by(Tournament) %>% summarise(perc.5 = sum(!is.na(W5))/n())
