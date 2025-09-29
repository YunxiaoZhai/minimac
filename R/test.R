# Set working directory
setwd("")

# Load function of minimac
source("minimac.R")

# Import data
data <- read.csv("../SPY_20141215_1s.csv")
y <- log(data$PRICE)

# Testing for Market Friction
options(digits = 6)
stat <- minimac(y,1/252)
summary_minimac(stat)
