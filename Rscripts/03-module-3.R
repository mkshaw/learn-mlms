# Load Data and Dependencies ----------------------------------------------

library(dplyr) # for data processing
library(lmtest) # for cluster-robust standard errors
library(sandwich) # for cluster-robust standard errors

data <- read.csv('data/heck2011.csv', fileEncoding = "UTF-8-BOM")

# Cluster-Robust Standard Errors ------------------------------------------

model <- lm(math ~ ses + female, data = data)
summary(model)

model_crse <- coeftest(model, vcov = vcovCL, cluster = ~ schcode)
model_crse