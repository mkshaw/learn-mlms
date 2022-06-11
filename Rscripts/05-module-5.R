# Load Data and Dependencies ----------------------------------------------

library(dplyr) # for data manipulation
library(ggplot2) # for visualizations
library(lme4) # for multilevel models
library(lmerTest) # for p-values
library(performance) # for intraclass correlation

data <- read.csv('data/heck2011.csv', fileEncoding = "UTF-8-BOM")

# MLM with Level-1 Predictor ----------------------------------------------

null_model <- lmer(math ~ 1 + (1|schcode), data = data)
summary(null_model)

ses_l1 <- lmer(math ~ ses + (1|schcode), data = data, REML = TRUE)
summary(ses_l1)

null <- sigma(null_model)^2
l1 <- sigma(ses_l1)^2

(null - l1) / null

performance::icc(ses_l1)

# Compare Regular and Multilevel Regression -------------------------------

model <- lm(math ~ ses, data = data)
summary(model)

model_crse <- lmtest::coeftest(model, vcov = sandwich::vcovCL, cluster = ~ schcode)
model_crse

summary(ses_l1)

# MLM with Level-2 Predictor ----------------------------------------------

ses_l1_public_l2 <- lmer(math ~ 1 + ses + public + (1|schcode), data = data, REML = TRUE)
summary(ses_l1_public_l2)

# level-1 variance reduced
sigma2_null <- sigma(null_model)^2
sigma2_public <- sigma(ses_l1_public_l2)^2
(sigma2_null - sigma2_public) / sigma2_null

# level-2 variance reduced
tau2_null <- VarCorr(null_model)$schcode[1]
tau2_public <- VarCorr(ses_l1_public_l2)$schcode[1]
(tau2_null - tau2_public) / tau2_null
