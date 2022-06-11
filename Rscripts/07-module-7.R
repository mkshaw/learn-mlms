# Load Data and Dependencies ----------------------------------------------

library(dplyr) # for data manipulation
library(ggplot2) # for graphing
library(lme4) # for multilevel models
library(lmerTest) # for p-values

data <- read.csv('data/heck2011.csv', fileEncoding = "UTF-8-BOM")

# Introduction to Estimation Problems -------------------------------------

ses_l1_random <- lmer(math ~ 1 + ses + (1 + ses|schcode), data = data, REML = TRUE)

# Estimation and Optimizers -----------------------------------------------

data %>% 
  filter(schcode <= 10) %>% # subset data to make it easier to see
  ggplot(mapping = aes(x = ses, y = math)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE)

# Singularity -------------------------------------------------------------

ses_l1_random <- lmer(math ~ 1 + ses + (1 + ses|schcode), data = data, REML = TRUE)

Matrix::bdiag(VarCorr(ses_l1_random))

summary(ses_l1_random)

confint(ses_l1_random, oldNames = FALSE)

ses_l1_random_cov0 <- lmer(math ~ 1 + ses + (1|schcode) + (0 + ses|schcode), data = data, REML = TRUE)
summary(ses_l1_random_cov0)

Matrix::bdiag(VarCorr(ses_l1_random_cov0))

# Deviance Testing for Model Comparison -----------------------------------

ses_l1 <- lmer(math ~ 1 + ses + (1|schcode), data = data, REML = TRUE)
ses_l1_random_cov0 <- lmer(math ~ 1 + ses + (1|schcode) + (0 + ses|schcode), data = data, REML = TRUE)

# deviance test to compare model fit
anova(ses_l1, ses_l1_random_cov0, refit = FALSE)

