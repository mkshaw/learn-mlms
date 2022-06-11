# Load Data and Dependencies ----------------------------------------------

library(dplyr) # for data manipulation
library(ggplot2) # for visualizations
library(lme4) # for multilevel models
library(lmerTest) # for p-values
library(performance) # for intraclass correlation

data <- read.csv('data/heck2011.csv', fileEncoding = "UTF-8-BOM")

# Why Multilevel Models? --------------------------------------------------

data_sub <- data %>% 
  filter(schcode <= 10)

data_sub %>% 
  ggplot(mapping = aes(x = ses, y = math)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE)

data_sub %>% 
  ggplot(mapping = aes(x = ses, y = math, colour = factor(schcode))) +
  geom_point() +
  geom_smooth(mapping = aes(group = schcode), method = "lm", se = FALSE, fullrange = TRUE) +
  labs(colour = "schcode")

# The Null Model ----------------------------------------------------------

null_model <- lmer(math ~ 1 + (1|schcode), data = data)
summary(null_model)

# Understanding Variance --------------------------------------------------

performance::icc(null_model)

Tau0 <- VarCorr(null_model)$schcode[1]

lower_bound <- null_model@beta - 1.96*sqrt(Tau0)
upper_bound <- null_model@beta + 1.96*sqrt(Tau0)

lower_bound
upper_bound

# Empirical Bayes Estimates -----------------------------------------------

data %>% 
  filter(schcode == 1) %>% # select only school code 1
  summarize(
    mean(math)
  )

data %>% 
  filter(schcode == 1) %>% 
  count()

empirical_bayes_data <- as_tibble(ranef(null_model))

head(empirical_bayes_data, 1)

ggplot(data = empirical_bayes_data, mapping = aes(x = condval)) + # "condval" is the name of the EB estimates returned by the ranef function above 
  geom_histogram() +
  labs(x = "EB estimate of U0j")

