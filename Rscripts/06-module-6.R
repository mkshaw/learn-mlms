# Load Data and Dependencies ----------------------------------------------

library(dplyr) # for data manipulation
library(ggplot2) # for visualizations
library(lme4) # for multilevel models
library(lmerTest) # for p-values

data <- read.csv('data/heck2011.csv', fileEncoding = "UTF-8-BOM")

# MLM with Random Slope Effect --------------------------------------------

data %>% 
  filter(schcode <= 10) %>% 
  ggplot(mapping = aes(x = ses, y = math, colour = factor(schcode))) +
  geom_point() +
  geom_smooth(mapping = aes(group = schcode), method = "lm", se = FALSE, fullrange = TRUE) +
  labs(colour = "schcode")

ses_l1_random <- lmer(math ~ ses + (1 + ses|schcode), data = data, REML = TRUE)
summary(ses_l1_random)

Matrix::bdiag(VarCorr(ses_l1_random))

-1.58/(1.79*0.88)

empirical_bayes_data <- ranef(ses_l1_random) # extract random effects for each school

empirical_bayes_intercepts <- empirical_bayes_data$schcode["(Intercept)"]

empirical_bayes_slopes <- empirical_bayes_data$schcode["ses"] # extracts the SES/slope EB estimates from the list

bind_cols(empirical_bayes_intercepts, empirical_bayes_slopes) %>%  # combine EB slopes and intercepts into a useable dataframe for graphing
  ggplot(mapping = aes(x = ses, y = `(Intercept)`)) +
  geom_point()

# MLM with Crosslevel Effect ----------------------------------------------

crosslevel_model <- lmer(math ~ 1 + ses + public + ses:public + (1 + ses|schcode), data = data, REML = TRUE)
summary(crosslevel_model)

Matrix::bdiag(VarCorr(crosslevel_model))

