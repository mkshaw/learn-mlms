# Load Data and Dependencies ----------------------------------------------

library(lme4) # for multilevel models
library(lmerTest) # for p-values
library(performance) # for ICC

data <- read.csv('data/hoffman2007.csv', fileEncoding = "UTF-8-BOM")
head(data)

# Random-Intercept-Only/Null Model ----------------------------------------

null_model <- lmer(lg_rt ~ 1 + (1|id), data = data, REML = FALSE) # note that REML = FALSE
performance::icc(null_model)

# Adding Level-1 Fixed Effects --------------------------------------------

l1_model <- lmer(lg_rt ~ 1 + c_mean + c_sal + (1|id), data = data, REML = FALSE)
summary(l1_model)

anova(null_model, l1_model)

# Adding Random Slopes ----------------------------------------------------

l1_random <- lmer(lg_rt ~ 1 + c_mean + c_sal + (1|id) + (0 + c_mean|id) + (0 + c_sal|id), data = data, REML = FALSE)
summary(l1_random)

l1_random_without_cmean <- lmer(lg_rt ~ 1 + c_mean + c_sal + (1|id) + (0 + c_sal|id), data = data, REML = FALSE)
summary(l1_random_without_cmean)

anova(l1_random, l1_random_without_cmean)

# Adding Level-2 Fixed Effects --------------------------------------------

l2_model <- lmer(lg_rt ~ 1 + c_mean + c_sal + oldage + sex + (1|id), data = data, REML = FALSE)
summary(l2_model)

# model
l2_model_no_sex <- lmer(lg_rt ~ 1 + c_mean + c_sal + oldage + (1|id), data = data, REML = FALSE)

# deviance test
anova(l2_model, l2_model_no_sex)

# Adding Cross-Level Interactions -----------------------------------------

crosslevel_model <- lmer(lg_rt ~ 1 + c_mean + c_sal + oldage + oldage:c_mean + (1|id), data = data, REML = FALSE)
summary(crosslevel_model)

