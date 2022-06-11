# Load Data and Dependencies ----------------------------------------------

library(dplyr) # for data manipulation
library(ggplot2) # for graphing
library(lme4) # for multilevel models
library(lmerTest) # for p-values
library(performance) # for ICC

data <- read.csv('data/casto2016.csv', fileEncoding = "UTF-8-BOM")
head(data)

# Visualizing Testosterone Levels Over Time -------------------------------

data %>% 
  group_by(time0) %>%
  mutate(tmean = mean(Testosterone)) %>% # mean testosterone per timepoint
  ggplot(mapping = aes(x = time0, y = tmean)) +
  geom_line() +
  labs(title = "Testosterone Over Time for Entire Sample")

data %>% 
  group_by(time0, Played) %>% # group by timepoint and played
  mutate(tmean = mean(Testosterone)) %>% 
  ggplot(mapping = aes(x = time0, y = tmean, colour = factor(Played))) +
  geom_line() +
  labs(title = "Testosterone Over Time, Played vs. Did Not Play")

data %>% 
  group_by(time0, HormonCont) %>% # group by timepoint and birth control
  mutate(tmean = mean(Testosterone)) %>% 
  ggplot(mapping = aes(x = time0, y = tmean, colour = factor(HormonCont))) +
  geom_line() +
  labs(title = "Testosterone Over Time, Birth Control or Not")

# Random-Intercept-Only/Null Model ----------------------------------------

null_model <- lmer(Testosterone ~ 1 + (1|Code), data = data, REML = FALSE)
summary(null_model)

performance::icc(null_model)


# Adding Level-1 Fixed and Random Effects ---------------------------------

l1_model <- lmer(Testosterone ~ 1 + time0 + (time0|Code), data = data, REML = FALSE)
summary(l1_model)

as.matrix(Matrix::bdiag(VarCorr(l1_model)))

# Evidence for Retaining Effects ------------------------------------------

l1_model_no_U1j <-  lmer(Testosterone ~ 1 + time0 + (1|Code), data = data, REML = FALSE)

anova(l1_model, l1_model_no_U1j)

confint(l1_model, oldNames = F)

l1_model_cov0 <- lmer(Testosterone ~ 1 + time0 + (1|Code) + (0 + time0|Code), data = data, REML = FALSE)
anova(l1_model, l1_model_cov0)

confint(l1_model_cov0, oldNames = F)

# Extract Empirical Bayes estimates and graph them
as_tibble(coef(l1_model)$Code) %>% 
  ggplot(mapping = aes(x = time0)) +
  geom_histogram(bins = 5)

# Adding Level-2 Fixed Effects --------------------------------------------

l2_played <- lmer(Testosterone ~ 1 + time0 + Played + Played:time0 + (1|Code) + (0 + time0|Code), data = data, REML = FALSE)
summary(l2_played)

anova(l1_model_cov0, l2_played)

l2_played_birthcontrol <- lmer(Testosterone ~ 1 + time0 + Played + time0:Played + HormonCont + time0:HormonCont + (1|Code) + (0 + time0|Code), data = data, REML = FALSE)
summary(l2_played_birthcontrol)
Matrix::bdiag(VarCorr(l2_played_birthcontrol))

anova(l2_played, l2_played_birthcontrol)

