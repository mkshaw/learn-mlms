# Load Data and Dependencies ----------------------------------------------

library(dplyr) # for data manipulation
library(magrittr) # for assignment pipe %<>%
library(lme4) # for multilevel models
library(lmerTest) # for p-values

data <- read.csv('data/heck2011.csv', fileEncoding = "UTF-8-BOM")

# Options for Centering in MLMs -------------------------------------------

model <- lmer(math ~ 1 + ses + (1|schcode), data = data, REML = TRUE)
summary(model)

# Centering Within Cluster (CWC) ------------------------------------------

data %<>% # this symbol is an assignment operator and pipe, equivalent to data <- data %>% 
  group_by(schcode) %>% 
  mutate(ses_mean = mean(ses))

data %<>%
  mutate(ses_cwc = ses - ses_mean)

data %>% 
  group_by(schcode) %>% 
  summarize(
    mean(ses_cwc)
  )

model_cwc <- lmer(math ~ 1 + ses_cwc + (1|schcode), data = data, REML = TRUE)
summary(model_cwc)

model_cwc_l2 <- lmer(math ~ 1 + ses_cwc + ses_mean + (1|schcode), data = data, REML = TRUE)
summary(model_cwc_l2)

# Centering Grand Mean (CGM) ----------------------------------------------

data %<>%
  ungroup() %>% # remove the grouping by school that we added in the CWC section
  mutate(ses_grand_mean = mean(ses))

data %<>%
  mutate(ses_cgm = ses - ses_grand_mean)

data %>% 
  summarize(
    mean(ses_cgm)
  )

cgm_model <- lmer(math ~ 1 + ses_cgm + ses_mean + (1|schcode), data = data, REML = TRUE)
summary(cgm_model)

