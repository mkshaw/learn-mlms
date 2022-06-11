## ----message=FALSE, warning=FALSE-----------------------------------------------------
library(lme4) # for multilevel models
library(lmerTest) # for p-values
library(dplyr) # for data manipulation
library(ggplot2) # for graphing


## ---- eval=FALSE----------------------------------------------------------------------
## data <- read.csv('rb2002.csv')


## ---- echo = FALSE--------------------------------------------------------------------
# this actually loads my code, but will be hidden
data <- read.csv('data/rb2002.csv', fileEncoding = "UTF-8-BOM")


## -------------------------------------------------------------------------------------
data %>% 
  ggplot(mapping = aes(x = ses, y = mathach)) +
  geom_point()


## -------------------------------------------------------------------------------------
data %>% 
  ggplot(mapping = aes(x = ses, y = mathach)) +
  geom_point(alpha = .2)


## -------------------------------------------------------------------------------------
data <- data %>%
    filter(SCHOOL %in% head(unique(SCHOOL), n = 30))


## -------------------------------------------------------------------------------------
data %>% 
  ggplot() +
  geom_point(mapping = aes(x = ses, y = mathach)) +
  facet_wrap(~ SCHOOL)


## -------------------------------------------------------------------------------------
data %>% 
  group_by(SCHOOL) %>% 
  ggplot(mapping = aes(x = ses, y = mathach, colour = factor(SCHOOL))) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = lm, se = FALSE, show.legend = FALSE, fullrange = TRUE)


## -------------------------------------------------------------------------------------
model <- lmer(mathach ~  CWCses*ses_mean + (1|SCHOOL) + (0 + CWCses|SCHOOL), data = data, REML = TRUE)
summary(model)


## -------------------------------------------------------------------------------------
data$l1resid <- residuals(model)
head(data$l1resid)


## -------------------------------------------------------------------------------------
data %>% 
  ggplot(mapping = aes(x = CWCses, y = l1resid)) +
  geom_point() +
  labs(x = "CWCses", y = "residuals")


## -------------------------------------------------------------------------------------
cor.test(data$l1resid, data$CWCses)


## -------------------------------------------------------------------------------------
data %>% 
  ggplot(mapping = aes(x = l1resid)) +
  geom_histogram()


## -------------------------------------------------------------------------------------
data %>% 
  ggplot(mapping = aes(x = l1resid)) +
  geom_histogram(bins = 15)


## -------------------------------------------------------------------------------------
data %>% 
  ggplot(mapping = aes(sample = l1resid)) +
  stat_qq()


## -------------------------------------------------------------------------------------
l2_data <- data %>% 
  group_by(SCHOOL) %>%  # group data by clustering variable, school
  mutate(
    mathach_mean = mean(mathach) # create mean math achievement per school
  ) %>% 
  select(SCHOOL, ses_mean, mathach_mean) %>% 
  unique() # select unique rows (rather than having school, ses_mean, and mathach_mean repeating over and over again)


## -------------------------------------------------------------------------------------
l2_data$intercept_resid = ranef(model)$SCHOOL[, 1]
l2_data$slope_resid = ranef(model)$SCHOOL[, 2]


## -------------------------------------------------------------------------------------
l2_data %>% 
  ggplot(mapping = aes(x = intercept_resid, y = ses_mean)) +
  geom_point()


## -------------------------------------------------------------------------------------
cor.test(l2_data$ses_mean, l2_data$intercept_resid)


## -------------------------------------------------------------------------------------
l2_data %>% 
  ggplot(mapping = aes(x = slope_resid, y =ses_mean)) +
  geom_point()

cor.test(l2_data$ses_mean, l2_data$slope_resid)


## -------------------------------------------------------------------------------------
l2_data %>% 
  ggplot(mapping = aes(x = slope_resid, y = intercept_resid)) +
  geom_point()

cor.test(l2_data$intercept_resid, l2_data$slope_resid)


## -------------------------------------------------------------------------------------
l2_data %>% 
  ggplot(mapping = aes(x = intercept_resid)) +
  geom_histogram(binwidth = .75)

l2_data %>% 
  ggplot(mapping = aes(sample = intercept_resid)) +
  stat_qq()


## -------------------------------------------------------------------------------------
l2_data %>% 
  ggplot(mapping = aes(x = slope_resid)) +
  geom_histogram(binwidth = .50)

l2_data %>% 
  ggplot(mapping = aes(sample = slope_resid)) +
  stat_qq()


## -------------------------------------------------------------------------------------
n_per_school <- data %>% 
    group_by(SCHOOL) %>% # group by school
    select(SCHOOL) %>% # we just want to count schools
    count() %>% 
    ungroup() %>% 
    select(n) %>% 
    unlist()


## -------------------------------------------------------------------------------------
data$intercept_resid <- rep(l2_data$intercept_resid, times = n_per_school)
data$slope_resid <- rep(l2_data$slope_resid, times = n_per_school)


## -------------------------------------------------------------------------------------
data %>% 
  ggplot(mapping = aes(x = l1resid, y = intercept_resid)) +
  geom_point()

cor.test(data$l1resid, data$intercept_resid)


## -------------------------------------------------------------------------------------
data %>% 
  ggplot(mapping = aes(x = l1resid, y = slope_resid)) +
  geom_point()

cor.test(data$l1resid, data$slope_resid)


## -------------------------------------------------------------------------------------
data %>% 
  ggplot(mapping = aes(x = l1resid, y = ses_mean)) +
  geom_point()

cor.test(data$l1resid, data$ses_mean)


## -------------------------------------------------------------------------------------
data %>% 
  ggplot(mapping = aes(x = intercept_resid, y = CWCses)) +
  geom_point()

cor.test(data$intercept_resid, data$CWCses)


## -------------------------------------------------------------------------------------
data %>% 
  ggplot(mapping = aes(x = slope_resid, y = CWCses)) +
  geom_point()

cor.test(data$slope_resid, data$CWCses)

