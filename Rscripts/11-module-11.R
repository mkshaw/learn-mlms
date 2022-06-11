# Load Data and Dependencies ----------------------------------------------

library(r2mlm) # for R-squared values
library(lme4) # for multilevel models
library(lmerTest) # for p-values
library(performance) # for ICC

data(teachsat)

# Single Model, Automatic Entry -------------------------------------------

null_model <- lmer(satisfaction ~ 1 + (1|schoolID), data = teachsat, REML = TRUE)
summary(null_model)

r2mlm(null_model)

performance::icc(null_model)

full_model <- lmer(satisfaction ~ 1 + control_c + salary_c + s_t_ratio + (1 + control_c | schoolID), 
                 data = teachsat,
                 REML = TRUE)
summary(full_model)
Matrix::bdiag(VarCorr(full_model))

r2mlm(full_model)

# Single Model, Manual Entry ----------------------------------------------

r2mlm_manual(data = teachsat,
             within_covs = c(4, 5),
             between_covs = c(8),
             random_covs = c(4),
             gamma_w = c(0.311, 0.074),
             gamma_b = c(7.186, -0.037),
             Tau = matrix(c(0.575, 0.009, 0.009, 0.028), 2, 2),
             sigma2 = 0.766,
             has_intercept = TRUE,
             clustermeancentered = TRUE)

# Model Comparison --------------------------------------------------------

reduced_model <- lmer(satisfaction ~ 1 + control_c + salary_c + (1 + control_c | schoolID), 
                 data = teachsat,
                 REML = TRUE)

r2mlm_comp(full_model, reduced_model)

