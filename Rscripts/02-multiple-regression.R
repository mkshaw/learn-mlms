# Load Data and Dependencies ----------------------------------------------

library(ggplot2) # for data visualization
library(magrittr) # for pipe, %>% 

data <- read.csv('heck2011.csv')
summary(data)

# Simple Linear Regression ------------------------------------------------

model1 <- lm(math ~ ses, data = data)
summary(model1)

ggplot(data = data, mapping = aes(x = ses, y = math)) +
  geom_point()

# Multiple Regression -----------------------------------------------------

model2 <- lm(math ~ ses + female, data = data)
summary(model2)

# Interaction Terms -------------------------------------------------------

model3 <- lm(math ~ ses + female + ses:female, data = data)
summary(model3)

# Could also succinctly code model3 as lm(math ~ ses*female, data = data)

sjPlot::plot_model(model3, type = "pred", terms = c("ses", "female"))

