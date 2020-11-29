# -------------------------------------------------------------
# Applied Statistics / Statistical methods for the Biosciences
# Day 3: Categorical regression: Greenflies example
# Bo Markussen
# December 1, 2018
# -----------------------------------------------

# Load libraries we will be using
library(gof)
library(emmeans)

# Read data from txt-file
greenflies <- read.delim("greenflies.txt")
greenflies

# Make saturated Poisson regresion
m1 <- glm(number~system*week*leave,data=greenflies,family=poisson)

# Automated model selection
step(m1,direction="both")

# Investigation of selected model
m2 <- glm(number~system+week+leave+system:week+week:leave,data=greenflies,family=poisson)
drop1(m2,test="Chisq")
plot(cumres(m2))

# The parameters in the final model are
cbind(logRR=coef(m2),confint(m2))
exp(cbind(RR=coef(m2),confint(m2)))

# em-means
pairs(emmeans(m2,~system|week),reverse=TRUE)
confint(pairs(emmeans(m2,~system|week),reverse=TRUE),type="response")
