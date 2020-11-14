# -------------------------------------------------------------
# Applied Statistics / Statistical methods in the Biosciences
# Day 1
# Example on glucose change
# Bo Markussen
# November 10, 2017
# -------------------------------------------------------------

# Read data and compute basic summary statistics
change <- c(0.77,5.14,3.38,1.44,5.34,-0.55,-0.72,2.89)
mean(change)
sd(change)

# Graphical check of normality
qqnorm(change)
abline(mean(change),sd(change))

# Quantitative check of normality
shapiro.test(change)
ks.test(change,"pnorm",mean(change),sd(change))

# One sample T-test
t.test(change)
