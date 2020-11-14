# -------------------------------------------------------------
# Applied Statistics / Statistical methods in the Biosciences
# Day 1
# Exercise on hypertension (a cross over study)
# Bo Markussen / rev. Anders Tolver
# Updated: November, 2020
# -------------------------------------------------------------

# First we read the data available in an R dataset
load("hypertension.RData")
summary(hypertension)

# Before proceeding let me emphasize that I usually would analyze a cross-over experiment
# by a random effect model (topic for course day 5). However, it is also possible to analyse
# this dataset using T-tests. In the exercise text 4 T-tests are proposed.

# The end-point (response variable) in this study is the change in blood pressure. This change
# is measured over periods where the patients received one of two drugs (either E or N), and
# we want to investigate whether one of the drugs is better at reducing the blood pressure.

# Here's is the answer to part of the exercise:
# The two sample T-test comparing 'E_diff_N' in the E/N-group against the N/E-group tests
# whether there is a spill-over effect (i.e. validates against Problem 1). The explanation is as
# follows: If the drugs have different effects and if there is a spill-over from period 1 to 
# period 2, then the differences between the changes in the E- and the N-period will depend on 
# the order the drugs were given.

# Before doing this T-test we first validate that the E_diff_N variable is normally distributed
# in both the E/N- and the N/E-group. This can be done like this:
library(ggplot2)
ggplot(hypertension,aes(sample=E_diff_N)) + geom_qq() + facet_grid(.~order)

# In both of these normal quantile plots the points appear to lie on a straingt line. Thus, we
# conclude that it is ok to assume the the E_diff_N variable is normally distributed in both groups.
# Thereafter we do a Welch two sample T-test (which doesn't assume equal variances):
t.test(E_diff_N~order,data=hypertension)

# The test is non-significant (p=0.2011), which means that we can not reject the null hypotesis.
# The null hypothesis says that there is no spill-over. Thus, our conclusion is that there is no
# spill-over (Problem 1 does not occur).

# Please note that actual R code is quite short (3 lines for this two sample T-test). So the challange
# in this exercise is to find the interpretation of the null hypothesis, and hence the meaning of
# the associated T-test.

# Now it is up to you to consider the remaining three T-tests.
