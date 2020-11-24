# -------------------------------------------------------------
# Applied Statistics / Statistical methods in the Biosciences
# Day 2
# Comparing two proportions: Avadex example
# Bo Markussen
# November 22, 2019
# -------------------------------------------------------------

# First we try the chi-squared test. Below both with and without the continuity correction.
# For applications I recommend that you always use the continuity correction (which also is the default).
prop.test(c(4,5),n=c(16,79))
prop.test(c(4,5),n=c(16,79),correct=FALSE)

# For this particular data the decision on whether to reject the null hypothesis depends on whether
# the continuity correction was used or not. 

# In general the continuity correction will give more valid p-values. However, in this special case
# R issues a warning that both test's are unrealiable. So instead we simulate the p-value. This is done
# conditioning on the row marginals as these presumable have been fixed by design!
library(LabApplStat)
chisq.test.simulate(matrix(c(4,5,12,74),2,2),"row")

# Since the simulated p-value is significant (p=0.02) we estimate the proportion in each of the treatment groups.
# The following code is an easy way of getting the confidence intervals.
prop.test(4,n=16)
prop.test(5,n=79)
