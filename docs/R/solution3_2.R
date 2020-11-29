# --------------------------------------------------------------------
# Applied Statistics / Statistical methods in the Biosciences
# Preparation for Exercise 3.2
# k x n table vs. ordinal regression: Chicken gait example from Day 2
# Bo Markussen
# November 30, 2019
# --------------------------------------------------------------------

# The following depends on the script 'exercise3_2.R' ----

# 1. Fit a multinomial regression to 'gait' using 'treat' as 
#    the explanatory variable.
#    Hint: See code for m0 on lecture slide 39, but without
#          the weights-option and the control-option.

m0 <- clm(gait~1,nominal=~treat,data=activity.long)

# 2. Fit a proportional odds model 'gait' using 'treat' as 
#    the explanatory variable.
#    Hint: See code for m1 on lecture slide 39.

m1 <- clm(gait~treat,data=activity.long)

# 3. Do a lack-of-fit test for the proportional odds assumption.
#    Hint: See code on lecture slide 40

anova(m1,m0)
# We see, that the null hypothesis that the proportional odds
# assumption is true is not rejected (p=0.4461). Thus, we 
# can use the proportional odds model.

# 4. Test for effect of treatment on gait score, and compare
#    the p-value to the three tests done on Day 2.

drop1(m1,test="Chisq")
# This test has power comparable to the Kruskal-Wallis test.
# But the proportional odds model has a more clear interpretation
# than the Kruskal-Wallis test!

# 5. Test if 'treat' can be used as a numerical explanatory
#    variable (with values A=1, B=2, C=3, D=4) in the
#    proportional odds model for 'gait'.
#    Hint: You can use 'as.numeric(treat)' to recode 'treat'
#          as a numerical variable.

m2 <- clm(gait~as.numeric(treat),data=activity.long)
anova(m2,m1)
# We see that the null hypothesis that 'treat' is a numerical
# variable is not rejected (p=.4196). Thus, if we want we may
# choose to use 'treat' as a numerical variable in the subsequent 
# analysis.

# 6. Test for effect of treatment (as a numerical variable)
#    on gait score, and compare the p-value to the three tests
#    done on Day 2.

drop1(m2,test="Chisq")
# This test has power comparable to the test on the Spearman
# rank correlation. But in my opinion the proportional odds model 
# has a more clear interpretation than the test on the Spearman
# rank correlation!

# 7. Return to the proportional odds model from question 2. 
#    Use the functions pairs() and emmeans() from the emmeans-package
#    to do pairwise comparisons of the 4 treatments.
#    Consider the pros and cons of using 'treat' as a categorical
#    and a numerical explanatory variable.

pairs(emmeans(m1,~treat))

# Remarks:
# a) Using 'treat' as a categorical variable allows for specific
#    statements on the 4 treatments, e.g. pairwise comparisons.
# b) Using 'treat' as a numerical variable giver higher power
#    in the effect test (question 6 vs. question 4), but perhaps
#    the description as a numerical variable is too simplistic!
