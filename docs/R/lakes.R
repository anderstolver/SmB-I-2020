# -------------------------------------------------------------
# Applied Statistics / Statistical methods in the Biosciences
# Day 1
# Example on two sample T-test: Phosphor in lakes
# Bo Markussen / rev. Anders Tolver
# Updated: November 18, 2020
# -------------------------------------------------------------

library(readxl)
library(ggplot2)
# Read data from Excel sheet (remember to change path to file):
lakes <- read_excel("../data/lakes.xlsx")

# Let's have an overview of the data
str(lakes)
summary(lakes)

# Actually, the read_excel()-function gives a 'tibble', which gives nice output when simply printed
lakes

# Graphical validation of normality within groups using the ggplot-package
# Remark: To avoid confusion we insert labels on the y-axis
ggplot(lakes,aes(sample=phosphor)) + ylab("phosphor concentration") + facet_grid(.~location) + geom_qq() + geom_qq_line() 
ggplot(lakes,aes(sample=log(phosphor))) + ylab("log(phosphor concentration)") + facet_grid(.~location) + geom_qq() + geom_qq_line() 

# Normal quantile plots shows that the cell counts should be
# log transformed.
# Let's confirm this by some numerical Goodness-of-Fit tests,
# although I rarely do this in my everyday work.
# Remarks: 1) Note the difference in the statistical power. 
#          2) High power isn't always desired here!?
#          3) Note that there is warning message from the Kolmogorov-Smirnov test.
#             Often such warnings are simply ignored. 
by(lakes$phosphor,lakes$location,shapiro.test)
by(lakes$phosphor,lakes$location,function(x){ks.test(x,"pnorm",mean(x),sd(x))})

# The two sample T-test without assuming equal variances
t.test(log(phosphor)~location,data=lakes)


# ------------------------------------------------------------
# For illustration short syntax for doing the correct analysis
# ------------------------------------------------------------
MASS::boxcox(lm(phosphor~location,data=lakes))
par(mfrow=c(2,2))
plot(lm(log(phosphor)~location,data=lakes))
t.test(log(phosphor)~location,data=lakes)

# Remarks: 1) The BoxCox analysis suggests that the
#             logarithm (lambda=0) is a good transformation.             
#          2) The validation plots look ok!
#          3) WARNING: The validation done using plot(lm()) implicitly assumes 
#             equal variances in the two groups. So this can be misleading when
#             the variances are very different.
#          4) We do the Welch test.
