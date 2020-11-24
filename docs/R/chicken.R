# -------------------------------------------------------------
# Applied Statistics / Statistical methods in the Biosciences
# Day 2
# k x n table: Chicken gait example
# Bo Markussen
# November 21, 2019
# -------------------------------------------------------------

# We insert the data in a matrix. The byrow-option allows us to do this row-wise.
# To enchance the interpretation we add names to the rows and the columns.
activity <- matrix(c(12,26,20,12,
                     13,27,22,13,
                     25,25,18, 8,
                     28,23,21, 3),4,4,byrow=TRUE)
rownames(activity) <- paste("Treatment",c("A","B","C","D"))
colnames(activity) <- c("0","1","2","3.5")

# For illustration we print the matrix to the console.
# And also make two different graphical representations.
print(activity)
assocplot(activity)
mosaicplot(activity)


# The chi-squared test is only borderline significant (p-value = 0.03909)
chisq.test(activity)

# The following code shows how the ordering of the response (gait score), and
# possibly also the explanatory variable (treatment), may be used to increase
# the statistical power.

# First we 'unfold' the data, such that each chicken is represented by a row in 
# a data frame. The following tidyverse code does this. Although you can't come up 
# with this code without quite a bit of experience, you mmight decipher what it is
# doing by only executing part of the pipe-stream (where the pipes are the '%>%').
library(tidyverse)
activity %>% as_tibble(rownames = "treat") %>% 
  pivot_longer(-"treat",names_to="gait",values_to="n") %>% 
  uncount(n) %>% 
  mutate(treat=factor(treat),gait=factor(gait)) -> 
  activity.long

# Let's have a look at these data, and check that they indeed encode the dataset.
activity.long
table(activity.long)

# Let's make graphical display from the long format
activity.long %>% ggplot(aes(x=treat,fill=gait)) + geom_bar(position=position_fill()) + ylab("Proportion of chicken")

# Having data on this long format we may use kruskal.test() to invoke the ordering of the response,
# and cor.test() in invoke the ordering of both variables.
# Before we do that we, however, should check that the levels of the factors have the same ordering
# as we want them to have.
levels(activity.long$gait)
levels(activity.long$treat)

# This was the case here. If not, then you may use the factor() function to reorder the levels.

# Using the ordering of the response we have:
kruskal.test(gait~treat,data=activity.long)

# Using the ordering of both the response and the explanatory variable we have:
with(activity.long,cor.test(as.numeric(gait),as.numeric(treat),method="spearman"))

# In conclusion we did the following tests. Please note, that using the ordering of the variables
# increases the statistical power with 1 order of magnitude!
#
# chi-squared test:             p-value = 0.03909,     df=3*3=9
# Kruskal-Wallis:               p-value = 0.004578,    df=3*1=1
# Spearman rank correlation:    p-value = 0.0006596,   df=1*1=1
