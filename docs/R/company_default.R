# --------------------------------------------------------------
# Applied Statistics / Statistical methods for the Biosciences
# Day 3: Categorical regression
# Company default example
# Bo Markussen
# November 29, 2019
# -----------------------------------------

# Install gof-package from github:
# > install.packages("devtools")
# > devtools::install_github("kkholst/gof")

# Load used packages
library(readr)
library(gof)
library(car)
library(emmeans)
library(tidyverse)

# Read dataset
Company_default <- read_csv("Company_default.csv")

# Only use companies that are less than 1 year old
Company_default %>% filter(Age<366) %>% 
  select("Age","Equity","Equity_sqrt","Default_nextyear") ->
  young

# The variable Equity_sqrt contains the signed squareroot of 
# the Equity variable. Let's check that graphically

ggplot(young,aes(x=sqrt(abs(Equity)),y=Equity_sqrt)) + geom_point()

# It turns out, that a logistic regression models works well
# against the Equity_sqrt variable. Why is unclear, but this is
# what we will do in this example.

# Remark: There are 4 young companies with a surprising large
# equity, that is with equity 100,000,000 DKK or more. None of
# of these default within the next year. These companies do not
# pose any modelling problems, but they are the course of a 
# warning message when we later fit a logistic regression model. 
# Namely this one:
#
# Warning message:
# glm.fit: fitted probabilities numerically 0 or 1 occurred 
# 
# If you are interested in this technicality, then ask for
# clarification.

# For the first introduction of the example in the lectures I prefer
# the explanatory variable to be categorical. So let's define 
# some quantiles to split the data according to Equity_sqrt.
# Remark: These were found by trail-and-error in order to
#         serve my storyline in the lecture.
(my.breaks <- seq(-1000,1000,length.out=5))

# And make the corresponding cross tabulation table using some
# fancy R code. And again; ask if you want to know more details
# about this.
young <- mutate(young,group=sapply(Equity_sqrt,function(x){1+sum(x>my.breaks)}))
my.table <- table(young$Default_nextyear,young$group)
rownames(my.table) <- c(" No","Yes")
colnames(my.table) <- c(my.breaks,"more")
my.table <- my.table[2:1,]
addmargins(my.table)

# Probit analysis ----
group <- 1:6
m1 <- glm(cbind(my.table["Yes",],my.table[" No",])~group,family=binomial(link="probit"))
summary(m1)
# Estimates for parameters in tolerance distribution
# 1. Standard deviation in tolerance distribution = -1/slope = -1/parameter(group).
#    Since the parameter is negative (!) we change the sign on both parameters in 
#    the model, and have the interpretation for "no-default".
-1/cbind(estimate=coef(m1),confint(m1))["group",]

# 2. Mean in tolerance distribution = LD50 = -intercept/slope:
# Remark: Although we changed sign of the slope, the formula 
#         for the LD50 is the same! 
deltaMethod(m1,"-b0/b1",parameterNames=c("b0","b1"))
# Model validation
pdf(file="default_cumres.pdf",width=10,height=6)
par(mfrow=c(1,2))
plot(cumres(m1))
dev.off()

# Let's compute odds and odds ratios by hand
(odds <- my.table[1,]/my.table[2,])
(odds.ratios <- odds[-6]/odds[-1])
round(odds.ratios,3)


# Let's fit and validate the corresponging logistic regression
m1 <- glm(Default_nextyear~group,data=young,family=binomial())
plot(cumres(m1))

# Lack-of-fit test
m0 <- glm(Default_nextyear~factor(group),data=young,family=binomial())
anova(m1,m0,test="Chisq")

# table-of-counts vs. logistic regression slide ----
prop.pred <- t(apply(my.table,2,function(y){
  tmp <- prop.test(x=y[1],n=sum(y))
  return(c(tmp$estimate,tmp$conf.int))}
  ))
colnames(prop.pred) <- c("prob","lower.CI","upper.CI")
(prop.pred <- cbind(data.frame(group=1:6),prop.pred))
#
tmp <- predict(m1,newdata=data.frame(group=seq(1,6,0.01)),type="response",se.fit=TRUE)
glm.pred <- data.frame(group=seq(1,6,0.01),prob=tmp$fit,lower.CI=tmp$fit-1.96*tmp$se.fit,upper.CI=tmp$fit+1.96*tmp$se.fit)
#
colors <- c("Table of counts"="blue","Logistic regression"="black")
ggplot() + 
  geom_point(aes(x=group,y=prob,col="Table of counts"),prop.pred) +
  geom_errorbar(aes(x=group,ymin=lower.CI,ymax=upper.CI,col="Table of counts"),prop.pred) +
  geom_line(aes(x=group,y=prob,col="Logistic regression"),glm.pred) +
  geom_line(aes(x=group,y=lower.CI,col="Logistic regression"),glm.pred,lty=2) +
  geom_line(aes(x=group,y=upper.CI,col="Logistic regression"),glm.pred,lty=2) +
  scale_color_manual(values=colors) +
  labs(y="probability for default within next year",
       x="Equity group",
       col="Statistical model") +
  ylim(0,1) +
  ggtitle("Observed proportions and predicted probabilities") +
  theme_light()
  ggsave("default_observed_predicted.png",width=16,height=10,units = "cm")
  