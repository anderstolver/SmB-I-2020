# --------------------------------------------------------------
# Applied Statistics / Statistical methods for the Biosciences
# Day 3: Regression analysis
# Beetle example
# Bo Markussen
# November 29, 2019
# --------------------------------------------------------------

# Install gof-package from GitHub
# > install.packages("devtools")
# > devtools::install_github("kkholst/gof")

# Load libraries we will be using
library(gof)
library(car)
library(LabApplStat)

# Read data from txt-file
beetle <- read.delim("beetle.txt")
beetle

# Make simply plot
plot(dead/total~log(dosis),data=beetle,pch=16)
abline(lm(dead/total~log(dosis),data=beetle))

# Make plot of design in relation to Lack-of-Fit test.
# Technicality: Threshold set to 0.01 in DD. This shouldn't be necessary, but appearently the package
# isn't yet fully developed.
plot(DD(~log(dosis)+factor(dosis),data=beetle,threshold=0.01))

# Make probit regression
m1 <- glm(cbind(dead,alive)~log(dosis),data=beetle,family=binomial(link="probit"))

# Make probit plot
plot(qnorm(dead/total)~log(dosis),data=beetle,pch=16)
abline(m1)

# Make residual plot
plot(log(beetle$dosis),qnorm(beetle$dead/beetle$total)-predict(m1),pch=16,
     xlab="log(dosis)",ylab="residuals")
abline(0,0,lty=2)

# Make cumulative residuals
#pdf(file="beetle_cumres_0.9.2.pdf",width=10,height=5)
#par(mfrow=c(1,2))
plot(cumres(m1))
#dev.off()

# Make a model where dosis is used as a categorical factor
m0 <- glm(cbind(dead,alive)~factor(dosis),data=beetle,
          family=binomial(link="probit"))

# Lack-of-Fit test: Test m1 as a hypothesis in m0
anova(m1,m0,test="Chisq")

# Hypothesis test
drop1(m1,test="Chisq")

# Parameter estimates and confidence intervals
cbind(estimate=coef(m1),confint(m1))

# Scale parameter in the tolerance distribution
1/cbind(estimate=coef(m1),confint(m1))[2,]

# Mean parameter in the tolerance distribution
deltaMethod(m1,"-alpha/beta",parameterNames=c("alpha","beta"))


# ----------------------------------------------------------------
# Plot of predicted probabilities (with 95pct confidence limits)
# ----------------------------------------------------------------

# make prediction with approximate 95pct confidence limits
newDosis <- seq(5,7,0.01)
bHat <- predict(m1,newdata=data.frame(dosis=newDosis),se.fit=TRUE)
pHat <- pnorm(bHat$fit)
lowerCI <- pnorm(bHat$fit-1.96*bHat$se.fit)
upperCI <- pnorm(bHat$fit+1.96*bHat$se.fit)

colors <- c("data"="black","model"="blue","95% confidence interval"="lightblue")
ggplot() + 
  geom_ribbon(aes(x=newDosis,ymin=lowerCI,ymax=upperCI,fill="95% confidence interval"),col="lightblue") +
  geom_line(aes(x=newDosis,y=pHat,col="model")) + 
  geom_point(aes(x=dosis,y=dead/total,col="data"),data=beetle) + 
  labs(y="Probability for death",
       x=expression(paste(CS[2]," dosis")),
       col="",
       fill="") +
  scale_color_manual(values=colors[1:2],limits=names(colors[1:2])) +
  scale_fill_manual(values=colors[3]) +
  theme_light()
ggsave("beetle_data_modelfit.png",width=14,height=8,unit="cm")
