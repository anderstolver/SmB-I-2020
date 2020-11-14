# -------------------------------------------------------------
# Applied Statistics / Statistical methods in the Biosciences
# Day 1
# Example on feed concentrate
# Bo Markussen / Anders Tolver
# Updated: November 18, 2020
# -------------------------------------------------------------

# Read data and compute test statistic
low  <- c(4132,3672,3664,4292,4881,4287,4087,4551)
high <- c(3860,4130,5531,4259,4908,4695,4920,4727)
T.obs <- mean(high)-mean(low)

# Resample test statistic and compute p-values
N <- 10000
T.resample <- replicate(N,{
  permuted.cows <- c(low,high)[sample(1:16)]
  group.1 <- permuted.cows[1:8]
  group.2 <- permuted.cows[9:16]
  mean(group.1)-mean(group.2)
})
p.value.onesided <- mean(T.resample >= T.obs)
p.value.twosided <- mean(abs(T.resample) >= abs(T.obs))

# Print results
cat("Difference in mean milk yield=",T.obs,"\n")
cat("One-sided p-value=",p.value.onesided,"\n")
cat("  alternative hypothesis: milk yield group 7.5 is just as high as group 4.5)\n")
cat("Two-sided p-value=",p.value.twosided,"\n")
cat("  alternative hypothesis: milk yield is different between the two groups)\n")

# Visualization of distribution of test statistic
par(cex=1.2)
hist(T.resample,prob=T,col="yellow",
     xlab="Difference of reshuffled group averages",
     main="Test statistic under null distribution")
lines(rep(T.obs,2),c(0,1),lwd=2,col="red")
lines(rep(-T.obs,2),c(0,1),lwd=2,lty=2,col="red")
text(450,0.0010,labels=paste("T.obs=",T.obs,sep=""),pos=4,col="red")
text(450,0.0004,labels="Probability=",pos=4)
text(520,0.0003,labels=paste(mean(T.resample >= T.obs)),pos=4)
text(-450,0.0004,labels="Probability=",pos=2)
text(-520,0.0003,labels=paste(mean(T.resample<= -T.obs)),pos=2)

# ------------------------
# Build in tests
# ------------------------

wilcox.test(low,high)
t.test(low,high)
mydata <- data.frame(yield=c(low,high),group=c(rep("low",length(low)),rep("high",length(high))))
anova(lmPerm::lmp(yield~group,data=mydata))
