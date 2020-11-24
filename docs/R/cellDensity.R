# -------------------------------------------------------------
# Applied Statistics / Statistical methods in the Biosciences
# Day 2
# Example on density of nerve cells
# Bo Markussen
# November 22, 2018
# -------------------------------------------------------------

# Hard code data into two vectors
mid <- c(50.6,39.2,35.2,17.0,11.2,14.2,24.2,37.4,35.2)
mes <- c(38.0,18.6,23.2,19.0, 6.6,16.4,14.4,37.6,24.2)

# Make t-test
qqnorm(mid-mes)
shapiro.test(mid-mes)
t.test(mid,mes,paired=TRUE)

# Make sign test
binom.test(sum(mid>mes),length(mid))

# Make Wilcoxon rank sum test
wilcox.test(mid,mes,paired=TRUE)
