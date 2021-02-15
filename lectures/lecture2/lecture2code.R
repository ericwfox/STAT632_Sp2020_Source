setwd('/Users/ericfox/Documents/CSUEB/teaching/STAT632_S19/lectures/lecture2/figure')

library(openintro)
bdims_males <- subset(bdims, sex == 1)
lm1 <- lm(wgt ~ hgt, data=bdims_males)
summary(lm1)

# function to compute 95% confidence intervals
confint(lm1)
# manual calculation
n <- nrow(bdims_males)
tcrit <- qt(0.975, df=n-2)
0.78257 - tcrit * 0.07901
0.78257 + tcrit * 0.07901
