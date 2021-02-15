library(MASS)
library(car)

# defective rate------------------
# defects <- read.table("defects.txt", header=TRUE)
# write.csv(defects[,-1], file="defects.csv", row.names=F)

defects <- read.csv("https://ericwfox.github.io/data/defects.csv")
# scatter plot matrix
pdf('figure/defects_pairs1.pdf', width=7, height=7)
par(mar=c(4.5, 4.5, 2, 2))
pairs(Defective ~ Temperature + Density + Rate, data=defects)
dev.off()

lm1 <- lm(Defective ~ Temperature + Density + Rate, data=defects)
summary(lm1)
pdf('figure/defects_resid1.pdf', width=7, height=3.5)
par(mfrow=c(1,2), mar=c(4.5, 4.5, 2, 2))
plot(lm1, 1:2)
dev.off()

pdf('figure/defects_resid_x1.pdf', width=8, height=2.5)
par(mfrow=c(1,3), mar=c(4.5, 4.5, 2, 2))
plot(defects$Temperature, rstandard(lm1), xlab="Temperature", ylab="Standardized Residuals")
plot(defects$Density, rstandard(lm1), xlab="Density", ylab="Standardized Residuals")
plot(defects$Rate, rstandard(lm1), xlab="Rate", ylab="Standardized Residuals")
dev.off()

# box-cox transformation
pdf('figure/defects_boxcox.pdf', width=4, height=3.5)
par(mar=c(4.5, 4.5, 2, 2))
boxcox(lm1, lambda=seq(0.3,0.65,by=0.05))
dev.off()
summary(powerTransform(lm1))

lm2 <- lm(sqrt(Defective) ~ Temperature + Density + Rate, data=defects)
summary(lm2)
pdf('figure/defects_resid2.pdf', width=7, height=3.5)
par(mfrow=c(1,2), mar=c(4.5, 4.5, 2, 2))
plot(lm2, 1:2)
dev.off()

pdf('figure/defects_resid_x2.pdf', width=8, height=2.5)
par(mfrow=c(1,3), mar=c(4.5, 4.5, 2, 2))
plot(defects$Temperature, rstandard(lm2), xlab="Temperature", ylab="Standardized Residuals")
plot(defects$Density, rstandard(lm2), xlab="Density", ylab="Standardized Residuals")
plot(defects$Rate, rstandard(lm2), xlab="Rate", ylab="Standardized Residuals")
dev.off()

# defective versus fitted (before/after) ggplot2
pdf('figure/defects_obs_pred.pdf', width=7, height=3.5)
par(mfrow=c(1,2), mar=c(4.5, 4.5, 2, 2))
plot(predict(lm1), defects$Defective, 
     xlab = "Fitted Values", ylab = "Defective")
abline(0,1)
lines(lowess(predict(lm1), defects$Defective), col='red')
plot(predict(lm2), sqrt(defects$Defective), 
     xlab = "Fitted Values", ylab = expression(sqrt(Defective)))
lines(lowess(predict(lm2), sqrt(defects$Defective)), col='red')
abline(0,1)
dev.off()

