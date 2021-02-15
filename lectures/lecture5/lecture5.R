setwd("~/Documents/CSUEB/teaching/STAT632_S19/lectures/lecture6")
library(alr4)

# log-log transformation example----------------------
plot(BrainWt ~ BodyWt, data=brains)
identify(brains$BodyWt, brains$BrainWt, rownames(brains))

pdf('figure/brain_hist.pdf', width=5, height=5)
par(mfrow=c(2,2), mar=c(5, 4.5, 1, 1))
hist(brains$BrainWt, breaks=10, main='', xlab='BrainWt')
hist(log(brains$BrainWt), breaks=10, main='', xlab='log(BrainWt)')
hist(brains$BodyWt, breaks=10, main='', xlab='BodyWt')
hist(log(brains$BodyWt), breaks=10, main='', xlab='log(BodyWt)')
dev.off()

lm1 <- lm(log(BrainWt) ~ log(BodyWt), data=brains)
summary(lm1)

pdf('figure/logbrain_scat.pdf', width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
plot(log(BrainWt) ~ log(BodyWt), data=brains)
abline(lm1)
dev.off()

# predict the brain weight (g) for a mammal with a body weight of 40 kg
# on log scale
pred <- predict(lm1, data.frame(BodyWt = 40), interval="prediction")
pred
# exponentiate to express in the original scale of the response (grams)
exp(pred)

brains2 <- data.frame(logBrainWt = log(brains$BrainWt), logBodyWt = log(brains$BodyWt))
lm2 <- lm(logBrainWt ~ logBodyWt, data=brains2)
predict(lm2, data.frame(logBodyWt = log(100)))

pdf('figure/logbrain_resid.pdf', width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
plot(predict(lm1), resid(lm1), xlab='Fitted values', ylab='Residuals')
abline(h=0, lty=2)
dev.off()

# sqrt nonconst var example ---------------------------
library(Stat2Data)
data("CountyHealth")
head(CountyHealth)

pdf("figure/MDs_scatter1.pdf", width=5, height=3.5)
par(mar=c(4.5, 4.5, 2, 2))
lm1 <- lm(MDs ~ Hospitals, data = CountyHealth)
plot(MDs ~ Hospitals, data = CountyHealth)
abline(lm1)
dev.off()

# diagnostics
pdf("figure/diagnostic1.pdf", width=7, height=3.55)
par(mfrow = c(1, 2))
plot(lm1, 1:2)
dev.off()

pdf("figure/MDs_scatter2.pdf", width=5, height=3.5)
par(mar=c(4.5, 4.5, 2, 2))
lm2 <- lm(sqrt(MDs) ~ Hospitals, data = CountyHealth)
plot(sqrt(MDs) ~ Hospitals, data = CountyHealth)
abline(lm2)
dev.off()

pdf("figure/diagnostic2.pdf", width=7, height=3.55)
par(mfrow = c(1, 2))
plot(lm2, 1:2)
dev.off()

summary(lm2)

predict(lm2, newdata = data.frame(Hospitals = 4))
predict(lm2, newdata = data.frame(Hospitals = 4))