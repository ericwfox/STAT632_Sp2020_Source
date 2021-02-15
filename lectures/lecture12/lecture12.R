nyc <- read.csv("https://ericwfox.github.io/data/nyc.csv")
head(nyc)

#pairs plot
pdf('figure/menu_pairs.pdf', width=7, height=7)
par(mar=c(4.5, 4.5, 2, 2))
pairs(Price~Food+Decor, data=nyc, gap=0.4, cex.labels=1.5)
dev.off()

lm2 <- lm(Price ~ Food + Decor + East, data=nyc)
summary(lm2)

# residual versus fitted plot
pdf('figure/resid_fitted.pdf', width=7, height=3.5)
par(mfrow=c(1,2), mar=c(4.5, 4.5, 2, 2))
plot(predict(lm2), rstandard(lm2), 
     xlab="Fitted Values", ylab="Standardized Residuals")
abline(h=0)
qqnorm(rstandard(lm2))
qqline(rstandard(lm2))
dev.off()

# residuals versus X
pdf('figure/resid_x.pdf', width=8, height=2.5)
par(mfrow=c(1,3), mar=c(4.5, 4.5, 2, 2))
plot(nyc$Food, rstandard(lm2), 
     xlab="Food Score", ylab="Standardized Residuals")
plot(nyc$Decor, rstandard(lm2), 
     xlab="Decor Score", ylab="Standardized Residuals")
plot(nyc$East, rstandard(lm2), 
     xlab="East (1 = East of Fifth Avenue)", 
     ylab = "Standardized Residuals")
dev.off()

# outliers and influes
pdf('figure/influence1.pdf', width=4, height=4)
par(mar=c(4.5, 4.5, 2, 2))
p <- 3
n <- nrow(nyc)
plot(hatvalues(lm2), rstandard(lm2), 
     xlab='Leverage', ylab='Standardized Residuals')
abline(v = 2*(p+1)/n, lty=2)
dev.off()

ind <- which(hatvalues(lm2) > 0.1)
nyc[ind, ]




# outliers and influence
library(faraway)
lm1 <- lm(sr ~ pop15 + ddpi, data=savings) 
summary(lm1)

plot(lm1, c(1,2))
p <- 2
n <- nrow(savings)
plot(savings$pop15, resid(lm1))
plot(savings$ddpi, resid(lm1))

plot(pop15 ~ ddpi, data=savings)


which(hatvalues(lm1) > 2*(p+1)/n)
which(abs(rstandard(lm1)) > 2)

sort(hatvalues(lm1), dec=T)[1:2]
sort(abs(rstandard(lm1)), dec=T)[1:2]


plot(hatvalues(lm1), rstandard(lm1))
abline(h=c(-2,2), lty=2)
abline(v=2*(p+1)/n, lty=2)
identify(hatvalues(lm1), rstandard(lm1), labels= row.names(savings))

savings[49,]
savings2 <- savings[-49, ]
lm2 <- lm(sr ~ pop15 + ddpi, data=savings2) 
summary(lm2)


