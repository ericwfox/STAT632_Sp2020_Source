setwd("~/Documents/CSUEB/teaching/STAT632_S19/lectures/lecture5")

# Anscombe's Four Data Sets
anscombe <- read.table("anscombe.txt",header=TRUE)
attach(anscombe)

pdf("figure/anscombe1.pdf", width=8, height=8)
par(mfrow=c(2,2))
plot(x1,y1,xlim=c(4,20),ylim=c(3,14),main="Data Set 1")
plot(x2,y2,xlim=c(4,20),ylim=c(3,14),main="Data Set 2")
plot(x3,y3,xlim=c(4,20),ylim=c(3,14),main="Data Set 3")
plot(x4,y4,xlim=c(4,20),ylim=c(3,14),main="Data Set 4")
dev.off()

#Figure 3.1 on page 46
pdf("figure/anscombe.pdf", width=8, height=8)
par(mfrow=c(2,2))
plot(x1,y1,xlim=c(4,20),ylim=c(3,14),main="Data Set 1")
abline(lsfit(x1,y1))
plot(x2,y2,xlim=c(4,20),ylim=c(3,14),main="Data Set 2")
abline(lsfit(x2,y2))
plot(x3,y3,xlim=c(4,20),ylim=c(3,14),main="Data Set 3")
abline(lsfit(x3,y3))
plot(x4,y4,xlim=c(4,20),ylim=c(3,14),main="Data Set 4")
abline(lsfit(x4,y4))
dev.off()

detach(anscombe)

# good leverage point---------------
set.seed(500) 
x <- rnorm(30)
eps <- rnorm(30)
y <- 1 + 2*x + eps
xl <- 5
yl <- 1 + 2*xl + rnorm(1)
xall <- c(x, xl)
yall <- c(y, yl)
lm1 <- lm(y~x)
lm2 <- lm(yall ~ xall)

pdf("good_leverage.pdf", width=5, height=3.5)
par(mar=c(4.5, 4.5, 2, 2))
plot(xall, yall, xlab='x', ylab='y', cex=1.25)
points(xl, yl, col='red', cex=1.25)
abline(lm1, lwd=1.5)
abline(lm2, col='red', lwd=1.5)
dev.off()


# bad leverage point------------------------
set.seed(500) 
x <- rnorm(30)
eps <- rnorm(30)
y <- 1 + 2*x + eps
xl <- 5
yl <- -4
xall <- c(x, xl)
yall <- c(y, yl)
lm1 <- lm(y~x)
lm2 <- lm(yall ~ xall)

pdf("bad_leverage.pdf", width=5, height=3.5)
par(mar=c(4.5, 4.5, 2, 2))
plot(xall, yall, xlab='x', ylab='y', cex=1.25)
points(xl, yl, col='red', cex=1.25)
abline(lm1, lwd=1.5)
abline(lm2, col='red', lwd=1.5)
dev.off()


# outlier that is not a leverage point--------------------
set.seed(500) 
x <- rnorm(30)
eps <- rnorm(30)
y <- 1 + 2*x + eps
xl <- 1
yl <- 17
xall <- c(x, xl)
yall <- c(y, yl)
lm1 <- lm(y~x)
lm2 <- lm(yall ~ xall)

pdf("outlier.pdf", width=5, height=3.5)
par(mar=c(4.5, 4.5, 2, 2))
plot(xall, yall, xlab='x', ylab='y', cex=1.25)
points(xl, yl, col='red', cex=1.25)
abline(lm1, lwd=1.5)
abline(lm2, col='red', lwd=1.5)
dev.off()

# 2000 Election example -------------------------------

library(Stat2Data)
data("PalmBeach")

# scatterplot
pdf('figure/election_plot.pdf', width=4, height=3)
lm1 <- lm(Buchanan ~ Bush, data=PalmBeach)
par(mar=c(4,4,1.5,1.5))
plot(Buchanan ~ Bush, data=PalmBeach,
     ylab = "Buchanan votes", xlab = "Bush votes")
abline(lm1)
dev.off()

# plot of standardized residuals
pdf('figure/election_resid.pdf', width=4, height=3)
par(mar=c(4,4,1.5,1.5))
plot(PalmBeach$Buchanan, rstandard(lm1),
     xlab = "Buchanan votes", ylab = "Standardized Residuals")
abline(h=c(-2,2), lty=2)
dev.off()

# identify outliers
ind <- which(abs(rstandard(lm1)) > 2)
PalmBeach[ind, ]

# residuals versus leverage
pdf('figure/election_leverage.pdf', width=4, height=3)
par(mar=c(4,4,1.5,1.5))
plot(hatvalues(lm1), rstandard(lm1), 
  xlab = "Leverage", ylab = "Standardized Residuals")
n <- nrow(PalmBeach)
abline(v=4/n, lty=2) # threshold for high leverage
abline(h=c(-2,2), lty=2) # threshold for outliers
dev.off()

# identify bad leverage points
ind <- which(abs(rstandard(lm1)) > 2 & hatvalues(lm1) > 4/n)
PalmBeach[ind, ]


# QQplots-----------------
set.seed(1)
x <- rnorm(500)
pdf('figure/qqnorm1.pdf', width=7, height=3.5)
par(mfrow=c(1,2))
hist(x)
qqnorm(x)
qqline(x)
dev.off()


pdf('figure/qqexp1.pdf', width=7, height=3.5)
par(mfrow=c(1,2))
set.seed(1)
x <- rexp(500) # random numbers from an exponential distribution
hist(x)
qqnorm(x)
qqline(x)
dev.off()

pdf('figure/qqt1.pdf', width=7, height=3.5)
par(mfrow=c(1,2))
set.seed(1)
x <- rt(500, df=5) # random numbers from an exponential distribution
hist(x)
qqnorm(x)
qqline(x)
dev.off()

library(openintro)
bdims_males <- subset(bdims, sex == 1) 
lm1 <- lm(wgt ~ hgt, data=bdims_males)
pdf('figure/qqres.pdf', width=7, height=3.5)
par(mfrow=c(1,2))
hist(resid(lm1))
qqnorm(resid(lm1))
qqline(resid(lm1))
dev.off()
