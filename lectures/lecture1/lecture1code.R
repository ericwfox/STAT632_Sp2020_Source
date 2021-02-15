setwd('/Users/ericfox/Documents/CSUEB/teaching/STAT632_S19/lectures/lecture1/figure')

# motivating example------------------------------------
library(openintro)
bdims_males <- subset(bdims, sex == 1)
pdf(file='wgt_hgt_plot.pdf', width=4, height=3.5)
par(mar=c(5,4,1,2))
plot(wgt ~ hgt, xlab = 'Height (cm)' , ylab = 'Weight (kg)', data=bdims_males, cex=0.5)
dev.off()

# other toy examples
# library(alr4)
# plot(dheight ~ mheight, data=Heights)

# data(package="openintro")
# plot(Age_Husband ~ Age_Wife, data=husbands.wives)

# cool 538 example
# library(fivethirtyeight)
# https://fivethirtyeight.com/features/fandango-movies-ratings/
# plot(rottentomatoes~rottentomatoes_user, data=fandango)
# lm1 <- lm(rottentomatoes~rottentomatoes_user, data=fandango)
# abline(lm1)

# linear model example-------------------
lm1 <- lm(wgt ~ hgt, data=bdims_males)
summary(lm1)

# plot with regression line
pdf(file='wgt_hgt_plot2.pdf', width=4, height=3.5)
par(mar=c(5,4,1,2))
plot(wgt ~ hgt, xlab = 'Height (cm)' , ylab = 'Weight (kg)', 
     data=bdims_males, cex=0.5)
abline(lm1)
dev.off()

which.max(bdims_males$wgt)
bdims_males[124, c('hgt', 'wgt')]
resid(lm1)[124]

# residual standard error
sum(resid(lm1)) #close to zero
n <- nrow(bdims_males)
sqrt(sum(resid(lm1)^2) / (n-2))

# Conditional distribution diagram-----------------
pdf(file='scatter_norm.pdf', width=7, height=5.5)
par(mar=c(4,2.5,1,1), cex=1.5)
set.seed(1)
plot(c(20,120), c(0,100), type='n', xaxt='n', yaxt='n', xlab='x', ylab='y')
abline(a=10, b=0.5) #y=10 + 0.5x
#xsim <- rnorm(100, mean=50, sd=15)
#ysim <- 10 + 0.5*xsim + rnorm(100, sd=10)
#points(xsim, ysim, col='lightgray')
n <- length(grd)
lines(c(40, 40), c(0,60))
lines(c(40,55.96), c(30, 30), col='cyan')
grd1 <- seq(0,60,0.1)
lines(400*dnorm(grd1, mean=30, sd=10) + 40, grd1) # x=40
lines(c(80, 80), c(20,80))
lines(c(80,95.96), c(50, 50), col='cyan')
grd2 <- seq(20,80,0.1)
lines(400*dnorm(grd2, mean=50, sd=10) + 80, grd2) # x=80
axis(1, at=c(40,80), labels = c(expression(x[1]), expression(x[2])))
axis(2, at=c(50), labels = c('y'), tick=F)
dev.off()

# partitioning variation-------------------------
set.seed(4000)
x <- rnorm(15)
y <- x + rnorm(15, 0, 1)
lm1 <- lm(y~x)

pdf(file='figure/scatter5_1.pdf', width=7, height=5.5)
par(mar=c(4, 4, 2, 1))
plot(x, y, col='blue', cex=1.25, xlim=c(-2.35, 1.4))
abline(lm1)
abline(h=mean(y), lty=2)
yhat <- predict(lm1)
i=15
points(x[i], y[i], cex=1.25)
lines(c(x[i], x[i]), c(y[i], mean(y)))
dev.off()

# coefficient of determination-----------------------------
set.seed(1)
x <- rnorm(30)
y <- 0.5*x
lm1 <- lm(y~x)

set.seed(2000)
x2 <- rnorm(30)
y2 <- rnorm(30)

pdf(file='R2.pdf', width=10, height=5)
par(mfrow=c(1,2))
plot(x, y, main = expression(R^2 == 1), cex.main=2, cex=1.5)
abline(lm1)
plot(x2, y2, xlab='x', ylab='y', main = expression(R^2 == 0), cex.main=2, cex=1.5)
abline(h=mean(y2))
dev.off()