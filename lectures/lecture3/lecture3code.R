# setwd("/Users/ericfox/Documents/CSUEB/teaching/STAT632_S19/lectures/lecture3/figure")
?trees

# estimate linear model
lm1 <- lm(Volume ~ Girth, data=trees)
summary(lm1)

# make prediction
pdf("scatter1.pdf", width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
plot(Volume ~ Girth, xlab='Diameter (in)', 
     ylab = 'Volume (ft^3)', data=trees, cex=0.9)
abline(lm1, lwd=1.5)
pred0 = predict(lm1, newdata=data.frame(Girth=17))
abline(v=17, col='blue', lty=2, lwd=1.5)
abline(h=pred0, col='blue', lty=2, lwd=1.5)
text(18, 20, "x*=17", cex=0.9)
text(9.5, 53, expression(paste(hat(y), textstyle("*")) == 49.2), cex=0.9)
text(20, 69, expression(hat(y)))
dev.off()

# prediction interval (one value)
pdf("scatter2.pdf", width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
plot(Volume ~ Girth, xlab='Diameter (in)', 
     ylab = 'Volume (ft^3)', data=trees, cex=0.9)
abline(lm1, lwd=1.5)
PI = predict(lm1, newdata=data.frame(Girth=17), 
             interval="prediction")
PI = as.data.frame(PI)
abline(v=17, col='blue', lty=2, lwd=1)
abline(h=PI$fit, col='blue', lty=2, lwd=1)
text(18, 20, "x*=17", cex=0.9)
text(9.5, 53, expression(paste(hat(y), textstyle("*")) == 49.2), cex=0.9)
text(20, 69, expression(hat(y)))
# add interval
points(17, PI$fit, cex=1.5, pch=19, col='blue')
lines(rep(17,2), c(PI$lwr, PI$upr), lwd=3, col='blue')
lines(c(16.75, 17.25), rep(PI$lwr, 2), lwd=2.5, col='blue')
lines(c(16.75, 17.25), rep(PI$upr, 2), lwd=2.5, col='blue')
dev.off()

# prediction band
pdf("scatter3.pdf", width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
plot(Volume ~ Girth, xlab='Diameter (in)', 
     ylab = 'Volume (ft^3)', data=trees, cex=0.9)
abline(lm1, lwd=1.5)
min_x <- min(trees$Girth)
max_x <- max(trees$Girth)
grd_x <- seq(min_x, max_x, 0.1)
newdata <- data.frame(Girth = grd_x)
PI <- predict(lm1, newdata = newdata, 
              interval="prediction", level=0.95)
PI <- as.data.frame(PI)
lines(grd_x, PI$lwr, lty=2, lwd=2, col="blue")
lines(grd_x, PI$upr, lty=2, lwd=2, col="blue")
dev.off()

# example prediciton interval
new <- data.frame(Girth = 17)
predict(lm1, newdata = new, interval="prediction")

# example confidence interval
new <- data.frame(Girth = 17)
predict(lm1, newdata = new, interval="confidence")

new <- data.frame(Girth = 17)
predict(lm1, newdata = new, interval="prediction", level=0.99)

# graph confidece interval for specific value
pdf("scatter4.pdf", width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
plot(Volume ~ Girth, xlab='Diameter (in)', 
     ylab = 'Volume (ft^3)', data=trees, cex=0.9)
abline(lm1, lwd=1.5)
CI = predict(lm1, newdata=data.frame(Girth=17), 
             interval="confidence")
CI = as.data.frame(PI)
points(17, PI$fit, cex=0.75, pch=19, col='red')
lines(rep(17,2), c(PI$lwr, PI$upr), lwd=2, col='red')
lines(c(16.85, 17.15), rep(PI$lwr, 2), lwd=1, col='red')
lines(c(16.85, 17.15), rep(PI$upr, 2), lwd=1, col='red')
dev.off()

# graph CI band
pdf("scatter5.pdf", width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
plot(Volume ~ Girth, xlab='Diameter (in)', 
     ylab = 'Volume (ft^3)', data=trees, cex=0.9)
abline(lm1, lwd=1.5)
min_x <- min(trees$Girth)
max_x <- max(trees$Girth)
grd_x <- seq(min_x, max_x, 0.1)
newdata <- data.frame(Girth = grd_x)
CI <- predict(lm1, newdata = newdata, interval="confidence")
CI <- as.data.frame(CI)
lines(grd_x, CI$lwr, lty=2, lwd=2, col="red")
lines(grd_x, CI$upr, lty=2, lwd=2, col="red")
dev.off()

# using ggplot2
ggplot(trees, aes(Girth, Volume)) +
  geom_point() + stat_smooth(method = "lm", se = TRUE) +
  xlab("Diameter (in)") + ylab("Volume (ft^3)")
  ggsave("figure/ggplot_ci.pdf", width=4, height=3.5, device = "pdf")


pdf("scatter6.pdf", width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
plot(Volume ~ Girth, xlab='Diameter (in)', 
     ylab = 'Volume (ft^3)', data=trees, cex=0.9)
abline(lm1, lwd=1.5)
PI = predict(lm1, newdata=data.frame(Girth=17), 
             interval="prediction")
PI = as.data.frame(PI)
# add interval
points(17, PI$fit, cex=1.5, pch=19, col='blue')
lines(rep(17,2), c(PI$lwr, PI$upr), lwd=3, col='blue')
lines(c(16.75, 17.25), rep(PI$lwr, 2), lwd=2.5, col='blue')
lines(c(16.75, 17.25), rep(PI$upr, 2), lwd=2.5, col='blue')
dev.off()


#comparision
pdf("scatter7.pdf", width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
plot(Volume ~ Girth, xlab='Diameter (in)', 
     ylab = 'Volume (ft^3)', data=trees, cex=0.9)
abline(lm1, lwd=1.5)
min_x <- min(trees$Girth)
max_x <- max(trees$Girth)
grd_x <- seq(min_x, max_x, by=0.1)
new_x <- data.frame(Girth = grd_x)
# add PI interval
PI <- predict(lm1, newdata = new_x, 
              interval="prediction", level=0.95)
PI <- as.data.frame(PI)
lines(grd_x, PI$lwr, lty=2, lwd=2, col="blue")
lines(grd_x, PI$upr, lty=2, lwd=2, col="blue")
# add CI
CI <- predict(lm1, newdata = newdata, interval="confidence")
CI <- as.data.frame(CI)
lines(grd_x, CI$lwr, lty=2, lwd=2, col="red")
lines(grd_x, CI$upr, lty=2, lwd=2, col="red")
legend("bottomright", lty=1, lwd=2, col=c('red', 'blue'), legend = c('CI', 'PI'))
dev.off()

pdf("resid.pdf", width=5, height=4)
df1 <- data.frame(resid = resid(lm1), fit = predict(lm1))
ggplot(df1, aes(fit, resid)) + geom_point() + 
  geom_smooth(se=FALSE, col='red', size=0.5) + 
  geom_hline(yintercept = 0, color="red", linetype='dashed')
dev.off()

pdf("resid.pdf", width=5, height=4)
par(mar=c(6,5,3,2))
plot(lm1, which=1)
dev.off()
