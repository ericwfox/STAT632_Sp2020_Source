# format data
# profsalary <- read.table("profsalary.txt", header=TRUE)
# profsalary <- profsalary[,-1]
# write.csv(profsalary, file = "data2/profsalary.csv", row.names=F)
# nyc <- read.csv("nyc.csv", header=TRUE)
# write.csv(nyc[,-1], file = "data2/nyc.csv", row.names=F)

# read in data from 
profsalary <- read.csv("https://ericwfox.github.io/data/profsalary.csv")
nyc <- read.csv("https://ericwfox.github.io/data/nyc.csv")

# linear model
lm1 <- lm(Salary ~ Experience, data=profsalary)

pdf('figure/salary_plot1.pdf', width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
plot(Salary ~ Experience, data=profsalary, 
     ylab='Salary', xlab='Years of Experience')
abline(lm1)
dev.off()

pdf('figure/salary_resid1.pdf', width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
plot(profsalary$Experience, resid(lm1), 
     xlab='Years of Experience', ylab='Residuals')
abline(h=0)
dev.off()
# qqnorm(rstandard(lm1))
# qqline(rstandard(lm1))

# polynomial model
lm2 <- lm(Salary ~ Experience + I(Experience^2), data=profsalary)
summary(lm2)
pdf('figure/salary_resid2.pdf', width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
plot(profsalary$Experience, resid(lm2), 
     xlab='Years of Experience', ylab='Residuals')
dev.off()
# qqnorm(rstandard(lm2))
# qqline(rstandard(lm2))

# compare models
summary(lm1)
summary(lm2)

# prediction interval
x_new <- data.frame(Experience = 10)
predict(lm2, newdata=x_new, interval="prediction")

# plot predictions
range(profsalary$Experience)
x_grd <- seq(1, 36, by=0.5)
x_new <- data.frame(Experience = x_grd)
preds <- predict(lm2, newdata = x_new)

pdf('figure/salary_polypred.pdf', width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
plot(Salary ~ Experience, data=profsalary, 
     ylab='Salary', xlab='Years of Experience')
lines(x_grd, preds, col='blue', lwd=2.5)
dev.off()

# using ggplot2
library(ggplot2)
ggplot(data=profsalary, aes(Experience, Salary)) +
  geom_point() +
  stat_smooth(method='lm', formula = y ~ poly(x, 2)) +
  ggsave("figure/salary_polypred2.pdf", width=5, height=4)

# Menu pricing
head(nyc)
dim(nyc)

# exploratory analysis
#Figure 1.5 on page 7
pdf('figure/menu_pairs.pdf', width=7, height=7)
par(mar=c(4.5, 4.5, 2, 2))
pairs(Price~Food+Decor+Service, data=nyc, gap=0.4, cex.labels=1.5)
dev.off()

#Figure 1.6 on page 10
pdf('figure/east_boxplot.pdf', width=5, height=4)
par(mar=c(4.5, 4.5, 2, 2))
boxplot(Price~East, data= nyc, 
        ylab="Price", xlab="East (1 = East of Fifth Avenue)")
dev.off()

# fit model
lm1 <- lm(Price ~ Food+Decor+Service+East, data=nyc)
summary(lm1)

lm2 <- lm(Price ~ Food+Decor+East, data=nyc)
summary(lm2)
confint(lm2)

# adjusted R^2
s1 <- summary(lm1)
s2 <- summary(lm2)

s1$r.squared
s2$r.squared

s1$adj.r.squared
s2$adj.r.squared

# make a prediction
new_x <- data.frame(Food = 20, Decor = 16, East = 1)
predict(lm2, newdata = new_x, interval="prediction")





# effects plot
new_x <- data.frame(Food = mean(nyc$Food), 
                    Decor = mean(nyc$Decor), 
                    East = 1)
predict(lm2, newdata = new_x, interval="prediction")
plot(Effect("Decor", lm2), xlim=c(0,30))

# Boston housing (lab?)
plot(medv ~ lstat, data=Boston)
lm1 <- lm(medv ~ lstat, data=Boston)
plot(lm1)

lm2 <- lm(medv ~ lstat + I(lstat^2), data=Boston)
plot(lm2)

anova(lm1, lm2)
