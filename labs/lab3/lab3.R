## ----fig.width=3, fig.height=3, warning=F--------------------------------
library(MASS)
library(ggplot2)
ggplot(data=Boston, aes(lstat, medv)) + geom_point(size=0.5)


## ------------------------------------------------------------------------
lm2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm2)


## ------------------------------------------------------------------------
lm3 <- lm(medv ~ poly(lstat, 3), data=Boston)
lm4 <- lm(medv ~ poly(lstat, 4), data=Boston)
lm5 <- lm(medv ~ poly(lstat, 5), data=Boston)
lm6 <- lm(medv ~ poly(lstat, 6), data=Boston)


## ------------------------------------------------------------------------
summary(lm5)


## ------------------------------------------------------------------------
summary(lm6)


## ----fig.width=4, fig.height=4, warning=F--------------------------------
ggplot(data=Boston, aes(lstat, medv)) +
  geom_point(size=0.5) +
  stat_smooth(method = 'lm', formula = y ~ poly(x, 5))


## ------------------------------------------------------------------------
compute_rmse <- function(y, y_pred) {
  n <- length(y)
  sqrt((1 / n) * sum((y - y_pred)^2))
}


## ------------------------------------------------------------------------
set.seed(100)
n <- nrow(Boston)
train <- sample(1:n, size=354)

lm1_train <- lm(medv ~ lstat, data=Boston, subset=train)
lm2_train <- lm(medv ~ poly(lstat, 2), data=Boston, subset=train)
lm3_train <- lm(medv ~ poly(lstat, 3), data=Boston, subset=train)
lm4_train <- lm(medv ~ poly(lstat, 4), data=Boston, subset=train)
lm5_train <- lm(medv ~ poly(lstat, 5), data=Boston, subset=train)
lm6_train <- lm(medv ~ poly(lstat, 6), data=Boston, subset=train)

rmse <- rep(0, 6)
y_test <- Boston$medv[-train]
new_x <- data.frame(lstat = Boston$lstat[-train])
y1_pred <- predict(lm1_train, newdata = new_x)
rmse[1] <- compute_rmse(y_test, y1_pred)
y2_pred <- predict(lm2_train, newdata = new_x)
rmse[2] <- compute_rmse(y_test, y2_pred)
y3_pred <- predict(lm3_train, newdata = new_x)
rmse[3] <- compute_rmse(y_test, y3_pred)
y4_pred <- predict(lm4_train, newdata = new_x)
rmse[4] <- compute_rmse(y_test, y4_pred)
y5_pred <- predict(lm5_train, newdata = new_x)
rmse[5] <- compute_rmse(y_test, y5_pred)
y6_pred <- predict(lm6_train, newdata = new_x)
rmse[6] <- compute_rmse(y_test, y6_pred)


## ----fig.width=4, fig.height=3-------------------------------------------
# print RMSE vector
rmse

par(mar=c(4.5,4,1,1)) # adjust margins
plot(c(1:6), rmse, xlab="Degree of polynomial", ylab="RMSE", type='b')


## ----fig.width=4.5, fig.height=4.5---------------------------------------
ggplot(data=Boston, aes(lstat, medv)) +
  geom_point(size=0.5) +
  stat_smooth(method = 'lm', formula = y ~ splines::bs(x,7))

