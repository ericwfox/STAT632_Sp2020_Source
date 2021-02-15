## ----eval=F, echo=T------------------------------------------------------
## install.packages("fivethirtyeight")


## ----warning=F, message=F------------------------------------------------
library(fivethirtyeight)


## ----eval=F--------------------------------------------------------------
## head(fandango)


## ----eval=F--------------------------------------------------------------
## help(fandango)


## ------------------------------------------------------------------------
summary(fandango$fandango_ratingvalue)
summary(fandango$imdb_norm)


## ------------------------------------------------------------------------
lm1 <- lm(fandango_ratingvalue ~ imdb_norm, data = fandango)


## ------------------------------------------------------------------------
summary(lm1)


## ----fig.width=4, fig.height=4-------------------------------------------
plot(fandango_ratingvalue ~ imdb_norm, data = fandango,
     ylab = "Fandango Rating", xlab="IMDb Rating",
     xlim=c(2,4.8), ylim = c(2,4.8))
abline(lm1) # add regression line
abline(0, 1, lty=2) # add 1-1 line


## ------------------------------------------------------------------------
attributes(lm1)
# extract coefficients (parameter estimates)
coef(lm1)
# extract residuals
head(resid(lm1))
# extract fitted (predicted) values
head(predict(lm1))
# 95% confidence intervals for intercept and slope
confint(lm1)


## ----fig.width=7, fig.height=3-------------------------------------------
par(mar=c(4,4,1,1), mfrow=c(1,2)) # adjust margins, and split graphics pane
# residual plot
plot(predict(lm1), resid(lm1), xlab="Fitted values", ylab="Residuals")
abline(h=0)
# histogram of residuals
hist(resid(lm1), main="", xlab="Residuals")


## ------------------------------------------------------------------------
new_x <- data.frame(imdb_norm = 4)
predict(lm1, newdata = new_x)


## ------------------------------------------------------------------------
new_x <- data.frame(imdb_norm = c(2.5,3.5,4.5))
predict(lm1, newdata = new_x)


## ------------------------------------------------------------------------
new_x <- data.frame(imdb_norm = 4)
predict(lm1, newdata = new_x, interval = "prediction")


## ----fig.width=3.5, fig.height=3.5, warning = FALSE----------------------
library(ggplot2)
ggplot(fandango, aes(imdb_norm, fandango_ratingvalue)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  xlab("IMDb Rating") + ylab("Fandango Rating")

