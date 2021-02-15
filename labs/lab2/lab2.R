## ------------------------------------------------------------------------
n <- 50 # sample size
beta0 <- 2 # population intercept
beta1 <- 3 # population slope
sigma <- 5 

set.seed(99)
x <- rnorm(n)
e <- rnorm(n, mean=0, sd=sigma)
y <- beta0 + beta1 * x + e

# estimate SLR model from simulated data
lm1 <- lm(y ~ x)
summary(lm1)


## ----fig.width=5, fig.height=4-------------------------------------------
par(mar=c(4.5,4.5,2,2)) #adjust margins
plot(y ~ x)
abline(beta0, beta1, lwd=1.5, lty=1) # population regression line
abline(lm1, lwd=1.5, lty=2) # least squares estimate
legend('bottomright', lwd=1.5, lty=c(1, 2), c('Population', 'Estimate'))


## ------------------------------------------------------------------------
set.seed(99)
beta1hat <- rep(0, 10000) # initialize vector of slope estimates
x <- rnorm(n) # simulate x values
for(i in 1:10000) {
  e <- rnorm(n, mean=0, sd=sigma)
  y <- beta0 + beta1 * x + e
  lm_i <- lm(y ~ x)
  beta1hat[i] <- as.numeric(coef(lm_i)[2])
}


## ----fig.width=4, fig.height=3-------------------------------------------
par(mar=c(4,4,1,1)) #adjust margins
hist(beta1hat, main='')
abline(v=3, col='blue', lwd=2)


## ------------------------------------------------------------------------
# variance of the slope estimates
var(beta1hat) 
# true (analytic) variance
SXX <- sum((x - mean(x))^2)
sigma^2 / SXX

