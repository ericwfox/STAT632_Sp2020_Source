setwd('/Users/ericfox/Documents/CSUEB/teaching/STAT632_S19/lectures/lecture4/figure')
library(openintro)

#remove outliers
ind <- which(marioKart$totalPr > 100)
marioKart2 <- marioKart[-ind, ]

# create dummy variable
n <- nrow(marioKart2)
marioKart2$cond01 <- rep(0, n)
marioKart2$cond01[marioKart2$cond == 'new'] <- 1

lm1 <- lm(totalPr ~ cond01, data = marioKart2)
summary(lm1)

pdf("price_cond.pdf", width=5, height=4)
par(mar=c(5, 4.5, 2, 2))
plot(totalPr ~ cond01, data = marioKart2, xaxt='n', 
     xlim=c(-0.1, 1.1), xlab = 'Condition', ylab = 'Total Price')
axis(1, at=c(0,1), labels=c('Used (0)', 'New (1)'))
abline(lm1)
dev.off()

confint(lm1)

