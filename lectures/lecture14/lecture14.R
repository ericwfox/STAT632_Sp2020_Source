library(faraway)
?seatpos
head(seatpos)

pdf('figure/seatpos_pairs.pdf', width=7, height=7)
par(mar=c(4.5, 4.5, 2, 2))
pairs(hipcenter ~., data=seatpos)
dev.off()


round(cor(seatpos[, -9]), 2)

lm1 <- lm(hipcenter ~ ., data=seatpos)
summary(lm1)
round(vif(lm1), 2)

# manually compure VIF
x <- seatpos[,-9] # data frame only containing predictors
summary(lm(Ht ~., data=x))$r.squared
1/(1 - 0.9969982)

# inspect correlation matrix
round(cor(x[, 3:9]), 2)
round(cor(seatpos[, 3:9]), 2)

# refit model
lm2 <- lm(hipcenter ~ Age + Weight + Ht, data=seatpos)
summary(lm2)
vif(lm2)

