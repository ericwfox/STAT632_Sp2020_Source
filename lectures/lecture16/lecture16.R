library(faraway)
library(alr4)
setwd("~/Documents/CSUEB/teaching/STAT432_S19/lecture/lecture14")

# prepage data
statedata <- data.frame(state.x77, row.names=state.abb)
head(statedata)

pdf('figure/state_pairs.pdf', width=7, height=7)
par(mar=c(4.5, 4.5, 2, 2))
pairs(Life.Exp ~ ., data=statedata)
dev.off()

# summary(powerTransform(cbind(Population, Income, Illiteracy, 
#                              Murder, HS.Grad, Area) ~ 1, statedata))
# 
# pairs(Life.Exp ~ log(Population) + Income + Illiteracy + 
#         Murder + HS.Grad + Frost + log(Area), data=statedata)


# full model with all predictors
options(scipen = 1)
lm1 <- lm(Life.Exp ~ ., data=statedata)
summary(lm1)

lm2 <- update(lm1, ~ . - Area)
summary(lm2)

lm3 <- update(lm2, ~ . - Illiteracy, data=statedata)
summary(lm3)

lm4 <- update(lm3, ~ . - Income, data=statedata)
summary(lm4)

summary(lm1)$r.squared
summary(lm4)$r.squared

summary(lm1)$adj.r.squared
summary(lm4)$adj.r.squared

#backwards stepwise --------------------------------------
library(ISLR)

head(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters2 <- na.omit(Hitters)
dim(Hitters2)

lm_full <- lm(Salary~., data=Hitters2)
lm2 <- step(lm_full)
summary(lm2)
AIC(lm_full, lm2)

n <- nrows(Hitters2)
lm3 <- step(lm_full, k=log(n))
summary(lm3)
length(coef(lm3))