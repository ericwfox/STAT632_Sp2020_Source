library(Stat2Data)
library(ggplot2)
data("Election16")
head(Election16, n=10)

glm2 <- glm(TrumpWin ~  BA + Dem.Rep, data=Election16, family=binomial) 
summary(glm2)
confint(glm2)

exp(-4.34) / (1+exp(-4.34))

new_x <- data.frame(BA = 31.4, Dem.Rep = 16)
# prediction for logit
predict(glm2, newdata=new_x)
# prediction for probability
predict(glm2, newdata=new_x, type="response")


# perspective plot---------------
library(plot3D)
library(viridis)
x1vals <- seq(19, 41, len=30)
x2vals <- seq(-33, 23, len=30)
grd <- expand.grid(BA = x1vals, Dem.Rep = x2vals)
preds <- predict(glm2, grd, type="response")
# pdf("figure/logit3D.pdf", width=8, height = 8)
persp3D(x1vals, x2vals, matrix(preds, 30, 30), col = viridis(200),
        theta=45, phi= 45, ticktype="detailed", expand=0.7, border="grey",
        xlab = "PctBA", ylab = "PctDem - PctRep", zlab = "ProbTrumpWin")
# dev.off()


# scatter plot-----------------
Election16$TrumpWin2 <- factor(Election16$TrumpWin,
                               levels = c(0,1),
                               labels = c("no", "yes"))
ggplot(Election16, aes(BA, Dem.Rep)) + 
  geom_point(aes(col = TrumpWin2)) +
  xlab("PctBA") + ylab("PctDem - PctRep") +
  guides(col = guide_legend("TrumpWin")) +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +
  ggsave("figure/scatter1.pdf", width=4, height=3)

# with descision boundry
b0 <- coef(glm2)[1]
b1 <- coef(glm2)[2]
b2 <- coef(glm2)[2]
ggplot(Election16, aes(BA, Dem.Rep)) + 
  geom_point(aes(col = TrumpWin2)) + 
  geom_abline(intercept = -1*b0/b2, slope = -1*b1/b2) +
  xlab("PctBA") + ylab("PctDem - PctRep") +
  guides(colour=guide_legend("TrumpWin")) +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +
  ggsave("figure/scatter2.pdf", width=4, height=3)



# model selection----------------------
glm0 <- glm(TrumpWin ~ 1, data=Election16, family=binomial)
glm1_1 <- glm(TrumpWin ~  BA, data=Election16, family=binomial)
glm1_2 <- glm(TrumpWin ~ Dem.Rep, data=Election16, family=binomial)
glm1_3 <- glm(TrumpWin ~  Income, data=Election16, family=binomial)
glm2_1 <- glm(TrumpWin ~ BA + Dem.Rep, data=Election16, family=binomial)
glm2_2 <- glm(TrumpWin ~ BA + Income, data=Election16, family=binomial)
glm2_3 <- glm(TrumpWin ~  Dem.Rep + Income, data=Election16, family=binomial) 
glm3 <- glm(TrumpWin ~ Income + BA + Dem.Rep, data=Election16, family=binomial)
AIC(glm0, glm1_1, glm1_2, glm1_3, glm2_1, glm2_2, glm2_3, glm3)
