library(Stat2Data)
library(ggplot2)
data("Election16")
help(Election16)


pdf("figure/scatter_box.pdf", width= 7 , height = 3)
par(mfrow = c(1,2), mar=c(4, 4, 2, 2), cex=0.8)
plot(TrumpWin ~ BA, xlab = "Percent College Grads", 
     ylab="Trump Win? (1=yes, 0=no)", data=Election16)
boxplot(BA ~ TrumpWin, ylab = "Percent College Grads", 
        xlab="Trump Win? (1=yes, 0=no)", data=Election16)
dev.off()


glm1 <- glm(TrumpWin ~ BA, data=Election16, family=binomial) 
summary(glm1)
confint(glm1)

new_x <- data.frame(BA = 31.4)
predict(glm1, newdata = new_x)

new_x <- data.frame(BA = 31.4)
predict(glm1, newdata = new_x, type="response")

ggplot(Election16, aes(BA, TrumpWin)) + geom_point()  +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se=F) +
  xlab("Percent College Grads") + ylab("Estimated Probability Trump Won") +
  theme_bw()
  ggsave("figure/trump_logistic_plot1.pdf", width=4, height=3.5)