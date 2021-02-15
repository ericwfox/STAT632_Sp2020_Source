library(Stat2Data)
data("Election16")
Election16$TrumpLose <- 1 - Election16$TrumpWin

lm1 <- lm(TrumpLose ~ Dem.Rep, data=Election16)
summary(lm1)

library(ggplot2)
ggplot(Election16, aes(Dem.Rep, TrumpLose)) + geom_point() +
  xlab("x") + ylab("p(x)") + 
  xlim(-33, 25) + ylim(-0.25, 1.25) + 
  geom_abline(intercept=0.453954, slope=0.026191) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggsave("figure/linreg.pdf", width=4, height=3)

ggplot(Election16, aes(Dem.Rep, TrumpLose)) + geom_point() +
  xlab("x") + ylab("p(x)") + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se=F, col="black", lwd=0.7) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggsave("figure/logistic.pdf", width=4, height=3)
