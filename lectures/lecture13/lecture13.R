library(ISLR)
library(ggplot2)

# parallel regression lines -------------------------
lm1 <- lm(Balance ~ Income + Student, data=Credit)
contrasts(Credit$Student) # shows coding R uses for the dummy variable
summary(lm1)


# plot lines
round(coef(lm1), 2)

pdf('figure/balance_parallel.pdf', width=5, height=4)
ggplot(Credit, aes(Income, Balance, colour = Student)) +
  geom_point(alpha=0.7, size=0.9) +
  geom_abline(intercept = 211.1, slope = 5.98, colour = "#F8766D", size=1) +
  geom_abline(intercept = 593.8, slope = 5.98, colour = "#00BFC4", size=1)
dev.off()

# ggplot_build(g)$data

# unrelated regression lines 
lm2 <- lm(Balance ~ Income + Student + Income:Student, data=Credit)
summary(lm2)

round(coef(lm2),2)

pdf('figure/balance_unrelated.pdf', width=5, height=4)
ggplot(Credit, aes(Income, Balance, colour = Student)) +
  geom_point(alpha=0.7, size=0.9) +
  geom_smooth(method="lm", se=FALSE, size=1)
dev.off()

summary(lm1)$adj.r.squared
summary(lm2)$adj.r.squared


# categorical variable with more than 2 levels---------------
lm1 <- lm(Balance ~ Ethnicity, data=Credit)

pdf('figure/educ_boxplots.pdf', width=5, height=3)
ggplot(Wage, aes(education, logwage)) + 
  geom_boxplot() + coord_flip()
dev.off()

lm1 <- lm(logwage ~ education, data=Wage)
summary(lm1)
plot(lm1, 1)
plot(lm1, 2)

lm2 <- lm(logwage ~ age + education, data=Wage)

lm3 <- lm(logwage ~ age + education + age:education, data=Wage)
summary(lm3)

lm1 <- lm(logwage ~ education, data=Wage)
summary(lm1)$adj.r.squared
lm2 <- lm(logwage ~ age + education, data=Wage)
summary(lm2)$adj.r.squared
lm3 <- lm(logwage ~ age + education + age:education, data=Wage)
summary(lm3)$adj.r.squared

anova(lm2, lm3)

pdf('figure/educ_age_lm_scatter.pdf', width=6, height=4)
ggplot(Wage, aes(age, logwage)) +
  geom_point(size = 0.3, alpha=0.6) + facet_wrap( ~ education) +
  geom_smooth(method='lm', size=1)
dev.off()

pdf('figure/educ_age_loess_scatter.pdf', width=6, height=4)
ggplot(Wage, aes(age, logwage)) +
  geom_point(size = 0.3, alpha=0.6) + facet_wrap( ~ education) +
  geom_smooth(method='loess', size=1)
dev.off()