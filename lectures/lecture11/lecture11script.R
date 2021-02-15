library(Stat2Data)
data(HousesNY)
dim(HousesNY)
head(HousesNY)

?HousesNY

pdf('figure/ny_pairs.pdf', width=7, height=7)
par(mar=c(4.5, 4.5, 1, 1))
pairs(Price ~ Beds + Baths + Size + Lot, data=HousesNY)
dev.off()

# F-test-------------
lm_full <- lm(Price ~ Beds + Baths + Size + Lot, data=HousesNY)
lm_null <- lm(Price ~ 1, data=HousesNY)
anova(lm_null, lm_full)

summary(lm_full)

# verification
n <- nrow(HousesNY)
p <- 4
rss <- sum(resid(lm_full)^2); rss
sst <- sum(resid(lm_null)^2); sst
fstat <- ((sst - rss) / p) / (rss / (n-p-1))
fstat
1 - pf(fstat, df1=p, df2=n-p-1)

# t-test-------------------
lm_full <- lm(Price ~ Beds + Baths + Size + Lot, data=HousesNY) 
lm1 <- lm(Price ~ Baths + Size + Lot, data=HousesNY) 
anova(lm1, lm_full)

# partial f-test---------------------
lm_full <- lm(Price ~ Beds + Baths + Size + Lot, data=HousesNY)
lm2 <- lm(Price ~ Baths + Size, data=HousesNY)
anova(lm2, lm_full)

s1 <- summary(lm_full)
s2 <- summary(lm2)

s1$r.squared
s2$r.squared

s1$adj.r.squared
s2$adj.r.squared

# your turn-----------------
lm_full <- lm(Price ~ Beds + Baths + Size + Lot, data=HousesNY)
lm4 <- lm(Price ~  Size, data=HousesNY)
anova(lm4, lm_full)




