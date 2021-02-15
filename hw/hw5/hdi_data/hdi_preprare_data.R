library(readxl)
library(dplyr)

hdi2018_df <- read_excel("hdi2018.xlsx", sheet=1)
pop15_64_df <- read_excel("hdi2018.xlsx", sheet=2)
pop_tot_df <- read_excel("hdi2018.xlsx", sheet=3)
pop65_df <- read_excel("hdi2018.xlsx", sheet=4)
pct_internet_df <- read_excel("hdi2018.xlsx", sheet=5)
median_age_df <- read_excel("hdi2018.xlsx", sheet=6)
pct_labour_df <- read_excel("hdi2018.xlsx", sheet=7)

data1 <- hdi2018_df %>% 
  inner_join(pop_tot_df, by="country") %>%
  inner_join(pop15_64_df, by="country") %>%
  inner_join(pop65_df, by="country") %>%
  inner_join(pct_internet_df, by="country") %>%
  inner_join(median_age_df, by="country") %>%
  inner_join(pct_labour_df, by="country")

data1 <- na.omit(data1)
dim(data1)

data2 <- data1 %>%
  mutate(pctpop15_64 = pop15_64 / pop_total,
         pctpop65 = pop65 / pop_total)

summary(data2$pctpop15_64)
summary(data2$pctpop65)
summary(data2$pct_internet)
summary(data2$median_age)
summary(data2$pct_labour)

plot(hdi_2018 ~ pctpop15_64, data=data2)
plot(hdi_2018 ~ pctpop65, data=data2)
plot(hdi_2018 ~ log(pctpop65+1), data=data2)
plot(hdi_2018 ~ pct_internet, data=data2)
plot(hdi_2018 ~ median_age, data=data2)
plot(hdi_2018 ~ pct_labour, data=data2)
pairs(hdi_2018 ~ pctpop65 + pct_internet + median_age, data=data2)

lm1 <- lm(hdi_2018 ~ pctpop65 + pct_internet + 
            median_age + pct_labour, data=data2)
lm2 <- lm(hdi_2018 ~ pct_internet + median_age, data=data2)  
anova(lm2, lm1)

summary(lm2)
ggplot(data2, aes(data2$median_age, resid(lm2))) +
  geom_point() +
  geom_smooth(method="loess")

which(abs(rstandard(lm2)) > 2)

# write data to file
data3 <- data2 %>% 
  select(country, hdi_2018, median_age, pctpop65, pct_internet, pct_labour)

write.csv(data3, file="hdi2018.csv", row.names=F)
