library(dplyr)
votes <- read.csv("votes.csv")

# check
pctDem = votes$votes_dem_2016 / votes$total_votes_2016
cbind(pctDem, votes$Clinton)

results <- votes %>% 
  select(state = state_abbr, county = county_name, 
         votes_dem_2016, votes_gop_2016, total_votes_2016,
         clinton_pctvotes = Clinton, 
         trump_pctvotes = Trump, 
         obama_pctvotes = Obama,
         pct_pop65 = age65plus,
         pct_black = Black,
         pct_white = White,
         pct_hispanic = Hispanic,
         pct_asian = RHI425214,
         highschool = Edu_highschool, # high school or higher
         bachelors = Edu_batchelors, # bachelors
         income = INC110213) # per capita income in past 12 months

results = results %>% 
  mutate(clinton_pctvotes = round(clinton_pctvotes, 4),
         trump_pctvotes = round(trump_pctvotes, 4), 
         obama_pctvotes = round(obama_pctvotes, 4),
         pct_pop65 = pct_pop65 / 100,
         pct_asian = pct_asian / 100,
         highschool = highschool / 100,
         bachelors = bachelors / 100,
         income = income / 1000)

head(results[ , 6:15])
results[ ,6:15] <- 100 * results[ ,6:15]

# more checks
results %>% filter(state == "CA") %>% 
  arrange(desc(total_votes_2016)) %>%
  select(state, county, pct_black, pct_white, pct_hispanic, pct_asian)

dim(results)
dim(na.omit(results))


# make binary response variable trump_win
n <- nrow(results)
trump_win <- rep(0, n)
trump_win[results$trump_pctvotes > results$clinton_pctvotes] <- 1
table(trump_win)

results$trump_win <- trump_win

# try some logistic regression models
library(ggplot2)
ggplot(results, aes(obama_pctvotes, trump_win)) + geom_point() +
  geom_smooth(method="glm", method.args = list(family = "binomial"), se=F)
ggplot(results, aes(pct_asian, trump_win)) + geom_point() +
  geom_smooth(method="glm", method.args = list(family = "binomial"), se=F)
ggplot(results, aes(income, trump_win)) + geom_point() +
  geom_smooth(method="glm", method.args = list(family = "binomial"), se=F)

glm1 <- glm(trump_win ~ obama_pctvotes, data=results, family = binomial)
summary(glm1)
glm2 <- glm(trump_win ~ pct_pop65 + pct_black +
              pct_white + pct_hispanic + pct_asian +
              highschool + bachelors + income, data=results, family=binomial)
glm3 <- update(glm2, ~ . - pct_pop65)
summary(glm3)

# remove vote total variables
county_votes16 <- results %>% 
  select(-votes_dem_2016, -votes_gop_2016, -total_votes_2016)

# write out as csv file
write.csv(county_votes16, "county_votes16.csv", row.names=F)

# write out as R object
saveRDS(county_votes16, "county_votes16.rds")



