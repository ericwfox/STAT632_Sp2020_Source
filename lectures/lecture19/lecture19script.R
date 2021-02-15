# read in data from URL
county_votes16 <- readRDS(url("https://ericwfox.github.io/data/county_votes16.rds"))
head(county_votes16)
dim(county_votes16)

set.seed(999) # set seed for reproducibility
n <- nrow(county_votes16); n
floor(0.7*n)

# randomly sample 70% of rows for training set
train <- sample(1:n, 2178) 

# fit model using training data
glm_train <- glm(trump_win ~ obama_pctvotes, data=county_votes16,
                 subset = train, family = binomial)
summary(glm_train)

# subset data frame for testing observations
county_votes16_test <- county_votes16[-train, ]
# make predictions for probabilities on test set
probs_test <- predict(glm_train, newdata = county_votes16_test,
                      type = "response")
head(probs_test)

# use 0.5 probability threshold to classify points on test set
# if predicted probability is greater than 0.5 classify as a Trump win (1)
# otherwise, if predicted probability is less than 0.5 classify as Trump lost (0)
length(probs_test)
preds_test <- rep(0, 934) 
preds_test[probs_test > 0.5] <- 1

head(probs_test)
head(preds_test)

# Next make the so-called confusion matrix.
# The confusion matrix tabulates the predicted versus acual
# results on the test set.
tb <- table(prediction = preds_test,
            actual = county_votes16_test$trump_win)
addmargins(tb)

# Accuaracy (percent correctly classified)
(129 + 758) / 934
# Sensitvity (percent of Trump wins (1) correctly classified)
758 / 786
# Specificity (percent of Trump losses (0) correctly classified)
129 / 148


# roc curve ----------------------------
library(pROC)

roc_obj <- roc(county_votes16_test$trump_win, probs_test)
# plot(roc_obj, print.auc = TRUE, legacy.axes = TRUE)


pdf(file="ROC1.pdf", width=4.5, height=3.5)
par(mar=c(4, 4, 2, 2))
plot(1 - roc_obj$specificities, roc_obj$sensitivities, type="l",
     xlab = "1 - Specificity", ylab = "Sensitivity")
points(x = 19/148, y = 758/786, col="red", pch=19) # using 0.5 threshold
abline(0, 1, lty=2)
dev.off()

auc(roc_obj)

# compare with manual function
roc_area <- function(prabs, probp) {
  presnt<-probp[prabs>.5];
  abb<-probp[prabs<.5];
  sumexc <- sapply(presnt, FUN = function(x,cc)(sum(as.numeric(x > cc)) + 0.5 * sum(as.numeric(x == cc))),cc=abb)
  sum(sumexc)/(length(presnt)*length(abb));
}
roc_area(county_votes16_test$trump_win, probs_test) #same-same

# try null / random noise model
set.seed(100)
county_votes16$noise <- rnorm(n)
county_votes16_test <- county_votes16[-train, ]
glm1 <- glm(trump_win ~ noise, data=county_votes16,
                 subset = train, family = binomial)
probs_test <- predict(glm1, newdata = county_votes16_test,
                      type = "response")
roc_obj <- roc(county_votes16_test$trump_win, probs_test)
plot(1 - roc_obj$specificities, roc_obj$sensitivities, type="l",
     xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1,lty=2)

preds_test <- rep(0, 934) 
preds_test[probs0_test > 0.2] <- 1
tb <- table(prediction = preds_test,
            actual = county_votes16_test$trump_win)
addmargins(tb)

# try just random
county_votes16_test <- county_votes16[-train, ]
dim(county_votes16_test)
probs_test = runif(934)
roc_obj <- roc(county_votes16_test$trump_win, probs_test)
plot(1 - roc_obj$specificities, roc_obj$sensitivities, type="l",
     xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1,lty=2)




# misc code tryping ROCR package
library(ROCR)
roc_pred <- prediction(probs_test, county_votes16_test$trump_win)
roc_perf <- performance(roc_pred, "tpr", "fpr")
plot(roc_perf)
points(x=0.1283784, y=0.9643766, col="blue") # using 0.5 threshold
abline(0, 1, lty=2)


