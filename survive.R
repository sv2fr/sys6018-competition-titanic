
# Data Input ------------------------------------------------------------

train <- read.csv('train.csv')
test <- read.csv('test.csv')

# Data Cleaning -----------------------------------------------------------

# fill in missing ages
# use mean function (na.rm : ignore missing values)
mean_age <- mean(train$Age, na.rm = TRUE) # average age overall
mean_age_male <- mean(train[train$Sex == 'male', ]$Age, na.rm = TRUE) # average age for males
mean_age_female <- mean(train[train$Sex == 'female', ]$Age, na.rm = TRUE) # average age for females

# approach 1 - fill in mean_age
train[is.na(train$Age), ]$Age <- mean_age
test[is.na(test$Age), ]$Age <- mean_age

# approach 2 - fill in mean_age by gender (this did not improve accuracy on kaggle submission)
# train[is.na(train$Age) & train$Sex == 'male', ]$Age <- mean_age_male
# train[is.na(train$Age) & train$Sex == 'female', ]$Age <- mean_age_female
# test[is.na(test$Age) & test$Sex == 'male', ]$Age <- mean_age_male
# test[is.na(test$Age) & test$Sex == 'female', ]$Age <- mean_age_female

# convert feature columns to factors if appropriate
train$Pclass <- factor(train$Pclass)
train$Sex <- factor(train$Sex)
test$Pclass <- factor(test$Pclass)
test$Sex <- factor(test$Sex)

# Data Analysis ----------------------------------------------------------------

# divide the train set into training (70%) and validation sets (30%)
train_rows <- sample(nrow(train), size=0.70 * nrow(train)) # get random row indices
training <- train[train_rows,] # 70% of data to train model
validation <- train[-train_rows,]  # remaining to validate the model

# use logistic regression since we are trying to predict categorical variable (yes or no)
# logistic regression on training set
logistic <- glm(Survived~ Pclass + Sex + Age + SibSp + Parch + Fare, data=training, family="binomial")
summary(logistic)

# logistic regression on validation set
probs <- as.vector(predict(logistic, newdata=validation, type="response"))
preds <- rep(0,nrow(validation))  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds, validation$Survived)

# removing in-significant variables from model
logistic <- glm(Survived~ Pclass + Sex + Age + SibSp, data=training, family="binomial")
summary(logistic)

# logistic regression on validation set
probs <- as.vector(predict(logistic, newdata=validation, type="response"))
preds <- rep(0,nrow(validation))  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds, validation$Survived)

# final logistic regression model using entire training set 
# logistic <- glm(Survived~ Pclass + Sex + Age + SibSp, data=train, family="binomial")
# with two variables seemed to give better accuracy on kaggle submission:
logistic <- glm(Survived~ Sex + Age , data=train, family="binomial")
summary(logistic)

# use model on testing set
probs <- as.vector(predict(logistic, newdata=test, type="response"))
test$Survived <- rep(0,nrow(test))  # Initialize prediction vector
test$Survived[probs>0.5] <- 1 # p>0.5 -> 1

# # use knn (default options, change only k)
# train <- read.csv('train.csv')
# test <- read.csv('test.csv')
# 
# train[, c('Name', 'Ticket', 'Cabin', 'Embarked')] <- NULL
# test[, c('Name', 'Ticket', 'Cabin', 'Embarked')] <- NULL
# 
# train$Sex <- as.numeric(train$Sex)
# test$Sex <- as.numeric(test$Sex)
# 
# # fill in age
# mean_age <- mean(train$Age, na.rm = TRUE)
# train[is.na(train$Age), ]$Age <- mean_age
# test[is.na(test$Age), ]$Age <- mean_age
# 
# test[is.na(test$Fare), ]$Fare <- mean(train$Fare, rm.na = TRUE)
# 
# knn_train <- train
# knn_train[, c('PassengerId', 'Survived')] <- NULL
# knn_test <- test
# knn_test[, c('PassengerId')] <- NULL
# 
# knn_pred <- class::knn(knn_train, knn_test, train$Survived, k = 30, l = 0, prob = FALSE, use.all = TRUE)
# write.csv(data.frame(PassengerId = test$PassengerId, Survived = knn_pred), file = 'predictions.csv', row.names = FALSE)

# Data Output --------------------------------------------------------------

write.csv(data.frame(PassengerId = test$PassengerId, Survived = test$Survived), file = 'predictions.csv', row.names = FALSE)

