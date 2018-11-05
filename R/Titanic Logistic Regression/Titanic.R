
train_titanic = read.csv("train.csv")
test_titanic = read.csv("test.csv")

str(train_titanic)
str(test_titanic)
dim(train_titanic)
dim(test_titanic)
set.seed(1)

test_titanic$Survived = rep(0,418)


#Prediction Three
#Remove the missing Values
combined = rbind(train_titanic,test_titanic)
summary(combined)
dim(combined)




#Find the total number of missing values in the combined data
sapply(combined , function(x) sum(is.na(x)))
sapply(combined, function(x) sum(x==""))

#Age, Fare is missing
#Cabin, Embarke is missing


#Fill the Fare column
class(combined$Fare)
sum(is.na(combined$Fare))
mean(combined$Fare, na.rm = TRUE)
combined$Fare[is.na(combined$Fare)] = 33.29548

#Fill the Age column
class(combined$Age)
sum(is.na(combined$Age))
hist(combined$Age)
median(combined$Age, na.rm = TRUE)
combined$Age[is.na(combined$Age)] = 28

#Fill Embarked Column
class(combined$Embarked)
summary(combined$Embarked)
######C-270, Q-123, S-914, we will take "S" to fill the two missing values as it occurs max times
combined$Embarked[combined$Embarked==""] = "S"

#Fill Caabin Coulmn
class(combined$Cabin)
#As it has more factors inside and more than 75%missing values we will omit this column


#Splitting back to Train and Test Data
tr_filled = head(combined, nrow(train_titanic))
ts_filled = tail(combined, nrow(test_titanic))

dim(tr_filled)
dim(ts_filled)

#Using Logistic regression to find the first model
logreg_titanic = glm(Survived~Pclass+Sex+Age, data = tr_filled,family="binomial")
summary(logreg_titanic)
pred_titanic = predict(logreg_titanic, tr_filled, type="response")
summary(pred_titanic)

#As the output of the predictin is probability we use the ifelse to convert the values to 0 and 1   
pred_titanic=ifelse(pred_titanic>=0.5,1,0)


# Confusion Matrix
table(tr_filled$Survived,pred_titanic)
accuracy = (454+249)/nrow(tr_filled)
accuracy
error = 1- accuracy
error

pred_test = predict(logreg_titanic, newdata = ts_filled,type="response")
pred_test=ifelse(pred_test>=0.5,1,0)
summary(pred_test)
table(ts_filled$Survived, pred_test)
acc_test = (0+162)/nrow(ts_filled)
acc_test
