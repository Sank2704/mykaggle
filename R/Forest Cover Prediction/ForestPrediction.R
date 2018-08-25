setwd("G:\\Personnal\\CTS\\Imarticus\\R Code\\Kaggle\\Forest Cover Type Prediction\\")
train = read.csv("train.csv")
test = read.csv("test.csv")

summary(train)
str(train)
head(train$Cover_Type)
table(train$Cover_Type)
class(train$Cover_Type)
train$Cover_Type = as.factor(train$Cover_Type)

test$Cover_Type = rep(0,565892)

combined = rbind(train,test)
class(combined$Cover_Type)
table(combined$Cover_Type)

sapply(combined, function(x) sum(is.na(x)))
sapply(combined, function(x) sum(x==""))

#No missing vaues are found, except the missing column vaues in test data for Cover_Type

#Creating partoin of data train data sets
library(caTools)
set.seed(1)

split <- sample.split(train$Cover_Type, SplitRatio = 0.75)
split_train = subset(train, split==TRUE)
split_test = subset(train, split==FALSE)



library(randomForest)
rf = randomForest(Cover_Type ~ .-Id, data = split_train, ntree = 2000, nodesize = 2, mtry = 50)
pred_rf = predict(rf)
table(split_train$Cover_Type, pred_rf)

(1215+1092+1316+1562+1537+1435+1559)/nrow(split_train)

pred_test = predict(rf, newdata = split_test)
table(split_test$Cover_Type, pred_test)
(407+353+444+530+518+500+520)/nrow(split_test)

prde_new_test = predict(rf,test)
solution <- data.frame(Id = test$Id , Cover_Type = prde_new_test)

write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)
