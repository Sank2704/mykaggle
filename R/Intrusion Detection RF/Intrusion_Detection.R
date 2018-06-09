
#Setting the Working Direcctory
setwd("G:\\Personnal\\CTS\\Imarticus\\DataSets for Project 2 3 4\\Project 4 Network Intrution Detection\\")

#Reading the Training Data into R
Train_intrusion = read.csv("R_Module_Day_12.1_Network_Intrusion_Train_data (2).csv")
#Reading the Test  Data into R
Test_intrusion = read.csv("R_Module_Day_12.2_Network_Intrusion_Test_data (2).csv")
#Reading the Validate  Data into R
Validate_intrusion = read.csv("R_Module_Day_12.3_Network_Intrusion_Validate_data (2).csv")
str(Train_intrusion)
str(Test_intrusion)
set.seed(1)
#Combine both test and train data to remove all missing values 
combined_data = rbind(Train_intrusion,Validate_intrusion)
str(combined_data)


sapply(combined_data , function(x) sum(is.na(x)))
sapply(combined_data , function(x) sum(x==""))
#No Missing values

library(rpart)
library(rpart.plot)
prop.table(table(Train_intrusion$class))
#Basline accuracy is around 53%

f1 = as.formula(paste("class ~ ", paste(names(Train_intrusion)[c(-3,-ncol(Train_intrusion))],collapse="+")))
f1

#Tree Building
tree = rpart(f1, data = Train_intrusion, minbucket = 100, method="class")
prp(tree)


pred_tree_train = predict(tree, type = "class")
rpart.plot(tree)
table(Train_intrusion$class,pred_tree_train )

#accuracy on Train Data
accuracy_Train = (11139+13078)/nrow(Train_intrusion)
accuracy_Train

#Predicting on Test Data
pred_test = predict(tree, newdata = Test_intrusion, type="class")
table(Validate_intrusion$class, pred_test)
accuracy = (7621+9410)/nrow(Validate_intrusion)
accuracy 



######Random Forest
install.packages("randomForest")
library(randomForest)

rf = randomForest(f1, data = Train_intrusion, ntree = 300, mtry = 5, nodesize = 20)
predict_rf = predict(rf, newdata = Validate_intrusion)
predict_rf


#Checking the accuracy
table(Validate_intrusion$class, predict_rf)
accuracy_rf = (8050+9447)/nrow(Validate_intrusion)
accuracy_rf
#77% accuracy on Validate data