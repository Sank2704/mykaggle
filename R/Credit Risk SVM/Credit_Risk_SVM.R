
#Setting the Working Direcctory
setwd("")

#Reading the Training Data into R
train_data = read.csv("R_Module_Day_7.2_Credit_Risk_Train_data.csv")
#Reading the Test  Data into R
test_data = read.csv("R_Module_Day_8.2_Credit_Risk_Validate_data.csv")
str(train_data)
str(test_data)

#Change column name in test data to merge data
test_data$Loan_Status = test_data$outcome
test_data$outcome = NULL

#Combine both test and train data to remove all missing values 
combined_data = rbind(train_data,test_data)
str(combined_data)

#Find the total number of missing values in the combined data
sapply(combined_data , function(x) sum(is.na(x)))
sapply(combined_data , function(x) sum(x==""))

#Cleaning and Converting the data
##Finding NA values in All Columns

sum(is.na(combined_data$LoanAmount)) #27 NA values
sum(is.na(combined_data$Loan_Amount_Term)) #20 NA values
sum(is.na(combined_data$Credit_History)) #79 NA values

str(combined_data)
str(combined_data$Loan_Amount_Term)

##Finding in categorical variables
sum(combined_data$Gender=="")  #24 NA Values
sum(combined_data$Married=="")  #3 NA Values
sum(combined_data$Dependents=="") #25 NA Values
sum(combined_data$Self_Employed=="")  #55 NA Values

summary(combined_data$Loan_Amount_Term)

#Filling the NA Values for Categorical Variables
combined_data$Gender[combined_data$Gender==""]="Male"
combined_data$Married[combined_data$Married==""]="Yes"
combined_data$Dependents[combined_data$Dependents==""]="0"
combined_data$Self_Employed[combined_data$Self_Employed==""]="No"

#Filling the NA Values for Numerical Variables
hist(combined_data$LoanAmount,breaks = 50)
median(combined_data$LoanAmount, na.rm = TRUE)
combined_data$LoanAmount[is.na(combined_data$LoanAmount)] = 126

hist(combined_data$Loan_Amount_Term)
combined_data$Loan_Amount_Term[is.na(combined_data$Loan_Amount_Term)] = 360

combined_data$Credit_History[is.na(combined_data$Credit_History)] = 1

#Converting Data Types
combined_data$CoapplicantIncome = as.integer(combined_data$CoapplicantIncome)

New_train = head(combined_data, nrow(train_data))
New_test = tail(combined_data , nrow(test_data))

prop.table(table(train_data$Loan_Status))
#Need to get more than 68% accuracy, as Baseline accuracy is 68%

#Load Library
library(e1071)
svm_model = svm(Loan_Status~ .-Loan_ID, data = New_train)
svm_model

predict_model_train = predict(svm_model)
predict_model_train


#Accuracy on train data
table(New_train$Loan_Status, predict_model_train)
acc = (85+416)/nrow(New_train)
acc

#Predict on test data
predict_model_test = predict(svm_model, newdata = New_test)
predict_model_test

#Accuracy on test data
table(New_test$Loan_Status, predict_model_test)
acc_test = (58+289)/nrow(New_test)
acc_test

#Trying to tune to check if we get better accuracy
svm_tune = tune.svm(Loan_Status~ .-Loan_ID, data = New_train, cost=seq(1,2,0.1), gamma =seq(1.5,1.6,0.01))
summary(svm_tune)
final_svm_tune = svm(Loan_Status~ .-Loan_ID, data = New_train, cost=1, gamma = 0.05263158)
predict_model_test_final = predict(final_svm_tune, newdata = New_test)
predict_model_test_final

#Accuracy on test data
table(New_test$Loan_Status, predict_model_test_final)
acc_test = (58+289)/nrow(New_test)
acc_test