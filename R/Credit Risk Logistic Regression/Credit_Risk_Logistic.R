
#Setting the Working Direcctory
setwd("G:\\Personnal\\CTS\\Imarticus\\Case Studies\\Credit Risk Contents\\")

#Reading the Training Data into R
df_train = read.csv("R_Module_Day_7.2_Credit_Risk_Train_data.csv")
#Loan Status is the dependent Variable

#Viewing the data types and summary
str(df_train)
dim(df_train)
summary(df_train)
class(df_train$ApplicantIncome)
class(df_train$CoapplicantIncome)

#Cleaning and Converting the data
##Finding NA values in All Columns

sum(is.na(df_train$LoanAmount)) #22 NA values
sum(is.na(df_train$Loan_Amount_Term)) #14 NA values
sum(is.na(df_train$Credit_History)) #50 NA values

str(df_train$Loan_Amount_Term)

##Finding in categorical variables
sum(df_train$Gender=="")  #13 NA Values
sum(df_train$Married=="")  #3 NA Values
sum(df_train$Dependents=="") #15 NA Values
sum(df_train$Self_Employed=="")  #32 NA Values

summary(df_train$Credit_History)

#Filling the NA Values for Categorical Variables
df_train$Gender[df_train$Gender==""]="Male"
df_train$Married[df_train$Married==""]="Yes"
df_train$Dependents[df_train$Dependents==""]="0"
df_train$Self_Employed[df_train$Self_Employed==""]="No"

#Filling the NA Values for Numerical Variables
hist(df_train$LoanAmount,breaks = 50)
median(df_train$LoanAmount, na.rm = TRUE)
df_train$LoanAmount[is.na(df_train$LoanAmount)] = 128

df_train$Loan_Amount_Term[is.na(df_train$Loan_Amount_Term)] = 360

df_train$Credit_History[is.na(df_train$Credit_History)] = 1

#Converting Data Types
df_train$CoapplicantIncome = as.integer(df_train$CoapplicantIncome)

#Preddicting the model 
colnames(df_train)
str(df_train)
logreg1 = glm(Loan_Status ~ Gender+Married+Credit_History+Property_Area, data = df_train,family = "binomial")
summary(logreg1)

pred1 = predict(logreg1,df_train,type="response")
library(ROCR)
ROCR1 = prediction(pred1,df_train$Loan_Status)
ROCCurve1= performance (ROCR1, "tpr","fpr")
plot(ROCCurve1,colorize=T)

auc.temp <- performance(ROCR1, "auc")
auc1 <- as.numeric(auc.temp@y.values)
auc1


#Select the threshold from the ROC Curve and use it in threshold
#Threshold 
pred1 = ifelse(pred1>=0.4,1,0)
pred1

# Confusion Matrix
table(df_train$Loan_Status,pred1)
accuracy1 = (82+415)/nrow(df_train)
accuracy1
error1 = 1- accuracy1
error1

test_data = read.csv("R_Module_Day_8.2_Credit_Risk_Validate_data.csv")
str(test_data)

##Finding NA values in All Columns

sum(is.na(test_data$LoanAmount)) #5 NA values
sum(is.na(test_data$Loan_Amount_Term)) #6 NA values
sum(is.na(test_data$Credit_History)) #29 NA values

str(test_data$Loan_Amount_Term)

##Finding in categorical variables
sum(test_data$Gender=="")  #11 NA Values
sum(test_data$Married=="")  #0 NA Values
sum(test_data$Dependents=="") #10 NA Values
sum(test_data$Self_Employed=="")  #23 NA Values

summary(test_data$Loan_Amount_Term)

#Filling the NA Values for Categorical Variables
test_data$Gender[test_data$Gender==""]="Male"
test_data$Married[test_data$Married==""]="Yes"
test_data$Dependents[test_data$Dependents==""]="0"
test_data$Self_Employed[test_data$Self_Employed==""]="No"

#Filling the NA Values for Numerical Variables
hist(test_data$LoanAmount,breaks = 50)
median(test_data$LoanAmount, na.rm = TRUE)
test_data$LoanAmount[is.na(test_data$LoanAmount)] = 125

hist(test_data$Loan_Amount_Term,breaks = 50)
test_data$Loan_Amount_Term[is.na(test_data$Loan_Amount_Term)] = 360

test_data$Credit_History[is.na(test_data$Credit_History)] = 1



#Predicting on the validate data
pred_test = predict(logreg1, newdata = test_data,type="response")
pred_test = ifelse(pred_test>=0.4,1,0)
table(test_data$outcome,pred_test)
acc_test = (58+289)/nrow(test_data)
acc_test
err_test = 1- acc_test
err_test
pred_test