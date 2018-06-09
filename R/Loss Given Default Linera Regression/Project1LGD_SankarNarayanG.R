#Setting the Working Directory
setwd("G:\\Personnal\\CTS\\Imarticus\\Case Studies\\LGD")
sourceData = read.csv("Loss Given Default Data Sets.csv")

#Data Frame with the CSV Data in R memory
sourceData$Number.of.Vehicles

summary(sourceData)
dim(sourceData)
#To Find the data types of each Column Name in the Data Frame
str(sourceData)

sourceData$Ac_No = as.integer(sourceData$Ac_No)

#Computing the Histogram
hist(sourceData$Losses.in.Thousands, breaks = 40)
boxplot(sourceData$Losses.in.Thousands)


#Splitting the Data into Training and Test Data
set.seed(1)
library(caTools)
splitedData = sample.split(sourceData$Losses.in.Thousands, SplitRatio = 0.70)
splitedData
#Training Data
trainingData = subset(sourceData, splitedData==TRUE)
dim(trainingData)

#Test Data
testData = subset(sourceData, splitedData==FALSE)
testData
dim(testData)


#Find the Co-relation between the columns which are numeric only
cor(sourceData[,c(2,3,7)])

#On Training Data Computing the Linear Regression
linreg = lm(log(Losses.in.Thousands) ~ Age+Gender+Married+Number.of.Vehicles,data = trainingData)
summary(linreg)

#Predicting on Test Data
pred = predict(linreg,newdata = testData)
act_pred = 2.718^(pred)
head(act_pred)

#Calculating The R2 on Test Data
SSE = sum((testData$Losses.in.Thousands - act_pred)^2)
SST = sum((testData$Losses.in.Thousands - mean(testData$Losses.in.Thousands))^2)
R2 = 1- (SSE/SST)
R2

#Improving the Model by spliting the age into groups of age

trainingData$AgeGroup = ifelse(trainingData$Age<=25,"Young",ifelse(trainingData$Age>=58,"old","middleage"))
testData$AgeGroup = ifelse(testData$Age<=25,"Young",ifelse(testData$Age>=58,"old","middleage"))
head(trainingData)
head(testData)

linreg_final = lm(log(Losses.in.Thousands) ~ Gender+Married+AgeGroup,data = trainingData)
summary(linreg_final)

#Prediciting on the Test data
pred_final = predict(linreg_final,newdata = testData)
act_final_pred = 2.718^(pred_final)

summary(act_final_pred)

SSEFinal = sum((testData$Losses.in.Thousands - act_final_pred)^2)
SSTFinal = sum((testData$Losses.in.Thousands - mean(testData$Losses.in.Thousands))^2)
R2Final = 1- (SSEFinal/SSTFinal)
R2Final
RMSE = (SSEFinal/nrow(sourceData))^0.5
RMSE
