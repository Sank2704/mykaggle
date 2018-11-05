
#Reading the data
train = read.csv("train.csv")
test = read.csv("test.csv")
str(tr$SalePrice)

colnames(train)
colnames(test)

test$SalePrice = rep(0,1459)
combine = rbind(train,test)


#Descriptive analysis
dim(combine)
summary(combine)
str(combine)

#Analysing each Variable

sapply(combine, function(x) sum(is.na(x))) #To find the missing values in Continous variable
#LotFrontage-486
#Alley-1369
#MasVnrType-8
#MasVnrArea-8
#BsmtQual-37
#BsmtExposure-38
#BsmtFinType1-37
#BsmtFinType2-38
#Electrical-1
#FireplaceQu-690
#GarageType-81
#GarageYrBlt-81
#GarageFinish-81
#GarageQual-81
#GarageCond-81
#PooQC  1453
#Fence-1179
#MiscFeature-1406

str(combine$LotFrontage)
hist(combine$LotFrontage)
combine$LotFrontage[is.na(combine$LotFrontage)] = median(combine$LotFrontage, na.rm = TRUE)

which(is.na(combine$MasVnrType))
table(combine$MasVnrType)
str(combine$MasVnrType)
combine$MasVnrType[is.na(combine$MasVnrType)] = "None"

MasVnrArea
which.max(table(combine$MasVnrArea))
hist(combine$MasVnrArea,breaks = 100)
str(combine$MasVnrArea)
combine$MasVnrArea[is.na(combine$MasVnrArea)] = 0
sum(is.na(combine$MasVnrArea))

str(combine$BsmtQual)
which.max((combine$BsmtQual))
combine$BsmtQual[is.na(combine$BsmtQual)] = "TA"
sum(is.na(combine$BsmtQual))

str(combine$BsmtExposure)
which.max(table(combine$BsmtExposure))
combine$BsmtExposure[is.na(combine$BsmtExposure)] = "No"
sum(is.na(combine$BsmtExposure))

str(combine$BsmtFinType1)
which.max(table(combine$BsmtFinType1))
combine$BsmtFinType1[is.na(combine$BsmtFinType1)] = "Unf"
sum(is.na(combine$BsmtFinType1))


str(combine$BsmtFinType2)
which.max(table(combine$BsmtFinType2))
combine$BsmtFinType2[is.na(combine$BsmtFinType2)] = "Unf"
sum(is.na(combine$BsmtFinType2))

Electrical
str(combine$Electrical)
which.max(table(combine$Electrical))
combine$Electrical[is.na(combine$Electrical)] = "SBrkr"
sum(is.na(combine$Electrical))

FireplaceQu
str(combine$FireplaceQu)
which.max(table(combine$FireplaceQu))
combine$FireplaceQu[is.na(combine$FireplaceQu)] = "Gd"
sum(is.na(combine$FireplaceQu))

GarageType
str(combine$GarageType)
which.max(table(combine$GarageType))
combine$GarageType[is.na(combine$GarageType)] = "Attchd"
sum(is.na(combine$GarageType))

GarageYrBlt
str(combine$GarageYrBlt)
str(combine$LotFrontage)
hist(combine$GarageYrBlt)
combine$GarageYrBlt[is.na(combine$GarageYrBlt)] = median(combine$GarageYrBlt, na.rm = TRUE)
sum(is.na(combine$GarageYrBlt))

GarageFinish
str(combine$GarageFinish)
which.max(table(combine$GarageFinish))
combine$GarageFinish[is.na(combine$GarageFinish)] = "Unf"
sum(is.na(combine$GarageFinish))


GarageQual
str(combine$GarageQual)
which.max(table(combine$GarageQual))
combine$GarageQual[is.na(combine$GarageQual)] = "TA"
sum(is.na(combine$GarageQual))

GarageCond
str(combine$GarageCond)
which.max(table(combine$GarageCond))
combine$GarageCond[is.na(combine$GarageCond)] = "TA"
sum(is.na(combine$GarageCond))




sapply(train, function(x) sum((x=="")))  #To find the missing values in Categorical variable
sapply(combine, function(x) sum(is.na(x))) #To find the missing values in Continous variable
#Omittng 'Alley', 'PoolQC', 'Fence', 'MiscFeature' from modelling, as there are large amount of missing values in these



str(combine$MSZoning)
which.max(table(combine$MSZoning))
combine$MSZoning[is.na(combine$MSZoning)] = "RL"
sum(is.na(combine$MSZoning))

str(combine$Utilities)
which.max(table(combine$Utilities))
combine$Utilities[is.na(combine$Utilities)] = "AllPub"
sum(is.na(combine$Utilities))


str(combine$Exterior1st)
which.max(table(combine$Exterior1st))
combine$Exterior1st[is.na(combine$Exterior1st)] = "VinylSd"
sum(is.na(combine$Exterior1st))

str(combine$Exterior2nd)
which.max(table(combine$Exterior2nd))
combine$Exterior2nd[is.na(combine$Exterior2nd)] = "VinylSd"
sum(is.na(combine$Exterior2nd))

str(combine$BsmtCond)
which.max(table(combine$BsmtCond))
combine$BsmtCond[is.na(combine$BsmtCond)] = "TA"
sum(is.na(combine$BsmtCond))


str(combine$BsmtFinSF1)
hist(combine$BsmtFinSF1)
combine$BsmtFinSF1[is.na(combine$BsmtFinSF1)] = median(combine$BsmtFinSF1, na.rm = TRUE)
sum(is.na(combine$BsmtFinSF1))


str(combine$BsmtFinSF2)
hist(combine$BsmtFinSF2)
combine$BsmtFinSF2[is.na(combine$BsmtFinSF2)] = median(combine$BsmtFinSF2, na.rm = TRUE)
sum(is.na(combine$BsmtFinSF2))


str(combine$BsmtUnfSF)
hist(combine$BsmtUnfSF)
combine$BsmtUnfSF[is.na(combine$BsmtUnfSF)] = median(combine$BsmtUnfSF, na.rm = TRUE)
sum(is.na(combine$BsmtUnfSF))

sapply(combine, function(x) sum(is.na(x))) #To find the missing values in Continous variable

str(combine$TotalBsmtSF)
hist(combine$TotalBsmtSF)
combine$TotalBsmtSF[is.na(combine$TotalBsmtSF)] = median(combine$TotalBsmtSF, na.rm = TRUE)
sum(is.na(combine$TotalBsmtSF))


str(combine$BsmtFullBath)
hist(combine$BsmtFullBath)
combine$BsmtFullBath[is.na(combine$BsmtFullBath)] = median(combine$TotalBsmtSF, na.rm = TRUE)
sum(is.na(combine$BsmtFullBath))


str(combine$BsmtHalfBath)
hist(combine$BsmtHalfBath)
combine$BsmtHalfBath[is.na(combine$BsmtHalfBath)] = median(combine$BsmtHalfBath, na.rm = TRUE)
sum(is.na(combine$BsmtHalfBath))


str(combine$KitchenQual)
which.max(table(combine$KitchenQual))
combine$KitchenQual[is.na(combine$KitchenQual)] = "TA"
sum(is.na(combine$KitchenQual))


str(combine$Functional)
which.max(table(combine$Functional))
combine$Functional[is.na(combine$Functional)] = "Typ"
sum(is.na(combine$Functional))

str(combine$GarageCars)
hist(combine$GarageCars)
combine$GarageCars[is.na(combine$GarageCars)] = median(combine$GarageCars, na.rm = TRUE)
sum(is.na(combine$GarageCars))


str(combine$GarageArea)
hist(combine$GarageArea)
combine$GarageArea[is.na(combine$GarageArea)] = median(combine$GarageArea, na.rm = TRUE)
sum(is.na(combine$GarageArea))

sapply(combine, function(x) sum(is.na(x))) #To find the missing values in Continous variable


str(combine$SaleType)
which.max(table(combine$SaleType))
combine$SaleType[is.na(combine$SaleType)] = "WD"
sum(is.na(combine$SaleType))



#Find the total number of missing values in the combined data
sapply(combine , function(x) sum(is.na(x)))
sapply(combine, function(x) sum(x==""))

#PoolQC-73      Fence-74   MiscFeature-75 Alley-7
#2909          2348          2814   2721
#Omitting the above variables, as they have larger amount of missing values


#Model Building
train_filled = head(combine, nrow(train))
test_filled = tail(combine, nrow(test))



f1 = as.formula(paste("SalePrice ~ ", paste(names(train_filled)[c(-3,-73,-74,-75,-7,-ncol(train_filled))],collapse="+")))
f1


linreg = lm(f1,data = train_filled )
summary(linreg)



#Predicting on Test Data
pred = predict(linreg,newdata = train_filled)
head(pred)
head(train_filled$SalePrice)
head(test_filled$Id)


prde_test = predict(linreg, newdata = test_filled)
head(prde_test)

setwd("G:\\Personnal\\CTS\\Imarticus\\R Code\\Kaggle\\House Prices\\")
submit <- data.frame(Id = test_filled$Id, SalePrice = prde_test)
write.csv(submit, file = "Submission1house.csv", row.names = FALSE)


#Calculating The R2 on Test Data
SSE = sum((test_filled$SalePrice - pred)^2)
SST = sum((test_filled$SalePrice - mean(test_filled$SalePrice))^2)
R2 = 1- (SSE/SST)
R2
