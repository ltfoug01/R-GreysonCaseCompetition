#Case Competition - Logisitic Regression
library(tidyverse)
library(ROCR)
library(caret)

Greyson <- read.csv('Greyson.csv', header = T)

Greyson

colnames(Greyson)

colSums(is.na(Greyson))    #Missing Values in each column


## Categorical data - convert to factor----------------------------------------
Greyson$Renewal <- as.factor(Greyson$Renewal)
Greyson$DwellingType <- as.factor(Greyson$DwellingType)
Greyson$Gender <- as.factor(Greyson$Gender)
Greyson$Marital <- as.factor(Greyson$Marital)
Greyson$ChildPresent <- as.factor(Greyson$ChildPresent)
Greyson$Occupation <- as.factor(Greyson$Occupation)
Greyson$GiftDonor <- as.factor(Greyson$GiftDonor)
Greyson$MagazineStatus <- as.factor(Greyson$MagazineStatus)
Greyson$LastPaymentType <- as.factor(Greyson$LastPaymentType)
Greyson$ResidenceLength <- as.factor(Greyson$ResidenceLength)
Greyson$HomeValue <- as.factor(Greyson$HomeValue)
Greyson$HouseholdSize <- as.factor(Greyson$HouseholdSize)

str(Greyson)



##Partition the data-----------------------------------------------------------
set.seed(4)
index <- sample(nrow(Greyson), nrow(Greyson) * 0.80) #80% training

Greyson_train = Greyson[index,]
Greyson_test = Greyson[-index,]



##Impute Missing Predictor Variables in Train Set------------------------------
library(imputeMissings)

trainMedians <- compute(Greyson_train, method = 'median/mode') #get medians/modes

Greyson_train <- impute(Greyson_train, object = trainMedians)  #impute train set

sum(is.na(Greyson_train))                       #check for missing values
colSums(is.na(Greyson_train))


Greyson_test <- impute(Greyson_test, object = trainMedians)    #impute test set

sum(is.na(Greyson_test))



##Convert categorical predictor var to dummy-----------------------------------
Greyson_train <- model.matrix(~., data = Greyson_train) #convert train 
Greyson_train <- data.frame(Greyson_train[,-1])         #take out intercept column

colnames(Greyson_train)

Greyson_test <- model.matrix(~., data = Greyson_test)   #convert test
Greyson_test <- data.frame(Greyson_test[,-1])           #take out intercept column



##Fit the Model----------------------------------------------------------------
Greyson_glm0 <- glm(Renewal1~., family = binomial(logit), data = Greyson_train) #Build logistic regression model

summary(Greyson_glm0)  #coefficient estimates



##Predicted probabilities for the test data------------------------------------
pred_glm0_test <- predict(Greyson_glm0, newdata = Greyson_test, type = 'response')
pred_glm0_test  

pred <- prediction(pred_glm0_test, Greyson_test$Renewal1)

perf <- performance(pred, 'tpr', 'fpr')

plot(perf, colorize = TRUE)                                #Plot ROC curve

AUC <- unlist(slot(performance(pred, 'auc'), 'y.values'))  #AUC
AUC

