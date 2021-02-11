library(tidyverse)
library(ROCR)
library(caret)

Greyson <- read.csv('Greyson.csv', header = T)

Greyson

colnames(Greyson)

colSums(is.na(Greyson))    #Missing Values in each column


## Categorical data - convert to factor----------------------------------------
Greyson$Renewal <- as.factor(Greyson$Renewal)
Greyson$DwellingType <- factor(Greyson$DwellingType, levels = c('S', 'M', 'U'))
Greyson$Gender <- factor(Greyson$Gender, levels = c('F', 'M', 'U'))
Greyson$Marital <- factor(Greyson$Marital, levels = c('S', 'M', 'O', 'U'))
Greyson$ChildPresent <- factor(Greyson$ChildPresent, levels = c('Y', 'N', 'U'))
Greyson$Occupation <- factor(Greyson$Occupation, levels = c('R', 'W', 'M', 'B', 'H', 'U'))
Greyson$ResidenceLength <- as.factor(Greyson$ResidenceLength)
Greyson$HouseholdSize <- as.factor(Greyson$HouseholdSize)
Greyson$HomeValue <- as.factor(Greyson$HomeValue)
Greyson$MagazineStatus <- factor(Greyson$MagazineStatus, levels = c('A', 'B', 'C', 'E', 'N', 'O', 'S', 'U'))
Greyson$LastPaymentType <- factor(Greyson$LastPaymentType, levels = c('A', 'C', 'D', 'E', 'F', 'G', 'I', 'K', 'L', 'M', 'S', 'U', 0:10))
Greyson$GiftDonor <- factor(Greyson$GiftDonor, levels = c('Y', 'N'))

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


Greyson_test <- impute(Greyson_test, object = trainMedians)    #impute test set

sum(is.na(Greyson_test))



##Convert categorical predictor var to dummy-----------------------------------
Greyson_train$Renewal <- as.factor(Greyson_train$Renewal)
Greyson_train$DwellingType <- factor(Greyson_train$DwellingType, levels = c('S', 'M', 'U'))
Greyson_train$Gender <- factor(Greyson_train$Gender, levels = c('F', 'M', 'U'))
Greyson_train$Marital <- factor(Greyson_train$Marital, levels = c('S', 'M', 'O', 'U'))
Greyson_train$ChildPresent <- factor(Greyson_train$ChildPresent, levels = c('Y', 'N', 'U'))
Greyson_train$Occupation <- factor(Greyson_train$Occupation, levels = c('R', 'W', 'M', 'B', 'H', 'U'))
Greyson_train$ResidenceLength <- as.factor(Greyson_train$ResidenceLength)
Greyson_train$HouseholdSize <- as.factor(Greyson_train$HouseholdSize)
Greyson_train$HomeValue <- as.factor(Greyson_train$HomeValue)
Greyson_train$MagazineStatus <- factor(Greyson_train$MagazineStatus, levels = c('A', 'B', 'C', 'E', 'N', 'O', 'S', 'U'))
Greyson_train$LastPaymentType <- factor(Greyson_train$LastPaymentType, levels = c('A', 'C', 'D', 'E', 'F', 'G', 'I', 'K', 'L', 'M', 'S', 'U', 0:10))
Greyson_train$GiftDonor <- factor(Greyson_train$GiftDonor, levels = c('Y', 'N'))

Greyson_train <- model.matrix(~., data = Greyson_train)
Greyson_train <- data.frame(Greyson_train[,-1])        #take out intercept column

Greyson_test$Renewal <- as.factor(Greyson_test$Renewal)
Greyson_test$DwellingType <- factor(Greyson_test$DwellingType, levels = c('S', 'M', 'U'))
Greyson_test$Gender <- factor(Greyson_test$Gender, levels = c('F', 'M', 'U'))
Greyson_test$Marital <- factor(Greyson_test$Marital, levels = c('S', 'M', 'O', 'U'))
Greyson_test$ChildPresent <- factor(Greyson_test$ChildPresent, levels = c('Y', 'N', 'U'))
Greyson_test$Occupation <- factor(Greyson_test$Occupation, levels = c('R', 'W', 'M', 'B', 'H', 'U'))
Greyson_test$ResidenceLength <- as.factor(Greyson_test$ResidenceLength)
Greyson_test$HouseholdSize <- as.factor(Greyson_test$HouseholdSize)
Greyson_test$HomeValue <- as.factor(Greyson_test$HomeValue)
Greyson_test$MagazineStatus <- factor(Greyson_test$MagazineStatus, levels = c('A', 'B', 'C', 'E', 'N', 'O', 'S', 'U'))
Greyson_test$LastPaymentType <- factor(Greyson_test$LastPaymentType, levels = c('A', 'C', 'D', 'E', 'F', 'G', 'I', 'K', 'L', 'M', 'S', 'U', 0:10))
Greyson_test$GiftDonor <- factor(Greyson_test$GiftDonor, levels = c('Y', 'N'))

Greyson_test<-model.matrix(~.,data=Greyson_test)
Greyson_test<-data.frame(Greyson_test[,-1]) #take out intercept column


##Fit the Model----------------------------------------------------------------
library(glmnet)

fitControl <- trainControl(method = "cv",        #Cross Validation
                           number = 5,           #5-Fold CV
                           verboseIter = FALSE)

#alpha=1 is for LASSO but alpha=0 for ridge 
RidgeGrid = expand.grid(alpha = 0,
                        lambda = seq(0, 1, by = 0.01))


modelFit <- train(as.factor(Renewal1)~., 
                  data = Greyson_train , 
                  method = "glmnet", 
                  trControl = fitControl,
                  tuneGrid = RidgeGrid)
plot(modelFit)


##Predicted probabilities------------------------------------------------------
Greyson_prob <- predict(modelFit, Greyson_test, type = 'prob')

head(Greyson_test)

pred <- prediction(Greyson_prob[,2], Greyson_test$Renewal1)

perf <- performance(pred, 'tpr', 'fpr')

plot(perf, colorize = TRUE)                                #Plot ROC curve

AUC <- unlist(slot(performance(pred, 'auc'), 'y.values'))  #AUC
AUC
