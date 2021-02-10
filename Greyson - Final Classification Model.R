#Greyson Data------------------------------------------------------------------
Greyson <- read.csv(file = "Greyson.csv", header = T)
head(Greyson)
str(Greyson)
colnames(Greyson)



## Categorical data - convert to factor w/ levels------------------------------
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



## Partition the data----------------------------------------------------------
set.seed(4)
index <- createDataPartition(Greyson$Renewal, p = .8,list = FALSE)
Greyson_train <- Greyson[index,]
Greyson_test <- Greyson[-index,]



##Impute Missing Predictor Variables-------------------------------------------
library(caret)
library(imputeMissings)

colSums(is.na(Greyson))   #Missing values in each column
sum(is.na(Greyson))       #all N/A values

trainMedians<- compute(Greyson_train, method = "median/mode")  #calc.the medians/modes from training data


Greyson_train <- imputeMissings::impute(Greyson_train,object = trainMedians) #into train set
Greyson_test <- imputeMissings::impute(Greyson_test,object = trainMedians)   #into test set

sum(is.na(Greyson_train))
sum(is.na(Greyson_test))

str(Greyson_train)


#Train categorical variables to factors----------------------------------------
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

Greyson_train <- model.matrix(~., data = Greyson_train) #train dummies
Greyson_train <- data.frame(Greyson_train[,-1])         #take out intercept col.


#Test categorical variables to factors-----------------------------------------
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

Greyson_test <- model.matrix(~., data = Greyson_test)   #test dummies
Greyson_test <- data.frame(Greyson_test[,-1])           #take out intercept col.



## Train and Fit Data----------------------------------------------------------
library(rpart)
library(rpart.plot)

Greyson_rpart <- rpart(formula = Renewal1 ~ ., data = Greyson_train, 
                       method = "class", parms = list(split = 'information'))



## Tree Plotting and Printing--------------------------------------------------
Greyson_rpart
prp(Greyson_rpart, extra = 1)



## Pruning/Overfitting---------------------------------------------------------
plotcp(Greyson_rpart)

prune_tree <- prune(Greyson_rpart, cp = 0.014)

prp(prune_tree, extra = 1)  #print pruned tree



## Predicted probabilities for the test data-----------------------------------
Greyson_test_prob <- predict(prune_tree, Greyson_test, type = "prob")

head(Greyson_test_prob)  #Use 2nd col. for predicted probability of Yes

library(ROCR)

pred = prediction(Greyson_test_prob[,2], Greyson_test$Renewal1)

perf = performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)                      #plot the ROC

slot(performance(pred, "auc"), "y.values")[[1]]  #get AUC



## Test additional unseen data - GreysonHoldout--------------------------------
Greyson_holdout <- read.csv("GreysonHoldout.csv", header = T)


## Convert categorical to factor-----------------------------------------------
Greyson_holdout$DwellingType <- factor(Greyson_holdout$DwellingType, levels = c('S', 'M', 'U'))
Greyson_holdout$Gender <- factor(Greyson_holdout$Gender, levels = c('F', 'M', 'U'))
Greyson_holdout$Marital <- factor(Greyson_holdout$Marital, levels = c('S', 'M', 'O', 'U'))
Greyson_holdout$ChildPresent <- factor(Greyson_holdout$ChildPresent, levels = c('Y', 'N', 'U'))
Greyson_holdout$Occupation <- factor(Greyson_holdout$Occupation, levels = c('R', 'W', 'M', 'B', 'H', 'U'))
Greyson_holdout$ResidenceLength <- as.factor(Greyson_holdout$ResidenceLength)
Greyson_holdout$HouseholdSize <- as.factor(Greyson_holdout$HouseholdSize)
Greyson_holdout$HomeValue <- as.factor(Greyson_holdout$HomeValue)
Greyson_holdout$MagazineStatus <- factor(Greyson_holdout$MagazineStatus, levels = c('A', 'B', 'C', 'E', 'N', 'O', 'S', 'U'))
Greyson_holdout$LastPaymentType <- factor(Greyson_holdout$LastPaymentType, levels = c('A', 'C', 'D', 'E', 'F', 'G', 'I', 'K', 'L', 'M', 'S', 'U', 0:10))
Greyson_holdout$GiftDonor <- factor(Greyson_holdout$GiftDonor, levels = c('Y', 'N'))

library(tidyverse)


str(Greyson_holdout)



## Impute missing values from the train Medians/Modes--------------------------
trainMediansnoResponse <- trainMedians[names(trainMedians) != "Renewal"] ##Get medians from training set without the response for holdout set
Greyson_holdout <- impute(Greyson_holdout, object = trainMediansnoResponse )

sum(is.na(Greyson_holdout))



##Convert categorical data to dummy variables
Greyson_holdout$DwellingType <- factor(Greyson_holdout$DwellingType, levels = c('S', 'M', 'U'))
Greyson_holdout$Gender <- factor(Greyson_holdout$Gender, levels = c('F', 'M', 'U'))
Greyson_holdout$Marital <- factor(Greyson_holdout$Marital, levels = c('S', 'M', 'O', 'U'))
Greyson_holdout$ChildPresent <- factor(Greyson_holdout$ChildPresent, levels = c('Y', 'N', 'U'))
Greyson_holdout$Occupation <- factor(Greyson_holdout$Occupation, levels = c('R', 'W', 'M', 'B', 'H', 'U'))
Greyson_holdout$ResidenceLength <- as.factor(Greyson_holdout$ResidenceLength)
Greyson_holdout$HouseholdSize <- as.factor(Greyson_holdout$HouseholdSize)
Greyson_holdout$HomeValue <- as.factor(Greyson_holdout$HomeValue)
Greyson_holdout$MagazineStatus <- factor(Greyson_holdout$MagazineStatus, levels = c('A', 'B', 'C', 'E', 'N', 'O', 'S', 'U'))
Greyson_holdout$LastPaymentType <- factor(Greyson_holdout$LastPaymentType, levels = c('A', 'C', 'D', 'E', 'F', 'G', 'I', 'K', 'L', 'M', 'S', 'U', 0:10))
Greyson_holdout$GiftDonor <- factor(Greyson_holdout$GiftDonor, levels = c('Y', 'N'))

Greyson_holdout <- model.matrix(~., data = Greyson_holdout)
Greyson_holdout <- data.frame(Greyson_holdout[,-1]) #take out intercept column


##Predicted Probabilities of new test set
Greyson_holdoutprob <- predict(Greyson_rpart, Greyson_holdout, type = "prob")
head(Greyson_holdoutprob)

write.csv(Greyson_holdoutprob,'GreysonHold2.csv') #write results to folder.
