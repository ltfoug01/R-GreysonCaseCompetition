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



##Partition the Data-----------------------------------------------------------
set.seed(4)
index <-createDataPartition(Greyson$Renewal, p = .8,list = FALSE)

Greyson_train <- Greyson[index,]
Greyson_test <- Greyson[-index,]



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

colnames(Greyson_test)



##Fit the Model----------------------------------------------------------------
fitControl <- trainControl(method = 'repeatedcv', #Cross Validation
                           number = 5, 
                           verboseIter = FALSE)


##Train the Model--------------------------------------------------------------
library(xgboost)
model_gbm <- train(as.factor(Renewal1) ~ ., #response var.
                   data = Greyson_train,    #use train set
                   method = 'xgbTree',      #implement graadient boosted tree
                   trControl = fitControl,  
                   verbose = FALSE)
model_gbm

plot(model_gbm)

plot(varImp(model_gbm, scale = F))  #important predictor variables


#Set up tuning parameter selection and CV for the training set
# define a grid of parameter options to try
xgb_grid <- expand.grid(
  nrounds = c(50,200),
  eta = c(0.025, 0.05, 0.1),
  max_depth = c(2, 3, 4, 5),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1)


# Train model with preprocessing & cv
model_rf_tune <- train(as.factor(Renewal1) ~.,
                       data = Greyson_train,
                       method = 'xgbTree',
                       trControl = fitControl,
                       tuneGrid = xgb_grid,    #provide grid of parameters
                       verbose  = FALSE)

plot(model_rf_tune)



#Predicted Probabilities-------------------------------------------------------
Greyson_prob <- predict(model_gbm, newdata = Greyson_test, type = 'prob') #predict testing set

Greyson_prob 


pred <- prediction(Greyson_prob[,2], Greyson_test$Renewal1)

perf <- performance(pred, 'tpr', 'fpr')

plot(perf, colorize = TRUE)                               #Plot ROC curve

AUC <- unlist(slot(performance(pred, 'auc'), 'y.values')) #AUC

AUC

#-------------------


