library(caret)
library(ROCR)

Greyson <- read.csv("Greyson.csv", header=T)

Greyson$Renewal <- as.factor(Greyson$Renewal)

Greyson<- imputeMissings::impute(Greyson,method = "median/mode")

sum(is.na(Greyson))


##Partition the Data-----------------------------------------------------------
set.seed(4)
index <- createDataPartition(Greyson$Renewal, p = .8, list = FALSE)

Greyson_train <- Greyson[index,]
Greyson_test <- Greyson[-index,]


##Fit the Model----------------------------------------------------------------
fitControl<-trainControl(method = "repeatedcv",  #cross-validation(minimize overfitting)
                         number = 5,             #5 fold CV
                         verboseIter = TRUE)


library(xgboost)

model_gbm <- train(Renewal ~ .,            #response var - Renewal
                   data = Greyson_train,   #data - train set
                   method = "xgbTree",     #predictive algorithm - XG Boosting
                   trControl = fitControl, #5 Fold CV
                   verbose = TRUE)
model_gbm

plot(model_gbm)

plot(varImp(model_gbm,scale=F))


##Tuning Parameters & Cross Validation-----------------------------------------

#Set up tuning parameter selection and CV for the training set
xgb_grid <- expand.grid(
  nrounds = c(50,200),
  eta = c(0.025, 0.05, 0.1),
  max_depth = c(2, 3, 4, 5),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1)


# Train model with preprocessing & cv
model_rf_tune <- train(Renewal~., 
                       data = Greyson_train,   #data - train set
                       method = "xgbTree",     #Boosting
                       trControl =fitControl,  #CV
                       tuneGrid = xgb_grid,    #provide a grid of parameters
                       verbose = TRUE)

plot(model_rf_tune)



##Get predictions using Testing Set Data---------------------------------------
Greyson_prob <- predict(model_gbm, newdata = Greyson_test, type = 'prob') #predict testing set
Greyson_prob

pred <- prediction(Greyson_prob[,2], Greyson_test$Renewal)

perf <- performance(pred, 'tpr', 'fpr')

plot(perf, colorize = TRUE) #Plot ROC curve

AUC <- unlist(slot(performance(pred, 'auc'), 'y.values')) #AUC
AUC

