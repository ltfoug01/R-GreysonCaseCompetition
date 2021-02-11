library(caret)
library(dplyr)
library(imputeMissings)

Greyson <- read.csv("Greyson.csv", header = T)


##Impute Missing Values--------------------------------------------------------
Greyson$Renewal <- as.factor(Greyson$Renewal)

Greyson<- imputeMissings::impute(Greyson,method = "median/mode")
sum(is.na(Greyson))

str(Greyson)

Greyson <- na.omit(Greyson)


##Partition the Data-----------------------------------------------------------
set.seed(4)

index <- createDataPartition(Greyson$Renewal, p = .8,list = FALSE)
Greyson_train <- Greyson[index,]
Greyson_test <- Greyson[-index,]



##Fit the Model----------------------------------------------------------------
fitControl <- trainControl(method = "cv",       #Cross Validation
                           number = 5,          #5 Fold CV
                           verboseIter = TRUE)

# Train model with preprocessing & cv
library(randomForest)
library(e1071)

model_rf <- train(Renewal ~ .,            #Response Var - Renewal
                  data = Greyson_train,   #Data - Train set
                  method = "rf",          #Algorithm - Random Forest
                  trControl =fitControl,  #CV
                  verbose = 0)
model_rf

plot(model_rf)

plot(varImp(model_rf,scale=F))  #important predictor variables



##Tune the Model---------------------------------------------------------------
rf_grid <- expand.grid(mtry = c(1,3,6,9))
rf_grid

model_rf_tune <- train(Renewal ~ .,            #Response Var - Renewal
                       data = Greyson_train,   #Data - Train set
                       method = "rf",          #random forest
                       trControl =fitControl,  #CV
                       tuneGrid = rf_grid,     #provide a grid of parameters
                       verbose = FALSE)

plot(model_rf_tune)


##Predicted Probabilities------------------------------------------------------
Greyson_prob<- predict(model_rf, Greyson_test, type = "prob")

library(ROCR)

pred = prediction(Greyson_prob[,2], Greyson_test$Renewal)
perf = performance(pred, "tpr", "fpr")

plot(perf, colorize = TRUE)  #Plot ROC

slot(performance(pred, "auc"), "y.values")[[1]] #AUC

