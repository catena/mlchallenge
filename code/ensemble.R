library(caret)
library(ggplot2)

source("code/adaboost.R")
source("code/c50.R")
source("code/rf.R")
source("code/svm.R")

set.seed(1010001105)
source("code/loadData.R")
models <- list(rf = modelFit.rf, adaboost = modelFit.adaboost,
               c50 = modelFit.c50, svm = modelFit.svm)
mydata <- createEnsembleData(models)
training <- mydata$training
testing <- mydata$testing
params <- mydata$params

## use gam or nnet
cctrl <- trainControl(method = "cv", number = 5, verboseIter = T,
                      classProbs = TRUE, summaryFunction = twoClassSummary)
modelFit.stack <- train(Churn ~ ., data = training, method = "gam", 
                        trControl = cctrl, metric = "ROC")

confusionMatrix(predict(modelFit.stack, training), training$Churn)
confusionMatrix(predict(modelFit.stack, testing), testing$Churn)
