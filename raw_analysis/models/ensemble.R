library(caret)
library(ggplot2)

source("raw_analysis/models/adaboost.R")
source("raw_analysis/models/c50.R")
source("raw_analysis/models/rf.R")
source("raw_analysis/models/svm.R")

set.seed(1010001105)
source("raw_analysis/models/loadData.R")
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
