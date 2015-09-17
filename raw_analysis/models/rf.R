library(caret)
library(ggplot2)

set.seed(1010001101)
source("raw_analysis/models/loadData.R")
mydata <- createSingleModelData()
training <- mydata$training
testing <- mydata$testing

cctrl <- trainControl(method = "repeatedcv", number = 2, repeats =  5,
                      verboseIter = TRUE, classProbs = TRUE,
                      summaryFunction = twoClassSummary)
modelFit.rf <- train(Churn ~ ., data = training, method = "rf", trControl = cctrl, 
                     metric = "ROC", importance = TRUE, ntree = 100, replace = TRUE)

confusionMatrix(predict(modelFit.rf, training), training$Churn)
confusionMatrix(predict(modelFit.rf, testing), testing$Churn)

varImpPlot(modelFit.rf$finalModel)
