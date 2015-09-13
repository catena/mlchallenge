library(caret)
library(ggplot2)

set.seed(1010001102)
source("code/loadData.R")
mydata <- createSingleModelData()
training <- mydata$training
testing <- mydata$testing

# maxdepth = 5, mfinal = 15, coeflearn = "Breiman"
grid <- expand.grid(maxdepth = 1:5, mfinal = seq(5, 30, 5),
                    coeflearn = "Breiman")
cctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, 
                      summaryFunction = twoClassSummary,
                      verboseIter = TRUE)
modelFit.adaboost <- train(Churn ~ ., data = training, method = "AdaBoost.M1", 
                  trControl = cctrl, tuneGrid = grid, metric = "ROC",
                  preProc = c("center", "scale"))

confusionMatrix(predict(modelFit.adaboost, training), training$Churn)
confusionMatrix(predict(modelFit.adaboost, testing), testing$Churn)

