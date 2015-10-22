library(caret)
library(ggplot2)

set.seed(1010001104)
source("raw_analysis/models/loadData.R")
mydata <- createSingleModelData()
training <- mydata$training
testing <- mydata$testing

# sigma = 0.03, C = 4, Weight = 2
# sigma <- sigest(Churn ~ ., training, frac = 1)
grid <- expand.grid(C = 2 * 1:8, Weight = 1:4, sigma = 0.03)
cctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
modelFit.svm <- train(Churn ~ ., data = training, method = "svmRadialWeights", 
                  trControl = cctrl, preProc = c("center", "scale"),
                  tuneGrid = grid, metric = "Kappa")

confusionMatrix(predict(modelFit.svm, training), training$Churn)
confusionMatrix(predict(modelFit.svm, testing), testing$Churn)

