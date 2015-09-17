library(caret)
library(ggplot2)

## read dataset
customers <- read.csv("data/Training.csv", quote = "", stringsAsFactors = F, comment.char = "")

## modify covariates
customers$Churn <- as.factor(1 - customers$Churn)
customers$State <- as.factor(customers$State)
customers$Area.Code <- as.factor(customers$Area.Code)
customers$Int.l.Plan <- as.factor(customers$Int.l.Plan)
customers$Exch.Code <- substr(customers$Phone, 1, 3)

## remove unnecessary variables
customers$Phone <- NULL

## train model
set.seed(1010001100)
inTrain <- createDataPartition(customers$Churn, p = 0.75, list = FALSE)
training <- customers[inTrain, ]
testing <- customers[-inTrain, ]
dim(training)

modelFit <- train(Churn ~ ., data = training, method = "glm", trControl = trainControl(method = "none"))

confusionMatrix(predict(modelFit, training), training$Churn)
confusionMatrix(predict(modelFit, testing), testing$Churn)





