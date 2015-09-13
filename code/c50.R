library(caret)
library(ggplot2)

set.seed(1010001103)
source("code/loadData.R")
mydata <- createSingleModelData()
training <- mydata$training
testing <- mydata$testing

# trials = 100, model = tree, winnow = FALSE, cost = 6
grid <- expand.grid(model = "tree", winnow = FALSE, trials = 100, 
                    cost = c(1, 2 * 1:5))
cctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
modelFit.c50 <- train(Churn ~ ., data = training, method = "C5.0Cost", 
                  trControl = cctrl, preProc = c("center", "scale"),
                  tuneGrid = grid, metric = "Kappa")

confusionMatrix(predict(modelFit.c50, training), training$Churn)
confusionMatrix(predict(modelFit.c50, testing), testing$Churn)

