
build.orf <- function(training) {
    cctrl <- trainControl(method = "cv", number = 5,
                          classProbs = TRUE, summaryFunction = twoClassSummary,
                          verboseIter = DEBUG)
    modelFit.orf <- train(Churn ~ ., data = training, method = "ORFlog",
                          trControl = cctrl, metric = "F1",
                          preProc = c("center", "scale"))
    modelFit.orf
}

build.logit <- function(training) {
    grid <- expand.grid(nIter = 1:5 * 200)
    cctrl <- trainControl(method = "cv", number = 5,
                          classProbs = TRUE, summaryFunction = twoClassSummary,
                          verboseIter = DEBUG)
    modelFit.logit <- train(Churn ~ ., data = training, method = "LogitBoost",
                            trControl = cctrl, metric = "F1", tuneGrid = grid,
                            preProc = c("center", "scale"))
    modelFit.logit
}

build.gam <- function(training) {
    grid <- expand.grid(select = FALSE, method = "GCV.Cp")
    cctrl <- trainControl(method = "none")
    modelFit.gam <- train(Churn ~ ., data = training, method = "gam",
                          trControl = cctrl, tuneGrid = grid,
                          preProc = c("center", "scale"))
    modelFit.gam
}