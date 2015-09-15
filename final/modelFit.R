library(caret)

readTrainData <- function() {
    customers <- read.csv("data/Training.csv", quote = "", stringsAsFactors = F, 
                          comment.char = "")
    customers
}

readTestData <- function() {
    customers <- read.csv("data/Testing.csv", quote = "", stringsAsFactors = F, 
                          comment.char = "")
    customers
}

## split into training and test set
splitData <- function(customers, p = 0.7) {
    inTrain <- createDataPartition(customers$Churn, p = p, list = FALSE)
    training <- customers[inTrain, ]
    testing <- customers[-inTrain, ]
    list(training = training, testing = testing)
}

calcFeatureParams <- function(customers) {
    params <- list(NULL)
    params
}

extractFeatures <- function(customers, params) {
    ## modify covariates
    customers$Churn <- factor(customers$Churn, levels = 1:0, 
                              labels = c("Churn", "NonChurn"))
    customers$Int.l.Plan <- as.factor(customers$Int.l.Plan)
    customers$Message.Plan <- as.factor(customers$Message.Plan)
    
    ## add new covariates
    customers <- within(customers, {
        TotalOut.Calls <- Day.Calls + Eve.Calls + Intl.Calls + Night.Calls
        TotalOut.Charge <- Day.Charge + Eve.Charge + Intl.Charge + Night.Charge
        
        Intl.AvgMins <- Intl.Mins / Intl.Calls
        Night.AvgMins <- Night.Mins / Night.Calls
        
        Message.Activity <- Messages / Account.Length..Weeks.
    })
    customers[sapply(customers, is.nan)] <- 0
    
    ## remove unnecessary variables
    rmVars <- c("Phone", "State", "Exch.Code", "Area.Code",
                "Account.Length..Weeks.",
                "Day.Charge", "Eve.Charge", "Intl.Charge", "Night.Charge",
                "Day.Calls", "Eve.Calls", "Day.Mins", "Eve.Mins")
    customers[, rmVars] <- list(NULL)
    customers
}

build.rf <- function(training) {
    cctrl <- trainControl(method = "repeatedcv", number = 2, repeats =  5,
                          classProbs = TRUE, summaryFunction = twoClassSummary)
    modelFit.rf <- train(Churn ~ ., data = training, method = "rf", 
                         trControl = cctrl, metric = "ROC", importance = TRUE, 
                         ntree = 100, replace = TRUE)
    modelFit.rf
}

build.treebag <- function(training) {
    training <- upSample(training, training$Churn, yname = "Churn")
    cctrl <- trainControl(method = "none")
    modelFit.treebag <- train(Churn ~ ., data = training, method = "treebag", 
                            trControl = cctrl, preProc = c("center", "scale"),
                            nbagg = 10)
    modelFit.treebag
}

build.xgboost <- function(training) {
    training <- upSample(training, training$Churn, yname = "Churn")
    cctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                          summaryFunction = twoClassSummary)
    modelFit.xgboost <- train(Churn ~ ., data = training, method = "xgbTree",
                          trControl = cctrl, metric = "ROC",
                          preProc = c("center", "scale"))
    modelFit.xgboost
}

build.ensemble <- function(training) {
    cctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, 
                          summaryFunction = twoClassSummary)
    modelFit.stack <- train(Churn ~ ., data = training, method = "nnet", 
                            trControl = cctrl, metric = "ROC")
    modelFit.stack
}

extractEnsembleFeatures <- function(customers, models) {
    x <- lapply(models, function(m) {
        predict(m, customers)
    })
    x <- do.call(cbind.data.frame, x)
    cbind(x, Churn = customers$Churn)
}

createSingleModelData <- function() {
    mydata <- splitData(readTrainData())
    training <- mydata$training; testing <- mydata$testing
    params <- calcFeatureParams(training)
    training <- extractFeatures(training, params)
    testing <- extractFeatures(testing, params)
    list(training = training, testing = testing)
}

createFinalModelData <- function() {
    training <- readTrainData(); testing <- readTestData()
    params <- calcFeatureParams(training)
    training <- extractFeatures(training, params)
    testing <- extractFeatures(testing, params)
    list(training = training, testing = testing)
}

stackEnsembleData <- function(createData) {
    function() {
        mydata <- createData()
        build.Models <- list(rf = build.rf, 
                             # adaboost = build.adaboost,
                             # c50 = build.c50, 
                             treebag = build.treebag,
                             xgboost = build.xgboost)
        models <- lapply(build.Models, function(g) g(mydata$training))
        training.stack <- extractEnsembleFeatures(mydata$training, models)
        testing.stack <- extractEnsembleFeatures(mydata$testing, models)
        list(training = training.stack, testing = testing.stack)
    }
}

config.singleModel <- list(createData = createSingleModelData,
                           build = build.xgboost, p = 0.7)
config.ensemble <- list(createData = stackEnsembleData(createSingleModelData),
                        build = build.ensemble, p = 0.7)
config.final <- list(createData = stackEnsembleData(createFinalModelData),
                     build = build.ensemble)

testModel <- function(config) {
    mydata <- config$createData()
    training <- mydata$training; testing <- mydata$testing
    modelFit <- config$build(training)
    print(confusionMatrix(predict(modelFit, training), training$Churn))
    print(confusionMatrix(predict(modelFit, testing), testing$Churn))
}

runFinalModel <- function(config) {
    mydata <- config$createData()
    training <- mydata$training; testing <- mydata$testing
    modelFit <- config$build(training)
    predictions <- predict(modelFit, testing)
    testing.orig <- readTestData()
    solution <- data.frame(Area.Code = testing.orig$Area.Code, 
                           Phone = testing.orig$Phone,
                           Churn = 2 - as.numeric(predictions))
    write.csv(solution, file = "data/solution.csv", row.names = F, quote = F)
    print("solution written")
}

# testModel(config.ensemble)
runFinalModel(config.final)

