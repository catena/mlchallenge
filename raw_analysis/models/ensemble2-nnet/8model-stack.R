library(caret)
library(pROC)

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
    
    customers <- addFeatures(customers)
    
    # find highly correlated pairs
    x <- subset(customers, select = -c(Churn, Phone, State))
    highlyCor <- findCorrelation(cor(x), cutoff = 0.99)
    
    params <- list(borutaVars = c("Phone", "State"),
                   highlyCorVars = names(x)[highlyCor])
    params
}

addFeatures <- function(customers) {
    
    customers <- within(customers, {
        TotalOut.Mins <- Day.Mins + Eve.Mins + Intl.Mins + Night.Mins
        TotalOut.Calls <- Day.Calls + Eve.Calls + Intl.Calls + Night.Calls
        TotalOut.Charge <- Day.Charge + Eve.Charge + Intl.Charge + Night.Charge
        
        Day.PropMins <- Day.Mins / TotalOut.Mins
        Eve.PropMins <- Eve.Mins / TotalOut.Mins
        Intl.PropMins <- Intl.Mins / TotalOut.Mins
        Night.PropMins <- Night.Mins / TotalOut.Mins
        
        Day.PropCharge <- Day.Charge / TotalOut.Charge
        Eve.PropCharge <- Eve.Charge / TotalOut.Charge
        Intl.PropCharge <- Intl.Charge / TotalOut.Charge
        Night.PropCharge <- Night.Charge / TotalOut.Charge
        
        Day.PropCalls <- Day.Calls / TotalOut.Calls
        Eve.PropCalls <- Eve.Calls / TotalOut.Calls
        Intl.PropCalls <- Intl.Calls / TotalOut.Calls
        Night.PropCalls <- Night.Calls / TotalOut.Calls
        
        Day.AvgMinsPerCall <- Day.Mins / Day.Calls
        Eve.AvgMinsPerCall <- Eve.Mins / Eve.Calls
        Intl.AvgMinsPerCall <- Intl.Mins / Intl.Calls
        Night.AvgMinsPerCall <- Night.Mins / Night.Calls
        TotalOut.AvgMinsPerCall <- TotalOut.Mins / TotalOut.Calls
        
        Day.AvgChargePerCall <- Day.Charge / Day.Calls
        Eve.AvgChargePerCall <- Eve.Charge / Eve.Calls
        Intl.AvgChargePerCall <- Intl.Charge / Intl.Calls
        Night.AvgChargePerCall <- Night.Charge / Night.Calls
        TotalOut.AvgChargePerCall <- TotalOut.Charge / TotalOut.Calls
        
        MessagesPerWeek <- Messages / Account.Length..Weeks.
        
        Day.MinsPerWeek <- Day.Mins / Account.Length..Weeks.
        Eve.MinsPerWeek <- Eve.Mins / Account.Length..Weeks.
        Intl.MinsPerWeek <- Intl.Mins / Account.Length..Weeks.
        Night.MinsPerWeek <- Night.Mins / Account.Length..Weeks.
        Total.MinsPerWeek <- TotalOut.Mins / Account.Length..Weeks.
        
        Day.CallsPerWeek <- Day.Calls / Account.Length..Weeks.
        Eve.CallsPerWeek <- Eve.Calls / Account.Length..Weeks.
        Intl.CallsPerWeek <- Intl.Calls / Account.Length..Weeks.
        Night.CallsPerWeek <- Night.Calls / Account.Length..Weeks.
        Total.CallsPerWeek <- TotalOut.Calls / Account.Length..Weeks.
        
        Day.ChargePerWeek <- Day.Charge / Account.Length..Weeks.
        Eve.ChargePerWeek <- Eve.Charge / Account.Length..Weeks.
        Intl.ChargePerWeek <- Intl.Charge / Account.Length..Weeks.
        Night.ChargePerWeek <- Night.Charge / Account.Length..Weeks.
        Total.ChargePerWeek <- TotalOut.Charge / Account.Length..Weeks.
    })
    customers[sapply(customers, is.nan)] <- 0
    
    customers
}

extractFeatures <- function(customers, params) {
    
    ## modify covariates
    customers$Churn <- factor(customers$Churn, levels = 1:0, 
                              labels = c("Churn", "NonChurn"))
    customers$Int.l.Plan <- as.factor(customers$Int.l.Plan)
    customers$Message.Plan <- as.factor(customers$Message.Plan)    
    
    # add new covariates
    customers <- addFeatures(customers)
    
    ## remove unnecessary variables (by boruta results & correlation analysis)
    rmVars <- c(params$borutaVars, params$highlyCorVars)
    customers[, rmVars] <- list(NULL)

    customers
}

twoClassSummary <- function (data, lev = NULL, model = NULL) {
    rocObject <- try(roc(data$obs, data[, lev[1]]), silent = TRUE)
    rocAUC <- if (class(rocObject)[1] == "try-error") 
        NA
    else rocObject$auc
    sens <- sensitivity(data[, "pred"], data[, "obs"], lev[1])
    ppv <- posPredValue(data[, "pred"], data[, "obs"], lev[2])
    f1 <- 2 * sens * ppv / (sens + ppv)
    out <- c(ROC = rocAUC, Sens = sens, PPV = ppv, F1 = f1)
    out
}

build.ada <- function(training) {
    training <- upSample(training, training$Churn); training$Class <- NULL
    grid <- expand.grid(mfinal = 1:5 * 20, maxdepth = 1:5)
    cctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                          summaryFunction = twoClassSummary,
                          verboseIter = DEBUG)
    modelFit.ada <- train(Churn ~ ., data = training, method = "AdaBag", 
                          trControl = cctrl, tuneGrid = grid, metric = "F1",
                          preProc = c("center", "scale"))
    modelFit.ada
}

build.ada.raw <- function(training) {
    grid <- expand.grid(mfinal = 1:5 * 20, maxdepth = 1:5)
    cctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                          summaryFunction = twoClassSummary,
                          verboseIter = DEBUG)
    modelFit.ada <- train(Churn ~ ., data = training, method = "AdaBag", 
                          trControl = cctrl, tuneGrid = grid, metric = "F1",
                          preProc = c("center", "scale"))
    modelFit.ada
}

build.rf <- function(training) {
    training <- upSample(training, training$Churn); training$Class <- NULL
    cctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5,
                          classProbs = TRUE, summaryFunction = twoClassSummary,
                          verboseIter = DEBUG)
    modelFit.rf <- train(Churn ~ ., data = training, method = "rf", 
                         trControl = cctrl, metric = "F1")
    modelFit.rf
}

build.rf.raw <- function(training) {
    cctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5,
                          classProbs = TRUE, summaryFunction = twoClassSummary,
                          verboseIter = DEBUG)
    modelFit.rf <- train(Churn ~ ., data = training, method = "rf", 
                         trControl = cctrl, metric = "F1")
    modelFit.rf
}

build.treebag <- function(training) {
    training <- upSample(training, training$Churn); training$Class <- NULL
    cctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                          summaryFunction = twoClassSummary,
                          verboseIter = DEBUG)
    modelFit.treebag <- train(Churn ~ ., data = training, method = "treebag", 
                            trControl = cctrl, preProc = c("center", "scale"),
                            nbagg = 20)
    modelFit.treebag
}

build.treebag.raw <- function(training) {
    cctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                          summaryFunction = twoClassSummary,
                          verboseIter = DEBUG)
    modelFit.treebag <- train(Churn ~ ., data = training, method = "treebag", 
                              trControl = cctrl, preProc = c("center", "scale"),
                              nbagg = 20)
    modelFit.treebag
}

build.xgboost <- function(training) {
    training <- upSample(training, training$Churn); training$Class <- NULL
    cctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5,
                          classProbs = TRUE, summaryFunction = twoClassSummary,
                          verboseIter = DEBUG)
    modelFit.xgboost <- train(Churn ~ ., data = training, method = "xgbTree",
                          trControl = cctrl, metric = "F1",
                          preProc = c("center", "scale"))
    modelFit.xgboost
}

build.xgboost.raw <- function(training) {
    cctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5,
                          classProbs = TRUE, summaryFunction = twoClassSummary,
                          verboseIter = DEBUG)
    modelFit.xgboost <- train(Churn ~ ., data = training, method = "xgbTree",
                              trControl = cctrl, metric = "F1",
                              preProc = c("center", "scale"))
    modelFit.xgboost
}

build.c50 <- function(training) {
    training <- upSample(training, training$Churn); training$Class <- NULL
    grid <- expand.grid(model = "tree", winnow = FALSE, 
                        cost = 1:3, trials = c(1, 10, 20))
    cctrl <- trainControl(method = "cv", number = 10, verboseIter = DEBUG)
    modelFit.c50 <- train(Churn ~ ., data = training, method = "C5.0Cost",
                          trControl = cctrl, tuneGrid = grid,
                          preProc = c("center", "scale"))
    modelFit.c50
}

build.c50.raw <- function(training) {
    grid <- expand.grid(model = "tree", winnow = FALSE, trials = 1:10 * 10)
    cctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                          summaryFunction = twoClassSummary, 
                          verboseIter = DEBUG)
    modelFit.c50 <- train(Churn ~ ., data = training, method = "C5.0",
                          trControl = cctrl, tuneGrid = grid, metric = "F1",
                          preProc = c("center", "scale"))
    modelFit.c50
}

build.ensemble <- function(training) {
    training <- upSample(training, training$Churn); training$Class <- NULL
    cctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                          summaryFunction = twoClassSummary, 
                          verboseIter = DEBUG)
    modelFit.stack <- train(Churn ~ ., data = training, method = "nnet", 
                            trControl = cctrl, metric = "F1", trace = FALSE)
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
        build.Models <- list(ada = build.ada,
                             ada.raw = build.ada.raw,
                             rf = build.rf,
                             rf.raw = build.rf.raw,
                             c50 = build.c50,
                             c50.raw = build.c50.raw,
                             treebag = build.treebag,
                             treebag.raw = build.treebag.raw,
                             xgboost = build.xgboost,
                             xgboost.raw = build.xgboost.raw)
        models <- lapply(build.Models, function(g) g(mydata$training))
        training.stack <- extractEnsembleFeatures(mydata$training, models)
        testing.stack <- extractEnsembleFeatures(mydata$testing, models)
        list(training = training.stack, testing = testing.stack)
    }
}

config.singleModel <- list(createData = createSingleModelData,
                           build = build.ada)
config.ensemble <- list(createData = stackEnsembleData(createSingleModelData),
                        build = build.ensemble)
config.singleFinal <- list(createData = createFinalModelData,
                           build = build.rf)
config.final <- list(createData = stackEnsembleData(createFinalModelData),
                     build = build.ensemble)

testModel <- function(config) {
    mydata <- config$createData()
    training <- mydata$training; testing <- mydata$testing
    modelFit <- config$build(training)
    print(confusionMatrix(predict(modelFit, training), training$Churn))
    print(confusionMatrix(predict(modelFit, testing), testing$Churn))
}

compareModels <- function() {
    config <- config.ensemble
    mydata <- config$createData()
    training <- mydata$training; testing <- mydata$testing
    t <- as.data.frame(sapply(testing, as.numeric))
    print(abs(cor(t)))
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

DEBUG <- TRUE
# testModel(config.singleModel)
# compareModels()
runFinalModel(config.final)




