library(caret)
library(pROC)
library(DMwR)

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
    x <- subset(customers, select = -c(Churn, Phone, State, Area.Code))
    highlyCor <- findCorrelation(cor(x), cutoff = 0.9)
    
    params <- list(borutaVars = c("Phone", "State", "Area.Code"),
                   highlyCorVars = names(x)[highlyCor])
    params
}

addFeatures <- function(customers) {
    
    customers <- within(customers, {
        TotalOut.Mins <- Day.Mins + Eve.Mins + Intl.Mins + Night.Mins
        TotalOut.Calls <- Day.Calls + Eve.Calls + Intl.Calls + Night.Calls
        TotalOut.Charge <- Day.Charge + Eve.Charge + Intl.Charge + Night.Charge
        TotalOut.AvgMinsPerCall <- TotalOut.Mins / TotalOut.Calls
        Total.MinsPerWeek <- TotalOut.Mins / Account.Length..Weeks.
        
        MessagesPerWeek <- Messages / Account.Length..Weeks.
        
        Day.PropMins <- Day.Mins / TotalOut.Mins
        Eve.PropMins <- Eve.Mins / TotalOut.Mins
        Intl.PropMins <- Intl.Mins / TotalOut.Mins
        Night.PropMins <- Night.Mins / TotalOut.Mins
        
        Day.PropCalls <- Day.Calls / TotalOut.Calls
        Eve.PropCalls <- Eve.Calls / TotalOut.Calls
        Night.PropCalls <- Night.Calls / TotalOut.Calls
        
        Day.AvgMinsPerCall <- Day.Mins / Day.Calls
        Eve.AvgMinsPerCall <- Eve.Mins / Eve.Calls
        Intl.AvgMinsPerCall <- Intl.Mins / Intl.Calls
        Night.AvgMinsPerCall <- Night.Mins / Night.Calls
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
    ppv <- posPredValue(data[, "pred"], data[, "obs"], lev[1])
    f1 <- 2 * sens * ppv / (sens + ppv)
    out <- c(ROC = rocAUC, Sens = sens, PPV = ppv, F1 = f1)
    out
}

smoteSampledBuild <- function(build) {
    function(training) {
        training <- SMOTE(Churn ~ ., data = training, k = 10,
                          perc.over = 200, perc.under = 200)
        build(training)
    }
}

overSampledBuild <- function(build) {
    function(training) {
        training <- upSample(training, training$Churn); training$Class <- NULL
        build(training)
    }
}

build.rf <- function(training) {
    cctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                          classProbs = TRUE, summaryFunction = twoClassSummary,
                          verboseIter = DEBUG)
    modelFit.rf <- train(Churn ~ ., data = training, method = "rf", 
                         trControl = cctrl, metric = "F1",
                         preProc = c("center", "scale"))
    modelFit.rf
}

build.treebag <- function(training) {
    cctrl <- trainControl(method = "none")
    modelFit.treebag <- train(Churn ~ ., data = training, method = "treebag",
                              trControl = cctrl, nbagg = 100,
                              preProc = c("center", "scale"))
    modelFit.treebag
}

build.xgboost <- function(training) {
    cctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                          classProbs = TRUE, summaryFunction = twoClassSummary,
                          verboseIter = DEBUG)
    modelFit.xgboost <- train(Churn ~ ., data = training, method = "xgbTree",
                              trControl = cctrl, metric = "F1",
                              preProc = c("center", "scale"))
    modelFit.xgboost
}

build.c50 <- function(training) {
    grid <- expand.grid(model = "tree", winnow = FALSE, 
                        cost = 1:3, trials = c(1, 10, 20))
    cctrl <- trainControl(method = "cv", number = 10, verboseIter = DEBUG)
    modelFit.c50 <- train(Churn ~ ., data = training, method = "C5.0Cost",
                          trControl = cctrl, tuneGrid = grid,
                          preProc = c("center", "scale"))
    modelFit.c50
}

source("final/adacost.R")
build.ada <- function(training) {
    grid <- expand.grid(mfinal = 1:5 * 20, maxdepth = 2:4, 
                        coeflearn = "Breiman")
    cctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, 
                          summaryFunction = twoClassSummary,
                          verboseIter = DEBUG)
    modelFit.ada <- train(Churn ~ ., data = training, method = "AdaBoost.M1", 
                          trControl = cctrl, misswt = 1.02, decay = 0.9,
                          cls = "Churn", tuneGrid = grid, metric = "F1",
                          preProc = c("center", "scale"))
    modelFit.ada
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
    modelData <- splitData(readTrainData())
    training <- modelData$training; testing <- modelData$testing
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
        modelData <- createData()
        build.Models <- list(rf.raw = build.rf,
                             ada.raw = build.ada,    # cost sensitive ada
                             c50.raw = build.c50,    # cost sensitive c50
                             treebag.raw = build.treebag,
                             rf = smoteSampledBuild(build.rf),
                             xgboost = overSampledBuild(build.xgboost),
                             treebag = smoteSampledBuild(build.treebag))
        models <- lapply(build.Models, function(g) g(modelData$training))
        training.stack <- extractEnsembleFeatures(modelData$training, models)
        testing.stack <- extractEnsembleFeatures(modelData$testing, models)
        list(training = training.stack, testing = testing.stack)
    }
}

config.singleModel <- list(createData = createSingleModelData,
                           build = build.ada)
config.ensemble <- list(createData = stackEnsembleData(createSingleModelData),
                        build = build.ensemble)
config.singleFinal <- list(createData = createFinalModelData,
                           build = build.treebag)
config.final <- list(createData = stackEnsembleData(createFinalModelData),
                     build = build.ensemble)

testModel <- function(config) {
    modelData <- config$createData()
    training <- modelData$training; testing <- modelData$testing
    modelFit <- config$build(training)
    print(confusionMatrix(predict(modelFit, training), training$Churn))
    print(confusionMatrix(predict(modelFit, testing), testing$Churn))
}

compareModels <- function() {
    config <- config.ensemble
    modelData <- config$createData()
    training <- modelData$training; testing <- modelData$testing
    t <- as.data.frame(sapply(testing, as.numeric))
    print(abs(cor(t)))
}

runFinalModel <- function(config) {
    modelData <- config$createData()
    training <- modelData$training; testing <- modelData$testing
    modelFit <- config$build(training)
    predictions <- predict(modelFit, testing)
    testing.orig <- readTestData()
    solution <- data.frame(Area.Code = testing.orig$Area.Code, 
                           Phone = testing.orig$Phone,
                           Churn = 2 - as.numeric(predictions))
    write.table(solution, file = "data/solution.csv", sep = ",", quote = F,
                row.names = F, col.names = F)
    print("solution written")
}

DEBUG <- TRUE
if(!exists("LOAD_AS_LIB") || !LOAD_AS_LIB) {
    # testModel(config.singleModel)
    runFinalModel(config.final)    
}

