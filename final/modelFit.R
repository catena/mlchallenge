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
        
        Day.PropMins <- Day.Mins / TotalOut.Mins
        Eve.PropMins <- Eve.Mins / TotalOut.Mins
        Intl.PropMins <- Intl.Mins / TotalOut.Mins
        Night.PropMins <- Night.Mins / TotalOut.Mins
        
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
    
    # add new covariates
    customers <- addFeatures(customers)
    
    ## remove unnecessary variables (by boruta results & correlation analysis)
    rmVars <- c(params$borutaVars, params$highlyCorVars)
    customers[, rmVars] <- list(NULL)

    ## modify covariates
    customers$Churn <- factor(customers$Churn, levels = 1:0, 
                              labels = c("Churn", "NonChurn"))
    if ("Int.l.Plan" %in% names(customers))
        customers$Int.l.Plan <- as.factor(customers$Int.l.Plan)
    if ("Message.Plan" %in% names(customers))
        customers$Message.Plan <- as.factor(customers$Message.Plan)    
    
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

build.rf <- function(training) {
    training <- upSample(training, training$Churn, yname = "Churn")
    cctrl <- trainControl(method = "repeatedcv", number = 2, repeats = 5,
                          classProbs = TRUE, summaryFunction = twoClassSummary)
    modelFit.rf <- train(Churn ~ ., data = training, method = "rf", 
                         trControl = cctrl, metric = "F1", importance = TRUE, 
                         ntree = 100)
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
                          trControl = cctrl, metric = "F1",
                          preProc = c("center", "scale"))
    modelFit.xgboost
}

build.ensemble <- function(training) {
    cctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, 
                          summaryFunction = twoClassSummary)
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
        build.Models <- list(rf = build.rf, 
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

