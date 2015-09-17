library(caret)

## read dataset
readData <- function(fname) {
    customers <- read.csv(fname, quote = "", stringsAsFactors = F, 
                          comment.char = "")
    customers
}

## split into training and test set
splitData <- function(customers) {
    inTrain <- createDataPartition(customers$Churn, p = 0.7, list = FALSE)
    training <- customers[inTrain, ]
    testing <- customers[-inTrain, ]
    list(training = training, testing = testing)
}

calcFeatureParams <- function(customers) {
    params <- list(NULL)
    params
}

extractFeatures <- function(customers, params, train = TRUE) {
    ## modify covariates
    if (train) {
        customers$Churn <- factor(customers$Churn, levels = 1:0, 
                                  labels = c("Churn", "NonChurn"))
    }
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
    if (train) {
        rmVars <- c("Phone", "State", "Exch.Code", "Area.Code",
                    "Account.Length..Weeks.",
                    "Day.Charge", "Eve.Charge", "Intl.Charge", "Night.Charge",
                    "Day.Calls", "Eve.Calls")
        customers[, rmVars] <- list(NULL)
    }    
    customers
}

createSingleModelData <- function() {
    fname <- "data/Training.csv"
    customers <- readData(fname)
    mydata <- splitData(customers)
    training <- mydata$training
    testing <- mydata$testing
    params <- calcFeatureParams(training)
    training <- extractFeatures(training, params)
    testing <- extractFeatures(testing, params)
    list(training = training, testing = testing, params = params)
}

createEnsembleData <- function(models) {
    mydata <- createSingleModelData()
    training.stack <- extractEnsembleFeatures(mydata$training, models)
    testing.stack <- extractEnsembleFeatures(mydata$testing, models)
    list(training = training.stack, testing = testing.stack, 
         params = mydata$params)
}

extractEnsembleFeatures <- function(customers, models) {
    x <- lapply(models, function(m) {
        predict(m, customers)
    })
    x <- do.call(cbind.data.frame, x)
    cbind(x, Churn = customers$Churn)
}

readProblemData <- function(params) {
    customers <- readData("data/Testing.csv")
    customers <- extractFeatures(customers, params, train = FALSE)
}



