library(caret)
library(parallel)

set.seed(10101019)

readTrainData <- function() {
    customers <- read.csv("data/Training.csv", quote = "", stringsAsFactors = F, 
                          comment.char = "", na.strings = "")
    customers <- customers[!is.na(customers$Customer.ID), ]
    customers
}

readTestData <- function() {
    customers <- read.csv("data/Testing.csv", quote = "", stringsAsFactors = F, 
                          comment.char = "")
    customers
}

# split into training and test set
splitData <- function(customers, p = 0.7) {
    inTrain <- createDataPartition(customers$Labels, p = p, list = FALSE)
    training <- customers[inTrain, ]
    testing <- customers[-inTrain, ]
    list(training = training, testing = testing)
}

extractFeatures <- function(training, testing) {
    training$Customer.ID <- testing$Customer.ID <- NULL
    
    training$Labels <- factor(training$Labels, levels = 1:0, 
                              labels = c("yes", "no"))
    testing$Labels <- factor(testing$Labels, levels = 1:0, 
                             labels = c("yes", "no"))
    
    # unique vars
    tt <- subset(training, select = -Labels)
    uniqueCount <- lapply(tt, function(x) length(unique(x)))
    uniqueVars <- names(tt)[uniqueCount == 1]
    training[, uniqueVars] <- testing[, uniqueVars] <- list(NULL)
    
    # zero variance predictors
    tt <- subset(training, select = -Labels)
    zeroVars <- names(tt)[apply(tt, 2, sd, na.rm = T) == 0]
    training[, zeroVars] <- 1 - is.na(training[, zeroVars])
    testing[, zeroVars] <- 1 - is.na(testing[, zeroVars])
    
    # remove 100% correlated vars
    tt <- subset(training, select = -Labels)
    tt.cor <- abs(cor(tt, use = "pairwise.complete.obs"))
    tt.cor[lower.tri(tt.cor, diag = TRUE)] <- NA
    g <- which(tt.cor == 1, arr.ind = T)
    eqVars <- unique(colnames(tt.cor)[g[, 2]])
    training[, eqVars] <- testing[, eqVars] <- list(NULL)
    
    # skew adjust
    predVars <- setdiff(names(training), c("Customer.ID", "Labels", zeroVars))
    pseudoLog10 <- function(x) asinh(x/2) / log(10)
    # logMod <- function(x) sign(x) * log10(abs(x) + 1)
    training[, predVars] <- pseudoLog10(training[, predVars])
    testing[, predVars] <- pseudoLog10(testing[, predVars])

    # write.csv(training, 'data/processed.csv', quote = F, row.names = F, na = "")
    list(training = training, testing = testing)
}

overSampledBuild <- function(build) {
    function(training) {
        training <- upSample(training, training$Labels); training$Class <- NULL
        build(training)
    }
}

downSampledBuild <- function(build) {
    function(training) {
        training <- downSample(training, training$Labels); training$Class <- NULL
        build(training)
    }
}

build.gbm <- function(training) {
    # grid <- expand.grid(interaction.depth = 1:5, n.trees = 1:5 * 50,
    #                     shrinkage = 0.1, n.minobsinnode = 10)
    # grid <- expand.grid(interaction.depth = c(5, 10), n.trees = c(250, 500),
    #                     shrinkage = 0.1, n.minobsinnode = 10)
    # n.trees = 500, interaction.depth = 10, shrinkage = 0.1, n.minobsinnode = 10
    grid <- expand.grid(interaction.depth = c(10, 20), n.trees = c(500, 1000),
                        shrinkage = 0.1, n.minobsinnode = 10)
    cctrl <- trainControl(method = "cv", number = 2, 
                          classProbs = TRUE, summaryFunction = twoClassSummary,
                          verboseIter = TRUE)
    modelFit <- train(Labels ~ ., data = training, method = "gbm", 
                      trControl = cctrl, metric = "ROC", tuneGrid = grid,
                      na.action = na.pass)    
    modelFit
}

build.c50 <- function(training) {
    # grid <- expand.grid(model = c("tree", "rules"), winnow = c(FALSE, TRUE), 
    #                     cost = 1:3, trials = c(1, 10, 20))
    # trials = 20, model = rules, winnow = FALSE, cost = 1
    grid <- expand.grid(model = "rules", winnow = FALSE, 
                        trials = c(20, 50, 100))
    cctrl <- trainControl(method = "cv", number = 2, 
                          classProbs = TRUE, summaryFunction = twoClassSummary,
                          verboseIter = TRUE)
    modelFit <- train(Labels ~ ., data = training, method = "C5.0", 
                      trControl = cctrl, tuneGrid = grid, metric = "ROC",
                      na.action = na.pass)    
    modelFit
}

build.ada <- function(training) {
    # grid <- expand.grid(mfinal = c(50, 100, 500, 1000), maxdepth = 1,
    #                     coeflearn = "Breiman")
    # mfinal = 1000, maxdepth = 1, coeflearn = Breiman
    grid <- expand.grid(mfinal = c(1000, 2000), maxdepth = 1, 
                        coeflearn = "Breiman")
    cctrl <- trainControl(method = "cv", number = 2, 
                          classProbs = TRUE, summaryFunction = twoClassSummary,
                          verboseIter = TRUE)
    modelFit <- train(Labels ~ ., data = training, method = "AdaBoost.M1", 
                      trControl = cctrl, tuneGrid = grid, metric = "ROC",
                      na.action = na.pass)
    modelFit
}

build.ensemble <- function(training) {
    training <- upSample(training, training$Labels); training$Class <- NULL
    cctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                          summaryFunction = twoClassSummary)
    modelFit.stack <- train(Labels ~ ., data = training, method = "nnet", 
                            trControl = cctrl, metric = "ROC", trace = FALSE)
    modelFit.stack
}

extractEnsembleFeatures <- function(customers, models) {
    x <- lapply(models, function(m) {
        predict(m, customers, na.action = na.pass)
    })
    x <- do.call(cbind.data.frame, x)
    cbind(x, Labels = customers$Labels)
}

createSingleModelData <- function() {
    rawData <- splitData(readTrainData())
    modelData <- extractFeatures(rawData$training, rawData$testing)
    list(training = modelData$training, testing = modelData$testing)
}

createFinalModelData <- function() {
    training <- readTrainData(); testing <- readTestData()
    modelData <- extractFeatures(training, testing)
    list(training = modelData$training, testing = modelData$testing)
}

stackEnsembleData <- function(createData) {
    function() {
        modelData <- createData()
        build.Models <- list(c50 = downSampledBuild(build.c50),
                             gbm = overSampledBuild(build.gbm),
                             ada = downSampledBuild(build.ada))
        models <- mclapply(build.Models, function(g) g(modelData$training),
                           mc.cores = 4)
        training.stack <- extractEnsembleFeatures(modelData$training, models)
        testing.stack <- extractEnsembleFeatures(modelData$testing, models)
        list(training = training.stack, testing = testing.stack)
    }
}

testModel <- function(config) {
    modelData <- config$createData()
    training <- modelData$training; testing <- modelData$testing
    modelFit <- config$build(training)
    training.pred <- predict(modelFit, training, na.action = na.pass)
    testing.pred <- predict(modelFit, testing, na.action = na.pass)
    print(confusionMatrix(training.pred, training$Labels))
    print(confusionMatrix(testing.pred, testing$Labels))
    f1 <- function(p, q) 2 * p * q / (p + q)
    oos.f1 <- f1(sensitivity(testing.pred, testing$Labels), 
                 posPredValue(testing.pred, testing$Labels))
    print(paste("OOS F1 score: ", oos.f1))
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
    solution <- data.frame(Customer.ID = testing.orig$Customer.ID, 
                           Labels = 2 - as.numeric(Labels))
    write.table(solution, file = "data/solution.csv", sep = ",", quote = F,
                row.names = F, col.names = F)
    print("solution written")
}

config.singleModel <- list(createData = createSingleModelData,
                           build = downSampledBuild(build.gbm))
config.ensemble <- list(createData = stackEnsembleData(createSingleModelData),
                        build = build.ensemble)
config.singleFinal <- list(createData = createFinalModelData,
                           build = downSampledBuild(build.gbm))
config.final <- list(createData = stackEnsembleData(createFinalModelData),
                     build = build.ensemble)

testModel(config.singleFinal)

