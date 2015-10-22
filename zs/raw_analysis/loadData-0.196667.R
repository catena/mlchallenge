library(caret)
library(impute)
library(mice)

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

customers <- readTrainData()
dim(customers)

# split data
inTrain <- createDataPartition(customers$Labels, p = 0.7, list = F)
training <- customers[inTrain, ]
testing <- customers[-inTrain, ]
table(training$Labels)

# factorize >30% NA vars
naVars <- names(training)[colMeans(!is.na(training)) < 0.7]
training[, naVars] <- is.na(training[, naVars]) * 1
testing[, naVars] <- is.na(testing[, naVars]) * 1

# near zero variance predictors
tt <- training[, !names(training) %in% c("Customer.ID", "Labels")]
uniqueCount <- sapply(tt, function(x) length(unique(x)))
zeroVars <- names(tt)[uniqueCount == 1]
training[, zeroVars] <- testing[, zeroVars] <- list(NULL)

# remove 100% correlated vars
tt <- training[, !names(training) %in% c("Customer.ID", "Labels")]
tt.cor <- cor(tt, use = "pairwise.complete.obs")
tt.cor[lower.tri(tt.cor, diag = TRUE)] <- NA
g <- which(tt.cor == 1, arr.ind = T)
eqVars <- unique(colnames(tt.cor)[g[, 2]])
training[, eqVars] <- testing[, eqVars] <- list(NULL)

# sqrt transformation
predVars <- setdiff(names(training), c("Customer.ID", "Labels", naVars))
countVars <- predVars[sapply(training[, predVars], function(x) {
    min(x, na.rm = T) >= 0 & class(x) == "integer"
})]
training[, countVars] <- sqrt(training[, countVars])
testing[, countVars] <- sqrt(testing[, countVars])

# continuous vars transformation
preProc <- preProcess(training[, predVars], 
                      method = c("center", "scale"))
training[, predVars] <- predict(preProc, training[, predVars])
testing[, predVars] <- predict(preProc, testing[, predVars])

write.csv(training, 'data/processed.csv', quote = F, row.names = F)

dim(training)
table(training$Labels)

training$Labels <- factor(training$Labels, levels = 1:0, 
                          labels = c("yes", "no"))
testing$Labels <- factor(testing$Labels, levels = 1:0, 
                         labels = c("yes", "no"))
training$Customer.ID <- NULL
testing$Customer.ID <- NULL

training <- downSample(training, training$Labels); training$Class <- NULL

# training <- complete(mice(training))
# testing <- complete(mice(testing))

# train model
cctrl <- trainControl(method = "cv", number = 5, 
                      classProbs = TRUE, summaryFunction = twoClassSummary,
                      verboseIter = TRUE)
modelFit <- train(Labels ~ ., data = training, method = "gbm", 
                  trControl = cctrl, metric = "ROC")
testing.pred <- predict(modelFit, testing, na.action = na.pass)
print(confusionMatrix(testing.pred, testing$Labels))

f1 <- function(p, q) 2*p*q / (p + q)
oos.f1 <- f1(sensitivity(testing.pred, testing$Labels), 
             posPredValue(testing.pred, testing$Labels))
print(paste("f1 score: ", oos.f1))




