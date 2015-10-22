library(caret)
library(impute)
library(mice)
library(corrplot)

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

# unique vars
tt <- training[, !names(training) %in% c("Customer.ID", "Labels")]
numVars <- names(tt)
training[, numVars] <- 1 - is.na(training[, numVars])

# zero variance predictors
nsv <- nearZeroVar(training, saveMetrics = TRUE)
training[, nsv$zeroVar] <- testing[, nsv$zeroVar] <- list(NULL)

# remove 100% correlated vars
tt <- training[, !names(training) %in% c("Customer.ID", "Labels")]
tt.cor <- abs(cor(tt))
tt.cor[lower.tri(tt.cor, diag = TRUE)] <- NA
edges <- which(tt.cor == 1, arr.ind = T)
eqVars <- unique(colnames(tt.cor)[edges[, 2]])
training[, eqVars] <- testing[, eqVars] <- list(NULL)


# write.csv(training, 'data/processed.csv', quote = F, row.names = F, na = "")


dim(training)
table(training$Labels)

training$Labels <- factor(training$Labels, levels = 1:0, 
                          labels = c("yes", "no"))
testing$Labels <- factor(testing$Labels, levels = 1:0, 
                         labels = c("yes", "no"))
training$Customer.ID <- NULL
testing$Customer.ID <- NULL

training <- upSample(training, training$Labels); training$Class <- NULL

# train model
cctrl <- trainControl(method = "cv", number = 5, 
                      classProbs = TRUE, summaryFunction = twoClassSummary,
                      verboseIter = TRUE)
modelFit <- train(Labels ~ ., data = training, method = "gbm", 
                  trControl = cctrl, metric = "ROC", na.action = na.pass)
testing.pred <- predict(modelFit, testing, na.action = na.pass)
print(confusionMatrix(testing.pred, testing$Labels))

f1 <- function(p, q) 2*p*q / (p + q)
oos.f1 <- f1(sensitivity(testing.pred, testing$Labels), 
             posPredValue(testing.pred, testing$Labels))
print(paste("f1 score: ", oos.f1))



# write.table(testing[, c("Customer.ID", "Labels")], 'data/submission.csv', 
# quote = F, row.names = F, col.names = F, na = "", sep = ",")

