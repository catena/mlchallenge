library(caret)
library(impute)

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

# percent missing allowed in cols
x <- which(apply(!is.na(training), 2, mean) > 0.5)
training <- training[, x]
testing <- testing[, x]

# percent missing allowed in rows
x <- which(apply(!is.na(training), 1, mean) > 0.5)
training <- training[x, ]

dim(training)
table(training$Labels)

# highly correlated predictors
tt <- as.matrix(training[, !names(training) %in% c("Customer.ID", "Label")])
tt.imputed <- impute.knn(tt, k = 10, rng.seed = 10101019)$data
x <- findCorrelation(cor(tt.imputed), cutoff = 0.9, names = TRUE)
training[, x] <- list(NULL)
testing[, x] <- list(NULL)

# near zero variance predictors
nsv <- nearZeroVar(tt.imputed, saveMetrics = TRUE)
x <- colnames(tt.imputed)[nsv$nzv]
training[, x] <- list(NULL)
testing[, x] <- list(NULL)

dim(training)
table(training$Labels)
# write.csv(training, file = "data/processed.csv", quote = F, row.names = F)

training$Labels <- factor(training$Labels, levels = 1:0, 
                          labels = c("yes", "no"))
testing$Labels <- factor(testing$Labels, levels = 1:0, 
                         labels = c("yes", "no"))
training$Customer.ID <- NULL
testing$Customer.ID <- NULL
training <- downSample(training, training$Labels); training$Class <- NULL

cctrl <- trainControl(method = "cv", number = 5,
                      classProbs = TRUE, summaryFunction = twoClassSummary,
                      verboseIter = TRUE)
modelFit <- train(Labels ~ ., data = training, method = "C5.0", 
                     trControl = cctrl, metric = "ROC",
                     preProcess = c("YeoJohnson", "center", "scale"))
# training.pred <- predict(modelFit, training, na.action = na.pass)
testing <- testing[complete.cases(testing), ]
testing.pred <- predict(modelFit, testing, na.action = na.pass)
# print(confusionMatrix(training.pred, training$Labels))
print(confusionMatrix(testing.pred, testing$Labels))

f1 <- function(p, q) 2*p*q / (p + q)
print(f1(sensitivity(testing.pred, testing$Labels), 
         posPredValue(testing.pred, testing$Labels)))




