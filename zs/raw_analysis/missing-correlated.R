library(RBGL)
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

g <- new("graphNEL", nodes = colnames(tt.cor), edgemode = "undirected")
apply(edges, 1, function(x) {
    g <<- addEdge(rownames(tt.cor)[x[1]], colnames(tt.cor)[x[2]], g)
})
print(connectedComp(g))
