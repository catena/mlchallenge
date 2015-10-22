library(ggplot2)
library(caret)
library(gridExtra)

LOAD_AS_LIB <- TRUE
source("final/modelFit.R")
LOAD_AS_LIB <- FALSE

training <- readTrainData()
training <- addFeatures(training)
training$Churn <- factor(training$Churn, levels = 1:0, 
                          labels = c("Churn", "NonChurn"))
training$Int.l.Plan <- as.factor(training$Int.l.Plan)
training$Message.Plan <- as.factor(training$Message.Plan)

cctrl <- trainControl(method = "cv", number = 2, verboseIter = TRUE)
modelFit.rf <- train(Churn ~ ., data = createSingleModelData()$training, 
                     method = "rf", metric = "F1", trControl = cctrl, 
                     ntree = 100, preProc = c("center", "scale"),
                     importance = TRUE)
varImp(modelFit.rf, n = 10)
randomForest::varImpPlot(modelFit.rf$finalModel, cex = 0.8, n = 10,
                         main = "Variable Importance")

ggplot(training, aes(x = CustServ.Calls, fill = Churn)) +
    geom_histogram(alpha = 0.5, binwidth = 0.5, position = "identity",
                   aes(y = ..density..)) +
    scale_x_discrete(breaks = 0:10) + 
    scale_fill_manual(values = c("red", "darkgreen")) +
    xlab("Customer Service Calls") +
    ylab("Density") +
    theme_bw()

ggplot(training, aes(x = TotalOut.Charge, fill = Churn)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("red", "darkgreen")) +
    xlab("Total Outgoing Calls Cost") + ylab("Density") +
    geom_vline(xintercept = 75, color = "blue", linetype - "longdash") +
    theme_bw()

ggplot(training, aes(x = as.integer(Int.l.Plan), fill = Churn)) +
    geom_histogram(alpha = 0.5, binwidth = 1, position = "identity",
                   aes(y = ..density..)) +
    scale_x_discrete(breaks = 1:2, labels = c("Plan 1", "Plan 2")) +
    scale_fill_manual(values = c("red", "darkgreen")) +
    xlab("International Plan") + ylab("Density") +
    theme_bw()

minVars <- c("Day.Mins", "Eve.Mins", "Night.Mins", "Intl.Mins")
idVars <- c("Area.Code", "Phone", "Churn")
training.mins <- melt(training, id.vars = idVars, measure.vars = minVars,
                      value.name = "mins")
ggplot(training.mins, aes(x = mins, fill = Churn)) + 
    geom_density(alpha = 0.5) + 
    scale_fill_manual(values = c("red", "darkgreen")) +
    facet_wrap(~ variable, scales = "free") +
    xlab("Density") + ylab("Minutes") +
    theme_bw()


ggplot(training, aes(x = Day.AvgMinsPerCall, fill = Churn)) +  geom_density(alpha = 0.5) + scale_fill_manual(values = c("red", "darkgreen")) 

ggplot(training, aes(x = Day.AvgMinsPerCall, fill = Churn)) + geom_histogram(alpha = 0.5, binwidth = 0.5, position = "identity", aes(y = ..density..)) + scale_fill_manual(values = c("red", "darkgreen"))

csp <- ggplot(training, aes(x = CustServ.Calls, fill = Churn)) +
    geom_histogram(alpha = 0.5, binwidth = 0.5, position = "identity",
                   aes(y = ..density..)) +
    scale_x_discrete(breaks = 0:10) + 
    scale_fill_manual(values = c("red", "darkgreen")) +
    xlab("Customer Service Calls") +
    ylab("Density") +
    theme_bw()

tcp <- ggplot(training, aes(x = TotalOut.Charge, fill = Churn)) +
    geom_histogram(alpha = 0.5, binwidth = 5, position = "identity",
                   aes(y = ..density..)) +
    scale_fill_manual(values = c("red", "darkgreen")) +
    xlab("Total Outgoing Calls Cost") + ylab("Density") +
    theme_bw()

ilp <- ggplot(training, aes(x = as.integer(Int.l.Plan), fill = Churn)) +
    geom_histogram(alpha = 0.5, binwidth = 1, position = "identity",
                   aes(y = ..density..)) +
    scale_x_discrete(breaks = 1:2, labels = c("Plan 1", "Plan 2")) +
    scale_fill_manual(values = c("red", "darkgreen")) +
    xlab("International Plan") + ylab("Density") +
    theme_bw()

grid.arrange(csp, tcp, ilp, ncol = 3)
