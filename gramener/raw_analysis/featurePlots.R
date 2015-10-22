library(caret)
library(reshape2)
library(ggplot2)

training <- read.csv("data/Training.csv", quote = "", stringsAsFactors = F, 
                     comment.char = "")
training$Int.l.Plan <- as.factor(training$Int.l.Plan)
training$Message.Plan <- as.factor(training$Message.Plan)
training$Churn <- factor(training$Churn, levels = 1:0, 
                          labels = c("Churn", "NonChurn"))
names(training)

qplot(Account.Length..Weeks., data = training, color = Churn, geom = "density")
qplot(Messages, data = training, color = Churn, geom = "density")
ggplot(training, aes(x = CustServ.Calls, fill = Churn)) +
    geom_histogram(alpha = 0.5, binwidth = 0.5, position = "identity") +
    scale_x_discrete(breaks = 0:10) + 
    scale_y_sqrt() + 
    scale_fill_manual(values = c("red", "darkgreen"))

minVars <- c("Day.Mins", "Eve.Mins", "Night.Mins", "Intl.Mins")
chargeVars <- c("Day.Charge", "Eve.Charge", "Night.Charge", "Intl.Charge")
callVars <- c("Day.Calls", "Eve.Calls", "Night.Calls", "Intl.Calls")
idVars <- c("Area.Code", "Phone", "Churn")

training.mins <- melt(training, id.vars = idVars, measure.vars = minVars,
                      value.name = "mins")
qplot(mins, data = training.mins, color = Churn, geom = "density") + 
    facet_wrap(~ variable, scales = "free")

training.calls <- melt(training, id.vars = idVars, measure.vars = callVars,
                      value.name = "calls")
qplot(calls, data = training.calls, color = Churn, geom = "density") + 
    facet_wrap(~ variable, scales = "free")


source("raw_analysis/featureSelection.R")
training$Int.l.Plan <- as.factor(training$Int.l.Plan)
training$Churn <- factor(training$Churn, levels = 1:0, 
                         labels = c("Churn", "NonChurn"))

minsPerWeekVars <- grep("MinsPerWeek", names(training), value = T)
avgChargePerCallVars <- grep("AvgChargePerCall", names(training), value = T)
avgMinsPerCallVars <- grep("AvgMinsPerCall", names(training), value = T)
avgCallsVar <- grep("AvgCalls", names(training), value = T)
avgMinsVar <- grep("AvgMins", names(training), value = T)
totalVars <- grep("TotalOut", names(training), value = T)

qplot(MessagesPerWeek, data = training, color = Churn, geom = "density") +
    scale_y_sqrt()

measureVars = list(minsPerWeekVars, avgChargePerCallVars, avgMinsPerCallVars)
for (vars in measureVars) {
    training.mins <- melt(training, id.vars = idVars, measure.vars = vars)
    qplot(value, data = training.mins, color = Churn, geom = "density") + 
        facet_wrap(~ variable, scales = "free")
}
