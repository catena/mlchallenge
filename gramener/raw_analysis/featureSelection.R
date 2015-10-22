
library(caret)
library(lattice)
library(ggplot2)
library(Boruta)

training <- read.csv("data/Training.csv", quote = "", stringsAsFactors = F, 
                      comment.char = "")

training <- within(training, {
    TotalOut.Mins <- Day.Mins + Eve.Mins + Intl.Mins + Night.Mins
    TotalOut.Calls <- Day.Calls + Eve.Calls + Intl.Calls + Night.Calls
    TotalOut.Charge <- Day.Charge + Eve.Charge + Intl.Charge + Night.Charge
    
    Day.PropMins <- Day.Mins / TotalOut.Mins
    Eve.PropMins <- Eve.Mins / TotalOut.Mins
    Intl.PropMins <- Intl.Mins / TotalOut.Mins
    Night.PropMins <- Night.Mins / TotalOut.Mins
    
    Day.PropCharge <- Day.Charge / TotalOut.Charge
    Eve.PropCharge <- Eve.Charge / TotalOut.Charge
    Intl.PropCharge <- Intl.Charge / TotalOut.Charge
    Night.PropCharge <- Night.Charge / TotalOut.Charge
    
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
    
    Day.CallsPerWeek <- Day.Calls / Account.Length..Weeks.
    Eve.CallsPerWeek <- Eve.Calls / Account.Length..Weeks.
    Intl.CallsPerWeek <- Intl.Calls / Account.Length..Weeks.
    Night.CallsPerWeek <- Night.Calls / Account.Length..Weeks.
    Total.CallsPerWeek <- TotalOut.Calls / Account.Length..Weeks.
    
    Day.ChargePerWeek <- Day.Charge / Account.Length..Weeks.
    Eve.ChargePerWeek <- Eve.Charge / Account.Length..Weeks.
    Intl.ChargePerWeek <- Intl.Charge / Account.Length..Weeks.
    Night.ChargePerWeek <- Night.Charge / Account.Length..Weeks.
    Total.ChargePerWeek <- TotalOut.Charge / Account.Length..Weeks.
})
training[sapply(training, is.nan)] <- 0
training[, c("Phone", "State")] <- list(NULL)

# training$Churn <- factor(training$Churn, levels = 1:0, 
#                          labels = c("Churn", "NonChurn"))
# training$Int.l.Plan <- as.factor(training$Int.l.Plan)
# training$Message.Plan <- as.factor(training$Message.Plan)
# 
# # run boruta search to extract important features
# results <- Boruta(Churn ~ ., data = training, doTrace = 2)
# 
# par(mar = c(7, 5, 1, 1))
# plot(results, las = 2, xlab = "", cex.axis = 0.7)

## boruta results implies to remove "Area.Code"
training$Area.Code <- NULL


## correlation plot
training$Int.l.Plan <- as.numeric(training$Int.l.Plan)
training$Message.Plan <- as.numeric(training$Message.Plan)
x <- subset(training, select = -Churn)
x.cor <- abs(cor(scale(x)))
diag(x.cor) <- 0
rgb.palette <- colorRampPalette(c("blue", "green", "yellow"), space = "rgb")
levelplot(x.cor[, nrow(x.cor):1], main = "", xlab = "", ylab = "", col.regions = rgb.palette, cuts = 100, at = seq(0, 1, 0.01), scales = list(x=list(rot=90)))

# find highly correlated pairs
y <- which(x.cor > 0.9, arr.ind = T)
print(cbind(rownames(x.cor)[y[, 1]], colnames(x.cor)[y[, 2]]))
highlyCor <- findCorrelation(x.cor, cutoff = 0.9)
print(names(x)[highlyCor])
training[, names(x)[highlyCor]] <- list(NULL)
print(names(training))

training <- head(training)


