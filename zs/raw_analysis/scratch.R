
qplot(Variable_19, data = training, fill = as.factor(Labels), geom = "density")
qplot(pseudoLog10(Variable_40), data = customers, fill = as.factor(Labels), geom = "density")
qplot(log10(Variable_166 + 1), data = training, fill = as.factor(Labels), geom = "density")
qplot(Variable_189, data = training, fill = as.factor(Labels), geom = "density")
qplot(Variable_187, data = trainingx, fill = as.factor(Labels), geom = "density")
prop.table(table(customers$Variable_2, customers$Labels), 1)
prop.table(table(is.na(customers$Variable_2), customers$Labels), 1)

predVars <- setdiff(names(training), c("Customer.ID", "Labels"))
preProc <- preProcess(training[, predVars], 
                      method = c("YeoJohnson"))
trainingx <- training
trainingx[, predVars] <- predict(preProc, training[, predVars])

cubert <- function(x) { sign(x) * abs(x)^(1/3) }

for (x in predVars) {
    p <- qplot(training[, x], data = training, fill = Labels, geom = "density")
    ggsave(paste0("fig/predvars/", x, ".png"), p)
}

