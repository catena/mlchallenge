source("raw_analysis/models/ensemble.R")

source("raw_analysis/models/loadData.R")
customers <- readProblemData(params)
processedData <- extractEnsembleFeatures(customers, models)
predictions <- predict(modelFit.stack, processedData)

solution <- data.frame(Area.Code = customers$Area.Code, Phone = customers$Phone,
                       Churn = 2 - as.numeric(predictions))
write.csv(solution, file = "data/solution.csv", row.names = F, quote = F)
