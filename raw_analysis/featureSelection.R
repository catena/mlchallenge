
library(caret)
library(lattice)
library(ggplot2)

training <- read.csv("data/Training.csv", quote = "", stringsAsFactors = F, 
                      comment.char = "")
training$Message.Activity <- training$Messages / training$Account.Length..Weeks.
x <- cor(training[, -c(18, 19, 20, 21)])
diag(x) <- 0
rgb.palette <- colorRampPalette(c("blue", "green", "yellow"), space = "rgb")
levelplot(abs(x[, nrow(x):1]), main = "", xlab = "", ylab = "", col.regions = rgb.palette, cuts = 100, at = seq(0, 1, 0.01), scales = list(x=list(rot=90)))

abs(x) > 0.8