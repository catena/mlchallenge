library(data.table)
library(arules)
library(arulesViz)

setwd("~/workspaces/r-workspace/ideatory/innoplexus")

clickdata <- fread("zcat data/ClickStreamData.txt.gz", stringsAsFactors = TRUE)
proddata <- fread("data/ProductCategoryData.txt", stringsAsFactors = TRUE)
userdata <- fread("data/UserProfileData.txt", stringsAsFactors = TRUE)

# merge tables
clickdata <- clickdata[, 1:7, with = FALSE]
clickdata <- merge(clickdata, proddata, by = "URL", all.x = TRUE, all.y = FALSE)
clickdata <- merge(clickdata, userdata, by = "User ID", 
                   all.x = TRUE, all.y = FALSE)

# rename vars
setnames(clickdata, "User ID", "UserID")
setnames(clickdata, "Region/State", "Region")
setnames(clickdata, "Number of friends", "Friends")
setnames(clickdata, "College Education", "Education")

# cleanup vars
clickdata[, Timestamp := as.numeric(gsub("(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)", 
                                         "\\3\\2\\1\\4\\5", as.character(Timestamp)))]
clickdata[, Education := factor(Education, levels = c("Yes", "No"),
                             labels = c("Educated", "Uneducated"))]
clickdata$Friends <- cut(clickdata$Friends, 0:4 * 50, include.lowest = TRUE,
                         labels = c("Friends(0-50)", "Friends(50-100)", 
                                    "Friends(100-150)", "Friends(150-200)"))

# remove redundant/spurious observations
clickdata[, c("IP Address", "URL") := NULL]
clickdata <- clickdata[!is.na(Category)]

# available categories
categories <- levels(clickdata$Category)

# identify all user clicks within 10mins window
setkey(clickdata, UserID, Timestamp)
clickdata[, Timeswitch := cumsum(abs(c(0, diff(Timestamp))) > 10)]
clickdata <- clickdata[, list(Categories = list(unique(Category))), 
                       by = list(UserID, Timeswitch, Region)] ## , Friends, Education)]
clickdata[, UserID := NULL][, Timeswitch := NULL]

# choose only transactions with multiple items purchased
clickdata <- clickdata[sapply(clickdata$Categories, length) > 1]

# group user clicks to transaction item lists
clickItems <- apply(clickdata, 1, function(row) {
    itemlist <- unname(c(lapply(row, function(item) {
        as.character(unlist(item))
    }), recursive = TRUE))
    sort(itemlist)
})

# save clickItems to csv file for verification
clickItemsOutFrame <- data.frame(sapply(clickItems, paste, collapse = ','))
write.table(clickItemsOutFrame, "data/clickdata.csv",
            row.names = F, col.names = F, quote = F)

# derive assoc rules, minlen = 2 discards single item rules
clickTransactions <- as(clickItems, "transactions")
rules <- apriori(clickTransactions, 
                 parameter = list(support = 0.01, confidence = 0.5, minlen = 2,
                                  target = "rules"))

# select only subrules whose lhs and rhs contains atleast one category
# and, antecedent has not more than 2 items
categories <- intersect(categories, clickTransactions@itemInfo$labels)
rules <- subset(rules, subset = rhs %in% categories & lhs %in% categories)
rules.sub <- subset(rules, subset = sapply(as(lhs(rules), "list"), length) <= 2)
inspect(head(sort(rules.sub, by = "confidence"), 20))

# derive maximally frequent itemsets
itemsets <- apriori(clickTransactions, 
                 parameter = list(support = 0.025, minlen = 2,
                                  target = "maximally frequent itemsets"))

# select itemsets that contain atleast 2 categories
clen <- sapply(as(items(itemsets), "list"), function(x) sum(x %in% categories))
itemsets <- subset(itemsets, subset = clen > 1)
inspect(sort(itemsets))

# graph
set.seed(10001)
# plot(rules, method = "grouped", measure = "confidence", shading = "lift",
#      control = list(k = 5))
plot(rules, method = "graph", measure = "confidence", shading = "lift",
     control = list(main = "Association Rule Graph", cex = 0.7, arrowSize = 0.35))
plot(itemsets, method = "graph",
     control = list(main = "Maximal Frequent Itemsets", cex = 0.8))
