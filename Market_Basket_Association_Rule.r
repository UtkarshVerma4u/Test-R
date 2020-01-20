# II. Convert raw and demographic data to transaction class
df <- data.frame(
  age   = as.factor(c(6, 6, 8, 8, NA, 9, 16)),
  grade = as.factor(c("A", "C", "C", "C", "F", NA, "C")),
  pass  = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)) 
head(df)
# I. Continuous variables need to be binned / discretized
dat$age2 = discretize(dat$age, method = "frequency", 3)
## convert to transaction
trans3 <- as(df, "transactions")
summary(trans3)
inspect(trans3)

################################################################
library("arules")
library("arulesViz")

data("AdultUCI")

head(AdultUCI)
edit(AdultUCI)
str(AdultUCI)
cols.num <- c(1,3,5,11,12,13)
AdultUCI[,cols.num] <- lapply(AdultUCI[,cols.num],factor)
sapply(AdultUCI, class)
Adult <- as(AdultUCI, "transactions")

rules = apriori(Adult, parameter=list(support=0.01, confidence=0.5))

rules

inspect(head(sort(rules, by="lift"),3));

plot(rules,jitter=0)

head(quality(rules))

plot(rules, measure=c("support","lift"), shading="confidence")

plot(rules, shading="order", control=list(main ="Two-key plot"))

sel = plot(rules, measure=c("support","lift"), shading="confidence", interactive=TRUE);

subrules = rules[quality(rules)$confidence > 0.8];

subrules

plot(subrules, method="matrix", measure="lift");

plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE));

plot(subrules, method="matrix3D", measure="lift");

plot(subrules, method="matrix3D", measure="lift", control = list(reorder=TRUE));

plot(subrules, method="matrix", measure=c("lift", "confidence"));

plot(subrules, method="matrix", measure=c("lift","confidence"), control = list(reorder=TRUE));

plot(rules, method="grouped");

plot(rules, method="grouped", control=list(k=50));

sel = plot(rules, method="grouped", interactive=TRUE);

subrules2 = head(sort(rules, by="lift"), 30);

plot(subrules2, method="graph");

plot(subrules2, method="graph", control=list(type="items"));

plot(subrules2, method="paracoord");

plot(subrules2, method="paracoord", control=list(reorder=TRUE));

oneRule = sample(rules, 1);

inspect(oneRule);

itemFrequencyPlot(Adult, support = 0.1, cex.names=0.8);

fsets = eclat(trans, parameter = list(support = 0.05), control = list(verbose=FALSE));

singleItems = fsets[size(items(fsets)) == 1];

singleSupport = quality(singleItems)$support;

names(singleSupport) = unlist(LIST(items(singleItems), decode = FALSE));

head(singleSupport, n = 5);

itemsetList = LIST(items(fsets), decode = FALSE);

allConfidence = quality(fsets)$support / sapply(itemsetList, function(x)
  
  max(singleSupport[as.character(x)]));

quality(fsets) = cbind(quality(fsets), allConfidence);

summary(fsets);

itemFrequencyPlot(subrules2, topN = 5)


###############################################################

#read transactions
df_groceries <- read.csv("D:\\Programs\\R\\R_Sessions\\test.csv.txt")
str(df_groceries)

df_sorted <- df_groceries[order(df_groceries$Member_number),]


#convert member number to numeric
df_sorted$Member_number <- as.numeric(df_sorted$Member_number)


#convert item description to categorical format

df_sorted$itemDescription <- as.factor(df_sorted$itemDescription)

str(df_sorted)

#convert dataframe to transaction format using ddply; 

if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}

#group all the items that were bought together; by the same customer on the same date
library(plyr)
df_itemList <- ddply(df_groceries, c("Member_number","Date"), function(df1)paste(df1$itemDescription,collapse = ","))

#remove member number and date
df_itemList$Member_number <- NULL
df_itemList$Date <- NULL

colnames(df_itemList) <- c("itemList")

#write to csv format
write.csv(df_itemList,"D:\\Programs\\R\\R_Sessions\\test2.csv.txt", quote = FALSE, row.names = TRUE)

#-------------------- association rule mining algorithm : apriori -------------------------#

#load package required
library(arules)

#convert csv file to basket format
txn = read.transactions(file="D:\\Programs\\R\\R_Sessions\\test2.csv.txt", rm.duplicates= FALSE, format="basket",sep=",",cols=1);

#remove quotes from transactions
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)


#run apriori algorithm
basket_rules <- apriori(txn,parameter = list(minlen=2,sup = 0.001, conf = 0.01, target="rules"))
#basket_rules <- apriori(txn,parameter = list(minlen=2,sup = 0.00001, conf = 0.01, target="rules"),appearance = list(lhs = "CLEMENTINES")))

#check if tm is attched; if yes then detach
if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:sentiment, unload=TRUE)
  detach(package:tm, unload=TRUE)
}

#view rules
inspect(basket_rules)

#convert to datframe and view; optional
# df_basket <- as(basket_rules,"data.frame")
# Confidence value of 1 indicates If someone buys Product N, they are 100% likely to
# buy Product D.The support  value of 0.067 indicates that 6.7% of the transaction in 
# the data involve Product lhs purchases
#df_basket$confidence <- df_basket$confidence * 100
#df_basket$support <- df_basket$support * 100


# Mining rules for recommendations:

# split lhs and rhs into two columns
library(reshape2)
df_basket <- transform(df_basket, rules = colsplit(rules, pattern = "=>", names = c("lhs","rhs")))

# Remove curly brackets around rules
df_basket$rules$lhs <- gsub("[[:punct:]]", "", df_basket$rules$lhs)
df_basket$rules$rhs <- gsub("[[:punct:]]", "", df_basket$rules$rhs)

# convert to chracter
df_basket$rules$lhs <- as.character(df_basket$rules$lhs)
df_basket$rules$rhs <- as.character(df_basket$rules$rhs)

library(stringi)
library(dplyr)
df_basket$rules %>%
  filter(stri_detect_fixed(lhs, "yogurt")) %>%
  select(rhs)



#plot the rules
library(arulesViz)
plot(basket_rules)

set.seed(8000)
plot(basket_rules, method = "grouped", control = list(k = 5))

plot(basket_rules[1:20,], method="graph", control=list(type="items"))

plot(basket_rules[1:10,], method="paracoord",  control=list(alpha=.5, reorder=TRUE))

itemFrequencyPlot(txn, topN = 5)

plot(basket_rules[1:10,],measure=c("support","lift"),shading="confidence",interactive=T)
