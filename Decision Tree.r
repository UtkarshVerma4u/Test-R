# Decision Tree

# https://www.listendata.com/2015/04/decision-tree-in-r.html

# How Decision Tree works:
# 1.Pick the variable that gives the best split (based on lowest Gini Index)
# 2.Partition the data based on the value of this variable
# 3.Repeat step 1 and step 2. Splitting stops when CART detects no further gain can be made,or some pre-set stopping
# rules are met. (Alternatively, the data are split as much as possible and then the tree is later pruned.


#read data file
GermanCredit= read.csv("C:\\Users\\Deepanshu Bhalla\\Desktop\\german_credit.csv")

# Check attributes of data
str(GermanCredit)

# Check number of rows and columns
dim(GermanCredit)

# Make dependent variable as a factor (categorical)
GermanCredit$Class = as.factor(GermanCredit$Class)

# Split data into training (70%) and validation (30%)
dt = sort(sample(nrow(GermanCredit), nrow(GermanCredit)*.7))
train<-GermanCredit[dt,]
val<-GermanCredit[-dt,] # Check number of rows in training data set
nrow(train)

# To view dataset
edit(train)

# Decision Tree Model
library(rpart)
mtree <- rpart(Class~., data = train, method="class", control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 10, usesurrogate = 2, xval =10 ))

mtree

#Plot tree
plot(mtree)
text(mtree)

#Beautify tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#view1
prp(mtree, faclen = 0, cex = 0.8, extra = 1)

#view2 - total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}

prp(mtree, faclen = 0, cex = 0.8, node.fun=tot_count)

#view3- fancy Plot
rattle()
fancyRpartPlot(mtree)

############################
########Pruning#############
############################

printcp(mtree)
bestcp <- mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"]

# Prune the tree using the best cp.
pruned <- prune(mtree, cp = bestcp)

# Plot pruned tree
prp(pruned, faclen = 0, cex = 0.8, extra = 1)

# confusion matrix (training data)
conf.matrix <- table(train$Class, predict(pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

#Scoring
library(ROCR)
val1 = predict(pruned, val, type = "prob")
#Storing Model Performance Scores
pred_val <-prediction(val1[,2],val$Class)

# Calculating Area under Curve
perf_val <- performance(pred_val,"auc")
perf_val

# Plotting Lift curve
plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)

# Calculating True Positive and False Positive Rate
perf_val <- performance(pred_val, "tpr", "fpr")

# Plot the ROC curve
plot(perf_val, col = "green", lwd = 1.5)

#Calculating KS statistics
ks1.tree <- max(attr(perf_val, "y.values")[[1]] - (attr(perf_val, "x.values")[[1]]))
ks1.tree

# Advanced Plot
prp(pruned, main="Beautiful Tree",
    extra=106, 
    nn=TRUE, 
    fallen.leaves=TRUE, 
    branch=.5, 
    faclen=0, 
    trace=1, 
    shadow.col="gray", 
    branch.lty=3, 
    split.cex=1.2, 
    split.prefix="is ", 
    split.suffix="?", 
    split.box.col="lightgray", 
    split.border.col="darkgray", 
    split.round=.5)