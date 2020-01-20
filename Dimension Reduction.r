# Link : https://www.listendata.com/2015/06/simplest-dimensionality-reduction-with-r.html
# Step I : Remove Redundant Variables



#The sample correlation matrix is shown below -
Corrmatrix <- structure(c(1, 0.82, 0.54, 0.36, 0.85, 0.82, 1, 0.01, 0.74, 0.36,
                            0.54, 0.01, 1, 0.65, 0.91, 0.36, 0.74, 0.65, 1, 0.36,
                            0.85, 0.36, 0.91, 0.36, 1),
                          .Dim = c(5L, 5L))
library(caret)
findCorrelation(Corrmatrix, cutoff = .6, verbose = TRUE, names = TRUE)

# load required libraries
library(caret)
library(corrplot)
library(plyr)

# load required dataset
dat <- read.csv("C:\\Users\\734904\\Downloads\\pml-training.csv")

# Set seed
set.seed(227)

head(dat)
str(dat)
dim(dat)
# Remove variables having high missing percentage (50%)
dat1 <- dat[, colMeans(is.na(dat)) <= .5]
dim(dat1)

# Remove Zero and Near Zero-Variance Predictors
nzv <- nearZeroVar(dat1)
dat2 <- dat1[, -nzv]
dim(dat2)

# Identifying numeric variables
numericData <- dat2[sapply(dat2, is.numeric)]
str(numericData)

# Calculate correlation matrix
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)
summary(descrCor[upper.tri(descrCor)])

# Check Correlation Plot
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)

# print indexes of highly correlated attributes
print(highlyCorrelated)

# Indentifying Variable Names of Highly Correlated Variables
highlyCorCol <- colnames(numericData)[highlyCorrelated]

# Print highly correlated attributes
highlyCorCol

# Remove highly correlated variables and create a new dataset
dat3 <- dat2[, -which(colnames(dat2) %in% highlyCorCol)]
dim(dat3)

# R Code : Feature Selection with Random Forest
# Best Algo as no underlaying assumptions besides this 2 imp feature is 
# The first one - Gini gain produced by the variable, averaged over all trees.
# The second one - Permutation Importance i.e. mean decrease in classification accuracy after 
# permuting the variable, averaged over all trees. 
# Sort the permutation importance score on descending order and select the TOP k variables

library(randomForest)

#Train Random Forest
rf <-randomForest(classe~.,data=dat3, importance=TRUE,ntree=1000)

#Evaluate variable importance
imp = importance(rf, type=1)
imp <- data.frame(predictors=rownames(imp),imp)

# Order the predictor levels by importance
imp.sort <- arrange(imp,desc(MeanDecreaseAccuracy))
imp.sort$predictors <- factor(imp.sort$predictors,levels=imp.sort$predictors)

# Select the top 20 predictors
imp.20<- imp.sort[1:20,]
print(imp.20)

# Plot Important Variables
varImpPlot(rf, type=1)

# Subset data with 20 independent and 1 dependent variables
dat4 = cbind(classe = dat3$classe, dat3[,c(imp.20$predictors)])
