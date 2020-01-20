setwd("D:\\Programs\\R\\R_Sessions")

# A few common steps in data model building are;
# 
# Pre-processing the predictor data (predictor - independent variable's)
# Estimating the model parameters
# Selecting the predictors for the model
# Evaluating the model performance
# Fine tuning the class prediction rules

# clear the workspace
rm(list=ls())
# ensure the process is reproducible
set.seed(2)

# Manual Sampleing
library(caret)
data("iris")
dim(iris)

iris[sample(nrow(iris),100),]

# Sampleing based on Probability

prb <- ifelse(iris$Species =="setosa",0.25, 0.75)
smpl<- iris[sample(nrow(iris),100, prob = prb),]
table(smpl$Species)
plot(smpl$Species)

# Random Sampleing CreateDataPartation 
library(caret)
train.rows<- createDataPartition(y=iris$Species, p=0.7, list = FALSE)
train.data<- iris[train.rows,] # 70% data goes in here
test.data<- iris[-train.rows,] # 30% data goes in here

table(test.data$Species)
table(train.data$Species)

plot(train.data$Species)
plot(test.data$Species)

sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))
