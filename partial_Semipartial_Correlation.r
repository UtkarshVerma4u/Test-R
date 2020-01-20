#R Script : Partial Correlation Coefficient
# Load Library
library(ppcor)
# Read data
mydata=read.csv("C:\\Users\\Deepanshu\\Documents\\Example1.csv")
# Partial correlation between "height" and "weight" given "age"
with(mydata, pcor.test(Height,Weight,Age))

#R Script : Semipartial Correlation Coefficient

#Semi partial correlation - Age constant for Weight only
with(mydata, spcor.test(Height,Weight,Age))

#Semi partial correlation coefficient - Age constant for Height only
with(mydata, spcor.test(Weight,Height,Age))

# Squared Partial Correlation >= Squared Semi-partial Correlation

set.seed(1)
k1 = sample(100:1000,1000, replace=TRUE)
k2 = sample(10:1010,1000) + k1 - 10**2 * runif(1000)
X=cbind(k1,k2)
c = cor(X)
eigen(c)$values[1]/ sum(eigen(c)$values)
