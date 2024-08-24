# KNN project
# Getting the data

library(ISLR)
head(iris)
str(iris)

# Standardizing the data
stand.features <- scale(iris[1:4])

var(stand.features[,1])

final.data <- cbind(stand.features,iris[5])

head(final.data)

# Training and Testing Splits

set.seed(101)

library(caTools)

sample <- sample.split(final.data$Species, SplitRatio = .60)
train <- subset(final.data, sample == TRUE)
test <- subset(final.data, sample == FALSE)

# Building a KNN model

library(class)

predicted.species <- knn(train[1:4],test[1:4],train$Species,k=1) # k = 1

predicted.species

# misclassification rate

mean(test$Species != predicted.species)

# Creating a plot of the error (misclassification) rate for k values ranging from 1 to 10

predicted.species <- NULL
error.rate <- NULL

for(i in 1:10){
  set.seed(101)
  predicted.species <- knn(train[1:4],test[1:4],train$Species,k=i)
  error.rate[i] <- mean(test$Species != predicted.species)
}

library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)

pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()
pl + geom_line(lty="dotted",color='black')
