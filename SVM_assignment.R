# SVM assignment

# Getting the data

loans <- read.csv('loan_data.csv')

str(loans)

summary(loans)

loans$credit.policy <- factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)

# Data Analysis

library(ggplot2)

pl <- ggplot(loans,aes(x=fico))
pl <- pl + geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha=0.5)
pl + scale_fill_manual(values = c('yellow','blue')) + theme_bw()

pl <- ggplot(loans,aes(x=factor(purpose)))
pl <- pl + geom_bar(aes(fill=not.fully.paid),position = "dodge")
pl + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(loans,aes(int.rate,fico)) +geom_point() + theme_bw()

ggplot(loans,aes(int.rate,fico)) +geom_point(aes(color=not.fully.paid),alpha=0.3) + theme_bw()

# Building the Model

# Train and Test Splits

library(caTools)

set.seed(101)

spl = sample.split(loans$not.fully.paid, 0.6)

train = subset(loans, spl == TRUE)

test = subset(loans, spl == FALSE)

library(e1071)

# Using the svm function

model <- svm(not.fully.paid ~ .,data=train)

summary(model)

predicted.values <- predict(model,test[1:13])

table(predicted.values,test$not.fully.paid)

# Tuning the model:

tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',
                     ranges=list(cost=c(1,10), gamma=c(0.1,1)))

model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)




