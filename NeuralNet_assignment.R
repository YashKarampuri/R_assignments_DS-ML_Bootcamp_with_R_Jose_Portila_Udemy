# Neural Network assignment

# Getting the data

df <- read.csv('bank_note_data.csv')

head(df)

str(df)

# Train Test Split

library(caTools)
set.seed(101)
split = sample.split(df$Class, SplitRatio = 0.60)

train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

str(train)

# Building the Neural Network

library(neuralnet)

nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train,hidden=7,linear.output=FALSE)

# Predictions

predicted.nn.values <- compute(nn,test[,1:4])

head(predicted.nn.values$net.result)

predictions <- sapply(predicted.nn.values$net.result,round)

head(predictions)

table(predictions,test$Class)              # Confusion matrix

# Comparing Models (With Random Forests)

library(randomForest)

df$Class <- factor(df$Class)
library(caTools)
set.seed(101)
split = sample.split(df$Class, SplitRatio = 0.60)

train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

model <- randomForest(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train)

rf.pred <- predict(model,test)

table(rf.pred,test$Class)


