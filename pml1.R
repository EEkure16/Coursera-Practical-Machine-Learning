
# libraries necessary or the analyses
library(caret)
library(ggthemes)
library(gridExtra)
library(ggplot2)
library(randomForest)
library(grid)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)




 set.seed(46)
# reading the training and test data
training <- read.csv("pml-training.csv", na.strings=c("NA", ""))
testing <- read.csv("pml-testing.csv", na.strings=c("NA", ""))
# cleaning data
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

train_sel <- training[, -c(1:7)]
test_sel <- testing[, -c(1:7)]
# partitioning data
train1 <- createDataPartition(train_sel$classe, p = 0.6, list = FALSE)
train2 <- train_sel[train1, ]
test2 <- train_sel[-train1, ]

# first model, random forest
set.seed(48)
model1 <- randomForest(classe ~ ., data = train2, ntrees = 500, Importance = TRUE)
print(model1)

# plotting error rate vs number of trees
plot(model1, main = "Random Froest: Error rate as a function of the number of trees")


# test data prediction
prediction1 <- predict(model1, newdata = test2)
confusionMatrix(prediction1, test2$classe)

# second model using classification trees
set.seed(48)
model2 <- rpart(classe ~ ., data = train2, method = "class")
printcp(model2)


# plot of te trees
fancyRpartPlot(model2)


# test prediction with classification tree model
prediction2 <- predict(model2, test2, type = "class")
confusionMatrix(prediction2, test2$classe)


