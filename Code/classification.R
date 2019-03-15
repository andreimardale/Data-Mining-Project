library("e1071")
library(kernlab)
library(caret)
library(deepboost)
library(xgboost)

splitted = splitTrainTest(merged_data)
train_data = splitted[[1]]
test_data = splitted[[2]]
test_labels = splitted[[3]]


#### SVM ####
set.seed(42)
trainmethod <- trainControl(method="cv", number=5, returnData=TRUE, returnResamp='all')
model1_svm <- train(attackslevel~., data=train_data, method = "svmRadial", trControl = trainmethod)
model1_svm$resample
model1_svm

#Tuning parameter 'sigma' was held constant at a value of 40.01995
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 40.01995 and C = 1.

svmfit_after_tune = svm(attackslevel ~ ., data = train_data, kernel = "radial", gamma=40.01995, cost=1)
print(confusionMatrix(train_data$attackslevel, predict(svmfit_after_tune)))

predicted = data.frame(predict(svmfit_after_tune, test_data))
score = compareResults(predicted, test_labels)
print(score)

# Test accuracy is  ~78.5%
#The model is slightly overfitting, but the test time accuracy is still fine.


svmPoly <- train(attackslevel~., data=train_data, method = "svmPoly", trControl = trainmethod)
svmPoly$resample
svmPoly  
#The final values used for the model were degree = 3, scale = 0.1 and C = 1.  
svmfit_after_tune = svm(attackslevel ~ ., data = train_data, kernel = "polynomial", degree=3, cost=1)
print(confusionMatrix(train_data$attackslevel, predict(svmfit_after_tune)))

predicted = data.frame(predict(svmfit_after_tune, test_data))
score = compareResults(predicted, test_labels)
print(score)

#### DeepBoost ####
deepboost <- train(attackslevel~., data=train_data, method = "deepboost", trControl = trainmethod)
deepboost$resample
deepboost
deepboost_model = deepboost(attackslevel~., data=train_data,  num_iter = 50, tree_depth = 3, beta = 0.00390625, lambda = 0.0625, loss_type = 'l')

predicted = data.frame(predict(deepboost_model, train_data))
dd = data.frame(train_data$attackslevel)
score = compareResults(predicted, dd)
print(score)

predicted = data.frame(predict(deepboost_model, test_data))
score = compareResults(predicted, test_labels)
print(score)

#### Random Forest ####
library(randomForest)
rf <- train(attackslevel~., data=train_data, method = "rf", trControl = trainmethod)
rf$resample
rf # The final value used for the model was mtry = 2.

rf1 <- randomForest(attackslevel ~ ., data = train_data, importance = TRUE, mtry = 2)
print(rf1)

predicted = data.frame(predict(rf1, test_data))
score = compareResults(predicted, test_labels)
print(score)


#### Decision Trees ####
library(rpart)
library(caret)
library(e1071)

model_dt = train(attackslevel ~ ., data = train_data, method = "rpart")
print(model_dt)

predicted = data.frame(predict(model_dt, train_data))
score = compareResults(predicted, data.frame(train_data$attackslevel))
print(paste("Training accuracy: ", toString(score)))
#Training accuracy is 83.29%.

predicted = data.frame(predict(model_dt, test_data))
score = compareResults(predicted, test_labels)
print(paste("Test accuracy: ", toString(score)))
#Test accuracy is 78.5%.


#### K-NN ####
trainmethod <- trainControl(method="cv", number=5, returnData=TRUE, returnResamp='all')

model_dt = train(attackslevel ~ ., data = train_data, method = "knn", trControl = trainmethod)
print(model_dt) #The final value used for the model was k = 5

predicted = data.frame(predict(model_dt, train_data))
score = compareResults(predicted, data.frame(train_data$attackslevel))
print(paste("Training accuracy: ", toString(score)))
#Training accuracy is 88%.

predicted = data.frame(predict(model_dt, test_data))
score = compareResults(predicted, test_labels)
print(paste("Test accuracy: ", toString(score)))
#Test accuracy is 78.7%.


splitted = splitTrainTest(merged_data)
train_data = splitted[[1]]
test_data = splitted[[2]]
test_labels = splitted[[3]]

trainmethod <- trainControl(method="cv", number=5, returnData=TRUE, returnResamp='all')

#### Neural Networks ####
model_dt = train(attackslevel ~ ., data = train_data, method = "nnet", trControl = trainmethod)
print(model_dt) #The final values used for the model were size = 3 and decay = 1e-04.

predicted = data.frame(predict(model_dt, train_data))
score = compareResults(predicted, data.frame(train_data$attackslevel))
print(paste("Training accuracy: ", toString(score)))
#Training accuracy is 0.816470588235294%.

predicted = data.frame(predict(model_dt, test_data))
score = compareResults(predicted, test_labels)
print(paste("Test accuracy: ", toString(score)))
#Test accuracy is 78.7%.

splitTrainTest <- function(data) {
  ## 80% of the sample size
  smp_size <- floor(0.8 * nrow(data))
  
  ## set the seed to make your partition reproducible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  test_labels = subset(test, select = c(attackslevel))
  test = subset(test, select = -c(attackslevel))
  
  return(list(train,test, test_labels))
}

compareResults <- function(predicted, labels){
  correct_predictions = sum(predicted == labels)
  score = as.numeric(correct_predictions / dim(labels)[1])
  return(score)
}