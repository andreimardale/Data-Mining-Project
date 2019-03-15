# Data Preparation for Regression
data_reg = getDataForRegression(terrorism, military_budget, density, emigrants, population)
data_reg$attacks = normalize(data_reg$attacks)
data_reg$military_budget = normalize(data_reg$military_budget)
data_reg$emigrants = normalize(data_reg$emigrants)
data_reg$PopTotal = normalize(data_reg$PopTotal)


#Plots to determine the general trends for number of attacks per year, military budget per year, total population and
#total number of emigrants in the world, per year.
plot(data_reg$year, data_reg$attacks, type="l", lwd=1, col = "red", ylab="", xlab="")
par(new=TRUE)
plot(data_reg$year, data_reg$military_budget, type="l", col = "blue", lwd=1, xlab="", ylab="", yaxt="n", xaxt="n")
par(new=TRUE)
plot(data_reg$year, data_reg$PopTotal, type="l", lwd=1,col="green", xlab="", ylab="", yaxt="n", xaxt="n")
par(new=TRUE)
plot(data_reg$year, data_reg$emigrants, type="l", lwd=1,col="purple", xlab="", ylab="", yaxt="n", xaxt="n")

title(xlab="Year", col.lab=rgb(0,0.5,0))
title(ylab="Normalized Value", col.lab=rgb(0,0.5,0))


#The plots show clearly that there exists a good correlation between all the variables, so a regression model might
#have success in determining the number of attacks in the following years.
pairs(data_reg)
cor(data_reg)

cor.test(data_reg$attacks, data_reg$PopTotal, method="pearson")
cor.test(data_reg$attacks, data_reg$emigrants, method="pearson")
cor.test(data_reg$attacks, data_reg$military_budget, method="pearson")

#After printing the correlation matrix, we can see that the number of attacks is correlated with:
# military budget: 0.8058181
# emigrants: 0.7009036
# total population: 0.6878820

#Model 1 - attack = function(PopTotal)
m1 <- lm(attacks~PopTotal,data=data_reg)
summary(m1)
m1

ggplot(data=data_reg, aes(x=PopTotal, y=attacks)) +
  geom_point() +
  geom_smooth(method="lm",formula= y ~ x) +
  scale_y_continuous(limits = c(-1, 2))


#Model 2 - attack = function(PopTotal ^ 2)
m2 <- lm(attacks~ PopTotal + I(PopTotal ^ 2),data=data_reg)
summary(m2)
m2

ggplot(data=data_reg, aes(x=PopTotal, y=attacks)) +
  geom_point() +
  geom_smooth(method="lm",formula= y ~ x + I(x^2)) +
  scale_y_continuous(limits = c(-1, 2))

#Model 3 - attack = function(military_budget)
m3 <- lm(attacks~military_budget,data=data_reg)
summary(m3)
m3

#Model 4 - attack = function(population, military_budget, emigrants)
m4 <- lm(attacks~PopTotal + I(PopTotal ^ 2) + military_budget + emigrants,data=data_reg)
summary(m4)
m4

# The ANOVA test
anova(m1, m2, test="F")
#p<0.05 for model 2, meaning it's the better model.

anova(m2, m3, test="F")
#p<0.05 for model 3

anova(m3, m4, test="F")

#Model validation and forecasting
#k-fold cross-validation
library(caret)

splitted = splitTrainTestForRegression(data_reg)
train_data = splitted[[1]]
test_data = splitted[[2]]
test_labels = splitted[[3]]

set.seed(42)

trainmethod <- trainControl(method="cv", number=5, returnData=TRUE, returnResamp='all')

model1 <- train(data=train_data, attacks~PopTotal, method='lm', trControl=trainmethod)
model2 <- train(data=train_data, attacks~ PopTotal + I(PopTotal ^ 2), method='lm', trControl=trainmethod)
model3 <- train(data=train_data, attacks~military_budget, method='lm', trControl=trainmethod)
model4 <- train(data=train_data, attacks~PopTotal + I(PopTotal ^ 2) + military_budget + emigrants, method='lm', trControl=trainmethod)
model1$resample
model1
# for model 1, RMSE = 0.7424499, R^2 = 0.6001737 -> 60% of the variance of the attacks number is explained by the population of the planet.

test_data_predict = predict(model1, test_data)
print(paste("RMSE: ", toString(RMSE(test_data_predict, test_labels$attacks))))
print(paste("Rsq: ", toString(rsq(test_data_predict, test_labels$attacks))))


model2$resample
model2
# for model 2, RMSE = 0.4684229, R^2 = 0.9602456 -> 96% of the variance of the attacks number is explained by the population^2 of the planet.
test_data_predict = predict(model2, test_data)
print(paste("RMSE: ", toString(RMSE(test_data_predict, test_labels$attacks))))
print(paste("Rsq: ", toString(rsq(test_data_predict, test_labels$attacks))))


model3$resample
model3
# for model 3, RMSE = 0.6330366 , R^2 = 0.6987232 -> 69% of the variance of the attacks number is explained by the military_budget of the planet.

model4$resample
model4
# for model 4, RMSE = 0.3805212 , R^2 = 0.854682 -> 85.46% of the variance of the attacks number is explained by the other 4 variables.
test_data_predict = predict(model4, test_data)
print(paste("RMSE: ", toString(RMSE(test_data_predict, test_labels$attacks))))
print(paste("Rsq: ", toString(rsq(test_data_predict, test_labels$attacks))))


# In conclusion, the best model is the 4th model that incorporates all 4 features.



# SVM Regression
library(e1071)
model1_svm <- train(attacks~PopTotal + I(PopTotal ^ 2) + military_budget + emigrants, data=train_data, method = "svmLinear", trControl = trainmethod)
model1_svm # RMSE = 0.4271642  R^2 = 0.9356972

test_data_predict = predict(model1_svm, test_data)

rmse = RMSE(test_data_predict, test_labels$attacks)
rsquared = rsq(test_data_predict, test_labels$attacks)
print(paste("RMSE: ", toString(rmse)))
print(paste("Rsq: ", toString(rsquared)))

# Random Forest Regression
library(randomForest)
model_rf = randomForest(attacks~PopTotal + I(PopTotal ^ 2) + military_budget + emigrants,data=train_data)
model1_rf <- train(attacks~PopTotal + I(PopTotal ^ 2) + military_budget + emigrants, data=train_data, method = "rf", trControl = trainmethod)
model1_rf$resample
model1_rf # 0.3056650  0.9687566
model_rf

test_data_predict = predict(model1_rf, test_data)
print(paste("RMSE: ", toString(RMSE(test_data_predict, test_labels$attacks))))
print(paste("Rsq: ", toString(rsq(test_data_predict, test_labels$attacks))))

# Bayesian Regularzied Neural Networks
modelFit <- train(attacks~PopTotal + I(PopTotal ^ 2) + military_budget + emigrants, data=train_data, method = "brnn", preProcess = c("center"))
modelFit #0.3246394  0.9023658
test_data_predict = predict(modelFit, test_data)
print(paste("RMSE: ", toString(RMSE(test_data_predict, test_labels$attacks))))
print(paste("Rsq: ", toString(rsq(test_data_predict, test_labels$attacks))))

#Bagged CART 
modelFit <- train(attacks~PopTotal + I(PopTotal ^ 2) + military_budget + emigrants, data=train_data, method = "treebag")
modelFit #0.5911111  0.8141044
test_data_predict = predict(modelFit, test_data)
print(paste("RMSE: ", toString(RMSE(test_data_predict, test_labels$attacks))))
print(paste("Rsq: ", toString(rsq(test_data_predict, test_labels$attacks))))


#Monotone Multi-Layer Perceptron Neural Network
modelFit <- train(attacks~PopTotal + I(PopTotal ^ 2) + military_budget + emigrants, data=train_data, method = "monmlp")
modelFit #0.5720484  0.7887362
test_data_predict = predict(modelFit, test_data)
print(paste("RMSE: ", toString(RMSE(test_data_predict, test_labels$attacks))))
print(paste("Rsq: ", toString(rsq(test_data_predict, test_labels$attacks))))

#Neural Network
modelFit <- train(attacks~PopTotal + I(PopTotal ^ 2) + military_budget + emigrants, data=train_data, method = "neuralnet")
modelFit #0.3782149  0.8796843
test_data_predict = predict(modelFit, test_data)
print(paste("RMSE: ", toString(RMSE(test_data_predict, test_labels$attacks))))
print(paste("Rsq: ", toString(rsq(test_data_predict, test_labels$attacks))))

#Penalized Linear Regression
modelFit <- train(attacks~PopTotal + I(PopTotal ^ 2) + military_budget + emigrants, data=train_data, method = "penalized")
modelFit #0.4297852  0.9000877
test_data_predict = predict(modelFit, test_data)
print(paste("RMSE: ", toString(RMSE(test_data_predict, test_labels$attacks))))
print(paste("Rsq: ", toString(rsq(test_data_predict, test_labels$attacks))))

#Supervised Principal Component Analysis
modelFit <- train(attacks~PopTotal + I(PopTotal ^ 2) + military_budget + emigrants, data=train_data, method = "superpc")
modelFit #0.4483441  0.8831527
test_data_predict = predict(modelFit, test_data)
print(paste("RMSE: ", toString(RMSE(test_data_predict, test_labels$attacks))))
print(paste("Rsq: ", toString(rsq(test_data_predict, test_labels$attacks))))


splitTrainTestForRegression <- function(data) {
  ## 80% of the sample size
  smp_size <- floor(0.8 * nrow(data))
  
  ## set the seed to make your partition reproducible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  
  train_data = subset(train, select = -c(year))
  test_labels = subset(test, select = c(attacks))
  test = subset(test, select = -c(attacks, year))
  
  return(list(train_data,test, test_labels))
}

rsq <- function (x, y) cor(x, y) ^ 2
