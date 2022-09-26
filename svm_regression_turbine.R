## Loading Data
turbine <- read.csv("/Users/azimkhan22/Documents/r_doc/final_code/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]

#support vector regressor
set.seed(1)
library(caret)
library(kernlab)
set.seed(1)
model3 <- train(P_avg ~ .,data = turbine.train,method = 'svmRadial')
model3

#prediction
pred1 <- predict(model3, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)

# model training using cross validation, 10 cross validation
ctrl <- trainControl(method = "cv",number = 10)
model4 <- train(P_avg ~ .,data = turbine.train,method = 'svmRadial', trCtrl = ctrl)
model4
#prediction
pred1 <- predict(model4, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)

# model training using tuning hyper parameter
tuneGrid <- expand.grid(C = c(0.25, .5, 1),sigma = 0.1)
model5 <- train(P_avg ~ .,data = turbine.train, method = 'svmRadial',trControl = ctrl,tuneGrid = tuneGrid)
model5

#prediction
pred1 <- predict(model5, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)
plot(pred1, turbine.test$P_avg)
