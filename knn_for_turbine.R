## Loading Data
turbine <- read.csv("/Users/azimkhan22/Documents/r_doc/final_code/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]
library(caret)
set.seed(1)
model3 <- train(P_avg ~ .,data = turbine.train, method = 'knn')
model3
plot(model3)
#prediction
pred1 <- predict(model3, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)
# model training using cross validation, 10 cross validation
ctrl <- trainControl(method = "cv",number = 10)
model4 <- train(P_avg ~ .,data = turbine.train,method = 'knn', trCtrl = ctrl)
model4
#prediction
pred1 <- predict(model4, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)

# model training using tuinng hyperparameter
tuneGrid <- expand.grid(k = seq(2, 9, by = 1))
model5 <- train(P_avg ~ .,data = turbine.train, method = 'knn',trControl = ctrl,tuneGrid = tuneGrid)
model5
#prediction
pred1 <- predict(model5, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)
