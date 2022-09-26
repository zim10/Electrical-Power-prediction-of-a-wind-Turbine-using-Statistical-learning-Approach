## Loading Data
turbine <- read.csv("/Users/azimkhan22/Documents/r_doc/final_code/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]

## Fitting Regression Trees
library(tree)
tree.turbine <- tree(P_avg ~ ., data = turbine.train)
summary(tree.turbine)
plot(tree.turbine)
text(tree.turbine, pretty = 0)
cv.turbine <- cv.tree(tree.turbine)
plot(cv.turbine$size, cv.turbine$dev, type = "b")
#pruning the tree
prune.turbine <- prune.tree(tree.turbine, best = 6)
plot(prune.turbine)
text(prune.turbine, pretty = 0)
#prediction
pred1 <- predict(tree.turbine, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)


## Bagging and Random Forests

library(randomForest)
set.seed(1)
bag.turbine <- randomForest(P_avg ~ ., data = turbine.train, mtry = 12, importance = TRUE)
bag.turbine
#prediction
pred1 <- predict(bag.turbine, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)
plot(pred1, turbine.test$P_avg)
### bagging another model, change number of tree
bag.turbine <- randomForest(P_avg ~ ., data = turbine.train, mtry = 12, ntree = 25)
#prediction
pred1 <- predict(bag.turbine, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)

### random forest reducing number of variables(mtry=6)
set.seed(1)
rf.turbine <- randomForest(P_avg ~ ., data = turbine.train, mtry = 6, importance = TRUE)
#prediction
pred1 <- predict(rf.turbine, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)
###importance of variable
importance(rf.turbine)
### plot
varImpPlot(rf.turbine)


## Boosting
library(gbm)
set.seed(1)
boost.turbine <- gbm(P_avg ~ ., data = turbine.train,distribution = "gaussian", n.trees = 5000,interaction.depth = 4)
summary(boost.turbine)
###plot
plot(boost.turbine, i = "Ws_avg")
plot(boost.turbine, i = "Ot_avg")

#prediction
pred1 <- predict(boost.turbine, newdata = turbine.test, n.trees = 5000)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)

### boosting another model
boost.turbine <- gbm(P_avg ~ ., data = turbine.train,distribution = "gaussian", n.trees = 5000,interaction.depth = 4, shrinkage = 0.2, verbose = F)
summary(boost.turbine)

#prediction
pred1 <- predict(boost.turbine, newdata = turbine.test, n.trees = 5000)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)



