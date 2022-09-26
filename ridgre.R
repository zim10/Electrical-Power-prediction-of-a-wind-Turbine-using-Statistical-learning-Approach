## Loading Data
turbine <- read.csv("/Users/azimkhan22/Documents/r_doc/final_code/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]

#Ridge Regression 
library(ISLR)
x=model.matrix(P_avg~.-1,data=turbine.train)
y=turbine.train$P_avg
#for test data
x_test=model.matrix(P_avg~.-1,data=turbine.test)
y_test=turbine.test$P_avg

library(glmnet)
#alpha=0 for Ridge regression 
grid =10^ seq (10 , -2 , length =100)
fit.ridge=glmnet(x,y,alpha=0, lambda = grid)
summary(fit.ridge)
plot(fit.ridge, xvar="lambda", label=TRUE)
#use cross validation to choose the tuning parameter lamda
set.seed (1)
cv.out = cv.glmnet(x,y,alpha =0)
cv.out
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
#fit again using bestlamda
fit.ridge=glmnet(x,y,alpha=0, lambda = 21)
# prediction
ridge.pred = predict (fit.ridge , s = bestlam , newx = x_test)

# test RMSE
sqrt(mean (( ridge.pred - y_test ) ^2))

# test R2
cor(ridge.pred, y_test) ^ 2


#ridge regression using R-caret package
library(caret)
model3 <- train(
  P_avg ~ .,
  data = turbine.train,
  method = 'ridge',
)
model3
set.seed(1)

tuneGrid <- expand.grid(
  .lambda = seq(0, .1, by = 0.01)
)

set.seed(1)
ctrl <- trainControl(
  method = "cv",
  number = 10,
)
tuneGrid <- expand.grid(
  .lambda = seq(0, .1, by = 0.01)
)
model5 <- train(
  P_avg ~ .,
  data = turbine.train,
  method = 'ridge',
  #preProcess = c("center", "scale"),
  trControl = ctrl,
  tuneGrid = tuneGrid
)
model5
plot(model5)

test.features = subset(turbine.test, select=-c(P_avg))
test.target = subset(turbine.test, select=P_avg)[,1]

predictions = predict(model5, newdata = test.features)

# RMSE
sqrt(mean((test.target - predictions)^2))
# R2
cor(test.target, predictions) ^ 2
