## Loading Data
turbine <- read.csv("/Users/azimkhan22/Documents/r_doc/final_code/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]

#Lasso Regression 
library(ISLR)
x=model.matrix(P_avg~.-1,data=turbine.train)
y=turbine.train$P_avg

# The Lasso, default alpha=1
fit.lasso=glmnet(x,y)
plot(fit.lasso, xvar="lambda", label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)
lasso.tr=glmnet(x,y)
lasso.tr

#prediction
#for test data
x_test=model.matrix(P_avg~.-1,data=turbine.test)
y_test=turbine.test$P_avg

pred=predict(lasso.tr, x_test)
pred
rmse=sqrt(apply((y_test-pred)^2,2,mean))
rmse
plot(log(lasso.tr$lambda), rmse, type="b", xlab="log(lambda)")
lam.best=lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr, s=lam.best)




#ridge regression using R-caret package
library(caret)
model3 <- train(
  P_avg ~ .,
  data = turbine.train,
  method = 'lasso',
)
model3
set.seed(1)

tuneGrid <- expand.grid(.fraction = seq(0, 1, by = 0.1))
ctrl <- trainControl(method = "cv",number = 10)

model5 <- train(
  P_avg ~ .,
  data = turbine.train,
  method = 'lasso',
  #preProcess = c("center", "scale"),
  trControl = ctrl,
  tuneGrid = tuneGrid
)
model5
plot(model5)

test.features = subset(turbine.test, select=-c(P_avg))
test.target = subset(turbine.test, select=P_avg)[,1]

predictions = predict(model5, newdata = test.features)

# Test-RMSE
sqrt(mean((test.target - predictions)^2))
# Test-R2
cor(test.target, predictions) ^ 2
