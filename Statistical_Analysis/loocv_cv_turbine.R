## Loading Data
turbine <- read.csv("./Documents/R_code_804/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]
dim(turbine.train)
set.seed(1)
library(boot)
glm.fit=glm(P_avg~.,data=turbine.train) # glm- generalize linear modle
#default is leave one out cross validation if K is unspecified
cv.err=cv.glm(turbine.train,glm.fit)
cv.err$delta # delta is the cross validation result

cv.error=rep(0,5) # 5 fold cross validation
for (i in 1:5){
  glm.fit=glm(P_avg~poly(Ws_avg,i),data=turbine.train)
  cv.error[i]=cv.glm(turbine.train,glm.fit)$delta[1]
}
cv.error
degree=1:5
#type="b": plot for both points and lines
plot(degree,cv.error, type="l")
#it will take huge time,as train data (9600, 13)
#so here i used 10 fold cros validatin

#10 fold cross validation
set.seed(17) # this is the random number that computer generates.
cv.error.10=rep(0,10) # 10 cross validation
for (i in 1:10){
  glm.fit=glm(P_avg~poly(Ws_avg,i),data=turbine.train)
  cv.error.10[i]=cv.glm(turbine.train,glm.fit,K=10)$delta[1]
}
cv.error.10
degree=1:10

#type="b": plot for both points and lines
plot(degree,cv.error.10, type="l")

#type="b": plot for both points and lines
plot(degree,cv.error.10, type="l")
which.min(cv.error.10)
#Cross Validation
cv.glm(data = turbine.train, glmfit = glm.fit, K = 9)$delta[2]

#model train using k fold cross validation
library(caret)
set.seed(1)
ctrl <- trainControl(
  method = "cv",
  number = 10,
)

model1 <- train(
  P_avg ~ .,
  data = turbine.train,
  method = 'lm',
  trControl = ctrl
)
(model1)
#prediction on test data
test.features = subset(turbine.test, select=-c(P_avg))
test.target = subset(turbine.test, select=P_avg)[,1]

predictions = predict(model4, newdata = test.features)
# RMSE
sqrt(mean((test.target - predictions)^2))
# R2
cor(test.target, predictions) ^ 2
