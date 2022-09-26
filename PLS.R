## Loading Data
turbine <- read.csv("/Users/azimkhan22/Documents/r_doc/final_code/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]

# Partial Least Squares
library(pls)
set.seed(2)
pls.fit=plsr(P_avg~., data=turbine.train,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")

#model with  using R-tool package (caret)
library(caret)
set.seed(1)
#cross validation is used on training data set
ctrl <- trainControl(method = "cv",number = 10)
# for tuning the component
tuneGrid <- expand.grid(ncomp   = seq(1, 10, by = 1))
model_pls <- train(P_avg ~ .,data = turbine.train,method = 'pls',
                   preProcess = c("center", "scale"),trControl = ctrl,tuneGrid = tuneGrid)
model_pls

#prediction using pls model
#remove the target variable from turbine test set
test.features = subset(turbine.test, select=-c(P_avg))
#ground truth value
test.target = subset(turbine.test, select=P_avg)[,1]
predictions = predict(model_pls, newdata = test.features)
# TEST RMSE
sqrt(mean((test.target - predictions)^2))
# Test R2
cor(test.target, predictions) ^ 2

