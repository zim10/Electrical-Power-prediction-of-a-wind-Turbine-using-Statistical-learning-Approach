## Loading Data
turbine <- read.csv("/Users/azimkhan22/Documents/r_doc/final_code/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]

# Principal Components Regression
library(pls)
set.seed(2)
pcr.fit=pcr(P_avg~., data=turbine.train,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
turbine.predictors <- subset(turbine, select=-c(P_avg))
turbine.pca <- prcomp(turbine.predictors, center = TRUE, scale = TRUE)
plot(turbine.pca)

#using 7 PCA that explained 95% variance of the data

pcr7.fit = pcr (P_avg~., data=turbine.train,scale = TRUE ,ncomp =7)
summary(pcr7.fit)

#prediction using 7 PCR model
#remove the target variable from turbine test set
test.features = subset(turbine.test, select=-c(P_avg))
#ground truth value
test.target = subset(turbine.test, select=P_avg)[,1]

pcr.pred = predict( pcr7.fit , test.features , ncomp =7)
# TEST RMSE
sqrt(mean((test.target - pcr.pred)^2))
# Test R2
cor(test.target, pcr.pred) ^ 2


#model with PCR using R-tool package (caret)
library(caret)
set.seed(1)
#cross validation is used on training data set
ctrl <- trainControl(method = "cv",number = 10)
model_pcr <- train(P_avg ~ .,data = turbine.train,method = 'lm',
          preProcess = c("center", "scale", "pca"),trControl = ctrl)
model_pcr

#prediction using pcr model
#remove the target variable from turbine test set
test.features = subset(turbine.test, select=-c(P_avg))
#ground truth value
test.target = subset(turbine.test, select=P_avg)[,1]

predictions = predict(model_pcr, newdata = test.features)
# TEST RMSE
sqrt(mean((test.target - predictions)^2))
# Test R2
cor(test.target, predictions) ^ 2

