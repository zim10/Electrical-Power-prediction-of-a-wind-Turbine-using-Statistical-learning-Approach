## Loading Data
turbine <- read.csv("./Documents/R_code_804/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]

#preprocess the test featurs, remove the features that are not important
test.features = subset(turbine.test, select=-c(P_avg,Ws_max,Wa_max,Ot_min,Ot_max,Ot_avg))
test.target = subset(turbine.test, select=P_avg)[,1]
names(test.features)
set.seed(1)
library(ISLR)
attach (turbine.train)
lm.fit=lm(P_avg~.,data=turbine.train)
summary(lm.fit)

# remove the less significant feature from the model based on p value
lm.fit2 = update(lm.fit, ~.-Ws_max-Wa_max-Ot_min-Ot_max-Ot_avg) 
summary(lm.fit2) 

#prediction on test data
pred1 <- predict(lm.fit2, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)
plot(turbine.test$P_avg, pred1)
plot(lm.fit2)

#another way to predict
predictions = predict(lm.fit2, newdata = test.features)
# RMSE
sqrt(mean((test.target - predictions)^2))
# R2
cor(test.target, predictions) ^ 2





