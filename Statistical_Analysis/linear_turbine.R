## Loading Data
turbine <- read.csv("/Users/azimkhan22/Documents/r_doc/final_code/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]
dim(turbine.train)
dim(turbine.test)
set.seed(1)
library(ISLR)
attach (turbine.train)
lm.fit=lm(P_avg~Ws_avg,data=turbine.train)
summary(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,turbine.test, interval="confidence")
#95% confidence interval associated with a wind_speed value of 10 is (1093,1100)
plot(Ws_avg,P_avg)

#prediction on test data
pred1 <- predict(lm.fit, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)

#adding interaction with simple linear model
# Interaction Terms
summary(lm(P_avg~Ws_avg*Wa_avg,data=turbine.train))

