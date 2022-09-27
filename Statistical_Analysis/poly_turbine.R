## Loading Data
turbine <- read.csv("/Users/azimkhan22/Documents/r_doc/final_code/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]

#polynomial regression
cor(turbine)
# wind speed with target variable (power) has highest correlation and it's 0.82, so here as polynomial, take wind speed as a feature variable
model_poly = lm(P_avg ~ poly(Ws_avg, 5), data = turbine.train)
summary(model_poly)

# Deciding  a degree. We can use hypothesis tests(e.g. ANOVA) to test nested sequence of models. 
poly_1 = lm(P_avg~Ws_avg, data = turbine.train)
poly_2 = lm(P_avg~poly(Ws_avg,2), data = turbine.train)
poly_3 = lm(P_avg~poly(Ws_avg,3), data = turbine.train)
poly_4 = lm(P_avg~poly(Ws_avg,4), data = turbine.train)
poly_5 = lm(P_avg~poly(Ws_avg,5), data = turbine.train)
print(anova(poly_1,poly_2,poly_3,poly_4,poly_5))

# fit a polynomial directly using degree 2 that selected by ANOVA
model_poly_fit=lm(P_avg~I(Ws_avg^2), data=turbine.train)
summary(model_poly_fit)
#prediction on test data

pred1 <- predict(model_poly_fit, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)
