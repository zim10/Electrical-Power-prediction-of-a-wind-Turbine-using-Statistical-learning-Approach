## Loading Data
turbine <- read.csv("/Users/azimkhan22/Documents/r_doc/final_code/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]

#splines
library(splines)
library(tidyverse)
cor(turbine)
# wind speed with target variable (power) has highest correlation and it's 0.82, so here as polynomial, take wind speed as a feature variable

# Build spline model using knots
knots <- quantile(turbine.train$Ws_avg, p = c(0.25, 0.5, 0.75))
model <- lm (P_avg ~ bs(Ws_avg, knots = knots), data = turbine.train)
summary(model)
#plot 
ggplot(turbine.train, aes(Ws_avg, P_avg) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 2))

#make prediction
pred1 <- predict(model, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)


#smoothing spline and use tuning parameter lamda
model_spline = smooth.spline(y = turbine.train$P_avg,
                             x = turbine.train$Ws_avg,
                             lambda = 0.01) 
model_spline

#model2, different lamda
mod_ss2 = smooth.spline(y = turbine.train$P_avg,
                        x = turbine.train$Ws_avg,
                        lambda = 1) 
mod_ss2
#cross-validation to select the best lamda for model flexibility.
mod_ss3 = smooth.spline(y = turbine.train$P_avg,
                        x = turbine.train$Ws_avg,
                        cv = T) 
mod_ss3
mod_ss3$lambda

# plot fitted model

turbine.train %>% 
  mutate(pred1 = fitted(model_spline),
         pred2 = fitted(mod_ss2),
         pred3 = fitted(mod_ss3)) %>% 
  ggplot(aes(Ws_avg, P_avg)) +
  geom_point() +
  geom_line(aes(y=pred1),col="blue",lwd=1.2) +
  geom_line(aes(y=pred2),col="red",lwd=1.2) +
  geom_line(aes(y=pred3),col="darkgreen",lwd=1.2)  

#prediction on test data for best fiited mod_ss3
ss_pred = predict(mod_ss3, turbine.test$Ws_avg)
# Model performance
ss_perf = data.frame(
  RMSE = sqrt(sum((ss_pred$y- turbine.test$P_avg)^2)/length(turbine.test$P_avg)),
  COR = cor(ss_pred$y, turbine.test$P_avg)
)
ss_perf 



