## Loading Data
turbine <- read.csv("/Users/azimkhan22/Documents/r_doc/final_code/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]
#local regression
library(splines)
library(tidyverse)
cor(turbine)
# wind speed with target variable (power) has highest correlation and it's 0.82, so here as polynomial, take wind speed as a feature variable
# loess: local regression, span controls the degree of smoothing
mod_loess = loess(P_avg ~ Ws_avg,span = 0.2,degree = 1,data = turbine.train)
summary(mod_loess)
turbine.train %>% 
  mutate(pred = mod_loess$fitted) %>% 
  ggplot(aes(Ws_avg, P_avg)) +
  geom_point() +
  geom_line(aes(Ws_avg,pred), col="red", lwd=1.5) 

#prediction on test data
pred1 <- predict(mod_loess, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)
