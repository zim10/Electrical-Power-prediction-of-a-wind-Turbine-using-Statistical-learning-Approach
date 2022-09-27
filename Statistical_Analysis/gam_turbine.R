## Loading Data
turbine <- read.csv("/Users/azimkhan22/Documents/r_doc/final_code/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]

#smoothing spline fit with GAM
library(gam)

turbine.gam <- gam(P_avg ~ s(Ws_avg) + s(Ws_max) + s(Ws_min) + s(Ws_std) + s(Wa_avg) +
                     s(Wa_max) + s(Wa_std) + s(Wa_min) + 
                     s(Ot_avg) + s(Ot_max) + s(Ot_min) + s(Ot_std), data = turbine.train)
summary(turbine.gam)
#plot
plot(turbine.gam, shade = TRUE, seWithMean = TRUE, scale = 0)

#prediction
pred1 <- predict(turbine.gam, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)

#natural spline
turbine.gam <- lm(P_avg ~ ns(Ws_avg,df=2) + ns(Ws_max,df=2) + ns(Ws_min,df=2) + ns(Ws_std,df=2) + ns(Wa_avg,df=2) +
                     ns(Wa_max,df=2) + ns(Wa_std,df=2) + ns(Wa_min, df=2) + 
                     ns(Ot_avg, df=2) + ns(Ot_max,df=2) + ns(Ot_min,df=2) + ns(Ot_std,df=2), data = turbine.train)


summary(turbine.gam)
plot(turbine.gam, se = TRUE)
#prediction
#prediction
pred1 <- predict(turbine.gam, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)