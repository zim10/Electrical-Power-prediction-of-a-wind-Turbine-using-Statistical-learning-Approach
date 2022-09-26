## Loading Data
turbine <- read.csv("./users/azimkhan22/Documents/r_doc/turbine_std.csv", na.strings = "?", stringsAsFactors = T)

# number of observations, or rows and variables or columns
dim(turbine)
names(turbine) 
# to check missing value
is.na(turbine) 
#to remove the missing value
turbine <- na.omit(turbine)
names(turbine)
#The Validation Set Approach
#random split into train and test
set.seed(1)
# 50%-%50% split for training and testing (training data half of the data)
train=sample(12000,6000)
lm.fit=lm(P_avg~Ws_avg+Ws_min+Ws_std+Wa_avg+Wa_min+Wa_std+Ot_std,data=turbine,subset=train) # linear model
attach(turbine)
#compute MSE on testing data
mean((P_avg-predict(lm.fit,turbine))[-train]^2)
# MSE for multiple linear model is = 18937.98

# another model, power of degree of 2
lm.fit2=lm(P_avg~Ws_avg+Ws_min+Ws_std+Wa_avg+Wa_min+Wa_std+Ot_std+
             I(Ws_avg^2)+I(Ws_min^2)+I(Ws_std^2)+I(Wa_avg^2)+I(Wa_min^2)+
             I(Wa_std^2)+I(Ot_std^2),data=turbine,subset=train) 
# now mean square error
mean((P_avg-predict(lm.fit2,turbine))[-train]^2) 
#MSE for power of degree of 2 is =  3237.284, mean square error lower than before

# Leave-One-Out Cross-Validation # 

glm.fit=glm(P_avg~Ws_avg,data=turbine) # glm- generalize liner model
coef(glm.fit)
lm.fit=lm(P_avg~Ws_avg,data=turbine) # lm- linear model
coef(lm.fit)
library(boot)
glm.fit=glm(P_avg~Ws_avg,data=turbine) # glm- generalize linear modle
#default is leave one out cross validation if K is unspecified
cv.err=cv.glm(turbine,glm.fit)
?cv.glm # cross validation , help ?
cv.err$delta # delta is the cross validation result
# for 5 fold, difference will be better
#use cv to evaluate Polynomial models with different degrees
cv.error=rep(0,5) # 5 fold cross validation
for (i in 1:5){
  glm.fit=glm(P_avg~poly(Ws_avg,i),data=turbine)
  cv.error[i]=cv.glm(turbine,glm.fit)$delta[1]
}
cv.error
degree=1:5
#type="b": plot for both points and lines
plot(degree,cv.error, type="b")

# 10-Fold Cross-Validation

set.seed(17) # this is the random number that computer generates.
cv.error.10=rep(0,10) # 10 cross validation
for (i in 1:10){
  glm.fit=glm(P_avg~poly(Ws_avg,i),data=turbine)
  cv.error.10[i]=cv.glm(turbine,glm.fit,K=10)$delta[1]
}
cv.error.10
degree=1:10

#type="b": plot for both points and lines
plot(degree,cv.error.10, type="l")
# if we don't specify k, default k =5 
#  [1] 72159.23 59722.22 19620.52 19518.22 13788.71 13448.09 12896.75 12872.69 12865.14
#[10] 12772.77

#k-fold cros validation for multiple features
glm.fit=glm(P_avg~Ws_avg+Ws_min+Ws_std+Wa_avg+Wa_min+Wa_std+Ot_std,data=turbine)
 
# 10-Fold Cross-Validation
set.seed(17) # this is the random number that computer generates.
cv.error.10=rep(0,10) # 10 cross validation
for (i in 1:10){
  glm.fit=glm(P_avg~poly(Ws_avg,i)+poly(Ws_min,i)+poly(Ws_std,i)+
                poly(Wa_avg,i)+poly(Wa_min,i)+poly(Wa_std)+poly(Ot_std),data=turbine)
  cv.error.10[i]=cv.glm(turbine,glm.fit,K=10)$delta[1]
}
cv.error.10
degree=1:10

#type="b": plot for both points and lines
plot(degree,cv.error.10, type="b")



