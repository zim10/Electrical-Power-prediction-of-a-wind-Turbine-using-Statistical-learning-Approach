turbine <- read.csv("/Users/azimkhan22/Documents/r_doc/final_code/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
dim(turbine)
names(turbine) 
is.na(turbine) 


#best subset selection_method
library(leaps)
best_subset=regsubsets(P_avg~.,data=turbine,nvmax=12)
best.summary=summary(best_subset)
best.summary
names(best.summary)
  
plot(best.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(best.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(best.summary$adjr2)
points(11,best.summary$adjr2[11], col="red",pch=20)
plot(best.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(best.summary$cp)
points(11,best.summary$cp[11],col="red",pch=20)
plot(best.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(best.summary$bic)
points(7,best.summary$bic[7],col="red",pch=20)
coef(best_subset,7)


# Forward subset selection method
subset.fwd=regsubsets(P_avg~.,data=turbine,nvmax=12,method="forward")
summary(subset.fwd)
#Backward subset selection method
subset.bwd=regsubsets(P_avg~.,data=turbine,nvmax=12,method="backward")
summary(subset.bwd)

# features obtained using best subset
coef(best_subset,7)
reg.summary=summary(best_subset)
reg.summary$adjr2[7]
#features obtained using forward subset method
coef(subset.fwd,7)
reg.summary=summary(subset.fwd)
reg.summary$adjr2[7]
#features obtained using backward subset method
coef(subset.bwd,7)
reg.summary=summary(subset.bwd)
reg.summary$adjr2[7]


# Choosing Among Models

# Choosing Among Models validation set approach

set.seed(1)
train=sample(c(TRUE,FALSE), nrow(turbine),rep=TRUE)
#train
test=(!train)
#test
subset.best=regsubsets(P_avg~.,data=turbine[train,],nvmax=12)
test.mat=model.matrix(P_avg~.,data=turbine[test,])
val.errors=rep(NA,12)

# model selection based on minimum test error

for(i in 1:12){
  coefi=coef(subset.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((turbine$P_avg[test]-pred)^2)
}
val.errors
which.min(val.errors)

#10-fold cross-validation for model selection
regfit.best=regsubsets(P_avg~.,data=turbine,nvmax=12)
k=10
set.seed(1)
folds=sample(1:k,nrow(turbine),replace=TRUE)
folds
cv.errors=matrix(NA,k,12, dimnames=list(NULL, paste(1:12)))
cv.errors
for(j in 1:k){
  best.fit=regsubsets(P_avg~.,data=turbine[folds!=j,],nvmax=12)
  for(i in 1:12){
    pred=predict(best.fit,turbine[folds==j, ], id=i)
    cv.errors[j,i]=mean( (turbine$P_avg[folds==j]-pred)^2)
  }
}
cv.errors
#apply function "mean" for each column
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
plot(mean.cv.errors,type='l')
which.min(mean.cv.errors)
reg.best=regsubsets(P_avg~.,data=turbine, nvmax=12)
#7 best variable 
coef(reg.best,7)
plot(reg.best)

#  linear model with best 7 features

turbine <- read.csv("./Documents/R_code_804/turbine_std.csv", na.strings = "?", stringsAsFactors = T)
#View(turbine)
#Split the turbine data set (80% as a training data), 20% as a testing data
index <- sample(nrow(turbine), nrow(turbine) * 0.80)
turbine.train <- turbine[index, ]
turbine.test <- turbine[-index, ]
set.seed(1)
library(ISLR)
attach (turbine.train)
subset_fit=lm(P_avg~Ws_avg+Ws_min+Ws_std+Wa_avg+Wa_min+Wa_std+Ot_std,data=turbine.train)
summary(subset_fit)

#prediction on test data

pred1 <- predict(subset_fit, newdata = turbine.test)
rmse <- sqrt(sum((pred1 - turbine.test$P_avg)^2)/length(turbine.test$P_avg))
c(RMSE = rmse, R2=cor(pred1, turbine.test$P_avg) ^ 2)
#plot(turbine.test$P_avg, pred1)
plot(subset_fit)
