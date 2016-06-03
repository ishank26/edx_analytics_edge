setwd("C:\\Users\\Shantanu\\Desktop\\analytics_edge")
library(mice)
library(Rcpp)
data<-read.csv(file= "loans.csv")
head(data)
summary(data)
md.pattern(data)

set.seed(144)
vars.for.imputation = setdiff(names(data), "not.fully.paid")
imputed = complete(mice(data[vars.for.imputation]))
data[vars.for.imputation] = imputed

library(caTools)
spl=sample.split(data$not.fully.paid, 0.7)
train = subset(data,spl==TRUE)
test = subset(data,spl==FALSE)
logit=glm(not.fully.paid~.,data=train ,family="binomial")
summary(logit)
test$predicted.risk = predict(logit, newdata = test, type= "response") 
table(test$not.fully.paid,test$predicted.risk > 0.5)
library(ROCR)
ROCpred = prediction(test$predicted.risk, test$not.fully.paid)
ROCperf= performance(ROCpred,"tpr","fpr")
plot(ROCperf)
as.numeric(performance(ROCpred,"auc")@y.values)

##### int.rate
logit_bivar = glm(not.fully.paid ~ int.rate, data = train, family = "binomial")
test$pred.bivar = predict(logit_bivar, newdata= test, type="response")
max(test$pred.bivar)
ROCpred.bivar= prediction(test$pred.bivar,test$not.fully.paid)
as.numeric(performance(ROCpred.bivar,"auc")@y.values)

highint = subset(test, int.rate >= 0.15)
summary(highint)
table(highint$not.fully.paid)

cutoff=sort(highint$predicted.risk, decreasing=FALSE)[100]
selectedLoans=subset(highint,predicted.risk<=cutoff)
summary(selectedLoans)
nrow(selectedLoans)
table(selectedLoans$not.fully.paid)
