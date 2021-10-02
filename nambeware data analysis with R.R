install.packages("matlib")
library(GLMsData)
library(matlib)
data(leukwbc)
data("nambeware")
?nambeware

#EDA
nambeware$Type<-as.factor(nambeware$Type)
namglm<-glm(Price~.,family=inverse.gaussian(link=log),data=nambeware)
par(mfrow=c(2,2))
plot(namglm)
summary(namglm)
abline(namglm,lty=2)
plot(log(nambeware$Price),log(namglm$fitted.values))
confint(namglm)
namlm<-lm(log(Price)~.,data=nambeware)
summary(namlm)

par(mfrow=c(1,1))
plot(namglm$fitted.values,namglm$residuals,xlab="hat(mu)",ylab="Deviance residuals")
summary(namglm$residuals)
invpr<-1/(nambeware$Price)
mean(invpr)