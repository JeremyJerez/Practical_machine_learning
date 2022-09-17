library(caret) ;data("faithful"); set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,
                              p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)

#making a plot of the variables eruptions and waiting time
plot(trainFaith$waiting, trainFaith$eruptions,pch=19,col="blue",xlab="waiting",
     ylab="Duration")

#fitting it to a linear model
lm1 <- lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)

#plotted model fit
plot(trainFaith$waiting, trainFaith$eruptions,pch=19,col="blue",xlab="waiting",
     ylab="Duration")
lines(trainFaith$waiting,lm1$fitted,lwd=3)

#predict a new value
coef(lm1)[1] + coef(lm1)[2]*180

#predict using Lm object
newdata <- data.frame(waiting=80)
predict(lm1,newdata)

#plot predictions-training and test
par(mfrow=c(1,2))
plot(trainFaith$waiting, trainFaith$eruptions,pch=19,col="blue",xlab="waiting",
     ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting, testFaith$eruptions,pch=19,col="blue",xlab="waiting",
     ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

#measures how close the fitted values are To the real values. 
#RMSE on training (root mean square error)
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))

#now RMSE on test set
sqrt(sum(predict(lm1,newdata=testFaith)-testFaith$eruptions)^2)

#prediction intervals
pred1 <- predict(lm1,newdata=testFaith,interval = "prediction")
ord<- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty=c(1,1,1),
         lwd = 3)
#same function, now with caret package
modFit<-train(eruptions ~ waiting,data=trainFaith,method="lm")
summary(modFit$finalModel)
