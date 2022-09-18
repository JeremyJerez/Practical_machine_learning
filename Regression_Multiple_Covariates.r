library(ISLR); library(ggplot2);library(caret);
data(Wage); Wage <-subset(Wage,select=-c(logwage))
summary(Wage)

inTrain<- createDataPartition(y=Wage$wage,
                              p=0.7, list=FALSE)

#Get training/test set
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

#feature plot to see how variables are related to each other
featurePlot(x=training[,c("age","education","jobclass")],
            y=training$wage,
            plot="pairs")
#plotting age vs wage as it's relevant
qplot(age,wage,data=training)

#plot age vs wage colour by class
qplot(age,wage,colour=jobclass,data=training)

#plot age vs wage colour by education
qplot(age,wage,colour=education,data=training)

#fit a linear module with multiple variables on it
modfit <- train(wage ~ age + jobclass + education,
                method ="lm",data=training)
finmod <- modFit$finalModel
print(modfit)

#Diagnostic plots for regression models
plot(finmod,1,pch=19,cex=0.5,col="00000010")

#Color by variables not used in the model
qplot(finmod$fitted,finmod$residuals,colour=race,data=training)

#Plot by index
plot(finmod$residuals,pch=19)

#Predicted vs truth in test set
pred <- predict(modfit,testing)
qplot(Wage,pred,colour=year,data=testing)

#If you want to use all covariates> using a dot (.)
modFitAll<-train(wage~.,data=training,method="lm")
pred<-predict(modFitAll,testing)
qplot(wage,pred,data=testing)
