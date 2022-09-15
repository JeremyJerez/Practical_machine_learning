library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                              p=0.75, list=FALSE)

training <-spam[inTrain,]
testing <-spam[-inTrain,]
hist(training$capitalAve,main="",xlab="ave. capital run length")

mean(training$capitalAve)
sd(training$capitalAve)

#standardizing
trainCapAve <- training$capitalAve
trainCapAves <-(trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAves)
sd(trainCapAves)

#standardizing test set
testCapAve <- testing$capitalAve
testCapAves <-(testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAves)
sd(testCapAves)

#standardizing with- preProcessFunction
preObj <- preProcess(training[,-58],method = c("center", "scale"))
trainCapAves <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAves)
sd(trainCapAves)
