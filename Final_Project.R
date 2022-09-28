#This it's my approach to the final project for practical machine learning
#using R

#Load  the libraries
library(caret)
library(corrplot)

#Random seed
set.seed(721)

#Remember to set the folder with the data as working directory
train_start <- read.csv("d:/onedrive/R/pml-training.csv")
test_start <- read.csv("d:/onedrive/R/pml-testing.csv")

#in case you want to see the summary of each
#summary(train_start)
#summary(test_start)

#check the dimensions for training and test data
dim(train_start); dim(test_start)

#remove N/A variables and others that are unnecessary
train_start <- train_start[,colMeans(is.na(train_start)) < .9]
#removing metadata that isn't useful
train_start <- train_start[,-c(1:7)]

#Now more cleaning by removing zero variance variables
nzv <- nearZeroVar(train_start)
train_start <- train_start[,-nzv]
dim(train_start)

#create training and test sets
inTrain <- createDataPartition(y=train_start$classe, p=0.7, list=FALSE)
training <- train_start[inTrain, ]
testing <- train_start[-inTrain, ]

dim(training); dim(testing)

#now we enter the creating and testing models Phase


#control for training, cross validation.

control <- trainControl(method="cv", number=3, verboseIter=F)

#Training
modFit <- train(classe~., data=training, method="rf", 
                trControl = control, tuneLength = 5)

modFit

#The final value used for the model was mtry = 14.

#Now we will use random forest method

pred <- predict(modFit, testing)
confmx <- confusionMatrix(pred, factor(testing$classe))
confmx

#Correlation Plot
Plotting <- cor(training[, -length(names(training))])
corrplot(Plotting, method="color")

#finally, running the predictions on the test set for the classe
pred_fin <- predict(modFit, test_start)
print(pred_fin)
