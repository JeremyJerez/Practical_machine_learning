#using data from ISLR
library(ISLR); library(ggplot2); library(caret);
library(Hmisc); library(gridExtra)
data(Wage)
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,
                              p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

featurePlot(x=training[,c("age","education", "jobclass")],
            y=training$wage,
            plot="pairs")

#qplot(age, wage, colour=jobclass, data=training)

qq <- qplot(age, wage, colour=education, data=training)
#qq + geom_smooth(method='lm', formula=x~y)

cutWage <- cut2(training$wage, g=4)
table(cutWage)

p1 <- qplot(cutWage, age, data=training,fill=cutWage,
            geom=c("boxplot"))

p2 <- qplot(cutWage, age, data=training,fill=cutWage,
           geom=c("boxplot", "jitter"))

#grid.arrange(p1, p2, ncol = 2)

t1 <-table(cutWage, training$jobclass)

prop.table(t1,1)

#density
#qd <- qplot(Wage,colour=education,data=training,geom="density")

