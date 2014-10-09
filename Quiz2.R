# Quiz 2 - Question 2

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[inTrain, ]
testing = mixtures[-inTrain, ]

summary(training)  # view summary of training data

dim(training);dim(testing)

featurePlot(x=training[,1:8],y=training$CompressiveStrength,plot="pairs")

index=seq(1,nrow(training))

qplot(index,training$CompressiveStrength,data=training)  # clearly shows steps

qplot(index,training$CompressiveStrength,colour=Age,data=training)  #no clear definition
qplot(index,training$CompressiveStrength,colour=FlyAsh,data=training) #no clear definition

# Quiz 2 - Question 3

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
qplot(training$Superplasticizer,data=training,geom="histogram") # histogram shows approx 300 samples  with Superplasticizer = 0

qplot(log(training$Superplasticizer+1),data=training,geom="histogram") # still shows skewed histogram

# Quiz 2 - Question 4

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_columns <- grep("^IL", colnames(training), value = TRUE)
preProc <- preProcess(training[, IL_columns], method = "pca", thresh = 0.8)
preProc$numComp # displays 7, or just use preProc
preProc    # outputs PCA needed 7 components to capture 80 percent of the variance

