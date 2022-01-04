# Loading the dataset
trainData_original = read.csv("pml-training.csv")
validData_original = read.csv("pml-testing.csv")

# loading the libraries
# Libraries
library(dplyr)
library(caret)
library(gbm)
library(randomForest)
library(rpart)
library(rattle)
# only grabbng the numeric columns

numcoltrain = sapply(trainData_original, is.numeric)
numcoltest = sapply(validData_original, is.numeric)

traindata = trainData_original[, numcoltrain ]
validData = validData_original[,numcoltest]



#we need to predict on the the test data
# so only takes the columns from trainselt which are in common with test set
colset = colnames(validData)


traindata = traindata %>% select(colset[-57])

# scale training data
traindata = scale(traindata)
traindata = as.data.frame(traindata)
#adding the predictor variable
traindata$classe = trainData_original$classe

traindata$classe = factor(traindata$classe)

set.seed(123)
intrain = createDataPartition(y=traindata$classe, p=.6, list = F )

traindata = traindata[intrain,]
testdata = traindata[-intrain,]

fitControl = trainControl(method='cv', number = 3)

#testing Decision Tree Model

modelDT = rpart(classe ~., data=traindata)
modelDT = train(classe~., traindata, method = "rpart")
# piloting the Decision Tree Model
fancyRpartPlot(modelDT)

pred_classDT = predict(modelDT, testdata)


confusionMatrix(pred_classDT, testdata$classe)$overall[1]


# testing Random Forest model

modelRF = train(classe ~ ., data= traindata, method = "rf", ntree=100)

pred_classRF = predict(modelRF, testdata)


confusionMatrix(pred_classRF, testdata$classe)$overall[1]

newdata = scale(validData)
newdata= as.data.frame(newdata)

pred_validation = predict(modelRF, newdata = newdata[-57])



