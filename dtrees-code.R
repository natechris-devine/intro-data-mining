# Classification Tree with rpart
# packages required
# install.packages("rplot.plot")
# install.packages("pROC")

library(rpart)
library(rpart.plot)
library(caTools)
library(pROC)

###STEP 1 -LOADING DATA
rm(list=ls()) # clear environment
data <- read.csv("HR.csv")
str(data)
summary(data)
View(data)

# left is our target variable: make category
data$left = as.factor(data$left)
data$Work_accident = as.factor(data$Work_accident)

###STEP 2 - SPLIT data into TRAIN and TEST - Stratified Sampling
set.seed(123) # sets predictable seed val for data split
newDataset <- sample.split(Y=data$left, SplitRatio = 0.7)
trainData <- data[newDataset,]
testData <- data[!newDataset,]

###STEP 3 - BUILD THE MODEL - Fit a DT using Training Data
# The ~ separates [target ~ input variables]. '.' is a catch-all
# DTModel <- rpart(left ~ salary+satisfaction_level,method = "class", data = trainData, parms = list (split="information gain"))

DTModel <- rpart(left ~.,method = "class", data = trainData,
                 parms = list (split="information gain"), 
                 control = rpart.control(minsplit = 100, maxdepth = 4))


rpart.plot(DTModel, type=3, extra = 101, fallen.leaves = F, cex = 0.7) #extra describes what data leaves will display

###STEP 4 - USE THE MODEL TO MAKE PREDICTIONS OF TEST DATA
predTest <- predict(DTModel, testData, type = "class")
probTest <- predict(DTModel, testData, type = "prob")
View(predTest, "Class Predictions")
View(probTest, "Class Probability Predictions")

actualTest <- testData$left

actualTest[0:10]
predTest[0:10]
tail(actualTest)
tail(predTest)

###STEP 5 - CALCULATE THE ACCURACY
t1 <- table(predictions = predTest, actual = actualTest)
t1 # See the table

accuracy1 = sum(diag(t1))/sum(t1) # get the accuracy of the model
accuracy1

# side by side comparison in test data
testData$predictions <- predTest
testData$actual <- actualTest
View(testData)

# adding probabilites
testData$prob0 <- probTest[,"0"]
testData$prob1 <- probTest[,"1"]

### ROC and Area Under the Curve
ROC <- roc(actualTest, probTest[,2])
plot(ROC, col="blue")
AUC <- auc(ROC)
AUC
