# Classification Tree with rpart
#packages required
#install.packages("rpart")
#install.packages("caTools")
#install.packages("pROC")

library(rpart)  #CART
library(rpart.plot)
library(caTools)
library(pROC)

###STEP 1 - LOADING DATA
data <-read.csv(file.choose())  #HR.csv
str(data)
summary(data)
data$left <- as.factor(data$left)
str(data)

###STEP 2 - SPLIT DATA INTO TRAIN and TEST - Stratified Sampling
set.seed(123)
newDataset <-sample.split(Y=data$left, SplitRatio = 0.7)
trainData <- data[newDataset,]
testData <- data[!newDataset,]

###STEP 3 - BUILD THE MODEL - Fit a DT using Training data
DTmodel<- rpart(left ~ ., method = "class", data = trainData,
                parms = list (split ="information gain"), 
                control = rpart.control(minsplit = 100, maxdepth = 4))  

rpart.plot(DTmodel, type = 3, extra = 3, fallen.leaves = F, cex = 0.6) #extra 2 4 8 101
DTmodel

###STEP 4 - USE THE MODEL TO MAKE PREDICTIONS ON TEST DATA
predTest <- predict(DTmodel, testData, type = "class")
probTest <- predict(DTmodel, testData, type = "prob")
actualTest <- testData$left

#Print 10 records
actualTest[1:10]
predTest[1:10]

#Add variables to Test Data
testData$Actual <- actualTest
testData$Predictions <- predTest
testData$Probability1 <- probTest[,2] #2nd column of the variable probTest 
testData$Probability0 <- probTest[,1] #1st column of the variable probTest 
testData$Probability <- ifelse(testData$Probability0 > testData$Probability1, testData$Probability0,testData$Probability1)

###STEP 5 - CALCULATE THE ACCURACY
t1 <- table(Predicted_Value = predTest, Actual_Value = actualTest)
t1
accuracy1 <- sum(diag(t1))/sum(t1)
accuracy1

### ROC and Area Under the Curve 
ROC <- roc(actualTest, probTest[,2])
plot(ROC, col="blue")
AUC <- auc(ROC)
AUC
