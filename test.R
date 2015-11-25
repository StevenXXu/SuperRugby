# Load libraries
library(caret)  
library(randomForest)
library(ROCR)

# load data
data <- readRDS("RWCData.rda")


# create training and test set
testIndex <- seq(1138,1352,1)  
train <- data[-testIndex,]  
test <- data[testIndex,]  


# tune Grid
rfGrid <-  expand.grid(mtry = c(1, 7, 14, 27, 40, 50))


# Tune using 5-fold cross-validation
fitControl <- trainControl(method = "cv",
                           number = 5,
                           repeats = 1)
# Train classifier
model <- train(x=train[,-1], y=train[,1], method="rf", ntree=1000,trControl=fitControl, tuneGrid=rfGrid)

tr <- predict(model, test, type="prob")


trpred <- prediction(tr[,2],test$Outcome)
trperf <- performance(trpred,"tpr","fpr")
plot(trperf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")