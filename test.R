# Load libraries
library(caret)  
library(randomForest)
library(ROCR)

# load data
data <- readRDS("RWCData_Full.rda")

set.seed(111)
# create training and test set
#testIndex <- seq(845,1017,1) 
#testIdex <- c(1:713)
train <- data[1:1593,]  
test <- data[1594:1685,]  


# tune Grid
rfGrid <-  expand.grid(mtry = c(1, 5, 7, 14, 20, 25, 30, 35, 40, 45, 48, 51))


# Tune using 5-fold cross-validation
fitControl <- trainControl(method = "cv",
                           number = 5,
                           repeats = 1)
# Train classifier
model <- train(x=train[,-1], y=train[,1], method="rf", ntree=1000,trControl=fitControl, tuneGrid=rfGrid)

#model <- randomForest(Outcome~., data=train,ntree=1000,  importance=TRUE,proximity=TRUE)

#varImpPlot(model)


tr <- predict(model, test, type="prob")
tr_raw <- predict(model, test, type="raw")

trpred <- prediction(tr[,2],test$Outcome)
trperf <- performance(trpred,"tpr","fpr")
plot(trperf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

result <- data.frame(tr,test)
write.csv(result,file="result.csv")
