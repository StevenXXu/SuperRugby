# Load libraries
library(caret)  
library(randomForest)
library(ROCR)

# load data
data <- readRDS("NRLData.rda")
#data<-read.csv(file="NRLData2005_2015.csv")

set.seed(111)
# create training and test set
testIndex <- seq(1,as.integer(0.9 * nrow(data))) 
#testIdex <- c(1:713)
train <- data[testIndex,]  
test <- data[-testIndex,]  
test[,4]<-0
test[,5]<-0
test[,9:14]<-0


# tune Grid
rfGrid <-  expand.grid(mtry = c(2))


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
write.csv(result,file="result_NRL.csv")
