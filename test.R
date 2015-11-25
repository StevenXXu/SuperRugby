# Load libraries
library(caret)  
library(randomForest)
library(ROCR)

# load data
data <- readRDS("RWCData.rda")

set.seed(111)
# create training and test set
#testIndex <- seq(845,1017,1) 
#testIdex <- c(1:713)
train <- data[1:731,]  
test <- data[732:1017,]  


# tune Grid
rfGrid <-  expand.grid(mtry = c(1, 5, 10, 14, 20, 25, 30, 35, 40, 45, 50, 55))


# Tune using 5-fold cross-validation
fitControl <- trainControl(method = "cv",
                           number = 5,
                           repeats = 1)
# Train classifier
model <- train(x=train[,-1], y=train[,1], method="rf", ntree=1000,trControl=fitControl, tuneGrid=NULL)

tr <- predict(model, test, type="raw")


trpred <- prediction(tr,test$Outcome)
trperf <- performance(trpred,"tpr","fpr")
plot(trperf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")