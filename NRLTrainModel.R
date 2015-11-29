### International Rugby game prediction model ###

# load libraries
library(caret)
library(randomForest)

# load data
data <- readRDS("NRLData.rda")

# tune Grid
#rfGrid <-  expand.grid(mtry = c(1,2,4,5,7,10,15,20,25,28))

# Tune using 5-fold cross-validation
fitControl <- trainControl(method = "cv",
                           number = 5,
                           repeats = 1)

# Train classifier
model <- train(x=data[,-1], y=data[,1], method="rf", ntree=1000,trControl=fitControl)#, tuneGrid=rfGrid)
# save model
saveRDS(model, file = "NRL_Prediction_Model.rda")