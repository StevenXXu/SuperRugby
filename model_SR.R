## Load Library
library(caret)
library(randomForest)
library(lubridate)

## Load Pre-trained model
model <- readRDS(file = "RWC_Prediction_Model.rda")

## Load predictors
teamStats <- readRDS(file="teamStats.rda")
rank <- readRDS("rank.rda")
rankScore <- readRDS("rankScore.rda")
#rankChange <- readRDS("rankChange.rda")
teams <- readRDS("teamNames.rda")

## Create a function to take team name inputs
## and then return a prediction for the match
predict_match <- function(homeTeam, awayTeam, month, year) {
  
  homeIndex <- which(teams == homeTeam)
  awayIndex <- which(teams == awayTeam) 
  
 swap <- awayTeam == "England"
  
 if(swap){
    
   temp <- homeTeam
   homeTeam <- awayTeam
   awayTeam <- temp
    
   temp <- homeIndex
   homeIndex <- awayIndex
   awayIndex <- temp
    
 }
  
  
  homeTeamStats <- c(teamStats[[homeIndex]], rank[homeIndex], rankScore[homeIndex])
  awayTeamStats <- c(teamStats[[awayIndex]], rank[awayIndex], rankScore[awayIndex])
  
  date <- Sys.Date()
  levelsx <- levels(factor(teams))
  levelsy <- levels(factor(c("loose","win")))
  newCase <- readRDS("newCase.rda")
  newCase[1,2] <- homeTeam
  newCase[1,3] <- awayTeam
  newCase[1,4] <- date
  newCase[1,5] <- factor(month)
  newCase[1,6] <- factor(year)
  newCase[1,7:29] <- homeTeamStats
  newCase[1,30:52] <- awayTeamStats
  
  
  ## Use the model for prediction
  
  
  ## Return the predicted class
  if(swap){
    y_probs <- predict(model, newCase, type="prob")
    return(as.character(rev(y_probs)))
  } else if(homeTeam == "England") {
    y_probs <- predict(model, newCase, type="prob")
    return(as.character(y_probs))
  } else {
    y_probs1 <- predict(model, newCase, type="prob")
    
    temp <- homeIndex
    homeIndex <- awayIndex
    awayIndex <- temp
    
    homeTeamStats <- c(teamStats[[homeIndex]], rank[homeIndex], rankScore[homeIndex])
    awayTeamStats <- c(teamStats[[awayIndex]], rank[awayIndex], rankScore[awayIndex])
    
    date <- Sys.Date()
    levelsx <- levels(factor(teams))
    levelsy <- levels(factor(c("loose","win")))
    newCase <- readRDS("newCase.rda")
    newCase[1,2] <- homeTeam
    newCase[1,3] <- awayTeam
    newCase[1,4] <- date
    newCase[1,5] <- factor(month)
    newCase[1,6] <- factor(year)
    newCase[1,7:29] <- homeTeamStats
    newCase[1,30:52] <- awayTeamStats
    
    y_probs2 <- predict(model, newCase, type="prob")
    looseProb <- round((y_probs1[1] + y_probs2[2])/2,4)
    winProb <- round((y_probs1[2] + y_probs2[1])/2,4)
    if(looseProb>winProb) {
      return ("loose")
    } else if(looseProb<winProb) {
      return ("win")
    }else{
      return ("tie")
    }    
#    return(as.character(c(looseProb, winProb)))
  }
}

predict_round <- function(filename){
  
  data_predict <- read.csv(file=filename,header=FALSE)
  pred<-list()
  
  for(i in 1:nrow(data_predict)){
    
    home_Team <- data_predict[i,2]
    away_Team <- data_predict[i,3]
    m <- data_predict[i,4]
    y <- data_predict[i,5]
    
    
    pred<-append(pred,predict_match(home_Team,away_Team,m,y))
  }
  
  write.csv(pred,file="R2015_SR_pred.csv")
  
}