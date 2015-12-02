## Load Library
library(caret)
library(randomForest)
library(lubridate)

## Load Pre-trained model
model <- readRDS(file = "NRL_Prediction_Model.rda")

## Load predictors
teamStats <- readRDS(file="NRLteamStats.rda")
teams <- readRDS(file="NRLTeams.rda")

## Create a function to take team name inputs
## and then return a prediction for the match
predict_match <- function(homeTeam, awayTeam, month, year) {
  
  homeIndex <- which(teams == homeTeam)
  awayIndex <- which(teams == awayTeam) 
  
  #swap <- awayTeam == "England"
  
  #if(swap){
    
  #  temp <- homeTeam
  #  homeTeam <- awayTeam
  #  awayTeam <- temp
    
  #  temp <- homeIndex
  #  homeIndex <- awayIndex
  #  awayIndex <- temp
    
  #}
  
  
  homeTeamStats <- teamStats[homeIndex,3:5]
  awayTeamStats <- teamStats[awayIndex,7:9]
  
  date <- Sys.Date()
  #levelsx <- levels(factor(teams))
  #levelsy <- levels(factor(c("loose","win")))
  newCase <- readRDS("newCase_NRL.rda")
  newCase[1,2] <- homeTeam
  newCase[1,3] <- awayTeam
  newCase[1,6] <- date
  newCase[1,7] <- factor(month)
  newCase[1,8] <- factor(year)
  newCase[1,9:11] <- homeTeamStats
  newCase[1,12:14] <- awayTeamStats
  
  
  ## Use the model for prediction
  
  
  ## Return the predicted class
  
    y_probs1 <- predict(model, newCase, type="prob")
    
    temp <- homeTeam
    homeTeam <- awayTeam
    awayTeam <- temp
    
    homeTeamStats <- teamStats[homeIndex,3:5]
    awayTeamStats <- teamStats[awayIndex,7:9]
    
    date <- Sys.Date()
    #levelsx <- levels(factor(teams))
    #levelsy <- levels(factor(c("loose","win")))
    newCase <- readRDS("newCase_NRL.rda")
    newCase[1,2] <- homeTeam
    newCase[1,3] <- awayTeam
    newCase[1,6] <- date
    newCase[1,7] <- factor(month)
    newCase[1,8] <- factor(year)
    newCase[1,9:11] <- homeTeamStats
    newCase[1,12:14] <- awayTeamStats
    
    y_probs2 <- predict(model, newCase, type="prob")
    looseProb <- round((y_probs1[1]*0.49 + y_probs2[2]*0.51)/2,4)
    winProb <- round((y_probs1[2]*0.51 + y_probs2[1]*0.49)/2,4)
    
   #looseProb <- round(y_probs1[1],4)
   #winProb <- round(y_probs1[2],4)    
    
   if(looseProb>winProb) {
     return ("loose")
   } else if(looseProb<winProb) {
     return ("win")
   }else{
     return ("tie")
   }
    
#    return(as.character(c(looseProb, winProb)))
  
}

predict_round <- function(filename){
  
  data_predict <- read.csv(file=filename, stringsAsFactors = FALSE)
  pred<-list()
  
  for(i in 1:nrow(data_predict)){
    
    home_Team <- data_predict[i,2]
    away_Team <- data_predict[i,3]
    m <- data_predict[i,5]
    y <- data_predict[i,6]
    
    
    pred<-append(pred,predict_match(home_Team,away_Team,m,y))
  }
  
  write.csv(pred,file="R2015_pred.csv")
  
}