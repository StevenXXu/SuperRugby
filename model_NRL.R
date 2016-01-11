## Load Library
library(caret)
library(randomForest)
library(lubridate)

## Load Pre-trained model
model <- readRDS(file = "NRL_Prediction_Model.rda")
data_NRL<-readRDS(file = "NRLData.rda")

## Load predictors
teamStats <- readRDS(file="NRLteamStats.rda")
teams <- readRDS(file="NRLTeams.rda")

## Create a function to take team name inputs
## and then return a prediction for the match
predict_match <- function(homeTeam, awayTeam, month, year) {
  
  homeIndex <- which(teams == homeTeam)
  awayIndex <- which(teams == awayTeam) 
  matchStats<-teams_stats(homeTeam,awayTeam)
  
  #swap <- awayTeam == "England"
  
  #if(swap){
    
  #  temp <- homeTeam
  #  homeTeam <- awayTeam
  #  awayTeam <- temp
    
  #  temp <- homeIndex
  #  homeIndex <- awayIndex
  #  awayIndex <- temp
    
  #}
  
  newCase <- readRDS("newCase_NRL.rda")
  date <- Sys.Date()
  
  if(is.null(matchStats)){
    
    newCase[1,4] <- matchStats[1,4]
    newCase[1,5] <- matchStats[1,5]
    
    newCase[1,9:11] <- matchStats[1,9:11]
    newCase[1,12:14] <- matchStats[1,12:14]
    
  }else{
    
    homeTeamStats <- teamStats[homeIndex,3:5]
    awayTeamStats <- teamStats[awayIndex,7:9]
    
    newCase[1,9:11] <- homeTeamStats
    newCase[1,12:14] <- awayTeamStats
  }
  
  #levelsx <- levels(factor(teams))
  #levelsy <- levels(factor(c("loose","win")))
 
  newCase[1,2] <- homeTeam
  newCase[1,3] <- awayTeam
  newCase[1,6] <- date
  newCase[1,7] <- factor(month)
  newCase[1,8] <- factor(year)
  
  
  
  ## Use the model for prediction
  
  
  ## Return the predicted class

  
 
    y_probs1 <- predict(model, newCase, type="prob")

    temp <- homeTeam
    homeTeam <- awayTeam
    awayTeam <- temp
    
    homeTeamStats <- teamStats[homeIndex,3:5]
    awayTeamStats <- teamStats[awayIndex,7:9]
    matchStats<-teams_stats(homeTeam,awayTeam)
    
    date <- Sys.Date()
    #levelsx <- levels(factor(teams))
    #levelsy <- levels(factor(c("loose","win")))
    newCase <- readRDS("newCase_NRL.rda")
    
    if(is.null(matchStats)){
      
      newCase[1,4] <- matchStats[1,4]
      newCase[1,5] <- matchStats[1,5]
      
      newCase[1,9:11] <- matchStats[1,9:11]
      newCase[1,12:14] <- matchStats[1,12:14]
      
    }else{
      
      homeTeamStats <- teamStats[homeIndex,3:5]
      awayTeamStats <- teamStats[awayIndex,7:9]
      
      newCase[1,9:11] <- homeTeamStats
      newCase[1,12:14] <- awayTeamStats
    }
    
    
    newCase[1,2] <- homeTeam
    newCase[1,3] <- awayTeam
    newCase[1,6] <- date
    newCase[1,7] <- factor(month)
    newCase[1,8] <- factor(year)

    
    y_probs2 <- predict(model, newCase, type="prob")

 
    looseProb <- round((y_probs1[1]*0.50 + y_probs2[2]*0.50)/2,4)
    winProb <- round((y_probs1[2]*0.50 + y_probs2[1]*0.50)/2,4)
    
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

teams_stats <- function(homeTeam, awayTeam){
    
    data_1<-subset(data_NRL,data_NRL[,2]==homeTeam)
    if(nrow(data_1)!=0){
      
      data_1<-subset(data_1,data_1[,3]==awayTeam)
      
    }
    
    if(nrow(data_1)!=0){
    
      testTeamStats<-data.frame(data_1[,1],data_1[,2],data_1[,3],mean(data_1[,4]),mean(data_1[,5]),data_1[,6],data_1[,7],data_1[,8],mean(data_1[,9]),mean(data_1[,10]),
                                mean(data_1[,11]),mean(data_1[,12]),mean(data_1[,13]),
                                mean(data_1[,14]))  
      return(testTeamStats)
    }
    
    return(NULL)
    
}