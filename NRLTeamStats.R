#NRLteamStats

# load data
data <- readRDS("NRLData.rda")

teams <- readRDS("NRLTeams.rda")

teamStats<-data.frame(Team=character(),HomeScores=numeric(),HomeHalfScores=numeric(),HomeScrums=numeric(),
                      homePenalties=numeric(),AwayScores=numeric(),AwayHalfScores=numeric(),AwayScrums=numeric(),
                      AwayPenalties=numeric(),stringsAsFactors = FALSE)
for(i in 1:nrow(teams)){
  
  team_a<-teams[i,1]
  
  home_data<-subset(data,data[,2]==team_a)
  away_data<-subset(data,data[,3]==team_a)
  
  teamStats[i,] <- data.frame(team_a,mean(home_data[,4]),mean(home_data[,9]),mean(home_data[,10]),
                              mean(home_data[,11]),mean(away_data[,5]),mean(away_data[,12]),mean(away_data[,13]),
                              mean(away_data[,14]))
  
}



#saveRDS(data, "NRLteamStats.rda")

