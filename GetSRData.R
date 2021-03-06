#### Construct SR data set ####

# The Teams
teams <- c("Blues","Bulls","Chiefs","Highlanders","Lions","Reds","Southern Kings","Waratahs","Brumbies"
          ,"Cheetahs","Crusaders","Hurricanes","Melbourne Rebels","Sharks","Stormers","Western Force")

saveRDS(teams, "teamNames.rda")

# Get rank data
library(XML)
library(RCurl)
Nteams <- length(teams)
rank <- numeric(Nteams)
rankScore <- numeric(Nteams)
rankChange <- numeric(Nteams)

urlRankScore <- "https://en.wikipedia.org/wiki/2015_Super_Rugby_season"
tabs1 <- getURL(urlRankScore)
tabs1 <- readHTMLTable(tabs1, stringsAsFactors = F)

#urlRankScore <- "http://www.rugby15.co.za/irb-world-rankings/"
#tabs1 <- getURL(urlRankScore)
#tabs1 <- readHTMLTable(tabs1)
#tabs1[[1]][,3] <- sapply(tabs1[[1]][,3], function(x) substr(x, 1, nchar(as.character(x))-4))
tabs1[[3]][,2][tabs1[[3]][,2] == "Rebels"] <- "Melbourne Rebels"
tabs1[[3]][,2][tabs1[[3]][,2] == "Force"] <- "Western Force"

tabs1[[3]][,14] <- as.numeric(as.character(tabs1[[3]][,14])) #rankscore - total points
tabs1[[3]][,1] <- as.numeric(as.character(tabs1[[3]][,1])) #rank - position

for(i in 1:length(tabs1[[3]][,1])){
  index <- which(teams == tabs1[[3]][i,2])
  if(length(index) != 0){
    rank[index] <- as.numeric(tabs1[[3]][i,1])
  }
}

#urlRank <- "https://en.wikipedia.org/wiki/2014_Super_Rugby_season"
#tabs2 <- getURL(urlRank)
#tabs2 <- readHTMLTable(tabs2, stringsAsFactors = F)
#for(i in 1:length(tabs2[[3]][,1])){
#  index <- which(teams == tabs2[[3]][i,14])
#  if(length(index) != 0){
#    rank[index] <- as.numeric(tabs2[[3]][i,1])
#    if(tabs2[[3]][i,14] == ""){
#      rankChange[index] <- 0
#    } else {
#      rankChange[index] <- as.numeric(tabs2[[3]][i,14])
#    }
#  }
#}

for(i in 1:length(tabs1[[3]][,14])){
  index <- which(teams == tabs1[[3]][i,2])
  if(length(index) != 0){
    rankScore[index] <- as.numeric(tabs1[[3]][i,14])
  }
}

saveRDS(rank, "rank.rda")
saveRDS(rankScore, "rankScore.rda")
#saveRDS(rankChange, "rankChange.rda")

teamLoop <- tolower(gsub(" ", "", teams, fixed = TRUE))

# Get match data
matchDate <- NULL
homeTeam <- NULL
scores <- NULL
awayTeam <- NULL

otherVarList <- list()

for(i in 1:Nteams){
  fileUrl <- paste("http://www.rugbydata.com/superrugby/", teamLoop[i], "/gamesplayed/", sep="")
  doc <- htmlTreeParse(fileUrl, useInternal=TRUE)
  matchDate <- c(matchDate, xpathSApply(doc, "//td[@class='match-date']", xmlValue)) 
  homeTeam <- c(homeTeam, xpathSApply(doc, "//td[@class='home-team']", xmlValue)) 
  scores <- c(scores, xpathSApply(doc, "//a[@class='match-score']", xmlValue)) 
  awayTeam <- c(awayTeam, xpathSApply(doc, "//td[@class='away-team']", xmlValue)) 
  otherVarList[[i]] <- as.numeric(xpathSApply(doc, "//span[@class='rdnumeric']", xmlValue))
}  

saveRDS(otherVarList, file = "teamStats.rda")

# Create date vector and compute linear weights
dates <- as.Date(matchDate, format="%d %B %Y")
numDates <- as.numeric(dates[order(dates)])
weights <- (numDates-min(numDates))/(max(numDates)-min(numDates))

# Extract scores
tempScores <- strsplit(scores, "-")

outcome <- NULL
ties <- NULL
countTies <- 1

# Populate win and loss labels for each matche while indexing tied matches
for(i in 1:length(tempScores)){
  diff <- as.numeric(tempScores[[i]][1]) - as.numeric(tempScores[[i]][2])
  if(diff > 0){
    outcome[i] <- "win"
  } else if (diff < 0){
    outcome[i] <- "loose"
  } else {
    ties[countTies] <- i
    countTies <- countTies + 1
  }
}

# Remove tied matches and matches not containing world cup teams
interData <- data.frame(outcome=(factor(outcome)), homeTeam, awayTeam, dates)
interData <- interData[-ties,]
countries <- c(homeTeam, awayTeam)
remove <- NULL
count <- 1
for(i in 1:nrow(interData)){
  if(!is.element(interData[i,2], teams)){
    remove[count] <- i
    count <- count+1
  } else if(!is.element(interData[i,3], teams)){
    remove[count] <- i
    count <- count+1
  }
}
if (!is.null(remove))
    interData <- interData[-remove,]
  
interData[,2] <- factor(interData[,2])
interData[,3] <- factor(interData[,3]) 
interData <- interData[order(interData$dates),]
interData <- interData[!duplicated(interData),]

# remove corresponding weights
if(!is.null(ties))
    weights <- weights[-ties]

if (!is.null(remove))
    weights <- weights[-remove]

weights <- weights[!duplicated(interData)]

# Add game date variables
library(lubridate)
month <- month(interData$dates)
year <- year(interData$dates)
#interData <- interData[,-4]
interData$month <- month
interData$year <- year


#dataIndex <- NULL 
#for(i in 1:length(teams)){
#  done <- 1
#  team <- teams[i]
#  count <- nrow(interData)
#  while(done < 17){
#    if(interData[count,2] == team || interData[count,3] == team){
#      if(!(count %in% dataIndex)){
#        dataIndex <- c(dataIndex, count)
#        done <- done + 1
#      }
#    }
#    count <- count-1
#  }
#}

#interData <- interData[dataIndex,]
#interData <- interData[order(interData$dates),]
#interData <- interData[,-4]

# Only select matches as far back as the previous world cup
#weights <- weights[interData$year < 2015]
#interData <- interData[interData$year < 2015,]


# Add remaining variables
homeMat <- matrix(0, nrow=nrow(interData), ncol=23)
awayMat <- matrix(0, nrow=nrow(interData), ncol=23)

for(i in 1:nrow(interData)){
  homeIndex <- which(teams == interData[i,2])
  awayIndex <- which(teams == interData[i,3])
  homeMat[i,] <- c(otherVarList[[homeIndex]], rank[homeIndex], rankScore[homeIndex])
  awayMat[i,] <- c(otherVarList[[awayIndex]], rank[awayIndex], rankScore[awayIndex])
}

# Export data as csv file
extraData <- data.frame(cbind(homeMat, awayMat))
data <- data.frame(interData, extraData)

dataColNames <- c("Outcome", "HomeTeam", "AwayTeam",
                  "date","month", "year",
                  "GamesPlayedHome", "GamesWonHome", "GamesLostHome",
                  "GamesDrawnHome", "LongestWinningStreakHome",
                  "LongestLosingStreakHome",
                  "TeamsPlayedHome", "TeamsBeatenHome", 
                  "TeamsBeatenByHome",
                  "TeamsDrawnWithHome", "GroundsPlayedAtHome", 
                  "LargestPointsForHome",
                  "LargestPointsAgainstHome", 
                  "LargestWinningMarginHome", 
                  "LargestLosingMarginHome", "TotalPointsForHome", 
                  "AvgPointsForHome",
                  "TotalPointsAgainstHome", "AvgPointsAgainstHome", 
                  "TotalPointsDifferenceHome", "AvgPointsDifferenceHome",
                  "RankHome", "RankScoreHome",
                  "GamesPlayedAway", "GamesWonAway", "GamesLostAway",
                  "GamesDrawnAway", "LongestWinningStreakAway",
                  "LongestLosingStreakAway",
                  "TeamsPlayedAway", "TeamsBeatenAway", 
                  "TeamsBeatenByAway",
                  "TeamsDrawnWithAway", "GroundsPlayedAtAway", 
                  "LargestPointsForAway",
                  "LargestPointsAgainstAway", 
                  "LargestWinningMarginAway", 
                  "LargestLosingMarginAway", "TotalPointsForAway", 
                  "AvgPointsForAway",
                  "TotalPointsAgainstAway", "AvgPointsAgainstAway", 
                  "TotalPointsDifferenceAway", "AvgPointsDifferenceAway",
                  "RankAway", "RankScoreAway")

colnames(data) <- dataColNames
data$month <- factor(data$month)
data$year <- factor(data$year)

# write data
saveRDS(data[1,], "newCase.rda")
saveRDS(data, "RWCData.rda")