library(XML)
library(RCurl)
library(lubridate)

homeTeam<-NULL
awayTeam<-NULL

matchDates_raw<-NULL
matchDates<-NULL
month<-NULL
year<-NULL
homeScores<-NULL
awayScores<-NULL
homeHalfScores<-NULL
awayHalfScores<-NULL
homeScrums<-NULL
awayScrums<-NULL
homePenalties<-NULL
awayPenalties<-NULL
homeFullback<-NULL
homeWing1<-NULL
homeWing2<-NULL
homeCenter1<-NULL
homeCenter2<-NULL
homeFiveEighth<-NULL
homeHalfback<-NULL
awayFullback<-NULL
awayWing1<-NULL
awayWing2<-NULL
awayCenter1<-NULL
awayCenter2<-NULL
awayFiveEighth<-NULL
awayHalfback<-NULL


urlNRL <- "http://www.rugbyleagueproject.org/seasons/"

temps<-sapply("nrl-", function(x) paste(x,seq(2005,2015,1),seq=""))
urlSeasons <- gsub(" ","",temps[,1],fixed=TRUE)

tempr<-sapply("round-", function(x) paste(x,seq(1,26,1),seq=""))
urlRounds <- gsub(" ","",tempr[,1],fixed=TRUE)

seasons_year<-seq(2005,2014,1)

#http://www.rugbyleagueproject.org/seasons/nrl-2015/results.html
urlMatchSummary <- gsub(" ","",paste(urlNRL,urlSeasons,"/results.html",seq=""),fixed=TRUE)

for(y in 1:length(seasons_year)){
  tabs1<- getURL(gsub(" ","",urlMatchSummary[y],fixed=TRUE))
  tabs1<- readHTMLTable(tabs1, stringsAsFactors = F)
  
  #locate the position of round title
  round_pos<-NULL
  game_num<-NULL
  teams<-NULL
  urlTeams<-NULL
  rounds<-0
  
  for(i in 1:nrow(tabs1[[1]])){
    
    if(str_detect(tabs1[[1]][i,1],"Round")) round_pos<-append(round_pos,i)
    
  } 
  
  #team -> brisbane-vs-south-sydney
  
  for(i in 1:length(round_pos)){
    
    if(i!=length(round_pos)) {
      m<-round_pos[i+1]-round_pos[i]-1
      game_num<-append(game_num,m)
    }
    else{
      #m<-nrow(tabs1[[1]])-round_pos[i]
      m<-game_num[1]
      game_num<-append(game_num,m)
    }
    
    for(j in 1:m){
      if(!is.na(tabs1[[1]][round_pos[i]+j,4]) && !is.na(tabs1[[1]][round_pos[i]+j,6])){
        teams <- append(teams,gsub(" ","-",trimws(paste(tabs1[[1]][round_pos[i]+j,4],"vs",tabs1[[1]][round_pos[i]+j,6],seq=""),which=c("right")),fixed=TRUE))
        homeTeam <- append(homeTeam,tabs1[[1]][round_pos[i]+j,4])  
        awayTeam <- append(awayTeam,tabs1[[1]][round_pos[i]+j,6])
        
        year <- append(year,seasons_year[y])
         # Add game date variables
        if(!is.na(tabs1[[1]][round_pos[i]+j,2])){
          
          matchDates<- append(matchDates, as.Date(tabs1[[1]][round_pos[i]+j,2],format="%B %d"))

        }
        
      }

    }
    
  }
  
  
  for(i in 1:length(round_pos)){
    for(j in 1:game_num[i]){
      urlTeams<- append(urlTeams, gsub(" ","",paste(urlNRL,urlSeasons[y],"/",urlRounds[i],"/",teams[j+rounds],"/summary.html",seq=""),fixed=TRUE))
    }
    rounds<-rounds+game_num[i]
    
  }
  
  #extract the dates

#  matchDates_raw<-subset(tabs1[[1]][,2],tabs1[[1]][,2]!="NA")
#  matchDates<-append(matchDates,as.Date(matchDates_raw,format="%B %d"))



  
  #extract game stats
  Ngames<-length(urlTeams)
  
  for(i in 1:Ngames){
    tabs2<- getURL(tolower(urlTeams[i]))
    tabs2<- readHTMLTable(tabs2, stringsAsFactors = F)
    
    homeScores <- append(homeScores,tabs2[[1]][1,3])
    awayScores <- append(awayScores,tabs2[[1]][1,5])
    
    homeHalfScores<-append(homeHalfScores,tabs2[[1]][3,3])
    awayHalfScores<-append(awayHalfScores,tabs2[[1]][3,5])
    homeScrums<-append(homeScrums,tabs2[[1]][4,3])
    awayScrums<-append(awayScrums,tabs2[[1]][4,5])
    homePenalties<-append(homePenalties,tabs2[[1]][5,3])
    awayPenalties<-append(awayPenalties,tabs2[[1]][5,5])
    
    wing_count <- 0
    centre_count <- 0
    
    for(j in 1:nrow(tabs2[[1]])){
      
      if(!is.na(tabs2[[1]][j,1])){
        
        if(tabs2[[1]][j,1]=="Fullback"){
          homeFullback<-append(homeFullback,tabs2[[1]][j,3])
          awayFullback<-append(awayFullback,tabs2[[1]][j,5])
        }else if(tabs2[[1]][j,1]=="Wing" && wing_count==0){
          wing_count<- 1
          homeWing1<-append(homeWing1,tabs2[[1]][j,3])
          awayWing1<-append(awayWing1,tabs2[[1]][j,5])
        }else if(tabs2[[1]][j,1]=="Centre" && centre_count==0){
          centre_count<- 1
          homeCenter1<-append(homeCenter1,tabs2[[1]][j,3])
          awayCenter1<-append(awayCenter1,tabs2[[1]][j,5])
        }else if(tabs2[[1]][j,1]=="Centre"){
          homeCenter2<-append(homeCenter2,tabs2[[1]][j,3])
          awayCenter2<-append(awayCenter2,tabs2[[1]][j,5])
        }else if(tabs2[[1]][j,1]=="Wing"){
          homeWing2<-append(homeWing2,tabs2[[1]][j,3])
          awayWing2<-append(awayWing2,tabs2[[1]][j,5])
        }else if(tabs2[[1]][j,1]=="Five-Eighth"){
          homeFiveEighth<-append(homeFiveEighth,tabs2[[1]][j,3])
          awayFiveEighth<-append(awayFiveEighth,tabs2[[1]][j,5])
        }else if(tabs2[[1]][j,1]=="Halfback"){
          homeHalfback<-append(homeHalfback,tabs2[[1]][j,3])
          awayHalfback<-append(awayHalfback,tabs2[[1]][j,5])
        }
        
      }
    }
  }
}

month <- month(matchDates)
#year <- year(matchDates)

#matchStats<-data.frame(as.numeric(homeHalfScores),as.numeric(homeScrums),as.numeric(homePenalties),homeFullback,homeWing1,homeCenter1,homeCenter2,homeWing2,homeFiveEighth,homeHalfback,
#                       as.numeric(awayHalfScores),as.numeric(awayScrums),as.numeric(awayPenalties),awayFullback,awayWing1,awayCenter1,awayCenter2,awayWing2,awayFiveEighth,awayHalfback)

matchStats_1<-data.frame(as.numeric(homeHalfScores),as.numeric(homeScrums),as.numeric(homePenalties),
                         as.numeric(awayHalfScores),as.numeric(awayScrums),as.numeric(awayPenalties))

outcome <- NULL
ties <- NULL
countTies <- 1

# Populate win and loss labels for each matche while indexing tied matches
for(i in 1:length(homeScores)){
  diff <- as.numeric(homeScores[i]) - as.numeric(awayScores[i])
  if(diff > 0){
    outcome[i] <- "win"
  } else if (diff < 0){
    outcome[i] <- "loose"
  } else {
    ties[countTies] <- i
    countTies <- countTies + 1
  }
}

data<-data.frame(factor(outcome),homeTeam,awayTeam,as.numeric(homeScores),as.numeric(awayScores), matchDates,factor(month),factor(year),matchStats_1)

#dataColNames <- c("Outcome","HomeTeam","AwayTeam","HomeScores","AwayScores","Date","Month","Year","HomeHalfScores","HomeScrums",
#                  "HomePenalties","HomeFullback",
#                  "HomeWing1","HomeCenter1","HomeCenter2","HomeWing2","HomeFiveEighth","HomeHalfback",
#                  "AwayHalfScores","AwayScrums","AwayPenalties","AwayFullback","AwayWing1","AwayCenter1",
#                  "AwayCenter2","AwayWing2","AwayFiveEighth","AwayHalfback")

dataColNames <- c("Outcome","HomeTeam","AwayTeam","HomeScores","AwayScores","Date","Month","Year","HomeHalfScores","HomeScrums","HomePenalties",
                  "AwayHalfScores","AwayScrums","AwayPenalties")

colnames(data) <- dataColNames

data<-subset(data,data[1]!="NA")
data<-subset(data,data[10]!="NA")
data<-subset(data,data[11]!="NA")
data<-subset(data,data[13]!="NA")
data<-subset(data,data[14]!="NA")

# write data
saveRDS(data[1,], "newCase_NRL.rda")
saveRDS(data, "NRLData.rda")