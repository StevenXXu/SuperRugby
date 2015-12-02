library(XML)
library(RCurl)
library(lubridate)

stats<-NULL
stats<-list()
varList<-data.frame(Team=character(),GamePlayed=numeric(),GamesWon=numeric(),GamesLose=numeric(),GamesDraw=numeric(),TotalFor=numeric(),TotalAgainst=numeric(),TotalMargin=numeric(),AvgMargin=numeric(),AvgFor=numeric(),AvgAgainst=numeric(),stringsAsFactors = FALSE)
urlTeams<-gsub(" ","-","South Sydney")
urlTeamStats<- gsub(" ","",paste("http://www.rugbyleagueproject.org/teams/",urlTeams,"/records.html",seq=""),fixed=TRUE)
doc <- htmlTreeParse(tolower(urlTeamStats), useInternal=TRUE)
stats_1<-readHTMLList(tolower(urlTeamStats))
competition <- data.frame(xpathSApply(doc, "//h3", xmlValue))
pos <- which(competition=="NRL")+4
TeamStats_tmp<-stats_1[[pos]]
#stats<-"South Sydney"
for(i in 1:6){
  
  if(!is.na(as.numeric(TeamStats_tmp[[i]]))){
    
    stats<-append(stats,as.numeric(TeamStats_tmp[[i]]))
  }
  else
  {  
    stats<-append(stats,as.numeric(gsub(",","",TeamStats_tmp[[i]])))
  }
  
  
}

stats<-append(stats,stats[[5]]-stats[[6]]) #margin
stats<-append(stats,stats[[7]]/stats[[1]]) #margin_avg
stats<-append(stats,stats[[5]]/stats[[1]]) #for_avg
stats<-append(stats,stats[[6]]/stats[[1]]) #against_avg

varList<-data.frame("South Sydney",stats,stringsAsFactors=FALSE)

