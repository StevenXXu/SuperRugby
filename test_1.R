library(XML)
library(RCurl)
library(lubridate)
urlTeams<-gsub(" ","-","South Sydney")
urlTeamStats<- gsub(" ","",paste("http://www.rugbyleagueproject.org/teams/",urlTeams,"/records.html",seq=""),fixed=TRUE)
doc <- htmlTreeParse(tolower(urlTeamStats), useInternal=TRUE)
stats<-readHTMLList(tolower(urlTeamStats))
competition <- data.frame(xpathSApply(doc, "//h3", xmlValue))
pos <- which(competition=="NRL")+4
TeamStats<-stats[[pos]]
