library(XML)
library(RCurl)
library(lubridate)
library(stringr)

urlAFL <- "http://www.afl.com.au/stats"
urlAFL1<-"http://www.afl.com.au/match-centre/2013/1/adel-v-ess"

tabs1<- getURL(urlAFL1)
doc <- htmlTreeParse(tolower(urlAFL1), useInternal=TRUE)
tabs1<- readHTMLTable(tabs1, stringsAsFactors = F)

home_afl <- c(xpathSApply(doc, "//*[@id='home-team-stats']", xmlValue))
title_afl <- c(xpathSApply(doc, "//*[@id='season-stats']/div[2]/ul[2]", xmlValue))
away_afl <- c(xpathSApply(doc, "//*[@id='away-team-stats']", xmlValue))
home_score <- c(xpathSApply(doc,"//*[@id='post-game']/div[1]/div[2]/div/div[5]/ul[1]/li[3]",xmlValue))
away_score <- c(xpathSApply(doc,"//*[@id='post-game']/div[1]/div[2]/div/div[5]/ul[2]/li[3]",xmlValue))


