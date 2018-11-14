setwd("~/FF")
#Download fantasy football scores from Yahoo.com

#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("plyr")
library("data.table")
library("RCurl")

#Functions
source(paste(getwd(),"/FantasyFootballAnalyticsR-master/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/FantasyFootballAnalyticsR-master/R Scripts/Functions/League Settings.R", sep=""))

week <- 10

yahoo_baseurl <- "https://football.fantasysports.yahoo.com/f1/48938/players?&sort=PTS&sdir=1&status=ALL"
yahoo_pages <- paste0("&count=", seq(0, 150, by=25))
yahoo_pos <- list(QB="QB", RB="RB", WR="WR", TE="TE", K="K", DST="DEF")
yahoo_urls <- paste0(yahoo_baseurl, yahoo_pages, "&pos=", rep(yahoo_pos, each=length(yahoo_pages)), "&stat1=S_W_", week)                        

#Scrape
yahoo <- lapply(yahoo_urls, function(x) {data.table(readHTMLTable(getURL(x), stringsAsFactors = FALSE)[2]$'NULL')})
yahooList <- yahoo

#Clean data
qbNames <- rbNames <- wrNames <- teNames <- c("star","player","add","forecast","owner","gp","bye","points","ownedPct","proj","actual","passAtt","passCmp","passInc","passYds","passTds","passInt","passPsx","passSck","pass1Dn","pass40Y","rushAtt","rushYds","rushTds","rush1Dn","rush40Y","recTgt","rec","recYds","recTds","rec1Dn","rec40Y","retYds","retTds","twoPts","fumbles","fumLost","missing")
kNames <- c("star","player","add","owner","gp","bye","points","ownedPct","proj","actual","fg019","fg2029","fg3039","fg4049","fg50","fgm019","fgm2029","fgm3039","fgm4049","fgm50","pat","patMiss","missing")
dstNames <- c("star","player","add","owner","gp","bye","points","ownedPct","proj","actual","dstPtsAllowed","dstSack","dstSafety","dstTfl","dstInt","dstFumlRec","dstDTd","dstBlk","dst4Dn","dstAllYds","dst3andO","dstRetYds","dstRetTd","missing")

#Clean data
for(i in 1:length(yahooList)){
  if(nrow(yahooList[[i]]) > 0){
    #Add position to projection
    yahooList[[i]][,pos := rep(names(yahoo_pos), each=length(yahoo_pages))[i]]
    yahooList[[i]][,pos := as.factor(pos)]
    
    #Add variable names
    if(unique(yahooList[[i]][,pos]) == "QB"){
      setnames(yahooList[[i]], c(qbNames, "pos"))
    } else if(unique(yahooList[[i]][,pos]) == "RB"){
      setnames(yahooList[[i]], c(rbNames, "pos"))
    } else if(unique(yahooList[[i]][,pos]) == "WR"){
      setnames(yahooList[[i]], c(wrNames, "pos"))
    } else if(unique(yahooList[[i]][,pos]) == "TE"){
      setnames(yahooList[[i]], c(teNames, "pos"))
    } else if(unique(yahooList[[i]][,pos]) == "K"){
      setnames(yahooList[[i]], c(kNames, "pos"))
    } else if(unique(yahooList[[i]][,pos]) == "DST"){
      setnames(yahooList[[i]], c(dstNames, "pos"))
    }
  }
}

#Merge
stats_yahoo <- rbindlist(yahoo, use.names=TRUE, fill=TRUE)

#Remove special characters (%, comma)
stats_yahoo <- stats_yahoo[,lapply(.SD, function(x) gsub("\\%", "", x))]
stats_yahoo <- stats_yahoo[,lapply(.SD, function(x) gsub("\\,", "", x))]

#Convert variables from character strings to numeric
numericVars <- names(stats_yahoo)[names(stats_yahoo) %in% c(scoreCategories, "dstDTd","dstRetTd")]
stats_yahoo[, (numericVars) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = numericVars]

#Calculate variables
stats_yahoo[,dstTd := mySum(stats_yahoo[, c("dstDTd","dstRetTd"), with=FALSE])]

#Player name and team
stats_yahoo[,player := str_trim(sapply(str_split(player, "\n"), "[", 2))]
stats_yahoo[,name_yahoo := str_trim(str_sub(gsub("(.*)\\-", "\\1", stats_yahoo$player), start=0, end=nchar(stats_yahoo$player)-4))]
stats_yahoo[,name_yahoo := str_trim(str_sub(gsub("  R", "", name_yahoo)))]
stats_yahoo[,name_yahoo := str_trim(str_sub(gsub("  W", "", name_yahoo)))]
stats_yahoo[,name_yahoo := str_trim(str_sub(gsub("  Q", "", name_yahoo)))]
stats_yahoo[,team_yahoo := cleanTeamAbbreviations(toupper(str_trim(str_sub(name_yahoo, start=nchar(name_yahoo)-2, end=nchar(name_yahoo)))))]
stats_yahoo[,name_yahoo := str_trim(str_sub(name_yahoo, start=0, end=nchar(name_yahoo)-3))]
stats_yahoo[which(pos == "DST"), name_yahoo := convertTeamName(stats_yahoo$team_yahoo[which(stats_yahoo$pos == "DST")])]
stats_yahoo[,name := nameMerge(name_yahoo)]

#Remove NA rows
stats_yahoo <- stats_yahoo[!is.na(name),]

#Format team names
stats_yahoo[which(team_yahoo == "ARZ"),]$team_yahoo <- "ARI"
stats_yahoo[which(team_yahoo == "TB"),]$team_yahoo <- "TBB"
stats_yahoo[which(team_yahoo == "KC"),]$team_yahoo <- "KCC"
stats_yahoo[which(team_yahoo == "GB"),]$team_yahoo <- "GBP"
stats_yahoo[which(team_yahoo == "NO"),]$team_yahoo <- "NOS"
stats_yahoo[which(team_yahoo == "NE"),]$team_yahoo <- "NEP"
stats_yahoo[which(team_yahoo == "SF"),]$team_yahoo <- "SFO"

write.csv(stats_yahoo,file="stats.csv",quote=FALSE,row.names = FALSE)
write.csv(stats_yahoo,file=paste0("stats_week",week,".csv"),quote=FALSE,row.names = FALSE)
