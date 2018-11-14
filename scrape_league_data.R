setwd("~/FF")

library(httr)
library(rvest)
options("httr_oob_default" = T)

cKey <- "dj0yJmk9aTZUNVJvNHcyVDJRJnM9Y29uc3VtZXJzZWNyZXQmc3Y9MCZ4PWE3"
cSecret <- "87d303f6688c8aa2d1701109e5bbbab8bb064057"

b_url <- "https://fantasysports.yahooapis.com" #base url

#Create Endpoint
yahoo <- httr::oauth_endpoint(authorize = "https://api.login.yahoo.com/oauth2/request_auth"
                              , access = "https://api.login.yahoo.com/oauth2/get_token"
                              , base_url = b_url)
#Create App
myapp <- httr::oauth_app("yahoo", key=cKey, secret = cSecret,redirect_uri = "oob")

#Open Browser to Authorization Code
httr::BROWSE(httr::oauth2.0_authorize_url(yahoo, myapp, scope="fspt-r"
                                          , redirect_uri = myapp$redirect_uri))

#Create Token
yahoo_token <- httr::oauth2.0_access_token(yahoo,myapp,code="qzzng32")
save(yahoo_token,file="yahoo_token.Rdata")

game_key <- 380
lg_id <- 1107315
leagueKey <- paste0(game_key,'.l.',lg_id)

##GetRosterData

rosters <- data.frame(team_id=integer(),
                      team_name=character(),
                      player_id=integer(),
                      player_name=character(),
                      stringsAsFactors=FALSE)

baseURL <- "https://fantasysports.yahooapis.com/fantasy/v2/team/"

for(team in 1:12) {
  teamURL <- paste0(baseURL,leagueKey,".t.",team,"/roster/players?format=json")
  team_page <- GET(teamURL,add_headers(Authorization=paste0("Bearer ", yahoo_token$access_token)))
  team_parse <- content(team_page, as = "parsed", encoding = "utf-8")
  team_name <- team_parse$fantasy_content$team[[1]][[3]]$name
  for(player in team_parse$fantasy_content$team[[2]]$roster$`0`$players)  {
    if(!is.atomic(player)) {
      rostered_player <- data.frame(team_id=team,team_name=team_name
                                    ,player_id=player$player[[1]][[2]]$player_id
                                    ,player_name=player$player[[1]][[3]]$name$full,stringsAsFactors=FALSE)
      rosters <- rbind(rosters,rostered_player)
    }
  }
}

write.csv(rosters,file="rosters.csv",quote=FALSE,row.names = FALSE)

##GetPlayerData
baseURL <- "https://fantasysports.yahooapis.com/fantasy/v2/players;plyaer_keys=NULL?format=json"
players_page <- GET(baseURL,add_headers(Authorization=paste0("Bearer ", yahoo_token$access_token)))
players_parse <- content(players_page, as = "parsed", encoding = "utf-8")
