library("XML")
library("rvest")
library("pipeR")
library("chron")


# Team codes for 2015-16
teams <- c("ATL","BOS","BRK","CHO","CHI","CLE","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOP","NYK","OKC","ORL","PHI","PHO","POR","SAC","SAS","TOR","UTA","WAS")

GetPlayerData <- function(team.lookup="",year=substring(Sys.Date(),1,4))
{
  require('rvest')
  require('XML')
  
  url_base <- "http://www.basketball-reference.com/teams/"
  
  # if left empty, lookup all teams
  if (team.lookup!="") {teams=team.lookup} 
  
  # Pull data from basketball-reference.com
  player.data <- data.frame()
  for(team in teams){
    
    
    theurl <- paste(url_base,team,"/",year,".html",sep="")
    
    tables <- readHTMLTable(theurl)
    team.results <-merge(tables$totals,tables$advanced,by="Player",suffixes=c(""))
    team.results$TeamCode <- team
    player.data <- rbind(player.data, team.results)
  }
  
  # Formatting
  player.data <- player.data[!duplicated(lapply(player.data,summary))]
  for (i in 2:46) player.data[,i] <- as.numeric(as.character(player.data[,i]))
  
  # Return
  player.data
}

GetTeamData <- function(year=substring(Sys.Date(),1,4))
{
  require('rvest')
  require('XML')
  
  url <- paste("http://www.basketball-reference.com/leagues/NBA_",year,"_standings.html",sep="")
  
  
  # Pull data from basketball-reference.com
  team.data <- data.frame()
  team.data <- readHTMLTable(url)$"expanded-standings"
  
  names(team.data)[16]<-"le3"
  names(team.data)[17]<-"ge10"
  
  # Convert win-loss records to a percentage
  Rec2Pct <- function(rec)
  {
    rec <- as.character(rec)
    dash_loc <- gregexpr("-",as.character(rec))[[1]][1]
    
    w <- substring(rec,1,dash_loc-1)
    l <- substring(rec,dash_loc+1,nchar(rec))
    
    
    if (strtoi(w)+strtoi(l)==0) { out <- 0 }
    else { out <- strtoi(w)/(strtoi(w)+strtoi(l)) }
    
    out
  }
  
  # Formatting
  for (i in 3:length(team.data)) { team.data[,i] <- sapply(team.data[,i],Rec2Pct) }
  team.data$Team <- sapply(team.data$Team,as.character)
  team.data <- team.data[order(team.data$Team),]
  team.data$Team <- teams
  
  
  # Return
  team.data
}

GetGameLog <- function(team,year=substring(Sys.Date(),1,4))
{
  require('rvest')
  require('XML')
  
  url<-paste("http://www.basketball-reference.com/teams", team, year,"gamelog/", sep="/")
  
  # Pull data from basketball-reference.com
  gamelog <- readHTMLTable(url)$tgl_basic[-c(21,22,43,44,65,66,87,88),-c(25)]
  
  # Formatting
  names(gamelog)[c(4,6,7,8,25:40)] <- c("Home","Win","Tm.Sc","Opp.Sc","FG.Opp","FGA.Opp","FG%.Opp","3P.Opp","3PA.Opp","3P%.Opp","FT.Opp","FTA.Opp","FT%.Opp","ORB.Opp","TRB.Opp","AST.Opp","STL.Opp","BLK.Opp","TOV.Opp","PF.Opp")
  for(i in c(1:2,7:40)) { gamelog[,i] <- as.numeric(as.character(gamelog[,i])) }
  gamelog$Home <- gamelog$Home!="@"
  gamelog$Win <- gamelog$Win=="W"
  gamelog$Date <- as.Date(gamelog$Date)
  gamelog$team <- as.character(team)
  gamelog$Opp  <- as.character(gamelog$Opp)
  
  # Return
  gamelog
}

GetInjuryList <- function()
{
  require('XML')
  require('rvest')
  require('pipeR')
  
  page <- "http://espn.go.com/nba/injuries"
  
  injured <- page %>%
    read_html %>%
    html_nodes(".tablehead a") %>%
    html_text() 

  # Return
  unique(injured)
}

GetBoxScore <- function(home.team, away.team, date,check.home=TRUE)
{
  require("chron")
  require("XML")
  
  url <- "http://www.basketball-reference.com/boxscores/"
  url <- paste(url,strftime(as.character(date), "%Y%m%d"), "0", home.team, ".html", sep="")
  
  if (check.home)
  {
    page <- readHTMLTable(url)
    out  <- page[[paste(home.team,"_basic",sep="")]][-6,]
  }
  else
  {
    page <- readHTMLTable(url)
    out  <- page[[paste(away.team,"_basic",sep="")]][-6,]
  }
  
  # Format Data
  out$MP[out$MP=="Did Not Play"] <- NA
  out$MP <- times(paste("0:",out$MP,sep="")) #Format as a time using the 'chron' package
  for (i in 3:length(out)) { out[,i] <- as.numeric(as.character(out[,i])) }
  
  
  out
}