library('plyr')


player.data <- GetPlayerData()
team.data <- GetTeamData()


# Determine offensive/defensive impact of each team
team.def <- ddply(player.data,.(TeamCode),summarize,Rating=sum(MP*DRtg,na.rm=T)/sum(MP))
team.def <- team.def[order(team.def$Rating),]
avg.def <- mean(team.def$Rating)
team.def$Rating <- team.def$Rating/avg.def

team.off <- ddply(player.data,.(TeamCode),summarize,Rating=sum(MP*ORtg,na.rm=T)/sum(MP))
team.off <- team.off[order(team.off$Rating),]
avg.off <- mean(team.off$Rating)
team.off$Rating <- team.off$Rating/avg.off


###################


SimMatchup <- function(team1,team2,sims=1000,inj.list1=c(),inj.list2=c())
{
  # Note: team1 is the home team
  
  cat("Collecting team data...\n")
  
  # Collect and calculate releveant statistics
  team1.data <- GetPlayerData(team1)
  team2.data <- GetPlayerData(team2)
  
  team1.data <- cbind(team1.data,MPG=team1.data$MP/team1.data$G,PPM=team1.data$PTS/team1.data$MP)
  team2.data <- cbind(team2.data,MPG=team2.data$MP/team2.data$G,PPM=team2.data$PTS/team2.data$MP)
  
  # Remove irrelevant data and players
  team1.data <- team1.data[team1.data$Rk<nrow(team1.data)-2,-c(3,7:26,28:40,43:47)]
  team2.data <- team2.data[team2.data$Rk<nrow(team2.data)-2,-c(3,7:26,28:40,43:47)]
  
  # Remove injured players
  team1.data <- team1.data[!(team1.data$Player %in% inj.list1),]
  team2.data <- team2.data[!(team2.data$Player %in% inj.list2),]
  
  # Calculate defensive impact
  team1.def <- sum(team1.data$MP*team1.data$DRtg)/(sum(team1.data$MP)*avg.def)
  team2.def <- sum(team2.data$MP*team2.data$DRtg)/(sum(team2.data$MP)*avg.def)
  
  # Calculate average score. Adjust for home field advantage
  lambda1 <- team2.def*(sum(team1.data$MP/sum(team1.data$MP)*5*48*team1.data$PPM)+.5)
  lambda2 <- team1.def*(sum(team2.data$MP/sum(team2.data$MP)*5*48*team2.data$PPM)-.5)
  
  # TO DO: Adjust for back to back games
  
  # Determine overtime lineup stats
  team1.ot <- team1.data[team1.data$Rk<8,]
  team2.ot <- team2.data[team2.data$Rk<8,]
  
  lambda1.ot <- team2.def*(sum(team1.ot$MP/sum(team1.ot$MP)*5*5*team1.ot$PPM)+.25)
  lambda2.ot <- team1.def*(sum(team2.ot$MP/sum(team2.ot$MP)*5*5*team2.ot$PPM)-.25)
  
  
  
  results <- data.frame(T1.Sc=integer(0),T2.Sc=integer(0),OT=logical(0))
  ot <- as.logical()
  
  cat("Simulating matchup...\n")
  
  for (i in 1:sims)
  {
    t1.results <- rpois(1,lambda1)
    t2.results <- rpois(1,lambda2)
    ot <- F
    
    # Chance for over time
    if (abs(t1.results-t2.results)<=3 & t1.results>t2.results)
    {
      r <-  runif(1)
      
      if (r<.15)      { t2.results <- t2.results+3 }
      else if (r<.30) { t2.results <- t2.results+2 }
      else if (r<.5)  { t2.results <- t2.results+1 }
    }
    if (abs(t1.results-t2.results)<=3 & t2.results>t1.results)
    {
      r <-  runif(1)
      
      if (r<.15)      { t1.results <- t1.results+3 }
      else if (r<.30) { t1.results <- t1.results+2 }
      else if (r<.5)  { t1.results <- t1.results+1 }
    }
    
    if (t1.results==t2.results) { ot=T }
    
    # Keep playing overtime until a team wins
    while(t1.results==t2.results)
    {
      t1.results <- t1.results + rpois(1,lambda1.ot)
      t2.results <- t2.results + rpois(1,lambda2.ot)
    }
    
    results<-rbind(results,data.frame(T1.Sc=t1.results,T2.Sc=t2.results,OT=ot))
  }
  
  cat("\n")
  cat(team1,"Average Score:",round(lambda1,2),"\n")
  cat(team2,"Average Score:",round(lambda2,2),"\n")
  cat("Total:",round(lambda1+lambda2,2),"\n\n")
  cat("Average Spread:",team1,round(lambda2-lambda1,2),"\n")
  results
}

Spread.Prob <- function(team.h,team.a,spread=0,inj1=GetInjuryList(team.h)$Player,inj2=GetInjuryList(team.a)$Player,sims=5000)
{
  # Report the probability of the home team beating the spread.
  
  results <- SimMatchup(team.h,team.a,sims,inj1,inj2)
  
  # Calculate win probability -- make it 20% more conservative than results suggest
  win.prob <- .8*mean(results[,1]+spread>results[,2]) + .2*.5
  kelly <- ((100/110)*win.prob-(1-win.prob))/(100/110)
  
  # Report Results
  cat("Vegas Spread:  ",team.h,spread,"\n")
  cat("\nInjured players on",team.h,": ")
  cat(inj1,sep=", ")
  cat("\nInjured players on",team.a,": ")
  cat(inj2,sep=", ")
  
  cat("\n\nProbability of",team.h,"covering",spread,":",win.prob,"\n\n")
  
  cat("The optimal Kelly bet is",kelly)
}


# Today's matchups
away = c("UTA")
home = c("PHI","BOS","OKC","DAL","MEM","CHI","SAS","UTA","POR","LAC")
spread= c(4,-3,-15,-7,-11.5,3.5,-10.5,-2,-2.5,-5)

for (i in 1:length(home))
{
  Spread.Prob(home[i],away[i],spread[i],sims=5000)
  cat("\n\n======================\n\n")
}