library(XML)
library(pipeR)
library(ineq)

# Get game log for every team, with days since last game
total.gl = NULL
for(team in teams){
  gl <- GetGameLog(team)
  
  days.since <- NULL
  for (i in 1:length(gl$Date)) {
    days.since[i] <- as.integer(gl$Date[i]-gl$Date[i-1])
  }
  
  total.gl <- rbind(total.gl,cbind(gl,days.since,team))
}

# Add a factor which determines if the team has only had 1 day off or not
total.gl <- cbind(total.gl,total.gl$days.since==1)
names(total.gl)[43] <- "played.yesterday"

attach(total.gl)

# Basic summary
mod <- lm(Tm.Sc - Opp.Sc ~ played.yesterday + Home)
summary(mod)


###############################
###############################

team.gini=NULL
opp.gini=NULL

# Calculate Gini minutes for each team
for (i in 1:nrow(total.gl))
{

  if (Home[i]) 
  { 
    box.score <- GetBoxScore(team[i], Opp[i], Date[i], TRUE)
    team.gini[i] =ineq(box.score$MP)
    
    box.score <- GetBoxScore(team[i], Opp[i], Date[i], FALSE)
    opp.gini[i] =ineq(box.score$MP)
  }
  else
  {
    box.score <- GetBoxScore(Opp[i], team[i], Date[i], FALSE)
    team.gini[i] =ineq(box.score$MP)
    
    box.score <- GetBoxScore(Opp[i], team[i], Date[i], TRUE)
    opp.gini[i] =ineq(box.score$MP)
  }
}


