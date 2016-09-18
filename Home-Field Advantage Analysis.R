library(ggplot2)
original <- read.csv("game_data_2007_2016.csv", header=TRUE)

########################################
# Parse Data
########################################

df <- original
df <- data.frame(df,PT.diff = df$Tm.Sc - df$Opp.Sc)
df$Date <- as.Date(df$Date)
df$team <- as.character(df$team)
df <- data.frame(df, season = NA)
for (i in 1:nrow(df))
{
  if      (df$Date[i] > as.Date("2006-7-01") && df$Date[i] <= as.Date("2007-7-01")) { df$season[i] = 2007 }
  else if (df$Date[i] > as.Date("2007-7-01") && df$Date[i] <= as.Date("2008-7-01")) { df$season[i] = 2008 }
  else if (df$Date[i] > as.Date("2008-7-01") && df$Date[i] <= as.Date("2009-7-01")) { df$season[i] = 2009 }
  else if (df$Date[i] > as.Date("2009-7-01") && df$Date[i] <= as.Date("2010-7-01")) { df$season[i] = 2010 }
  else if (df$Date[i] > as.Date("2010-7-01") && df$Date[i] <= as.Date("2011-7-01")) { df$season[i] = 2011 }
  else if (df$Date[i] > as.Date("2011-7-01") && df$Date[i] <= as.Date("2012-7-01")) { df$season[i] = 2012 }
  else if (df$Date[i] > as.Date("2012-7-01") && df$Date[i] <= as.Date("2013-7-01")) { df$season[i] = 2013 }
  else if (df$Date[i] > as.Date("2013-7-01") && df$Date[i] <= as.Date("2014-7-01")) { df$season[i] = 2014 }
  else if (df$Date[i] > as.Date("2014-7-01") && df$Date[i] <= as.Date("2015-7-01")) { df$season[i] = 2015 }
  else if (df$Date[i] > as.Date("2015-7-01") && df$Date[i] <= as.Date("2016-7-01")) { df$season[i] = 2016 }
}

df.home <- df[df$Home == TRUE,]
df.away <- df[df$Home == FALSE,]
df.2016 <- df[df$season == 2016,]

avg.home.win <- mean(df.home$PT.diff)
plot.breaks <- c(-30,-20,-10,2.93,10,20,30)

########################################
# Histogram of Home Court Advantage
########################################

ggplot(df.home, aes(PT.diff)) + 
  geom_histogram(binwidth=1, col = 'red', fill = 'pink', alpha = .6) +
  geom_vline(xintercept = mean(df.home$PT.diff), linetype = "dashed", size = 1.5, color = 'blue') +
  ggtitle("Home Court Point Differential from 2007 - 2016") +
  scale_x_continuous(breaks = plot.breaks) +
  scale_y_continuous() +
  coord_cartesian(xlim=c(-30,30)) +
  theme(axis.title.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank())

########################################
# Statistical test of Home Court Advantage
########################################

t.test(df.home$PT.diff)


########################################
# Parse HC Advantage by team/year
########################################

pd.data = data.frame(Team = unique(df$team))

for (i in 2007:2016)
{
  avg.pd.home <- tapply(df[df$Home == TRUE & df$season == i,]$PT.diff, 
                        df[df$Home == TRUE & df$season == i,]$team,
                        mean)
  avg.pd.away <- tapply(df[df$Home == FALSE & df$season == i,]$PT.diff, 
                        df[df$Home == FALSE & df$season == i,]$team, 
                        mean)
  
  # Take difference of home vs away differential.
  # This is to factor out which team has been better over the years
  temp <- data.frame(Team = names(avg.pd.home), 
                     avg.pd.home-avg.pd.away)
  pd.data <- merge(pd.data, temp, by = "Team", all.x = TRUE)
}

names(pd.data) <- c("Team",
                    "season_2007","season_2008","season_2009","season_2010","season_2011",
                    "season_2012","season_2013","season_2014","season_2015","season_2016")

ggplot(pd.data[!is.na(pd.data$season_2016),c(1,11)], 
       aes(reorder(Team,season_2016,mean),season_2016, fill = Team)) +
  geom_bar(stat = "identity") +
  ggtitle("Avg Point Differential by Team in 2015-2016") +
  theme(legend.position="none",
        axis.title.x = element_blank())



########################################
# Home Field Advantage over the Years
########################################

ggplot(df[df$Home == TRUE,], aes(season,PT.diff, fill = season)) +
  stat_boxplot() +
  scale_y_continuous() +
  ggtitle("Trends in Home Court Advantage") +
  ylab("Home Court Point Advantage") +
  xlab("Season") +
  theme(legend.position = "none")


















