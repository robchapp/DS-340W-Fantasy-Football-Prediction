require(forecast)
# Read offense Table
offense <- read.csv("Clean_offense.csv", header=TRUE)

players <- read.csv("Clean_players.csv", header=TRUE)

#Table of forecasted values for each offensive metric for each player
nFactors <- 13
nPlayers <- 409
Forecast_offense <- matrix(0, nrow = nPlayers, ncol = nFactors+2)
colnames(Forecast_offense) <- c("player", "pa", "pc","py","ints","tdp","ra","ry","tdr","trg","rec","recy","tdrec","fum","FF")

FF_weights <- c(0.0,0.0,0.0,-2.4,0.0,6.0,0.1,0.1,6.0,0.0,0.1,0.1,6.0,-2)

for (player in 1:nPlayers) {
  
  #Filtering by each individual player and sorting by season and week
  playerID <- as.character(players[player,1])
  Forecast_offense[player,1] <- playerID
  
  player_data <- subset(offense, offense$player == playerID) #CHANGE
  sort_player_data <- player_data[order(player_data$GAME.seas,player_data$GAME.wk),]
  
  for (factor in 1:nFactors) {
    
    #fitting the best ARIMA model for the given scoring metric
    ARIMAfilt <- auto.arima(sort_player_data[[7+factor]]) #CHANGE TO DOUBLE BRACKET COLUMN NUMBER
    pred <- forecast(ARIMAfilt,h=16) #using the best ARIMA model to predict values for the given offensive metric out 16 periods
    pred$mean
    Forecast_offense[player,1+factor] <- sum(pred$mean)
  }
  
  #add line of code to calculate FF score for each player and write it in column 15 of the table (have to add another column to table in line 11)
  player_factors <- as.double(Forecast_offense[player,2:(nFactors+1)])
  FF <- crossprod(FF_weights,player_factors)
  Forecast_offense[player,nFactors+2] <- FF
}

#add line of code to save "forecast_offense" into a CSV file, should call it the same as the table here
write.csv(Forecast_offense,"forecast_offense.csv")



require(forecast)
# Read kicking Table
kicking <- read.csv("Clean_kickScore.csv", header=TRUE)

players <- read.csv("Clean_kickers.csv", header=TRUE)

#Table of forecasted values for each kicking metric for each player
nFactors <- 4
nPlayers <- 37
Forecast_kickers <- matrix(0, nrow = nPlayers, ncol = nFactors+2)
colnames(Forecast_kickers) <- c("player","PAT","FGS","FGM","FGL","FFP")

FF_weights <- c(1,3,4,5)

for (player in 1:nPlayers) {
  
  #Filtering by each individual player and sorting by season and week
  playerID <- as.character(players[player,1])
  Forecast_kickers[player,1] <- playerID
  
  player_data <- subset(kicking,player == playerID) #CHANGE
  sort_player_data <- player_data[order(player_data$GAME.seas,player_data$GAME.wk),]
  
  for (factor in 1:nFactors) {
    
    #fitting the best ARIMA model for the given scoring metric
    ARIMAfilt <- auto.arima(sort_player_data[[6+factor]])
    pred <- forecast(ARIMAfilt,h=16) #using the best ARIMA model to predict values for the given kicking metric out 16 periods
    pred$mean
    Forecast_kickers[player,1+factor] <- sum(pred$mean)
  }
  
  #add line of code to calculate FF score for each player and write it in the table (have to add another column to table in line 11)
  player_factors <- as.double(Forecast_kickers[player,2:(nFactors+1)])
  FF <- crossprod(FF_weights,player_factors)
  Forecast_kickers[player,nFactors+2] <- FF
}

#add line of code to save "forecast_kickers" into a CSV file, should call it the same as the table here
write.csv(Forecast_kickers,"forecast_kickers.csv")



require(forecast)
# Read defense Table
defense <- read.csv("Clean_defense.csv", header=TRUE)

#team <- read.csv("")
players <- read.csv("Clean_Teams.csv", header=TRUE)

#Table of forecasted values for each defensive metric for each team
nFactors <- 1
nTeams <- 32
Forecast_defense <- matrix(0, nrow=nTeams, ncol = nFactors+2)
colnames(Forecast_defense) <- c("Team","DEF Fant. Pts.")

FF_weights <- c(1.0)

for (team in 1:nTeams) {
  
  #Filtering by each individual team and sorting by season and week
  this_team <- as.character(players[team,1])
  Forecast_defense[team,1] <- this_team
  
  player_data <- subset(defense,team == this_team) #CHANGE
  sort_player_data <- player_data[order(player_data$GAME.seas,player_data$GAME.wk),]
  
  for (factor in 1:nFactors) {
    
    #fitting the best ARIMA model for the given scoring metric
    ARIMAfilt <- auto.arima(sort_player_data[[3+factor]])
    pred <- forecast(ARIMAfilt,h=16) #using the best ARIMA model to predict values for the given defensive points out 16 periods
    pred$mean
    Forecast_defense[team,1+factor] <- sum(pred$mean)
  }
  
  #add line of code to calculate FF score for each player and write it in the table (have to add another column to table in line 11)
  player_factors <- as.double(Forecast_defense[team,2:(nFactors+1)])
  FF <- crossprod(FF_weights,player_factors)
  Forecast_defense[team,nFactors+2] <- FF
}

#add line of code to save "forecast_defense" into a CSV file, should call it the same as the table here
write.csv(Forecast_defense,"forecast_defense.csv")



require(forecast)
# Read Test 2007 Table
offense <- read.csv("Test_offense.csv", header=TRUE)

players <- read.csv("Clean_players.csv", header=TRUE)

#Table of forecasted values for each offensive metric for each player
nFactors <- 13
nPlayers <- 409
Forecast_offense_Test <- matrix(0, nrow = nPlayers, ncol = nFactors+2)
colnames(Forecast_offense_Test) <- c("player", "pa", "pc","py","ints","tdp","ra","ry","tdr","trg","rec","recy","tdrec","fum","FF")

FF_weights <- c(0.0,0.0,0.0,-2.4,0.0,6.0,0.1,0.1,6.0,0.0,0.1,0.1,6.0,-2)

for (player in 1:nPlayers) {
  
  #Filtering by each individual player and sorting by season and week
  playerID <- as.character(players[player,1])
  Forecast_offense_Test[player,1] <- playerID
  
  player_data <- subset(offense, offense$player == playerID) #CHANGE
  sort_player_data <- player_data[order(player_data$GAME.seas,player_data$GAME.wk),]
  
  #for (factor in 1:nFactors) {
  
  #fitting the best ARIMA model for the given scoring metric
  #ARIMAfilt <- auto.arima(sort_player_data[[7+factor]]) #CHANGE TO DOUBLE BRACKET COLUMN NUMBER
  #pred <- forecast(ARIMAfilt,h=16) #using the best ARIMA model to predict values for the given offensive metric out 16 periods
  #pred$mean
  #Forecast_offense_Test[player,1+factor] <- sum(pred$mean)
  #}
  
  #add line of code to calculate FF score for each player and write it in column 15 of the table (have to add another column to table in line 11)
  player_factors <- as.double(Forecast_offense_Test[player,2:(nFactors+1)])
  FF <- crossprod(FF_weights,player_factors)
  Forecast_offense_Test[player,nFactors+2] <- FF
}

#add line of code to save "forecast_offense" into a CSV file, should call it the same as the table here
write.csv(Forecast_offense_Test,"forecast_offense_Test2007.csv")
