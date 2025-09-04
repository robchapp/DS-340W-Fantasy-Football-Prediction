install.packages(c("forecast","nflfastR","nflreadr"))
library(forecast)
library(nflfastR)
library(nflreadr)

require(forecast)
require(nflfastR)

# --------------------
# NEW: build "offense" (player-week offense) from nflreadr/nflfastR
# Columns are arranged so that metric 1 starts at column 8 (so [[7+factor]] works unchanged)
# --------------------
start_season <- 1999
end_season   <- nflreadr::most_recent_season() - 1   # <- 2024 for today


off_raw <- nflreadr::load_player_stats(
  seasons   = start_season:end_season,
  stat_type = "offense"
)

# Normalize expected columns
player_col <- if ("player_name" %in% names(off_raw)) "player_name" else if ("player" %in% names(off_raw)) "player" else "name"
team_col   <- if ("recent_team" %in% names(off_raw)) "recent_team" else if ("team" %in% names(off_raw)) "team" else NA

# Build offense with EXACT index alignment:
# cols 1..7: non-metric placeholders + keys
# cols 8..20: the 13 metrics in your order: pa, pc, py, ints, tdp, ra, ry, tdr, trg, rec, recy, tdrec, fum
offense <- transform(
  data.frame(
    player    = off_raw[[player_col]],
    TEAM      = if (!is.na(team_col)) off_raw[[team_col]] else NA_character_,
    POS       = if ("position" %in% names(off_raw)) off_raw[["position"]] else NA_character_,
    GAME.seas = off_raw[["season"]],
    GAME.wk   = off_raw[["week"]],
    C5        = NA_real_,  # filler to keep pa at column 8
    C6        = NA_real_,  # filler to keep pa at column 8

    # metrics (keep order!)
    pa    = if ("attempts"           %in% names(off_raw)) off_raw[["attempts"]]            else if ("passing_attempts"   %in% names(off_raw)) off_raw[["passing_attempts"]]   else 0,
    pc    = if ("completions"        %in% names(off_raw)) off_raw[["completions"]]         else if ("passing_completions"%in% names(off_raw)) off_raw[["passing_completions"]] else 0,
    py    = if ("passing_yards"      %in% names(off_raw)) off_raw[["passing_yards"]]       else if ("pass_yards"         %in% names(off_raw)) off_raw[["pass_yards"]]          else 0,
    ints  = if ("interceptions"      %in% names(off_raw)) off_raw[["interceptions"]]       else if ("int"                %in% names(off_raw)) off_raw[["int"]]                 else 0,
    tdp   = if ("passing_tds"        %in% names(off_raw)) off_raw[["passing_tds"]]         else if ("pass_td"            %in% names(off_raw)) off_raw[["pass_td"]]             else 0,
    ra    = if ("carries"            %in% names(off_raw)) off_raw[["carries"]]             else if ("rushing_attempts"   %in% names(off_raw)) off_raw[["rushing_attempts"]]    else 0,
    ry    = if ("rushing_yards"      %in% names(off_raw)) off_raw[["rushing_yards"]]       else if ("rush_yards"         %in% names(off_raw)) off_raw[["rush_yards"]]          else 0,
    tdr   = if ("rushing_tds"        %in% names(off_raw)) off_raw[["rushing_tds"]]         else if ("rush_td"            %in% names(off_raw)) off_raw[["rush_td"]]             else 0,
    trg   = if ("targets"            %in% names(off_raw)) off_raw[["targets"]]             else if ("receive_targets"    %in% names(off_raw)) off_raw[["receive_targets"]]     else 0,
    rec   = if ("receptions"         %in% names(off_raw)) off_raw[["receptions"]]          else if ("receive_receptions" %in% names(off_raw)) off_raw[["receive_receptions"]]  else 0,
    recy  = if ("receiving_yards"    %in% names(off_raw)) off_raw[["receiving_yards"]]     else if ("receive_yards"      %in% names(off_raw)) off_raw[["receive_yards"]]       else 0,
    tdrec = if ("receiving_tds"      %in% names(off_raw)) off_raw[["receiving_tds"]]       else if ("receive_td"         %in% names(off_raw)) off_raw[["receive_td"]]          else 0,
    fum   = if ("fumbles_lost"       %in% names(off_raw)) off_raw[["fumbles_lost"]]        else if ("fumbles"            %in% names(off_raw)) off_raw[["fumbles"]]             else 0
  ),
  stringsAsFactors = FALSE
)

# Keep your team summaries (unchanged)
team_stats <- calculate_team_stats(seasons = 1999:most_recent_season())

# --------------------
# NEW: replace players CSV with names from offense
# Expectation: first column is the player ID string, as your code uses players[player,1]
# --------------------
players <- data.frame(player = unique(offense$player), check.names = FALSE, stringsAsFactors = FALSE)

# ======================================================================
# FROM HERE DOWN your original OFFENSE block stays the same
# ======================================================================

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



# =========================
# KICKERS
# =========================
require(forecast)

# --------------------
# NEW: build "kicking" from nflreadr; shape so metric 1 starts at col 7 (so [[6+factor]] works)
# --------------------
kick_raw <- nflreadr::load_player_stats(
  seasons   = start_season:end_season,
  stat_type = "kicking"
)

k_player_col <- if ("player_name" %in% names(kick_raw)) "player_name" else if ("player" %in% names(kick_raw)) "player" else "name"

kicking <- data.frame(
  player    = kick_raw[[k_player_col]],
  TEAM      = if ("recent_team" %in% names(kick_raw)) kick_raw[["recent_team"]] else if ("team" %in% names(kick_raw)) kick_raw[["team"]] else NA_character_,
  GAME.seas = kick_raw[["season"]],
  GAME.wk   = kick_raw[["week"]],
  C5        = NA_real_,   # filler so that PAT is column 7
  C6        = NA_real_,   # filler so that PAT is column 7
  PAT = if ("extra_points_made"   %in% names(kick_raw)) kick_raw[["extra_points_made"]]   else if ("xpm" %in% names(kick_raw)) kick_raw[["xpm"]] else 0,
  FGS = if ("field_goals_made"    %in% names(kick_raw)) kick_raw[["field_goals_made"]]    else if ("fgm" %in% names(kick_raw)) kick_raw[["fgm"]] else 0,
  FGM = if ("field_goals_made"    %in% names(kick_raw)) kick_raw[["field_goals_made"]]    else if ("fgm" %in% names(kick_raw)) kick_raw[["fgm"]] else 0,
  FGL = if ("field_goals_longest" %in% names(kick_raw)) kick_raw[["field_goals_longest"]] else if ("fg_long" %in% names(kick_raw)) kick_raw[["fg_long"]] else 0,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# --------------------
# NEW: replace kickers CSV with names from kicking
# --------------------
players <- data.frame(player = unique(kicking$player), check.names = FALSE, stringsAsFactors = FALSE)

# (UNCHANGED) Your original kicker block
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



# =========================
# DEFENSE
# =========================
require(forecast)

# --------------------
# NEW: build team-week defensive fantasy points proxy from play-by-play, then shape to expected columns
# We keep a single metric in column 4 (so [[3+factor]] works with nFactors=1)
# --------------------
pbp <- nflfastR::load_pbp(start_season:end_season)

# Simple DST-point proxy (edit if needed)
dst_weekly <- aggregate(
  list(dst_pts = ifelse(pbp$defteam == pbp$return_team & pbp$touchdown == 1, 6, 0) +
                 ifelse(!is.na(pbp$sack) & pbp$sack == 1, 1, 0) +
                 ifelse(!is.na(pbp$interception) & pbp$interception == 1, 2, 0) +
                 ifelse(!is.na(pbp$fumble_forced) & pbp$fumble_forced == 1, 2, 0)
  ),
  by = list(team = pbp$defteam, GAME.seas = pbp$season, GAME.wk = pbp$week),
  FUN = sum, na.rm = TRUE
)

# Defense table with expected columns
defense <- data.frame(
  team      = dst_weekly$team,
  GAME.seas = dst_weekly$GAME.seas,
  GAME.wk   = dst_weekly$GAME.wk,
  DEFPTS    = dst_weekly$dst_pts,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# --------------------
# NEW: replace teams CSV with list from defense
# --------------------
players <- data.frame(Team = sort(unique(defense$team)), check.names = FALSE, stringsAsFactors = FALSE)

# (UNCHANGED) Your original defense block
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



# =========================
# TEST 2007 (OFFENSE)
# =========================
require(forecast)

# --------------------
# NEW: replace 2007 test CSVs with filtered offense + derived players
# Keep the same object names used in your block
# --------------------
offense <- subset(offense, GAME.seas == 2007)
players <- data.frame(player = sort(unique(offense$player)), check.names = FALSE, stringsAsFactors = FALSE)

# (UNCHANGED) Your original Test block
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
  
  # (keeping your commented-out forecast section as-is)

  #add line of code to calculate FF score for each player and write it in column 15 of the table (have to add another column to table in line 11)
  player_factors <- as.double(Forecast_offense_Test[player,2:(nFactors+1)])
  FF <- crossprod(FF_weights,player_factors)
  Forecast_offense_Test[player,nFactors+2] <- FF
}

#add line of code to save "forecast_offense" into a CSV file, should call it the same as the table here
write.csv(Forecast_offense_Test,"forecast_offense_Test2007.csv")
