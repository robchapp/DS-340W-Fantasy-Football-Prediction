# ─────────────────────────────────────────────
# Packages
# ─────────────────────────────────────────────
suppressPackageStartupMessages({
  library(forecast)
  library(nflfastR)
  library(nflreadr)
})

# ─────────────────────────────────────────────
# Seasons
# ─────────────────────────────────────────────
start_season <- 1999
end_season   <- nflreadr::most_recent_season() - 1

# ─────────────────────────────────────────────
# Helper: replacement for calculate_team_stats()
# ─────────────────────────────────────────────
calculate_team_stats <- function(seasons) {
  off_raw <- nflreadr::load_player_stats(seasons = seasons)
  team_col <- if ("recent_team" %in% names(off_raw)) "recent_team" else if ("team" %in% names(off_raw)) "team" else NA
  if (is.na(team_col)) stop("No team column found")
  
  num_cols <- intersect(
    c("passing_yards","rushing_yards","receiving_yards",
      "passing_tds","rushing_tds","receiving_tds",
      "interceptions","fumbles_lost"),
    names(off_raw)
  )
  aggregate(
    off_raw[num_cols],
    by = list(team = off_raw[[team_col]], season = off_raw[["season"]]),
    FUN = sum, na.rm = TRUE
  )
}

# ─────────────────────────────────────────────
# OFFENSE
# ─────────────────────────────────────────────
off_raw <- nflreadr::load_player_stats(seasons = start_season:end_season)
player_col <- if ("player_name" %in% names(off_raw)) "player_name" else if ("player" %in% names(off_raw)) "player" else "name"
team_col   <- if ("recent_team" %in% names(off_raw)) "recent_team" else if ("team" %in% names(off_raw)) "team" else NA

offense <- transform(
  data.frame(
    player    = off_raw[[player_col]],
    TEAM      = if (!is.na(team_col)) off_raw[[team_col]] else NA_character_,
    POS       = if ("position" %in% names(off_raw)) off_raw[["position"]] else NA_character_,
    GAME.seas = off_raw[["season"]],
    GAME.wk   = off_raw[["week"]],
    C5        = NA_real_, C6 = NA_real_,
    pa    = off_raw[["passing_attempts"]]   %||% off_raw[["attempts"]]    %||% 0,
    pc    = off_raw[["passing_completions"]] %||% off_raw[["completions"]] %||% 0,
    py    = off_raw[["passing_yards"]]      %||% off_raw[["pass_yards"]]  %||% 0,
    ints  = off_raw[["interceptions"]]      %||% off_raw[["int"]]         %||% 0,
    tdp   = off_raw[["passing_tds"]]        %||% off_raw[["pass_td"]]     %||% 0,
    ra    = off_raw[["rushing_attempts"]]   %||% off_raw[["carries"]]     %||% 0,
    ry    = off_raw[["rushing_yards"]]      %||% off_raw[["rush_yards"]]  %||% 0,
    tdr   = off_raw[["rushing_tds"]]        %||% off_raw[["rush_td"]]     %||% 0,
    trg   = off_raw[["targets"]]            %||% off_raw[["receive_targets"]]    %||% 0,
    rec   = off_raw[["receptions"]]         %||% off_raw[["receive_receptions"]] %||% 0,
    recy  = off_raw[["receiving_yards"]]    %||% off_raw[["receive_yards"]]      %||% 0,
    tdrec = off_raw[["receiving_tds"]]      %||% off_raw[["receive_td"]]         %||% 0,
    fum   = off_raw[["fumbles_lost"]]       %||% off_raw[["fumbles"]]            %||% 0
  ),
  stringsAsFactors = FALSE
)

`%||%` <- function(x,y) if (!is.null(x)) x else y

players_off <- data.frame(player = unique(offense$player))
nPlayers_off <- nrow(players_off)

Forecast_offense <- as.data.frame(matrix(0, nrow = nPlayers_off, ncol = 15))
colnames(Forecast_offense) <- c("player","pa","pc","py","ints","tdp","ra","ry","tdr","trg","rec","recy","tdrec","fum","FF")
Forecast_offense$player <- as.character(players_off$player)

# Named weights (13 metrics)
FF_weights_off <- c(
  pa=0.0, pc=0.0, py=0.0, ints=-2.4, tdp=0.0,
  ra=6.0, ry=0.1, tdr=0.1, trg=6.0, rec=0.0,
  recy=0.1, tdrec=0.1, fum=-2.0
)

off_metric_cols <- names(FF_weights_off)

for (i in seq_len(nPlayers_off)) {
  playerID <- Forecast_offense$player[i]
  pdata <- subset(offense, player == playerID)
  if (nrow(pdata) == 0) next
  pdata <- pdata[order(pdata$GAME.seas,pdata$GAME.wk),]
  
  for (j in seq_along(off_metric_cols)) {
    series <- as.numeric(pdata[[off_metric_cols[j]]])
    series <- series[is.finite(series)]
    if (length(series) >= 2 && any(series != series[1])) {
      fit <- auto.arima(series)
      pred <- forecast(fit,h=16)
      Forecast_offense[i, off_metric_cols[j]] <- sum(pred$mean)
    } else {
      Forecast_offense[i, off_metric_cols[j]] <- 0
    }
  }
  vals <- as.numeric(Forecast_offense[i, off_metric_cols])
  Forecast_offense$FF[i] <- sum(vals * FF_weights_off[off_metric_cols], na.rm=TRUE)
}
write.csv(Forecast_offense,"forecast_offense.csv",row.names=FALSE)

# ─────────────────────────────────────────────
# KICKERS
# ─────────────────────────────────────────────
kick_raw <- nflreadr::load_player_stats(seasons=start_season:end_season)
k_player_col <- if ("player_name" %in% names(kick_raw)) "player_name" else if ("player" %in% names(kick_raw)) "player" else "name"

kicking <- data.frame(
  player    = kick_raw[[k_player_col]],
  TEAM      = kick_raw[["recent_team"]] %||% kick_raw[["team"]],
  GAME.seas = kick_raw[["season"]],
  GAME.wk   = kick_raw[["week"]],
  C5=NA_real_, C6=NA_real_,
  PAT = kick_raw[["extra_points_made"]] %||% kick_raw[["xpm"]] %||% 0,
  FGS = kick_raw[["field_goals_made"]]  %||% kick_raw[["fgm"]] %||% 0,
  FGM = kick_raw[["field_goals_made"]]  %||% kick_raw[["fgm"]] %||% 0,
  FGL = kick_raw[["field_goals_longest"]] %||% kick_raw[["fg_long"]] %||% 0
)

players_k <- data.frame(player=unique(kicking$player))
nPlayers_k <- nrow(players_k)

Forecast_kickers <- as.data.frame(matrix(0,nrow=nPlayers_k,ncol=6))
colnames(Forecast_kickers) <- c("player","PAT","FGS","FGM","FGL","FFP")
Forecast_kickers$player <- as.character(players_k$player)

FF_weights_k <- c(PAT=1,FGS=3,FGM=4,FGL=5)

for (i in seq_len(nPlayers_k)) {
  playerID <- Forecast_kickers$player[i]
  pdata <- subset(kicking, player==playerID)
  if (nrow(pdata)==0) next
  pdata <- pdata[order(pdata$GAME.seas,pdata$GAME.wk),]
  
  for (j in names(FF_weights_k)) {
    series <- as.numeric(pdata[[j]])
    series <- series[is.finite(series)]
    if (length(series)>=2 && any(series!=series[1])) {
      fit <- auto.arima(series); pred <- forecast(fit,h=16)
      Forecast_kickers[i,j] <- sum(pred$mean)
    } else Forecast_kickers[i,j] <- 0
  }
  vals <- as.numeric(Forecast_kickers[i,names(FF_weights_k)])
  Forecast_kickers$FFP[i] <- sum(vals*FF_weights_k,na.rm=TRUE)
}
write.csv(Forecast_kickers,"forecast_kickers.csv",row.names=FALSE)

# ─────────────────────────────────────────────
# DEFENSE
# ─────────────────────────────────────────────
pbp <- nflfastR::load_pbp(start_season:end_season)
dst_weekly <- aggregate(
  list(dst_pts =
         ifelse(pbp$defteam==pbp$return_team & pbp$touchdown==1,6,0)+
         ifelse(pbp$sack==1,1,0)+
         ifelse(pbp$interception==1,2,0)+
         ifelse(pbp$fumble_forced==1,2,0)),
  by=list(team=pbp$defteam,GAME.seas=pbp$season,GAME.wk=pbp$week),
  FUN=sum,na.rm=TRUE
)

defense <- data.frame(team=dst_weekly$team,GAME.seas=dst_weekly$GAME.seas,
                      GAME.wk=dst_weekly$GAME.wk,DEFPTS=dst_weekly$dst_pts)

teams_df <- data.frame(Team=sort(unique(defense$team)))
nTeams <- nrow(teams_df)

Forecast_defense <- as.data.frame(matrix(0,nrow=nTeams,ncol=2))
colnames(Forecast_defense) <- c("Team","DEF Fant. Pts.")
Forecast_defense$Team <- as.character(teams_df$Team)

for (i in seq_len(nTeams)) {
  tm <- Forecast_defense$Team[i]
  pdata <- subset(defense,team==tm)
  if (nrow(pdata)==0) next
  pdata <- pdata[order(pdata$GAME.seas,pdata$GAME.wk),]
  series <- as.numeric(pdata$DEFPTS); series <- series[is.finite(series)]
  if (length(series)>=2 && any(series!=series[1])) {
    fit<-auto.arima(series); pred<-forecast(fit,h=16)
    Forecast_defense[i,"DEF Fant. Pts."] <- sum(pred$mean)
  } else Forecast_defense[i,"DEF Fant. Pts."] <- 0
}
write.csv(Forecast_defense,"forecast_defense.csv",row.names=FALSE)

# ─────────────────────────────────────────────
# TEST 2007 (OFFENSE)
# ─────────────────────────────────────────────
offense_2007 <- subset(offense,GAME.seas==2007)
players_2007 <- data.frame(player=sort(unique(offense_2007$player)))
nPlayers_t <- nrow(players_2007)

Forecast_offense_Test <- as.data.frame(matrix(0,nrow=nPlayers_t,ncol=15))
colnames(Forecast_offense_Test) <- colnames(Forecast_offense)

Forecast_offense_Test$player <- as.character(players_2007$player)
for (i in seq_len(nPlayers_t)) {
  playerID <- Forecast_offense_Test$player[i]
  vals <- as.numeric(Forecast_offense_Test[i,off_metric_cols])
  Forecast_offense_Test$FF[i] <- sum(vals*FF_weights_off[off_metric_cols],na.rm=TRUE)
}
write.csv(Forecast_offense_Test,"forecast_offense_Test2007.csv",row.names=FALSE)
