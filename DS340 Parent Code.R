# ─────────────────────────────────────────────
# Fast/robust controls (tune these)
# ─────────────────────────────────────────────
FAST_MODE         <- TRUE
SEASONS_BACK      <- if (FAST_MODE) 3 else 26     # last N seasons
DEF_SEASONS_BACK  <- SEASONS_BACK                 # defense PBP window
MIN_WEEKS         <- if (FAST_MODE) 8 else 2      # min obs per player/metric
TOP_N_PLAYERS     <- if (FAST_MODE) 600 else Inf  # cap players for offense
TOP_N_KICKERS     <- if (FAST_MODE) 120 else Inf  # cap players for kickers
FORECAST_H        <- if (FAST_MODE) 8 else 16

# auto.arima knobs
ARIMA_APPROX      <- TRUE
ARIMA_STEPWISE    <- TRUE
ARIMA_SEASONAL    <- FALSE
ARIMA_MAX_ORDER   <- 5

# ─────────────────────────────────────────────
# Packages
# ─────────────────────────────────────────────
suppressPackageStartupMessages({
  library(forecast)
  library(nflfastR)
  library(nflreadr)
})

# small helper FIRST (used in data.frame() below)
`%||%` <- function(x, y) if (!is.null(x)) x else y

# ─────────────────────────────────────────────
# Seasons
# ─────────────────────────────────────────────
most <- nflreadr::most_recent_season()
start_season <- max(1999, most - SEASONS_BACK + 1)
end_season   <- most - 1  # stop at last completed season

# convenience
season_range_off <- start_season:end_season
season_range_def <- (most - DEF_SEASONS_BACK + 1):end_season

# ─────────────────────────────────────────────
# Helper: team summary replacement for calculate_team_stats()
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
# Utility: fit + forecast sum for a numeric vector
# ─────────────────────────────────────────────
forecast_sum <- function(series, h = FORECAST_H) {
  series <- as.numeric(series)
  series <- series[is.finite(series)]
  if (length(series) >= 2 && any(series != series[1])) {
    fit <- auto.arima(
      series,
      approximation = ARIMA_APPROX,
      stepwise      = ARIMA_STEPWISE,
      seasonal      = ARIMA_SEASONAL,
      max.order     = ARIMA_MAX_ORDER
    )
    pred <- forecast(fit, h = h)
    return(sum(as.numeric(pred$mean)))
  }
  0
}

# ─────────────────────────────────────────────
# OFFENSE
# ─────────────────────────────────────────────
off_raw <- nflreadr::load_player_stats(seasons = season_range_off)

player_col <- if ("player_name" %in% names(off_raw)) "player_name" else if ("player" %in% names(off_raw)) "player" else "name"
team_col   <- if ("recent_team" %in% names(off_raw)) "recent_team" else if ("team" %in% names(off_raw)) "team" else NA

offense <- transform(
  data.frame(
    player    = off_raw[[player_col]],
    TEAM      = if (!is.na(team_col)) off_raw[[team_col]] else NA_character_,
    POS       = off_raw[["position"]] %||% NA_character_,
    GAME.seas = off_raw[["season"]],
    GAME.wk   = off_raw[["week"]],
    # metrics (keep names stable)
    pa    = off_raw[["passing_attempts"]]    %||% off_raw[["attempts"]]               %||% 0,
    pc    = off_raw[["passing_completions"]] %||% off_raw[["completions"]]            %||% 0,
    py    = off_raw[["passing_yards"]]       %||% off_raw[["pass_yards"]]             %||% 0,
    ints  = off_raw[["interceptions"]]       %||% off_raw[["int"]]                    %||% 0,
    tdp   = off_raw[["passing_tds"]]         %||% off_raw[["pass_td"]]                %||% 0,
    ra    = off_raw[["rushing_attempts"]]    %||% off_raw[["carries"]]                %||% 0,
    ry    = off_raw[["rushing_yards"]]       %||% off_raw[["rush_yards"]]             %||% 0,
    tdr   = off_raw[["rushing_tds"]]         %||% off_raw[["rush_td"]]                %||% 0,
    trg   = off_raw[["targets"]]             %||% off_raw[["receive_targets"]]        %||% 0,
    rec   = off_raw[["receptions"]]          %||% off_raw[["receive_receptions"]]     %||% 0,
    recy  = off_raw[["receiving_yards"]]     %||% off_raw[["receive_yards"]]          %||% 0,
    tdrec = off_raw[["receiving_tds"]]       %||% off_raw[["receive_td"]]             %||% 0,
    fum   = off_raw[["fumbles_lost"]]        %||% off_raw[["fumbles"]]                %||% 0
  ),
  stringsAsFactors = FALSE
)

# Filter: drop players with too few weeks overall & prefer recent activity
offense$has_play <- rowSums(is.finite(as.matrix(offense[, c("pa","pc","py","ints","tdp","ra","ry","tdr","trg","rec","recy","tdrec","fum")]))) > 0

weeks_by_player <- aggregate(list(nw = offense$has_play), by = list(player = offense$player), FUN = sum, na.rm = TRUE)
recent_flag <- offense$GAME.seas >= max(season_range_off) - 1
recent_weeks <- aggregate(list(nw_recent = recent_flag), by = list(player = offense$player), FUN = sum, na.rm = TRUE)
sel <- merge(weeks_by_player, recent_weeks, by = "player", all.x = TRUE)
sel$nw_recent[is.na(sel$nw_recent)] <- 0

sel <- sel[sel$nw >= MIN_WEEKS, ]
# Rank by recent weeks then total weeks
sel <- sel[order(-sel$nw_recent, -sel$nw), ]
if (is.finite(TOP_N_PLAYERS)) sel <- head(sel, TOP_N_PLAYERS)

players_off <- sel$player
nPlayers_off <- length(players_off)

# Forecast table
off_metric_cols <- c("pa","pc","py","ints","tdp","ra","ry","tdr","trg","rec","recy","tdrec","fum")
Forecast_offense <- as.data.frame(matrix(0, nrow = nPlayers_off, ncol = length(off_metric_cols) + 2))
colnames(Forecast_offense) <- c("player", off_metric_cols, "FF")
Forecast_offense$player <- as.character(players_off)

# Named weights (13 metrics)
FF_weights_off <- c(
  pa=0.0, pc=0.0, py=0.0, ints=-2.4, tdp=0.0,
  ra=6.0, ry=0.1, tdr=0.1, trg=6.0, rec=0.0,
  recy=0.1, tdrec=0.1, fum=-2.0
)

for (i in seq_len(nPlayers_off)) {
  pid <- players_off[i]
  pdata <- offense[offense$player == pid, ]
  if (!nrow(pdata)) next
  pdata <- pdata[order(pdata$GAME.seas, pdata$GAME.wk), ]
  
  # per-metric forecasts
  for (m in off_metric_cols) {
    Forecast_offense[i, m] <- forecast_sum(pdata[[m]], h = FORECAST_H)
  }
  vals <- as.numeric(Forecast_offense[i, off_metric_cols])
  Forecast_offense$FF[i] <- sum(vals * FF_weights_off[off_metric_cols], na.rm = TRUE)
}
write.csv(Forecast_offense, "forecast_offense.csv", row.names = FALSE)

# ─────────────────────────────────────────────
# KICKERS
# ─────────────────────────────────────────────
kick_raw <- nflreadr::load_player_stats(seasons = season_range_off)
k_player_col <- if ("player_name" %in% names(kick_raw)) "player_name" else if ("player" %in% names(kick_raw)) "player" else "name"

kicking <- data.frame(
  player    = kick_raw[[k_player_col]],
  TEAM      = kick_raw[["recent_team"]] %||% kick_raw[["team"]],
  GAME.seas = kick_raw[["season"]],
  GAME.wk   = kick_raw[["week"]],
  PAT = kick_raw[["extra_points_made"]] %||% kick_raw[["xpm"]]    %||% 0,
  FGS = kick_raw[["field_goals_made"]]  %||% kick_raw[["fgm"]]    %||% 0,
  FGM = kick_raw[["field_goals_made"]]  %||% kick_raw[["fgm"]]    %||% 0,
  FGL = kick_raw[["field_goals_longest"]] %||% kick_raw[["fg_long"]] %||% 0,
  stringsAsFactors = FALSE
)

weeks_by_k <- aggregate(list(nw = rep(1, nrow(kicking))), by = list(player = kicking$player), FUN = length)
k_recent_weeks <- aggregate(list(nw_recent = kicking$GAME.seas >= max(season_range_off) - 1), by = list(player = kicking$player), FUN = sum, na.rm = TRUE)
ksel <- merge(weeks_by_k, k_recent_weeks, by = "player", all.x = TRUE)
ksel$nw_recent[is.na(ksel$nw_recent)] <- 0
ksel <- ksel[ksel$nw >= MIN_WEEKS, ]
ksel <- ksel[order(-ksel$nw_recent, -ksel$nw), ]
if (is.finite(TOP_N_KICKERS)) ksel <- head(ksel, TOP_N_KICKERS)

players_k <- ksel$player
nPlayers_k <- length(players_k)

k_metric_cols <- c("PAT","FGS","FGM","FGL")
Forecast_kickers <- as.data.frame(matrix(0, nrow = nPlayers_k, ncol = length(k_metric_cols) + 2))
colnames(Forecast_kickers) <- c("player", k_metric_cols, "FFP")
Forecast_kickers$player <- as.character(players_k)

FF_weights_k <- c(PAT=1, FGS=3, FGM=4, FGL=5)

for (i in seq_len(nPlayers_k)) {
  pid <- players_k[i]
  pdata <- kicking[kicking$player == pid, ]
  if (!nrow(pdata)) next
  pdata <- pdata[order(pdata$GAME.seas, pdata$GAME.wk), ]
  
  for (m in k_metric_cols) {
    Forecast_kickers[i, m] <- forecast_sum(pdata[[m]], h = FORECAST_H)
  }
  vals <- as.numeric(Forecast_kickers[i, k_metric_cols])
  Forecast_kickers$FFP[i] <- sum(vals * FF_weights_k[k_metric_cols], na.rm = TRUE)
}
write.csv(Forecast_kickers, "forecast_kickers.csv", row.names = FALSE)

# ─────────────────────────────────────────────
# DEFENSE (trimmed PBP window)
# ─────────────────────────────────────────────
pbp <- nflfastR::load_pbp(season_range_def)

dst_weekly <- aggregate(
  list(dst_pts =
         ifelse(pbp$defteam == pbp$return_team & pbp$touchdown == 1, 6, 0) +
         ifelse(!is.na(pbp$sack)         & pbp$sack == 1, 1, 0) +
         ifelse(!is.na(pbp$interception) & pbp$interception == 1, 2, 0) +
         ifelse(!is.na(pbp$fumble_forced)& pbp$fumble_forced == 1, 2, 0)),
  by = list(team = pbp$defteam, GAME.seas = pbp$season, GAME.wk = pbp$week),
  FUN = sum, na.rm = TRUE
)

defense <- data.frame(
  team      = dst_weekly$team,
  GAME.seas = dst_weekly$GAME.seas,
  GAME.wk   = dst_weekly$GAME.wk,
  DEFPTS    = dst_weekly$dst_pts,
  stringsAsFactors = FALSE
)

teams_df <- data.frame(Team = sort(unique(defense$team)), stringsAsFactors = FALSE)
nTeams <- nrow(teams_df)

Forecast_defense <- data.frame(Team = teams_df$Team, `DEF Fant. Pts.` = 0)

for (i in seq_len(nTeams)) {
  tm <- Forecast_defense$Team[i]
  pdata <- defense[defense$team == tm, ]
  if (!nrow(pdata)) next
  pdata <- pdata[order(pdata$GAME.seas, pdata$GAME.wk), ]
  Forecast_defense[i, "DEF Fant. Pts."] <- forecast_sum(pdata$DEFPTS, h = FORECAST_H)
}
write.csv(Forecast_defense, "forecast_defense.csv", row.names = FALSE)

# ─────────────────────────────────────────────
# TEST 2023 (OFFENSE) — keep structure, fast path (no re-forecasting)
# ─────────────────────────────────────────────
offense_2023 <- subset(offense, GAME.seas == 2023)
players_2023 <- sort(unique(offense_2023$player))
Forecast_offense_Test <- data.frame(
  player = players_2023,
  pa=0, pc=0, py=0, ints=0, tdp=0, ra=0, ry=0, tdr=0, trg=0, rec=0, recy=0, tdrec=0, fum=0,
  FF=0, check.names = FALSE
)
# If you later add per-metric forecasts for 2023 here, reuse forecast_sum() and FF_weights_off.

# Optional: team summary (not used elsewhere but kept for parity with your original)
team_stats <- calculate_team_stats(seasons = season_range_off)
