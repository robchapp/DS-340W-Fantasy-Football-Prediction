# ============================================================
# Fast ARIMAX Fantasy Projections with Context xregs (End-to-End)
# ============================================================

# ────────────────────────────────────────────────────────────
# Controls (speed-tuned)
# ────────────────────────────────────────────────────────────
FAST_MODE         <- TRUE
SEASONS_BACK      <- 2                  # ↓ was 3
MIN_WEEKS         <- 10                 # ↓ avoid tiny series
TOP_N_PLAYERS     <- 300                # ↓ was 600
FORECAST_H        <- 6                  # ↓ was 8

# ARIMA knobs (smaller search)
ARIMA_APPROX      <- TRUE
ARIMA_STEPWISE    <- TRUE
ARIMA_SEASONAL    <- FALSE
ARIMA_MAX_ORDER   <- 3                  # ↓ was 5

# ────────────────────────────────────────────────────────────
# Packages
# ────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr);  library(tidyr);  library(stringr)
  library(forecast); library(nflfastR); library(nflreadr)
  library(zoo);    library(readr)
  library(future); library(future.apply)
})

plan(multisession, workers = max(1, parallel::detectCores() - 1))

# helpers
`%||%` <- function(x, y) if (!is.null(x)) x else y

# ────────────────────────────────────────────────────────────
# Seasons (stop at last COMPLETED season)
# ────────────────────────────────────────────────────────────
most <- nflreadr::most_recent_season()
start_season <- max(1999, most - SEASONS_BACK + 1)
end_season   <- most - 1
season_range_off <- start_season:end_season
message(sprintf("Seasons used: %d–%d", start_season, end_season))

# ────────────────────────────────────────────────────────────
# Feature engineering (team-week context)
# ────────────────────────────────────────────────────────────
bucket_weather <- function(x) {
  x <- tolower(as.character(x)); x[is.na(x) | x==""] <- "unknown"
  case_when(
    str_detect(x, "rain|shower|drizzle") ~ "rain",
    str_detect(x, "snow|flurr")          ~ "snow",
    str_detect(x, "wind")                ~ "windy",
    str_detect(x, "overcast|cloud")      ~ "cloudy",
    str_detect(x, "sun|clear|fair")      ~ "clear",
    TRUE                                 ~ "unknown"
  )
}

norm_roof <- function(x) {
  x <- tolower(as.character(x))
  case_when(
    str_detect(x, "indoor|dome|closed") ~ "indoor",
    TRUE                                ~ "outdoor"
  )
}

norm_surface <- function(x) {
  x <- tolower(as.character(x))
  case_when(
    str_detect(x, "turf|artificial") ~ "turf",
    str_detect(x, "grass")           ~ "grass",
    TRUE                             ~ "other"
  )
}

build_teamweek_features <- function(seasons) {
  pbp <- nflfastR::load_pbp(seasons)
  
  team_week <- pbp %>%
    filter(!is.na(posteam)) %>%
    group_by(season, week, game_id, posteam) %>%
    summarise(
      rush_attempts = sum(rush_attempt %in% c(1, TRUE), na.rm = TRUE),
      pass_attempts = sum(pass_attempt %in% c(1, TRUE), na.rm = TRUE),
      roof    = norm_roof(first(na.omit(roof))),
      surface = norm_surface(first(na.omit(surface))),
      weather = bucket_weather(first(na.omit(weather))),
      home_team = first(na.omit(home_team)),
      away_team = first(na.omit(away_team)),
      .groups = "drop"
    ) %>%
    mutate(
      team    = posteam,
      is_home = if_else(team == home_team, 1L, 0L)
    ) %>%
    select(season, week, game_id, team,
           home_team, away_team, is_home, roof, surface, weather,
           rush_attempts, pass_attempts)
  
  team_roll <- team_week %>%
    arrange(team, season, week) %>%
    group_by(team) %>%
    mutate(
      rush_share = rush_attempts / pmax(1, rush_attempts + pass_attempts),
      pass_share = 1 - rush_share,
      rush_attempts_rm = lag(rollapplyr(rush_attempts, 5, mean, na.rm = TRUE, partial = TRUE)),
      pass_attempts_rm = lag(rollapplyr(pass_attempts, 5, mean, na.rm = TRUE, partial = TRUE))
    ) %>%
    ungroup()
  
  list(team_week = team_week, team_roll = team_roll)
}

feat <- build_teamweek_features(season_range_off)
team_week <- feat$team_week
team_roll <- feat$team_roll

# stable factor levels for HIST/FUT design matrices
team_week$roof    <- factor(team_week$roof,    levels = c("indoor","outdoor"))
team_week$surface <- factor(team_week$surface, levels = c("grass","turf","other"))
team_week$weather <- factor(team_week$weather, levels = c("clear","cloudy","rain","snow","windy","unknown"))

lvl_ref <- list(
  roof    = levels(team_week$roof),
  surface = levels(team_week$surface),
  weather = levels(team_week$weather)
)

# ────────────────────────────────────────────────────────────
# xreg builders (NO team fixed effects → faster, stabler)
# ────────────────────────────────────────────────────────────
make_xreg <- function(df, lvl_ref) {
  df$roof    <- factor(norm_roof(df$roof),    levels = lvl_ref$roof)
  df$surface <- factor(norm_surface(df$surface), levels = lvl_ref$surface)
  df$weather <- factor(bucket_weather(df$weather), levels = lvl_ref$weather)
  
  # lean formula (no team FE)
  mm <- model.matrix(~ is_home + roof + surface + weather +
                       rush_attempts + pass_attempts, data = df)
  storage.mode(mm) <- "double"
  mm
}

# FUTURE xreg cache
xreg_fut_cache <- new.env(parent = emptyenv())

build_future_xreg <- function(team_id, start_season, start_week, horizon,
                              team_week, team_roll, lvl_ref) {
  key <- paste(team_id, start_season, start_week, horizon, sep = "_")
  if (exists(key, envir = xreg_fut_cache)) return(get(key, envir = xreg_fut_cache))
  
  sched <- nflreadr::load_schedules(most) %>%
    transmute(season, week, game_id, home_team, away_team, roof = norm_roof(roof), surface = norm_surface(surface))
  
  fut <- sched %>%
    filter((season > start_season) | (season == start_season & week > start_week)) %>%
    arrange(season, week) %>%
    mutate(my_team = if_else(home_team == team_id, home_team,
                             if_else(away_team == team_id, away_team, NA_character_))) %>%
    filter(!is.na(my_team)) %>%
    mutate(
      is_home = if_else(my_team == home_team, 1L, 0L),
      weather = "unknown"
    ) %>%
    head(horizon)
  
  if (nrow(fut) == 0) {
    # return zero matrix with right columns
    stub <- data.frame(is_home=0, roof="outdoor", surface="grass",
                       weather="unknown", rush_attempts=0, pass_attempts=0)
    mm   <- make_xreg(stub, lvl_ref)
    mm0  <- matrix(0, nrow = horizon, ncol = ncol(mm)); colnames(mm0) <- colnames(mm)
    assign(key, mm0, envir = xreg_fut_cache); return(mm0)
  }
  
  last_roll <- team_roll %>% filter(team == team_id) %>%
    arrange(season, week) %>% slice_tail(n = 1)
  
  ra_rm <- last_roll$rush_attempts_rm %||% 25
  pa_rm <- last_roll$pass_attempts_rm %||% 30
  
  fut2 <- fut %>% transmute(
    is_home, roof, surface, weather,
    rush_attempts = ra_rm, pass_attempts = pa_rm
  )
  
  Xf <- make_xreg(fut2, lvl_ref)
  assign(key, Xf, envir = xreg_fut_cache)
  Xf
}

# ────────────────────────────────────────────────────────────
# Load offense data (weekly player stats)
# ────────────────────────────────────────────────────────────
off_raw <- nflreadr::load_player_stats(seasons = season_range_off)

player_col <- if ("player_name" %in% names(off_raw)) "player_name" else
  if ("player"      %in% names(off_raw)) "player"      else "name"
team_col   <- if ("recent_team" %in% names(off_raw)) "recent_team" else
  if ("team"        %in% names(off_raw)) "team"        else NA

offense <- transform(
  data.frame(
    player    = off_raw[[player_col]],
    TEAM      = if (!is.na(team_col)) off_raw[[team_col]] else NA_character_,
    POS       = off_raw[["position"]] %||% NA_character_,
    GAME.seas = off_raw[["season"]],
    GAME.wk   = off_raw[["week"]],
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

# Player filter: must have MIN_WEEKS rows; prefer recent activity implicitly
offense$has_play <- rowSums(is.finite(as.matrix(offense[, c("pa","pc","py","ints","tdp","ra","ry","tdr","trg","rec","recy","tdrec","fum")]))) > 0
weeks_by_player <- offense %>%
  group_by(player) %>%
  summarise(nw = sum(has_play, na.rm = TRUE), .groups = "drop") %>%
  filter(nw >= MIN_WEEKS) %>%
  arrange(desc(nw))

if (is.finite(TOP_N_PLAYERS)) weeks_by_player <- head(weeks_by_player, TOP_N_PLAYERS)
players_off   <- weeks_by_player$player
nPlayers_off  <- length(players_off)
message(sprintf("Offense players kept: %d", nPlayers_off))

# ────────────────────────────────────────────────────────────
# ARIMAX forecaster (sum over horizon)
# ────────────────────────────────────────────────────────────
forecast_sum_arimax <- function(y_vec, xreg_hist, xreg_fut, h) {
  y <- as.numeric(y_vec)
  keep <- is.finite(y) & apply(is.finite(xreg_hist), 1, all)
  y <- y[keep]
  X <- xreg_hist[keep, , drop = FALSE]
  
  if (length(y) < 2 || length(unique(y)) < 2) {
    mu <- mean(y, na.rm = TRUE) %||% 0
    return(sum(rep(mu, h)))
  }
  
  fit <- forecast::auto.arima(
    y,
    xreg          = X,
    approximation = ARIMA_APPROX,
    stepwise      = ARIMA_STEPWISE,
    seasonal      = ARIMA_SEASONAL,
    max.order     = ARIMA_MAX_ORDER
  )
  fc <- forecast::forecast(fit, xreg = xreg_fut, h = h)
  sum(as.numeric(fc$mean))
}

# ────────────────────────────────────────────────────────────
# OFFENSE projections with ARIMAX
# ────────────────────────────────────────────────────────────
off_metric_cols <- c("pa","pc","py","ints","tdp","ra","ry","tdr","trg","rec","recy","tdrec","fum")

# Fantasy weights (example; adjust to your scoring)
FF_weights_off <- c(
  pa=0, pc=0, py=0, ints=-2.4, tdp=0,
  ra=6.0, ry=0.1, tdr=0.1, trg=6.0, rec=0.0,
  recy=0.1, tdrec=0.1, fum=-2.0
)

Forecast_offense <- as.data.frame(matrix(0, nrow = nPlayers_off, ncol = length(off_metric_cols) + 4))
colnames(Forecast_offense) <- c("player", "TEAM", off_metric_cols, "FF")
Forecast_offense$player <- as.character(players_off)

for (i in seq_len(nPlayers_off)) {
  pid <- players_off[i]
  pdata <- offense %>% filter(player == pid) %>% arrange(GAME.seas, GAME.wk)
  
  if (nrow(pdata) == 0) next
  
  # Join team-week context for historical xreg
  xctx <- pdata %>%
    left_join(
      team_week,
      by = c("GAME.seas" = "season",
             "GAME.wk"   = "week",
             "TEAM"      = "team")
    ) %>%
    mutate(
      is_home       = tidyr::replace_na(is_home, 0L),
      roof          = factor(tidyr::replace_na(as.character(roof), "outdoors"), levels = lvl_ref$roof),
      surface       = factor(tidyr::replace_na(as.character(surface), "grass"),    levels = lvl_ref$surface),
      weather       = factor(tidyr::replace_na(as.character(weather), "unknown"),  levels = lvl_ref$weather),
      team          = factor(tidyr::replace_na(as.character(TEAM), levels(team_week$team)[1]), levels = lvl_ref$team),
      rush_attempts = tidyr::replace_na(rush_attempts, 25),
      pass_attempts = tidyr::replace_na(pass_attempts, 30)
    )
  
  X_hist <- make_xreg(xctx, lvl_ref)
  
  # Build future xreg from schedule + tendencies
  last_season <- tail(pdata$GAME.seas, 1)
  last_week   <- tail(pdata$GAME.wk,   1)
  team_id     <- as.character(tail(pdata$TEAM, 1) %||% NA_character_)
  
  if (!is.na(team_id) && team_id %in% lvl_ref$team) {
    X_fut <- build_future_xreg(team_id, last_season, last_week, FORECAST_H, team_week, team_roll, lvl_ref)
  } else {
    # fallback to zero-matrix of correct width
    X_fut <- matrix(0, nrow = FORECAST_H, ncol = ncol(X_hist))
    colnames(X_fut) <- colnames(X_hist)
  }
  
  # Per-metric ARIMAX forecasts
  for (m in off_metric_cols) {
    Forecast_offense[i, m] <- forecast_sum_arimax(
      y_vec    = pdata[[m]],
      xreg_hist= X_hist,
      xreg_fut = X_fut,
      h        = FORECAST_H
    )
  }
  
  Forecast_offense$TEAM[i] <- tail(pdata$TEAM, 1) %||% NA_character_
  vals <- as.numeric(Forecast_offense[i, off_metric_cols])
  Forecast_offense$FF[i] <- sum(vals * FF_weights_off[off_metric_cols], na.rm = TRUE)
}

# Sort & write output
Forecast_offense_ranked <- Forecast_offense %>%
  arrange(desc(FF))

out_file <- "forecast_offense_arimax.csv"
readr::write_csv(Forecast_offense_ranked, out_file)
message(sprintf("Wrote %s with %d players.", out_file, nrow(Forecast_offense_ranked)))

# Top preview
print(utils::head(Forecast_offense_ranked, 20))
