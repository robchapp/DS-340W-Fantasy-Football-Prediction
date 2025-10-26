# ============================================================
# ARIMAX vs Neural Net (MLP) Fantasy Projections + Backtest
# ============================================================

# ────────────────────────────────────────────────────────────
# Optional: auto-install missing packages (set to TRUE once)
# ────────────────────────────────────────────────────────────
INSTALL_MISSING <- FALSE
if (INSTALL_MISSING) {
  pkgs <- c("dplyr","tidyr","stringr","forecast","nnfor","nflfastR","nflreadr",
            "zoo","readr","future","future.apply")
  to_get <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_get)) install.packages(to_get)
}

# ────────────────────────────────────────────────────────────
# Controls (speed-tuned)
# ────────────────────────────────────────────────────────────
FAST_MODE         <- TRUE
SEASONS_BACK      <- 2
MIN_WEEKS         <- 10
TOP_N_PLAYERS     <- 300
FORECAST_H        <- 6

# ARIMA knobs
ARIMA_APPROX      <- TRUE
ARIMA_STEPWISE    <- TRUE
ARIMA_SEASONAL    <- FALSE
ARIMA_MAX_ORDER   <- 3

# Neural net knobs
NN_SIZE    <- if (FAST_MODE) 5  else 15   # hidden units
NN_REPS    <- if (FAST_MODE) 1  else 5    # ensemble reps
NN_DECAY   <- 0.001                        # weight decay
NN_LAGS    <- 8                            # AR lags of y
NN_DIFF    <- FALSE                        # differencing (usually FALSE)

# ────────────────────────────────────────────────────────────
# Packages
# ────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr);  library(tidyr);  library(stringr)
  library(forecast); library(nnfor)
  library(nflfastR); library(nflreadr)
  library(zoo);    library(readr)
  library(future); library(future.apply)
})

plan(multisession, workers = max(1, parallel::detectCores() - 1))
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

# lock factor levels
team_week$roof    <- factor(team_week$roof,    levels = c("indoor","outdoor"))
team_week$surface <- factor(team_week$surface, levels = c("grass","turf","other"))
team_week$weather <- factor(team_week$weather, levels = c("clear","cloudy","rain","snow","windy","unknown"))

lvl_ref <- list(
  roof    = levels(team_week$roof),
  surface = levels(team_week$surface),
  weather = levels(team_week$weather)
)

# ────────────────────────────────────────────────────────────
# xreg builders
# ────────────────────────────────────────────────────────────
make_xreg <- function(df, lvl_ref) {
  df$roof    <- factor(norm_roof(df$roof),       levels = lvl_ref$roof)
  df$surface <- factor(norm_surface(df$surface), levels = lvl_ref$surface)
  df$weather <- factor(bucket_weather(df$weather), levels = lvl_ref$weather)
  mm <- model.matrix(~ is_home + roof + surface + weather +
                       rush_attempts + pass_attempts, data = df)
  storage.mode(mm) <- "double"; mm
}

# FUTURE xreg cache (for production forecasts only)
xreg_fut_cache <- new.env(parent = emptyenv())
build_future_xreg <- function(team_id, start_season, start_week, horizon,
                              team_week, team_roll, lvl_ref) {
  key <- paste(team_id, start_season, start_week, horizon, sep = "_")
  if (exists(key, envir = xreg_fut_cache)) return(get(key, envir = xreg_fut_cache))
  
  sched <- nflreadr::load_schedules(most) %>%
    transmute(season, week, game_id, home_team, away_team,
              roof = norm_roof(roof), surface = norm_surface(surface))
  
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

# keep players with enough history
offense$has_play <- rowSums(is.finite(as.matrix(offense[, c("pa","pc","py","ints","tdp","ra","ry","tdr","trg","rec","recy","tdrec","fum")]))) > 0
weeks_by_player <- offense %>%
  group_by(player) %>%
  summarise(nw = sum(has_play, na.rm = TRUE), .groups = "drop") %>%
  filter(nw >= MIN_WEEKS) %>%
  arrange(desc(nw))
if (is.finite(TOP_N_PLAYERS)) weeks_by_player <- head(weeks_by_player, TOP_N_PLAYERS)
players_off  <- weeks_by_player$player
nPlayers_off <- length(players_off)
message(sprintf("Players kept: %d", nPlayers_off))

# ────────────────────────────────────────────────────────────
# Forecast helpers (ARIMAX & NN), both return SUM of next h
# ────────────────────────────────────────────────────────────
prepare_design <- function(y, X_hist, X_fut) {
  row_keep <- is.finite(y) & apply(is.finite(X_hist), 1, all)
  y  <- as.numeric(y[row_keep])
  Xh <- X_hist[row_keep, , drop = FALSE]
  Xf <- X_fut
  keep_var <- apply(Xh, 2, function(col) sd(col, na.rm = TRUE) > 1e-9)
  if (!any(keep_var) || length(unique(na.omit(y))) < 2)
    return(list(y=NULL,Xh=NULL,Xf=NULL, trivial=TRUE, mu=mean(y,na.rm=TRUE)))
  Xh <- Xh[, keep_var, drop = FALSE]; Xf <- Xf[, keep_var, drop = FALSE]
  qrh <- qr(Xh); piv <- qrh$pivot[seq_len(qrh$rank)]
  Xh <- Xh[, piv, drop = FALSE]; Xf <- Xf[, piv, drop = FALSE]
  center <- colMeans(Xh); sds <- apply(Xh, 2, sd)
  scale_cols <- function(A, center, scale) sweep(sweep(A, 2, center, `-`), 2, ifelse(scale == 0, 1, scale), `/`)
  Xh <- scale_cols(Xh, center, sds); Xf <- scale_cols(Xf, center, sds)
  list(y=y,Xh=Xh,Xf=Xf,trivial=FALSE,mu=NA_real_)
}

forecast_sum_arimax <- function(y_vec, xreg_hist, xreg_fut, h) {
  y <- as.numeric(y_vec)
  if (length(y) < 2 || length(unique(na.omit(y))) < 2) {
    mu <- mean(y, na.rm = TRUE); return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  prep <- prepare_design(y, xreg_hist, xreg_fut)
  if (isTRUE(prep$trivial)) {
    mu <- prep$mu; return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  y <- prep$y; Xh <- prep$Xh; Xf <- prep$Xf
  sum_fc <- function(fit, Xfuture = NULL) {
    fc <- if (is.null(Xfuture)) forecast(fit, h = h) else forecast(fit, xreg = Xfuture, h = h)
    sum(as.numeric(fc$mean))
  }
  fit1 <- try(Arima(y, order = c(0,1,1), xreg = Xh, include.mean = FALSE), silent = TRUE)
  if (!inherits(fit1, "try-error")) return(sum_fc(fit1, Xf))
  fit2 <- try(auto.arima(y, xreg = Xh,
                         approximation = ARIMA_APPROX, stepwise = ARIMA_STEPWISE,
                         seasonal = ARIMA_SEASONAL, max.order = ARIMA_MAX_ORDER), silent = TRUE)
  if (!inherits(fit2, "try-error")) return(sum_fc(fit2, Xf))
  fit4 <- try(auto.arima(y, approximation = TRUE, stepwise = TRUE,
                         seasonal = ARIMA_SEASONAL, max.order = ARIMA_MAX_ORDER), silent = TRUE)
  if (!inherits(fit4, "try-error")) return(sum_fc(fit4))
  mu <- mean(y, na.rm = TRUE); sum(rep(ifelse(is.finite(mu), mu, 0), h))
}

forecast_sum_nn <- function(y_vec, xreg_hist, xreg_fut, h) {
  y <- as.numeric(y_vec)
  if (length(y) < 2 || length(unique(na.omit(y))) < 2) {
    mu <- mean(y, na.rm = TRUE); return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  prep <- prepare_design(y, xreg_hist, xreg_fut)
  if (isTRUE(prep$trivial)) {
    mu <- prep$mu; return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  y <- prep$y; Xh <- prep$Xh; Xf <- prep$Xf
  fit <- try(
    nnfor::mlp(
      y,
      xreg      = Xh,
      reps      = NN_REPS,
      hd        = NN_SIZE,
      difforder = if (isTRUE(NN_DIFF)) 1 else 0,
      lags      = 1:NN_LAGS,
      decay     = NN_DECAY
    ),
    silent = TRUE
  )
  if (inherits(fit, "try-error")) {
    mu <- mean(y, na.rm = TRUE); return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  fc <- try(forecast::forecast(fit, h = h, xreg = Xf), silent = TRUE)
  if (inherits(fc, "try-error") || !is.numeric(fc$mean)) {
    mu <- mean(y, na.rm = TRUE); return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  sum(as.numeric(fc$mean))
}

# ────────────────────────────────────────────────────────────
# Offense projections (production: forecasts for next H weeks)
# ────────────────────────────────────────────────────────────
off_metric_cols <- c("pa","pc","py","ints","tdp","ra","ry","tdr","trg","rec","recy","tdrec","fum")
FF_weights_off  <- c(pa=0, pc=0, py=0, ints=-2.4, tdp=0, ra=6.0, ry=0.1, tdr=0.1,
                     trg=6.0, rec=0.0, recy=0.1, tdrec=0.1, fum=-2.0)

# -------- Forecast with ARIMAX --------
t_arimax <- system.time({
  rows_arimax <- future_lapply(seq_len(nPlayers_off), function(i) {
    pid <- players_off[i]
    pdata <- offense %>% filter(player == pid) %>% arrange(GAME.seas, GAME.wk)
    if (!nrow(pdata)) return(NULL)
    
    xctx <- pdata %>%
      left_join(
        team_week,
        by = c("GAME.seas" = "season", "GAME.wk" = "week", "TEAM" = "team")
      ) %>%
      mutate(
        is_home       = replace_na(is_home, 0L),
        roof          = replace_na(roof, "outdoor"),
        surface       = replace_na(surface, "grass"),
        weather       = replace_na(weather, "unknown"),
        rush_attempts = replace_na(rush_attempts, 25),
        pass_attempts = replace_na(pass_attempts, 30)
      )
    X_hist <- make_xreg(xctx, lvl_ref)
    
    last_season <- tail(pdata$GAME.seas, 1)
    last_week   <- tail(pdata$GAME.wk,   1)
    team_id     <- as.character(tail(pdata$TEAM, 1) %||% NA_character_)
    if (!is.na(team_id)) {
      X_fut <- build_future_xreg(team_id, last_season, last_week, FORECAST_H, team_week, team_roll, lvl_ref)
    } else {
      stub <- data.frame(is_home=0, roof="outdoor", surface="grass",
                         weather="unknown", rush_attempts=0, pass_attempts=0)
      mm   <- make_xreg(stub, lvl_ref)
      X_fut <- matrix(0, nrow = FORECAST_H, ncol = ncol(mm)); colnames(X_fut) <- colnames(mm)
    }
    
    vals <- vapply(off_metric_cols, function(m) {
      forecast_sum_arimax(pdata[[m]], X_hist, X_fut, FORECAST_H)
    }, numeric(1))
    
    FF <- sum(vals * FF_weights_off[off_metric_cols], na.rm = TRUE)
    c(player = pid, TEAM = tail(pdata$TEAM, 1) %||% NA_character_, vals, FF = FF)
  })
})
Forecast_offense_arimax <- do.call(rbind, rows_arimax) |> as.data.frame(check.names = FALSE)
for (nm in intersect(colnames(Forecast_offense_arimax), off_metric_cols)) {
  Forecast_offense_arimax[[nm]] <- as.numeric(Forecast_offense_arimax[[nm]])
}
Forecast_offense_arimax$FF     <- as.numeric(Forecast_offense_arimax$FF)
Forecast_offense_arimax$TEAM   <- as.character(Forecast_offense_arimax$TEAM)
Forecast_offense_arimax$player <- as.character(Forecast_offense_arimax$player)
Forecast_offense_arimax_ranked <- Forecast_offense_arimax %>% arrange(desc(FF))
write_csv(Forecast_offense_arimax_ranked, "forecast_offense_arimax_fast.csv")

# -------- Forecast with NN --------
t_nn <- system.time({
  rows_nn <- future_lapply(seq_len(nPlayers_off), function(i) {
    pid <- players_off[i]
    pdata <- offense %>% filter(player == pid) %>% arrange(GAME.seas, GAME.wk)
    if (!nrow(pdata)) return(NULL)
    
    xctx <- pdata %>%
      left_join(
        team_week,
        by = c("GAME.seas" = "season", "GAME.wk" = "week", "TEAM" = "team")
      ) %>%
      mutate(
        is_home       = replace_na(is_home, 0L),
        roof          = replace_na(roof, "outdoor"),
        surface       = replace_na(surface, "grass"),
        weather       = replace_na(weather, "unknown"),
        rush_attempts = replace_na(rush_attempts, 25),
        pass_attempts = replace_na(pass_attempts, 30)
      )
    X_hist <- make_xreg(xctx, lvl_ref)
    
    last_season <- tail(pdata$GAME.seas, 1)
    last_week   <- tail(pdata$GAME.wk,   1)
    team_id     <- as.character(tail(pdata$TEAM, 1) %||% NA_character_)
    if (!is.na(team_id)) {
      X_fut <- build_future_xreg(team_id, last_season, last_week, FORECAST_H, team_week, team_roll, lvl_ref)
    } else {
      stub <- data.frame(is_home=0, roof="outdoor", surface="grass",
                         weather="unknown", rush_attempts=0, pass_attempts=0)
      mm   <- make_xreg(stub, lvl_ref)
      X_fut <- matrix(0, nrow = FORECAST_H, ncol = ncol(mm)); colnames(X_fut) <- colnames(mm)
    }
    
    vals <- vapply(off_metric_cols, function(m) {
      forecast_sum_nn(pdata[[m]], X_hist, X_fut, FORECAST_H)
    }, numeric(1))
    
    FF <- sum(vals * FF_weights_off[off_metric_cols], na.rm = TRUE)
    c(player = pid, TEAM = tail(pdata$TEAM, 1) %||% NA_character_, vals, FF = FF)
  })
})
Forecast_offense_nn <- do.call(rbind, rows_nn) |> as.data.frame(check.names = FALSE)
for (nm in intersect(colnames(Forecast_offense_nn), off_metric_cols)) {
  Forecast_offense_nn[[nm]] <- as.numeric(Forecast_offense_nn[[nm]])
}
Forecast_offense_nn$FF     <- as.numeric(Forecast_offense_nn$FF)
Forecast_offense_nn$TEAM   <- as.character(Forecast_offense_nn$TEAM)
Forecast_offense_nn$player <- as.character(Forecast_offense_nn$player)
Forecast_offense_nn_ranked <- Forecast_offense_nn %>% arrange(desc(FF))
write_csv(Forecast_offense_nn_ranked, "forecast_offense_nn_fast.csv")

message(sprintf("ARIMAX runtime: %.2fs | NN runtime: %.2fs", t_arimax["elapsed"], t_nn["elapsed"]))

# ────────────────────────────────────────────────────────────
# Backtest (single split: last H weeks as test using actual xregs)
# ────────────────────────────────────────────────────────────
backtest_rows <- lapply(seq_len(nPlayers_off), function(i) {
  pid <- players_off[i]
  pdata <- offense %>% filter(player == pid) %>% arrange(GAME.seas, GAME.wk)
  n <- nrow(pdata)
  if (n < (MIN_WEEKS + FORECAST_H)) return(NULL)
  
  xctx <- pdata %>%
    left_join(
      team_week,
      by = c("GAME.seas" = "season", "GAME.wk" = "week", "TEAM" = "team")
    ) %>%
    mutate(
      is_home       = replace_na(is_home, 0L),
      roof          = replace_na(roof, "outdoor"),
      surface       = replace_na(surface, "grass"),
      weather       = replace_na(weather, "unknown"),
      rush_attempts = replace_na(rush_attempts, 25),
      pass_attempts = replace_na(pass_attempts, 30)
    )
  X_all <- make_xreg(xctx, lvl_ref)
  
  split_idx <- n - FORECAST_H
  if (split_idx < MIN_WEEKS) return(NULL)  # ensure enough train
  
  # Use ACTUAL future context rows for fair model comparison
  X_hist_bt <- X_all[1:split_idx, , drop = FALSE]
  X_fut_bt  <- X_all[(split_idx+1):n, , drop = FALSE]
  
  # Per-metric forecasts and actual sums
  sums_arimax <- vapply(off_metric_cols, function(m) {
    forecast_sum_arimax(pdata[[m]][1:split_idx], X_hist_bt, X_fut_bt, FORECAST_H)
  }, numeric(1))
  
  sums_nn <- vapply(off_metric_cols, function(m) {
    forecast_sum_nn(pdata[[m]][1:split_idx], X_hist_bt, X_fut_bt, FORECAST_H)
  }, numeric(1))
  
  sums_actual <- vapply(off_metric_cols, function(m) {
    sum(pdata[[m]][(split_idx+1):n], na.rm = TRUE)
  }, numeric(1))
  
  FF_hat_arimax <- sum(sums_arimax * FF_weights_off[off_metric_cols], na.rm = TRUE)
  FF_hat_nn     <- sum(sums_nn *     FF_weights_off[off_metric_cols], na.rm = TRUE)
  FF_actual     <- sum(sums_actual * FF_weights_off[off_metric_cols], na.rm = TRUE)
  
  data.frame(
    player = pid,
    TEAM   = tail(pdata$TEAM, 1) %||% NA_character_,
    FF_hat_arimax = FF_hat_arimax,
    FF_hat_nn     = FF_hat_nn,
    FF_actual     = FF_actual,
    err_arimax    = FF_hat_arimax - FF_actual,
    err_nn        = FF_hat_nn - FF_actual,
    abs_err_arimax = abs(FF_hat_arimax - FF_actual),
    abs_err_nn     = abs(FF_hat_nn - FF_actual),
    sq_err_arimax  = (FF_hat_arimax - FF_actual)^2,
    sq_err_nn      = (FF_hat_nn - FF_actual)^2,
    stringsAsFactors = FALSE
  )
})

Backtest <- bind_rows(backtest_rows)
if (nrow(Backtest) > 0) {
  summary_tbl <- tibble::tibble(
    metric = c("MAE (FF sum over H)", "RMSE (FF sum over H)"),
    ARIMAX = c(mean(Backtest$abs_err_arimax, na.rm = TRUE),
               sqrt(mean(Backtest$sq_err_arimax, na.rm = TRUE))),
    NN     = c(mean(Backtest$abs_err_nn,     na.rm = TRUE),
               sqrt(mean(Backtest$sq_err_nn,  na.rm = TRUE)))
  ) %>%
    mutate(NN_vs_ARIMAX_rel = (NN - ARIMAX) / pmax(1e-9, ARIMAX))
  
  print(summary_tbl)
  write_csv(Backtest,    "backtest_player_level_ff_sum.csv")
  write_csv(summary_tbl, "backtest_summary_ff_sum.csv")
} else {
  message("Backtest skipped: not enough players with MIN_WEEKS + FORECAST_H history.")
}

# Done — artifacts:
# - forecast_offense_arimax_fast.csv
# - forecast_offense_nn_fast.csv
# - backtest_player_level_ff_sum.csv
# - backtest_summary_ff_sum.csv

