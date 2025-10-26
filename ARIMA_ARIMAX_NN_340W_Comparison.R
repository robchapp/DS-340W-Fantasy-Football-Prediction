# ============================================================
# ARIMA (uni) vs ARIMAX (xreg) vs NN (xreg) — Offense, 3-way
# ============================================================

# --- Controls ---
FAST_MODE         <- TRUE
SEASONS_BACK      <- if (FAST_MODE) 2 else 4
MIN_WEEKS         <- 10
TOP_N_PLAYERS     <- if (FAST_MODE) 300 else 800
FORECAST_H        <- if (FAST_MODE) 6 else 8

# ARIMA/ARIMAX knobs
ARIMA_APPROX      <- TRUE
ARIMA_STEPWISE    <- TRUE
ARIMA_SEASONAL    <- FALSE
ARIMA_MAX_ORDER   <- 3

# NN knobs
NN_SIZE    <- if (FAST_MODE) 5  else 15
NN_REPS    <- if (FAST_MODE) 1  else 5
NN_DECAY   <- 0.001
NN_LAGS    <- 8
NN_DIFF    <- FALSE

# --- Packages ---
suppressPackageStartupMessages({
  library(dplyr);  library(tidyr);  library(stringr)
  library(forecast); library(nnfor)
  library(nflfastR); library(nflreadr)
  library(zoo);    library(readr)
  library(future); library(future.apply)
})
plan(multisession, workers = max(1, parallel::detectCores() - 1))
`%||%` <- function(x, y) if (!is.null(x)) x else y

# --- Seasons (stop at last completed season) ---
most <- nflreadr::most_recent_season()
start_season <- max(1999, most - SEASONS_BACK + 1)
end_season   <- most - 1
season_range_off <- start_season:end_season
message(sprintf("Seasons used: %d–%d", start_season, end_season))

# --- Team-week context (xregs) ---
bucket_weather <- function(x){
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
norm_roof <- function(x){
  x <- tolower(as.character(x))
  ifelse(str_detect(x, "indoor|dome|closed"), "indoor", "outdoor")
}
norm_surface <- function(x){
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
      .groups="drop"
    ) %>%
    mutate(team = posteam, is_home = if_else(team == home_team, 1L, 0L)) %>%
    select(season, week, game_id, team, home_team, away_team, is_home,
           roof, surface, weather, rush_attempts, pass_attempts)
  
  team_roll <- team_week %>%
    arrange(team, season, week) %>%
    group_by(team) %>%
    mutate(
      rush_share = rush_attempts / pmax(1, rush_attempts + pass_attempts),
      pass_share = 1 - rush_share,
      rush_attempts_rm = lag(rollapplyr(rush_attempts, 5, mean, na.rm = TRUE, partial = TRUE)),
      pass_attempts_rm = lag(rollapplyr(pass_attempts, 5, mean, na.rm = TRUE, partial = TRUE))
    ) %>% ungroup()
  list(team_week=team_week, team_roll=team_roll)
}

feat <- build_teamweek_features(season_range_off)
team_week <- feat$team_week
team_roll <- feat$team_roll

team_week$roof    <- factor(team_week$roof,    levels = c("indoor","outdoor"))
team_week$surface <- factor(team_week$surface, levels = c("grass","turf","other"))
team_week$weather <- factor(team_week$weather, levels = c("clear","cloudy","rain","snow","windy","unknown"))
lvl_ref <- list(roof=levels(team_week$roof),
                surface=levels(team_week$surface),
                weather=levels(team_week$weather))

make_xreg <- function(df, lvl_ref){
  df$roof    <- factor(norm_roof(df$roof),       levels = lvl_ref$roof)
  df$surface <- factor(norm_surface(df$surface), levels = lvl_ref$surface)
  df$weather <- factor(bucket_weather(df$weather), levels = lvl_ref$weather)
  mm <- model.matrix(~ is_home + roof + surface + weather +
                       rush_attempts + pass_attempts, data = df)
  storage.mode(mm) <- "double"; mm
}

xreg_fut_cache <- new.env(parent = emptyenv())
build_future_xreg <- function(team_id, start_season, start_week, horizon,
                              team_week, team_roll, lvl_ref){
  key <- paste(team_id, start_season, start_week, horizon, sep="_")
  if (exists(key, envir=xreg_fut_cache)) return(get(key, envir=xreg_fut_cache))
  sched <- nflreadr::load_schedules(most) %>%
    transmute(season, week, game_id, home_team, away_team,
              roof=norm_roof(roof), surface=norm_surface(surface))
  fut <- sched %>%
    filter((season > start_season) | (season == start_season & week > start_week)) %>%
    arrange(season, week) %>%
    mutate(my_team = if_else(home_team==team_id, home_team,
                             if_else(away_team==team_id, away_team, NA_character_))) %>%
    filter(!is.na(my_team)) %>%
    mutate(is_home = if_else(my_team == home_team, 1L, 0L),
           weather = "unknown") %>%
    head(horizon)
  if (nrow(fut) == 0) {
    stub <- data.frame(is_home=0, roof="outdoor", surface="grass",
                       weather="unknown", rush_attempts=0, pass_attempts=0)
    mm <- make_xreg(stub, lvl_ref)
    mm0 <- matrix(0, nrow=horizon, ncol=ncol(mm)); colnames(mm0) <- colnames(mm)
    assign(key, mm0, envir=xreg_fut_cache); return(mm0)
  }
  last_roll <- team_roll %>% filter(team==team_id) %>% arrange(season, week) %>% slice_tail(n=1)
  ra_rm <- last_roll$rush_attempts_rm %||% 25
  pa_rm <- last_roll$pass_attempts_rm %||% 30
  fut2 <- fut %>% transmute(
    is_home, roof, surface, weather,
    rush_attempts = ra_rm, pass_attempts = pa_rm
  )
  Xf <- make_xreg(fut2, lvl_ref); assign(key, Xf, envir=xreg_fut_cache); Xf
}

# --- Load offense data ---
off_raw <- nflreadr::load_player_stats(seasons = season_range_off)
player_col <- if ("player_name" %in% names(off_raw)) "player_name" else
  if ("player" %in% names(off_raw)) "player" else "name"
team_col   <- if ("recent_team" %in% names(off_raw)) "recent_team" else
  if ("team" %in% names(off_raw)) "team" else NA

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

offense$has_play <- rowSums(is.finite(as.matrix(offense[, c(
  "pa","pc","py","ints","tdp","ra","ry","tdr","trg","rec","recy","tdrec","fum"
)]))) > 0
weeks_by_player <- offense %>% group_by(player) %>%
  summarise(nw = sum(has_play, na.rm = TRUE), .groups="drop") %>%
  filter(nw >= MIN_WEEKS) %>% arrange(desc(nw))
if (is.finite(TOP_N_PLAYERS)) weeks_by_player <- head(weeks_by_player, TOP_N_PLAYERS)
players_off <- weeks_by_player$player
nPlayers_off <- length(players_off)
message(sprintf("Players kept: %d", nPlayers_off))

# --- Fantasy weights ---
off_metric_cols <- c("pa","pc","py","ints","tdp","ra","ry","tdr","trg","rec","recy","tdrec","fum")
FF_weights_off  <- c(pa=0, pc=0, py=0, ints=-2.4, tdp=0, ra=6.0, ry=0.1, tdr=0.1,
                     trg=6.0, rec=0.0, recy=0.1, tdrec=0.1, fum=-2.0)

# ========== Forecasters (return SUM of next h) ==========
.prepare_design <- function(y, X_hist, X_fut){
  row_keep <- is.finite(y) & apply(is.finite(X_hist), 1, all)
  y  <- as.numeric(y[row_keep])
  Xh <- X_hist[row_keep, , drop = FALSE]
  Xf <- X_fut
  keep_var <- apply(Xh, 2, function(col) sd(col, na.rm=TRUE) > 1e-9)
  if (!any(keep_var) || length(unique(na.omit(y))) < 2)
    return(list(y=NULL,Xh=NULL,Xf=NULL, trivial=TRUE, mu=mean(y,na.rm=TRUE)))
  Xh <- Xh[, keep_var, drop = FALSE]; Xf <- Xf[, keep_var, drop = FALSE]
  qrh <- qr(Xh); piv <- qrh$pivot[seq_len(qrh$rank)]
  Xh <- Xh[, piv, drop = FALSE]; Xf <- Xf[, piv, drop = FALSE]
  center <- colMeans(Xh); sds <- apply(Xh, 2, sd)
  scale_cols <- function(A, center, scale) sweep(sweep(A, 2, center, `-`), 2, ifelse(scale==0,1,scale), `/`)
  Xh <- scale_cols(Xh, center, sds); Xf <- scale_cols(Xf, center, sds)
  list(y=y,Xh=Xh,Xf=Xf,trivial=FALSE,mu=NA_real_)
}

# ARIMA (univariate, NO xreg)
forecast_sum_arima_uni <- function(y_vec, h){
  y <- as.numeric(y_vec); y <- y[is.finite(y)]
  if (length(y) < 2 || length(unique(y)) < 2) {
    mu <- mean(y, na.rm=TRUE); return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  fit <- try(auto.arima(y, approximation=ARIMA_APPROX, stepwise=ARIMA_STEPWISE,
                        seasonal=ARIMA_SEASONAL, max.order=ARIMA_MAX_ORDER),
             silent=TRUE)
  if (inherits(fit, "try-error")) {
    mu <- mean(y, na.rm=TRUE); return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  sum(as.numeric(forecast(fit, h=h)$mean))
}

# ARIMAX (with xreg)
forecast_sum_arimax <- function(y_vec, xreg_hist, xreg_fut, h){
  y <- as.numeric(y_vec)
  if (length(y) < 2 || length(unique(na.omit(y))) < 2) {
    mu <- mean(y, na.rm=TRUE); return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  prep <- .prepare_design(y, xreg_hist, xreg_fut)
  if (isTRUE(prep$trivial)) {
    mu <- prep$mu; return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  y <- prep$y; Xh <- prep$Xh; Xf <- prep$Xf
  fit1 <- try(Arima(y, order=c(0,1,1), xreg=Xh, include.mean=FALSE), silent=TRUE)
  sum_fc <- function(fit, Xfuture=NULL){
    fc <- if (is.null(Xfuture)) forecast(fit, h=h) else forecast(fit, xreg=Xfuture, h=h)
    sum(as.numeric(fc$mean))
  }
  if (!inherits(fit1, "try-error")) return(sum_fc(fit1, Xf))
  fit2 <- try(auto.arima(y, xreg=Xh, approximation=ARIMA_APPROX, stepwise=ARIMA_STEPWISE,
                         seasonal=ARIMA_SEASONAL, max.order=ARIMA_MAX_ORDER), silent=TRUE)
  if (!inherits(fit2, "try-error")) return(sum_fc(fit2, Xf))
  fit4 <- try(auto.arima(y, approximation=TRUE, stepwise=TRUE,
                         seasonal=ARIMA_SEASONAL, max.order=ARIMA_MAX_ORDER), silent=TRUE)
  if (!inherits(fit4, "try-error")) return(sum_fc(fit4))
  mu <- mean(y, na.rm=TRUE); sum(rep(ifelse(is.finite(mu), mu, 0), h))
}

# NN (mlp with xreg)
forecast_sum_nn <- function(y_vec, xreg_hist, xreg_fut, h){
  y <- as.numeric(y_vec)
  if (length(y) < 2 || length(unique(na.omit(y))) < 2) {
    mu <- mean(y, na.rm=TRUE); return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  prep <- .prepare_design(y, xreg_hist, xreg_fut)
  if (isTRUE(prep$trivial)) {
    mu <- prep$mu; return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  y <- prep$y; Xh <- prep$Xh; Xf <- prep$Xf
  fit <- try(nnfor::mlp(y, xreg=Xh, reps=NN_REPS, hd=NN_SIZE,
                        difforder=if (isTRUE(NN_DIFF)) 1 else 0,
                        lags=1:NN_LAGS, decay=NN_DECAY),
             silent=TRUE)
  if (inherits(fit, "try-error")) {
    mu <- mean(y, na.rm=TRUE); return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  fc <- try(forecast::forecast(fit, h=h, xreg=Xf), silent=TRUE)
  if (inherits(fc, "try-error") || !is.numeric(fc$mean)) {
    mu <- mean(y, na.rm=TRUE); return(sum(rep(ifelse(is.finite(mu), mu, 0), h)))
  }
  sum(as.numeric(fc$mean))
}

# ========== PRODUCTION FORECASTS (next H) ==========
t_prod <- system.time({
  rows_prod <- future_lapply(seq_len(nPlayers_off), function(i){
    pid <- players_off[i]
    pdata <- offense %>% filter(player==pid) %>% arrange(GAME.seas, GAME.wk)
    if (!nrow(pdata)) return(NULL)
    xctx <- pdata %>%
      left_join(team_week, by=c("GAME.seas"="season","GAME.wk"="week","TEAM"="team")) %>%
      mutate(
        is_home=replace_na(is_home,0L),
        roof=replace_na(roof,"outdoor"),
        surface=replace_na(surface,"grass"),
        weather=replace_na(weather,"unknown"),
        rush_attempts=replace_na(rush_attempts,25),
        pass_attempts=replace_na(pass_attempts,30)
      )
    X_hist <- make_xreg(xctx, lvl_ref)
    last_season <- tail(pdata$GAME.seas, 1); last_week <- tail(pdata$GAME.wk, 1)
    team_id <- as.character(tail(pdata$TEAM, 1) %||% NA_character_)
    if (!is.na(team_id)) {
      X_fut <- build_future_xreg(team_id, last_season, last_week, FORECAST_H, team_week, team_roll, lvl_ref)
    } else {
      stub <- data.frame(is_home=0, roof="outdoor", surface="grass",
                         weather="unknown", rush_attempts=0, pass_attempts=0)
      mm <- make_xreg(stub, lvl_ref)
      X_fut <- matrix(0, nrow=FORECAST_H, ncol=ncol(mm)); colnames(X_fut) <- colnames(mm)
    }
    
    vals_arima  <- vapply(off_metric_cols, function(m) forecast_sum_arima_uni(pdata[[m]], FORECAST_H), numeric(1))
    vals_arimax <- vapply(off_metric_cols, function(m) forecast_sum_arimax(pdata[[m]], X_hist, X_fut, FORECAST_H), numeric(1))
    vals_nn     <- vapply(off_metric_cols, function(m) forecast_sum_nn    (pdata[[m]], X_hist, X_fut, FORECAST_H), numeric(1))
    
    c(
      player = pid,
      TEAM   = tail(pdata$TEAM, 1) %||% NA_character_,
      setNames(vals_arima , paste0("ARIMA_",  off_metric_cols)),
      setNames(vals_arimax, paste0("ARIMAX_", off_metric_cols)),
      setNames(vals_nn    , paste0("NN_",     off_metric_cols)),
      ARIMA_FF  = sum(vals_arima  * FF_weights_off[off_metric_cols], na.rm=TRUE),
      ARIMAX_FF = sum(vals_arimax * FF_weights_off[off_metric_cols], na.rm=TRUE),
      NN_FF     = sum(vals_nn     * FF_weights_off[off_metric_cols], na.rm=TRUE)
    )
  })
})
Prod <- do.call(rbind, rows_prod) |> as.data.frame(check.names=FALSE)
num_cols <- setdiff(names(Prod), c("player","TEAM"))
for (nm in num_cols) Prod[[nm]] <- as.numeric(Prod[[nm]])
Prod$player <- as.character(Prod$player); Prod$TEAM <- as.character(Prod$TEAM)

# Write three production leaderboards
write_csv(Prod %>% select(player, TEAM, ARIMA_FF)  %>% arrange(desc(ARIMA_FF)),  "forecast_offense_ARIMA_uni.csv")
write_csv(Prod %>% select(player, TEAM, ARIMAX_FF) %>% arrange(desc(ARIMAX_FF)), "forecast_offense_ARIMAX_xreg.csv")
write_csv(Prod %>% select(player, TEAM, NN_FF)     %>% arrange(desc(NN_FF)),     "forecast_offense_NN_xreg.csv")

# ========== BACKTEST (last H weeks; ARIMAX/NN use actual xregs, ARIMA uses none) ==========
bt_rows <- lapply(seq_len(nPlayers_off), function(i){
  pid <- players_off[i]
  pdata <- offense %>% filter(player==pid) %>% arrange(GAME.seas, GAME.wk)
  n <- nrow(pdata); if (n < (MIN_WEEKS + FORECAST_H)) return(NULL)
  
  xctx <- pdata %>%
    left_join(team_week, by=c("GAME.seas"="season","GAME.wk"="week","TEAM"="team")) %>%
    mutate(
      is_home=replace_na(is_home,0L),
      roof=replace_na(roof,"outdoor"),
      surface=replace_na(surface,"grass"),
      weather=replace_na(weather,"unknown"),
      rush_attempts=replace_na(rush_attempts,25),
      pass_attempts=replace_na(pass_attempts,30)
    )
  X_all <- make_xreg(xctx, lvl_ref)
  split_idx <- n - FORECAST_H
  if (split_idx < MIN_WEEKS) return(NULL)
  X_hist_bt <- X_all[1:split_idx, , drop=FALSE]
  X_fut_bt  <- X_all[(split_idx+1):n, , drop=FALSE]
  
  # per-metric sums
  sums_arima <- vapply(off_metric_cols, function(m)
    forecast_sum_arima_uni(pdata[[m]][1:split_idx], FORECAST_H), numeric(1))
  sums_arimax <- vapply(off_metric_cols, function(m)
    forecast_sum_arimax(pdata[[m]][1:split_idx], X_hist_bt, X_fut_bt, FORECAST_H), numeric(1))
  sums_nn <- vapply(off_metric_cols, function(m)
    forecast_sum_nn(pdata[[m]][1:split_idx], X_hist_bt, X_fut_bt, FORECAST_H), numeric(1))
  sums_actual <- vapply(off_metric_cols, function(m)
    sum(pdata[[m]][(split_idx+1):n], na.rm=TRUE), numeric(1))
  
  data.frame(
    player = pid,
    TEAM   = tail(pdata$TEAM, 1) %||% NA_character_,
    FF_hat_ARIMA  = sum(sums_arima  * FF_weights_off[off_metric_cols], na.rm=TRUE),
    FF_hat_ARIMAX = sum(sums_arimax * FF_weights_off[off_metric_cols], na.rm=TRUE),
    FF_hat_NN     = sum(sums_nn     * FF_weights_off[off_metric_cols], na.rm=TRUE),
    FF_actual     = sum(sums_actual * FF_weights_off[off_metric_cols], na.rm=TRUE),
    stringsAsFactors=FALSE
  )
})
BT <- bind_rows(bt_rows)

if (nrow(BT)) {
  BT <- BT %>%
    mutate(
      err_ARIMA  = FF_hat_ARIMA  - FF_actual,
      err_ARIMAX = FF_hat_ARIMAX - FF_actual,
      err_NN     = FF_hat_NN     - FF_actual,
      abs_ARIMA  = abs(err_ARIMA),
      abs_ARIMAX = abs(err_ARIMAX),
      abs_NN     = abs(err_NN),
      sq_ARIMA   = err_ARIMA^2,
      sq_ARIMAX  = err_ARIMAX^2,
      sq_NN      = err_NN^2
    )
  
  summary_tbl <- tibble::tibble(
    metric = c("MAE (FF sum over H)", "RMSE (FF sum over H)"),
    ARIMA  = c(mean(BT$abs_ARIMA,  na.rm=TRUE), sqrt(mean(BT$sq_ARIMA,  na.rm=TRUE))),
    ARIMAX = c(mean(BT$abs_ARIMAX, na.rm=TRUE), sqrt(mean(BT$sq_ARIMAX, na.rm=TRUE))),
    NN     = c(mean(BT$abs_NN,     na.rm=TRUE), sqrt(mean(BT$sq_NN,     na.rm=TRUE)))
  ) %>%
    mutate(
      NN_vs_ARIMA_rel  = (NN  - ARIMA)  / pmax(1e-9, ARIMA),
      NN_vs_ARIMAX_rel = (NN  - ARIMAX) / pmax(1e-9, ARIMAX),
      ARIMAX_vs_ARIMA_rel = (ARIMAX - ARIMA) / pmax(1e-9, ARIMA)
    )
  
  # Optional: player-level win rates
  wins <- BT %>%
    transmute(
      player, TEAM, FF_actual,
      win_ARIMA  = (abs_ARIMA  <= pmin(abs_ARIMAX, abs_NN)),
      win_ARIMAX = (abs_ARIMAX <= pmin(abs_ARIMA,  abs_NN)),
      win_NN     = (abs_NN     <= pmin(abs_ARIMA,  abs_ARIMAX))
    ) %>%
    summarise(
      N = n(),
      ARIMA_win_rate  = mean(win_ARIMA,  na.rm=TRUE),
      ARIMAX_win_rate = mean(win_ARIMAX, na.rm=TRUE),
      NN_win_rate     = mean(win_NN,     na.rm=TRUE)
    )
  
  print(summary_tbl); print(wins)
  
  write_csv(BT,          "backtest_player_level_ff_sum_3way.csv")
  write_csv(summary_tbl, "backtest_summary_ff_sum_3way.csv")
  write_csv(wins,        "backtest_win_rates_3way.csv")
} else {
  message("Backtest skipped: not enough series with length >= MIN_WEEKS + FORECAST_H.")
}

message("Done. Wrote: forecast_offense_ARIMA_uni.csv, forecast_offense_ARIMAX_xreg.csv, forecast_offense_NN_xreg.csv, backtest_*_3way.csv")
