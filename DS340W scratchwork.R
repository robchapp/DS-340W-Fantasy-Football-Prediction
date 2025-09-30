# install.packages(c("nflfastR", "nflreadr", "dplyr", "purrr", "readr", "stringr", "tidyr"))  # if needed
library(nflfastR)
library(nflreadr)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

# --- choose your seasons ---
seasons <- 2022:2024  # adjust as needed

# --- load pbp for the seasons ---
pbp <- nflfastR::load_pbp(seasons)

pbp_small <- pbp %>%
  select(
    season, week, game_id, home_team, away_team,
    roof, surface, weather, posteam,
    rush_attempt, pass_attempt
  )

# -------- Per-GAME totals (both teams combined) --------
game_totals <- pbp_small %>%
  group_by(season, week, game_id, home_team, away_team, roof, surface) %>%
  summarise(
    rush_attempts = sum(rush_attempt %in% c(1, TRUE), na.rm = TRUE),
    pass_attempts = sum(pass_attempt %in% c(1, TRUE), na.rm = TRUE),
    weather      = dplyr::first(na.omit(weather)),
    .groups = "drop"
  )

# -------- Per-TEAM-per-GAME totals --------
team_game_totals <- pbp_small %>%
  filter(!is.na(posteam)) %>%
  group_by(season, week, game_id, posteam, roof, surface) %>%
  summarise(
    rush_attempts = sum(rush_attempt %in% c(1, TRUE), na.rm = TRUE),
    pass_attempts = sum(pass_attempt %in% c(1, TRUE), na.rm = TRUE),
    weather      = dplyr::first(na.omit(weather)),
    .groups = "drop"
  )

# -------- Injuries: weekly reports (pre-game) --------
# add: library(janitor) if you don't have it loaded
# install.packages("janitor")  # if needed
library(janitor)

inj_raw <- nflreadr::load_injuries(seasons)

# Helper: safely coalesce across a set of possible columns; returns NA if none exist
safe_first_present <- function(df, choices, default = NA_character_) {
  present <- intersect(choices, names(df))
  if (length(present) == 0) {
    rep(default, nrow(df))
  } else {
    dplyr::coalesce(!!!df[present], default)
  }
}

inj0 <- inj_raw %>% janitor::clean_names()

injuries_clean <- inj0 %>%
  mutate(
    # status
    status_raw = safe_first_present(
      ., c("game_status", "final_game_status", "report_status", "status", "practice_status")
    ),
    status_std = stringr::str_to_lower(as.character(status_raw)),
    status_bucket = dplyr::case_when(
      stringr::str_detect(status_std, "out|injured reserve|\\bir\\b|inactive|physically unable|\\bpup\\b") ~ "out",
      stringr::str_detect(status_std, "doubt") ~ "doubtful",
      stringr::str_detect(status_std, "question") ~ "questionable",
      stringr::str_detect(status_std, "probable") ~ "probable",
      stringr::str_detect(status_std, "did not|dnp|no practice") ~ "did_not_practice",
      stringr::str_detect(status_std, "limited") ~ "limited",
      stringr::str_detect(status_std, "full") ~ "full",
      TRUE ~ "other"
    ),
    # IDs (these fields vary by vendor/season)
    player_name = safe_first_present(., c("player_name", "gsis_name", "name")),
    position    = safe_first_present(., c("position", "pos")),
    body_part   = safe_first_present(., c("injury_body_part", "body_part")),
    team        = safe_first_present(., c("team", "club", "club_code"))
  ) %>%
  dplyr::select(season, week, team, player_name, position, body_part, status_bucket)

# Summarize injuries per team-week
injuries_team_week <- injuries_clean %>%
  group_by(season, week, team) %>%
  summarise(
    n_out          = sum(status_bucket == "out", na.rm = TRUE),
    n_doubtful     = sum(status_bucket == "doubtful", na.rm = TRUE),
    n_questionable = sum(status_bucket == "questionable", na.rm = TRUE),
    n_probable     = sum(status_bucket == "probable", na.rm = TRUE),
    n_dnp          = sum(status_bucket == "did_not_practice", na.rm = TRUE),
    injured_players = paste(
      unique(na.omit(
        ifelse(
          status_bucket %in% c("out", "doubtful", "questionable"),
          paste0(player_name, ifelse(!is.na(position), paste0(" (", position, ")"), "")),
          NA_character_
        )
      )),
      collapse = "; "
    ),
    .groups = "drop"
  )

# Join onto per-TEAM-per-GAME``
team_game_with_injuries <- team_game_totals %>%
  left_join(injuries_team_week, by = c("season", "week", "posteam" = "team")) %>%
  mutate(
    dplyr::across(c(n_out, n_doubtful, n_questionable, n_probable, n_dnp), ~tidyr::replace_na(., 0)),
    injured_players = tidyr::replace_na(injured_players, "")
  )

# Join onto per-TEAM-per-GAME
team_game_with_injuries <- team_game_totals %>%
  left_join(injuries_team_week, by = c("season", "week", "posteam" = "team")) %>%
  mutate(
    dplyr::across(c(n_out, n_doubtful, n_questionable, n_probable, n_dnp), ~tidyr::replace_na(., 0)),
    injured_players = tidyr::replace_na(injured_players, "")
  )

# Optional: write out
# write_csv(team_game_with_injuries, "nflfastR_team_game_weather_attempts_injuries.csv")

# Peek
dplyr::glimpse(team_game_with_injuries)
