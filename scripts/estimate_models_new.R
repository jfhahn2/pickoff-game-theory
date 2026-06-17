
# TODO: Handle full-count strike-em-out throw-em-out (currently debit for strikeout is assigned to steal decision)

fit_glmer_models <- FALSE

# WRANGLE DATA ----
logger::log_info("Wrangling data")    # 1 minute

data_2022 <- pickoffgame::read_data(2022)
data_2023 <- pickoffgame::read_data(2023)

event_map <- data.table::fread("input/data/batter_event.csv")
pitch_map <- data.table::fread("input/data/batter_pitch.csv")

game_state_2023 <- pickoffgame::wrangle_data(
  play = data_2023$play,
  pitch = data_2023$pitch,
  lead = data_2023$lead,
  event = data_2023$event,
  sprint_speed = data_2023$sprint_speed,
  arm_strength = data_2023$arm_strength,
  pitch_map = pitch_map,
  event_map = event_map
)

game_state_2022 <- pickoffgame::wrangle_data(
  play = data_2022$play,
  pitch = data_2022$pitch,
  lead = data_2022$lead,
  event = data_2022$event,
  # TODO: Decide how to handle the fact that mean sprint speed and arm strength might not be the same
  #       across the two years of data (and they are centered separately from each other).
  sprint_speed = data_2022$sprint_speed,
  arm_strength = data_2022$arm_strength,
  pitch_map = pitch_map,
  event_map = event_map
)


if (fit_glmer_models) {

  fit_runner_outcome <- dplyr::bind_rows(game_state_2022, game_state_2023) |>
    pickoffgame::estimate_runner_outcome_model()

  saveRDS(fit_runner_outcome$po_attempt, "output/models/fit_po_attempt.rds")
  saveRDS(fit_runner_outcome$po_success, "output/models/fit_po_success.rds")
  saveRDS(fit_runner_outcome$sb_attempt, "output/models/fit_sb_attempt.rds")
  saveRDS(fit_runner_outcome$sb_success, "output/models/fit_sb_success.rds")

} else {

  fit_runner_outcome <- list(
    po_attempt = readRDS("output/models/fit_po_attempt.rds")
    po_success = readRDS("output/models/fit_po_success.rds")
    sb_attempt = readRDS("output/models/fit_sb_attempt.rds")
    sb_success = readRDS("output/models/fit_sb_success.rds")
  )
}

percentile_players <- list()
for (outcome in names(fit_runner_outcome)) {
  percentile_players[[outcome]] <- pickoffgame::extract_percentile_players(
    object = fit_runner_outcome[[outcome]],
    data = data_glmer
  )
}

# TODO: decide whether to include 2022
policy_mdp <- pickoffgame::estimate_game_model(data = game_state_2023, players = "one")

# TODO: decide whether to include 2022
policy_zsg <- pickoffgame::estimate_game_model(data = game_state_2023, players = "two")

logger::log_info("Done")
