
bootstrap_samples <- 100

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
  # TODO: Decide how to handle the fact that mean sprint speed and arm strength might not be the
  #       same across the two years of data (and they are centered separately from each other).
  sprint_speed = data_2022$sprint_speed,
  arm_strength = data_2022$arm_strength,
  pitch_map = pitch_map,
  event_map = event_map
)

game_state <- dplyr::bind_rows(game_state_2022, game_state_2023)

set.seed(42)
bootstrap <- rsample::bootstraps(data = game_state, times = bootstrap_samples, strata = year)

for (b in 1:bootstrap_samples) {
  logger::log_info("Analyizing bootstrap sample {b}/{bootstrap_samples}")
  rsample::analysis(bootstrap$splits[[b]]) |>
    pickoffgame::run_analysis_pipeline() |>
    saveRDS(glue::glue("output/bootstrap/{b}.rds"))
}
