
bags <- 100

# WRANGLE DATA ----
logger::log_info("Wrangling data")    # 1 minute

fit_runner_outcome <- readRDS("output/models/fit_runner_outcome.rds")

data_2022 <- pickoffgame::read_data(2022)
data_2023 <- pickoffgame::read_data(2023)

event_map <- data.table::fread("input/data/batter_event.csv")
pitch_map <- data.table::fread("input/data/batter_pitch.csv")

game_state_2022 <- pickoffgame::wrangle_data(
  play = data_2022$play,
  pitch = data_2022$pitch,
  lead = data_2022$lead,
  event = data_2022$event,
  sprint_speed = data_2022$sprint_speed,
  arm_strength = data_2022$arm_strength,
  pitch_map = pitch_map,
  event_map = event_map
)

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

game_state <- dplyr::bind_rows(game_state_2022, game_state_2023)

set.seed(42)
bootstrap_resample <- rsample::bootstraps(
  data = game_state,
  times = bags,
  strata = year
)
bootstrap_simulate <- list()
for (outcome in names(fit_runner_outcome)) {
  bootstrap_simulate[[outcome]] <- glmmTMB:::simulate.glmmTMB(
    object = fit_runner_outcome[[outcome]],
    nsim = bags,
    seed = 42
  )
}

for (b in 1:bags) {
  logger::log_info("Analyzing bootstrap sample {b}/{bags}")
  result <- pickoffgame::run_analysis_pipeline(
    game_state = game_state,
    bootstrap_index = b,
    bootstrap_resample = bootstrap_resample,
    bootstrap_simulate = bootstrap_simulate,
    bootstrap_fit_original = fit_runner_outcome
  )
  # remove large objects before saving to file
  result$data_glmer <- NULL
  result$fit_runner_outcome <- NULL
  gc()
  saveRDS(result, glue::glue("output/bootstrap/{b}.rds"))
}
