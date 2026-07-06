
# TODO: Handle full-count strike-em-out throw-em-out (currently debit for strikeout is assigned to steal decision)

validate_glmer_models <- FALSE

# WRANGLE DATA ----
logger::log_info("Wrangling data")    # 1 minute

data_2022 <- pickoffgame::read_data(2022)
data_2023 <- pickoffgame::read_data(2023)

event_map <- data.table::fread("input/data/batter_event.csv")
pitch_map <- data.table::fread("input/data/batter_pitch.csv")

game_state_2022 <- pickoffgame::wrangle_data(
  play = data_2022$play,
  pitch = data_2022$pitch,
  lead_distance = data_2022$lead_distance,
  event = data_2022$event,
  sprint_speed = data_2022$sprint_speed,
  arm_strength = data_2022$arm_strength,
  pitch_map = pitch_map,
  event_map = event_map
)

game_state_2023 <- pickoffgame::wrangle_data(
  play = data_2023$play,
  pitch = data_2023$pitch,
  lead_distance = data_2023$lead_distance,
  event = data_2023$event,
  sprint_speed = data_2023$sprint_speed,
  arm_strength = data_2023$arm_strength,
  pitch_map = pitch_map,
  event_map = event_map
)

game_state <- dplyr::bind_rows(game_state_2022, game_state_2023)


# RUN FULL ANALYSIS PIPELINE ----

results <- pickoffgame::run_analysis_pipeline(
  game_state = game_state,
  validate_glmer_models = validate_glmer_models
)


# WRITE RESULTS TO FILE ----

data.table::fwrite(results$data_glmer, file = "output/data/data_glmer.csv")

if (validate_glmer_models) {
  data.table::fwrite(
    x = results$runner_outcome_model_validation,
    file = "output/runner_outcome_validation.csv"
  )
}

saveRDS(results$fit_runner_outcome, file = "output/models/fit_runner_outcome.rds")

data.table::fwrite(results$policy_mdp, file = "output/policy_mdp.csv")
data.table::fwrite(results$policy_zsg, file = "output/policy_zsg.csv")
data.table::fwrite(results$policy_mdp_skill, file = "output/policy_mdp_skill.csv")

logger::log_info("Done")
