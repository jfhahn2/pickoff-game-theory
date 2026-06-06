
# TODO: Handle full-count strike-em-out throw-em-out (currently debit for strikeout is assigned to steal decision)

fit_glmer_models <- FALSE
value_iteration_threshold <- 1e-4   # well-accepted default

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
  sprint_speed = data_2022$sprint_speed,
  arm_strength = data_2022$arm_strength,
  pitch_map = pitch_map,
  event_map = event_map
)

game_state <- dplyr::bind_rows(game_state_2022, game_state_2023)

data_glmer <- game_state |>
  dplyr::group_by(pre_runner_1b_id) |>
  dplyr::filter(
    # For modeling purposes, we consider only plays in which only first base is occupied
    is_1b_only,
    # Exclude full count with two outs because runners because these are not really steal attempts
    !is_full_count_two_outs,
    # We include only runners who attempted at least three stolen bases in our sample
    sum(is_sb_attempt) >= 3,
    # The runner taking the lead from first base should match the runner on first base
    runner_id == pre_runner_1b_id | runner_id == 0
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    pre_disengagements = dplyr::case_when(
      # Before 2023, there was no limit on disengagements, equivalent to zero pre-disengagments
      year < 2023 ~ 0,
      # If the pitcher reaches three disengagements (rare), runners advance, and the counter resets
      pre_disengagements >= 3 ~ pre_disengagements %% 3,
      TRUE ~ pre_disengagements
    ) |>
      as.factor(),
    year = as.factor(year),
  )


# FIT RUNNER OUTCOME MODELS ----
if (fit_glmer_models) {
  logger::log_info("Fitting runner outcome models")   # 10 minutes

  logger::log_info("  Estimating GLMM for PO attempt probability")    # 2 minutes
  fit_po_attempt <- lme4::glmer(
    formula = is_po_attempt ~ year + pre_outs + pre_balls + pre_strikes + pre_disengagements +
      lead_distance_centered + (1 | pitcher_id),
    data = data_glmer,
    family = binomial()
  )
  
  logger::log_info("  Estimating GLMM for PO success probability")    # 0 minutes
  fit_po_success <- lme4::glmer(
    formula = is_po_success ~ lead_distance_centered + (1 | pitcher_id),
    data = data_glmer |>
      dplyr::filter(is_po_attempt),
    family = binomial()
  )
  
  logger::log_info("  Estimating GLMM for SB attempt probability")    # 7 minutes
  fit_sb_attempt <- lme4::glmer(
    is_sb_attempt ~ pre_outs + pre_balls + pre_strikes + pre_disengagements +
      sprint_speed_centered + (1 | runner_id) + (1 | pitcher_id) + arm_strength_centered + (1 | catcher_id),
    data = data_glmer |>
      dplyr::filter(!is_po_attempt),
    family = binomial()
  )
  
  logger::log_info("  Estimating GLMM for SB success probability")    # 1 minute
  fit_sb_success <- lme4::glmer(
    is_stolen_base ~ year + lead_distance_centered +
      sprint_speed_centered + (1 | runner_id) + (1 | pitcher_id) + arm_strength_centered + (1 | catcher_id),
    data = data_glmer |>
      dplyr::filter(is_sb_attempt),
    family = binomial
  )

  saveRDS(fit_po_success, "output/models/fit_po_success.rds")
  saveRDS(fit_po_attempt, "output/models/fit_po_attempt.rds")
  saveRDS(fit_sb_success, "output/models/fit_sb_success.rds")
  saveRDS(fit_sb_attempt, "output/models/fit_sb_attempt.rds")

} else {

  fit_po_success <- readRDS("output/models/fit_po_success.rds")
  fit_po_attempt <- readRDS("output/models/fit_po_attempt.rds")
  fit_sb_success <- readRDS("output/models/fit_sb_success.rds")
  fit_sb_attempt <- readRDS("output/models/fit_sb_attempt.rds")
}


# COMPUTE STATE TRANSITION PROBABILITIES ----
logger::log_info("Computing state transition probabilities")    # 0 minutes

# Compute empirical transition probabilities between reduced states
# TODO: Fix the problem of a 0-0 triple with runner on second looking just like a stolen base.
#       To do this, we'd need to include a new batter indicator in the state space.
transition_conditional_observed <- game_state_2023 |> # TODO: decide whether to include
  dplyr::filter(type %in% c("pickoff", "pitch")) |>   # TODO: decide whether to keep this
  dplyr::group_by(pre_state_reduced, runner_outcome, post_state_reduced) |>
  dplyr::summarize(n = dplyr::n(), reward = mean(runs_on_play), .groups = "drop") |>
  dplyr::group_by(pre_state_reduced, runner_outcome) |>
  dplyr::mutate(prob = n / sum(n)) |>
  dplyr::ungroup()

# We never observe some combinations of pre-states and runner outcomes. For these, we manually
# determine the post-state using simple assumptions (pickoffgame::apply_runner_outcome).
transition_conditional_unobserved <- tidyr::expand_grid(
  pre_state_reduced = unique(transition_conditional_observed$pre_state_reduced),
  runner_outcome = unique(transition_conditional_observed$runner_outcome)
) |>
  dplyr::mutate(pre = pickoffgame::deconstruct_state(pre_state_reduced)) |>
  # When the action space is null, we don't need to fill in conditional transitions
  dplyr::filter(pre$bases == "100", !(pre$outs == 2 & pre$balls == 3 & pre$strikes == 2)) |>
  dplyr::anti_join(
    y = transition_conditional_observed,
    by = c("pre_state_reduced", "runner_outcome")
  ) |>
  dplyr::mutate(
    post_state_reduced = pickoffgame::apply_runner_outcome(pre_state_reduced, runner_outcome),
    prob = 1,
    reward = 0
  )

# This is where we append disengagements back onto the state. The logic is tricky and might be
# worth revisiting. For example, we do not correctly handle the (rare) case where bases, count and
# outs stay the same (and it's not a new batter). In this case, we should increment disengagements.
transition_conditional <- transition_conditional_observed |>
  dplyr::bind_rows(transition_conditional_unobserved) |>
  # Expand reduced state to full state by tacking on disengagements
  tidyr::expand_grid(pre_disengagements = 0:2) |>
  dplyr::mutate(
    pre = pickoffgame::deconstruct_state(pre_state_reduced),
    post = pickoffgame::deconstruct_state(post_state_reduced),
    pre_state = pickoffgame::update_state(
      state = pre_state_reduced,
      new_disengagements = pre_disengagements
    ),
    post_disengagements = pre_disengagements + ifelse(runner_outcome %in% c("P+", "P-"), 1, 0),
    is_end_of_pa = post$first == 1,
    is_runner_movement = pre$bases != post$bases,
    post_state = dplyr::case_when(
      # If the reduced post-state is a terminal end-of-inning state, do not append disengagements
      nchar(post_state_reduced) == 1 ~ post_state_reduced,
      # If the transition is end of plate appearance or runner movement, reset disengagements
      is_end_of_pa | is_runner_movement ~ pickoffgame::update_state(
        state = post_state_reduced,
        new_disengagements = 0
      ),
      # If the pre-state is not 1B occupied, 2B & 3B empty, then post-disengagements must be zero
      # because we only count disengagements with 1B occupied, 2B & 3B empty
      pre$bases != "100" ~ pickoffgame::update_state(
        state = post_state_reduced,
        new_disengagements = 0
      ),
      # Assuming pre_bases == "100", then post_bases = "010" and post_disengements = 0
      post_disengagements == 3 ~ pickoffgame::update_state(
        state = post_state_reduced,
        new_bases = "010",
        new_disengagements = 0
      ),
      TRUE ~ pickoffgame::update_state(post_state_reduced, new_disengagements = post_disengagements)
    )
  ) |>
  dplyr::select(pre_state, runner_outcome, post_state, prob, reward)

runner_outcome_grid <- transition_conditional |>
  dplyr::distinct(pre_state) |>
  tidyr::expand_grid(
    runner_outcome = unique(transition_conditional$runner_outcome),
    lead_distance = seq(from = 0, to = 30, by = 0.1)
  ) |>
  dplyr::mutate(
    year = factor(2023, levels = c(2022, 2023)),
    pre = pickoffgame::deconstruct_state(pre_state),
    pre_bases = pre$bases,
    pre_outs = pre$outs,
    pre_balls = pre$balls,
    pre_strikes = pre$strikes,
    pre_disengagements = factor(pre$disengagements, levels = 0:2),
    lead_distance_centered = lead_distance - 10,
    sprint_speed_centered = 0,
    arm_strength_centered = 0,
    pitcher_id = "0",
    is_1b_only = pre$bases == "100",
    is_full_count_two_outs = (pre$balls == 3) & (pre$strikes == 2) & (pre$outs == 2)
  ) |>
  # Action space is null unless only 1B is occupied without full count and two strikes
  dplyr::filter(lead_distance == 0 | (is_1b_only & !is_full_count_two_outs))

runner_outcome_prob <- runner_outcome_grid |>
  dplyr::mutate(
    prob_po_attempt = ifelse(
      # our model only allows pickoffs and steals with only 1B occupied, not full count two outs
      test = is_1b_only & !is_full_count_two_outs,
      yes = predict(fit_po_attempt, newdata = runner_outcome_grid, type = "response", re.form = NA),
      no = 0
    ),
    prob_po_success = predict(fit_po_success, newdata = runner_outcome_grid, type = "response", re.form = NA),
    prob_sb_attempt = ifelse(
      # our model only allows pickoffs and steals with only 1B occupied, not full count two outs
      test = is_1b_only & !is_full_count_two_outs,
      yes = predict(fit_sb_attempt, newdata = runner_outcome_grid, type = "response", re.form = NA),
      no = 0
    ),
    prob_sb_success = predict(fit_sb_success, newdata = runner_outcome_grid, type = "response", re.form = NA),
    prob = dplyr::case_when(
      runner_outcome == "P+" ~ prob_po_attempt * prob_po_success,
      runner_outcome == "P-" ~ prob_po_attempt * (1 - prob_po_success),
      runner_outcome == "S+" ~ (1 - prob_po_attempt) * prob_sb_attempt * prob_sb_success,
      runner_outcome == "S-" ~ (1 - prob_po_attempt) * prob_sb_attempt * (1 - prob_sb_success),
      runner_outcome == "N"  ~ (1 - prob_po_attempt) * (1 - prob_sb_attempt)
    )
  ) |>
  dplyr::filter(prob > 0) |>
  dplyr::select(pre_state, lead_distance, runner_outcome, prob)

transition <- runner_outcome_prob |>
  dplyr::full_join(
    y = transition_conditional,
    by = c("pre_state", "runner_outcome"),
    suffix = c("_runner_outcome", "_conditional"),
    relationship = "many-to-many"
  ) |>
  dplyr::group_by(pre_state, lead_distance, post_state) |>
  dplyr::summarize(
    prob = sum(prob_runner_outcome * prob_conditional),
    # reward should not depend on runner outcome
    reward = weighted.mean(reward, w = prob_runner_outcome * prob_conditional),
    .groups = "drop"
  )

# PERFORM VALUE ITERATION ----
logger::log_info("Performing value iteration")    # 0 minutes

state <- transition |>
  dplyr::distinct(state = pre_state) |>
  dplyr::mutate(value = 0)

max_value_change <- Inf

while (max_value_change > value_iteration_threshold) {

  state_update <- state |>
    dplyr::left_join(transition, by = c("state" = "pre_state")) |>
    dplyr::left_join(state, by = c("post_state" = "state"), suffix = c("_before", "_after")) |>
    # The value of terminal end-of-inning states is definitionally zero
    dplyr::mutate(value_after = ifelse(nchar(post_state) == 1, 0, value_after)) |>
    dplyr::group_by(state, value_before, lead_distance) |>
    dplyr::summarize(value = sum(prob * (reward + value_after)), .groups = "drop") |>
    dplyr::group_by(state, value_before) |>
    dplyr::arrange(-value) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  max_value_change <- state_update |>
    with(max(abs(value - value_before)))

  state <- state_update |>
    dplyr::select(state, action = lead_distance, value)
}

logger::log_info("Done")
