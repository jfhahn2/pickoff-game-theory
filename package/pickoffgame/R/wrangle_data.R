#' Join and clean raw baseball data into game-state transitions
#'
#' Merges raw play, pitch, lead distance, event, and Statcast measurement data frames. 
#' The function imputes missing values, centers continuous covariates, normalizes 
#' disengagement counts, and constructs discrete state definitions for sequential modeling.
#'
#' @details
#' This data-cleaning function processes the raw inputs via the following steps:
#' \itemize{
#'   \item \strong{Data Merging:} Executes sequential left joins across all inputs using tracking, game, and play identifiers.
#'   \item \strong{Imputation:} Replaces missing arm strength and sprint speed records with their respective sample means.
#'   \item \strong{Covariate Centering:} Centers sprint speed and arm strength around their means, and subtracts 10 from lead distance to improve lme4 regression convergence.
#'   \item \strong{Outcome Mapping:} Classifies plays into mutually exclusive categories: "P+" (successful pickoff), "P-" (failed pickoff), "S+" (stolen base), "S-" (caught stealing), or "N" (no pickoff or steal attempt).
#'   \item \strong{Sparsity Reduction:} Recodes low-frequency player IDs (<500 plays for pitchers/catchers, <10 for runners) to a common baseline identifier ("0").
#'   \item \strong{Disengagement Correction:} Recalculates pre- and post-disengagement counts within play stints, forcing resets to zero when runners advance or a plate appearance ends.
#'   \item \strong{State Construction:} Generates string codes representing the structural bases, outs, count, and disengagement levels before and after each play.
#' }
#'
#' @param play table of play info containing base occupancy, balls, strikes, and outs
#' @param pitch table containing pitch descriptions and play identifiers
#' @param lead_distance table containing lead distances from first base
#' @param event table tracking player identities and event indexes
#' @param sprint_speed table containing runner sprint speed
#' @param arm_strength table containing catcher arm strength
#' @param pitch_map table mapping text descriptions to standardized pitch outcomes
#' @param event_map table mapping text descriptions to standardized plate appearance outcomes
#'
#' @return A data frame containing processed capability covariates, categorical runner outcomes, 
#' and standardized pre- and post-state string identifiers.
#'
#' @importFrom dplyr select left_join arrange mutate case_when coalesce group_by n filter ungroup lag
#' @export
wrangle_data <- function(play,
                         pitch,
                         lead_distance,
                         event,
                         sprint_speed,
                         arm_strength,
                         pitch_map,
                         event_map) {

  event_info <- event |>
    dplyr::select(
      game_id, event_index, pitcher_id, catcher_id = fielder_2_id, event, inning, half_inning
    )
  
  play_detail <- play |>
    dplyr::left_join(pitch, by = "play_id") |>
    dplyr::left_join(lead_distance, by = "play_id") |>
    dplyr::left_join(event_info, by = c("game_id", "event_index")) |>
    dplyr::left_join(sprint_speed, by = c("runner_id" = "player_id")) |>
    dplyr::left_join(arm_strength, by = c("catcher_id" = "player_id")) |>
    dplyr::left_join(pitch_map, by = "description") |>
    dplyr::left_join(event_map, by = "event") |>
    dplyr::arrange(game_id, event_index, play_index) |>
    dplyr::mutate(
      # Impute average arm strength and sprint speed when missing
      arm_strength = dplyr::coalesce(arm_strength, mean(arm_strength, na.rm = TRUE)),
      sprint_speed = dplyr::coalesce(sprint_speed, mean(sprint_speed, na.rm = TRUE)),
      # Center sprint speed, arm strength and lead distance purely for better lme4 convergence
      sprint_speed_centered = scale(sprint_speed, scale = FALSE),
      arm_strength_centered = scale(arm_strength, scale = FALSE),
      lead_distance_centered = lead_distance - 10,
      pitch_event = dplyr::case_when(
        is.na(batter_description) ~ "No Pitch",
        batter_description == "In Play" ~ batter_event,
        TRUE ~ batter_description
      ),
      is_po_attempt = type == "pickoff",
      is_po_success = dplyr::coalesce(is_pickoff, FALSE),
      is_sb_attempt = !is_po_attempt & (is_stolen_base | is_caught_stealing),
      is_sb_success = is_sb_attempt & is_stolen_base,
      is_1b_only = !is.na(pre_runner_1b_id) & is.na(pre_runner_2b_id) & is.na(pre_runner_3b_id),
      runner_outcome = dplyr::case_when(
        !is_1b_only ~ "N",   # runner outcome only refers to 1B occupied with 2B & 3B empty
        is_po_attempt & is_po_success ~ "P+",
        is_po_attempt & !is_po_success ~ "P-",
        is_sb_attempt & is_sb_success ~ "S+",
        is_sb_attempt & !is_sb_success ~ "S-",
        !is_po_attempt & !is_sb_attempt ~ "N"
      ),
      # TODO: reset disengagement counter for runner movement
      #       (and set post-disengagements to zero for final pitch of pa)
      # If the play ends the inning, the post-state reports the number of runs scored on the play
      pre_bases = paste0(
        as.numeric(!is.na(pre_runner_1b_id)),
        as.numeric(!is.na(pre_runner_2b_id)),
        as.numeric(!is.na(pre_runner_3b_id))
      ),
      post_bases = paste0(
        as.numeric(!is.na(post_runner_1b_id)),
        as.numeric(!is.na(post_runner_2b_id)),
        as.numeric(!is.na(post_runner_3b_id))
      )
    ) |>
    # Replace small-sample player identifers with "replacement" identifier
    dplyr::group_by(pitcher_id) |>
    dplyr::mutate(pitcher_id = ifelse(dplyr::n() >= 500, as.character(pitcher_id), "0")) |>
    dplyr::group_by(catcher_id) |>
    dplyr::mutate(catcher_id = ifelse(dplyr::n() >= 500, as.character(catcher_id), "0")) |>
    dplyr::group_by(runner_id) |>
    dplyr::mutate(runner_id = ifelse(dplyr::n() >= 10, as.character(runner_id), "0")) |>
    dplyr::group_by(game_id, event_index) |>
    # Throw out any plate appearances with too many strikes or too many balls
    dplyr::filter(!any(pre_balls > 3), !any(pre_strikes > 2)) |>
    dplyr::ungroup()

  game_state <- play_detail |>
    # Correctly handle pre- and post-disengagements. In the raw data, the counter doesn't reset
    # after runner movement, and post-disengagements is not zero at the end of a plate appearance.
    dplyr::group_by(game_id, event_index) |>
    dplyr::mutate(
      is_last_play_of_event = 1:dplyr::n() == dplyr::n(),
      # Within each event, group plays into stints between runner movement
      disengagement_stint = ((!is_last_play_of_event) & (pre_bases != post_bases)) |>
        dplyr::lag(1) |>
        dplyr::coalesce(0) |>
        cumsum()
    ) |>
    dplyr::group_by(game_id, event_index, disengagement_stint) |>
    # Within each stint between runner movement, reset disengagements to start at zero
    dplyr::mutate(
      disengagements_reset = min(pre_disengagements),
      # Before 2023, we consider pre- and post-disengagmenets to be definitionally zero
      pre_disengagements = ifelse(year < 2023, 0, pre_disengagements - disengagements_reset),
      # Disengagement counter resets if plate appearance ends or if runners move
      post_disengagements = ifelse(
        test = (year < 2023) | is_last_play_of_event | (pre_bases != post_bases),
        yes = 0,
        no = post_disengagements - disengagements_reset
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # Reset count at the end of a plate appearance
      post_balls = ifelse(is_last_play_of_event, 0, post_balls),
      post_strikes = ifelse(is_last_play_of_event, 0, post_strikes),
      pre_state = construct_state(pre_bases, pre_outs, pre_balls, pre_strikes, pre_disengagements),
      pre_state_reduced = substring(pre_state, 1, 8),
      post_state = ifelse(
        test = post_outs == 3,
        yes = as.character(runs_on_play),
        no = construct_state(post_bases, post_outs, post_balls, post_strikes, post_disengagements)
      ),
      post_state_reduced = substring(post_state, 1, 8),
    )

  return(game_state)
}