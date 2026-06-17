#' Estimate game model
#'
#' Estimates the full game model by first estimating transition probabilities
#' between states and then using value iteration to find the optimal policy.
#'
#' @param data A data frame containing historical play-by-play data used to 
#'   estimate transitions.
#' @param players Character string. Specifies the game type: \code{"one"} 
#'   (Markov decision process) or \code{"two"} (stochastic zero-sum game). 
#'   Defaults to \code{"one"}.
#' @param percentile_players A list of player performance data frames (from 
#'   \code{extract_percentile_players}) used to set specific runner/battery 
#'   skill levels. Defaults to \code{NULL}.
#' @param runner_percentile Numeric. The percentile value to filter for the 
#'   runner's performance. Defaults to \code{NULL}.
#' @param battery_percentile Numeric. The percentile value to filter for the 
#'   battery's performance. Defaults to \code{NULL}.
#'
#' @return A data frame containing the optimal \code{policy_runner} and 
#'   the resulting state \code{value} for every game state in the transition model.
#'
#' @importFrom dplyr distinct mutate filter full_join group_by summarize
#' @importFrom tidyr expand_grid
#' @importFrom pickoffgame estimate_conditional_transition_probability deconstruct_state solve_optimal_policy
#' @export
estimate_game_model <- function(data,
                                players = c("one", "two"),
                                percentile_players = NULL,
                                runner_percentile = NULL,
                                battery_percentile = NULL) {

  players <- match.arg(players)

  transition_conditional <- data |>
    pickoffgame::estimate_conditional_transition_probability()
  
  runner_outcome_grid <- transition_conditional |>
    dplyr::distinct(pre_state) |>
    tidyr::expand_grid(
      runner_outcome = unique(transition_conditional$runner_outcome),
      action_runner = seq(from = 0, to = 30, by = 0.1),
      action_pitcher = c("pickoff", "pitch", "stochastic")
    ) |>
    dplyr::mutate(
      year = factor(2023, levels = c(2022, 2023)),
      pre = pickoffgame::deconstruct_state(pre_state),
      pre_bases = pre$bases,
      pre_outs = pre$outs,
      pre_balls = pre$balls,
      pre_strikes = pre$strikes,
      pre_disengagements = factor(pre$disengagements, levels = 0:2),
      lead_distance_centered = action_runner - 10,
      is_1b_only = pre$bases == "100",
      is_full_count_two_outs = (pre$balls == 3) & (pre$strikes == 2) & (pre$outs == 2)
    ) |>
    # Action space is null unless only 1B is occupied without full count and two strikes
    dplyr::filter(
      (is_1b_only & !is_full_count_two_outs) | action_runner == 0,
      (is_1b_only & !is_full_count_two_outs) | action_pitcher == "stochastic"
    )
  
  runner_outcome_prob <- predict_runner_outcome_prob(
    runner_outcome_grid = runner_outcome_grid,
    fit_runner_outcome = fit_runner_outcome,
    percentile_players = percentile_players,
    runner_percentile = runner_percentile,
    battery_percentile = battery_percentile
  )
  
  transition <- runner_outcome_prob |>
    dplyr::full_join(
      y = transition_conditional,
      by = c("pre_state", "runner_outcome"),
      suffix = c("_runner_outcome", "_conditional"),
      relationship = "many-to-many"
    ) |>
    dplyr::group_by(pre_state, action_runner, action_pitcher, post_state) |>
    dplyr::summarize(
      prob = sum(prob_runner_outcome * prob_conditional),
      # reward should not depend on runner outcome
      reward = weighted.mean(reward, w = prob_runner_outcome * prob_conditional),
      .groups = "drop"
    )
  
  policy <- transition |>
    pickoffgame::solve_optimal_policy(players = players)

  return(policy)
}
