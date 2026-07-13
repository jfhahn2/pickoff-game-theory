#' Solve for the optimal policy using value iteration
#'
#' Computes the optimal lead distance policy for the runner in either a one-player
#' Markov decision process or a two-player zero-sum game, in which case the runner's
#' strategy is minimax.
#'
#' @param transition A data frame containing the transition model. Expected columns:
#'   \code{pre_state}, \code{post_state}, \code{prob}, \code{reward}, 
#'   \code{action_runner}, and \code{action_pitcher}.
#' @param players Character string specifying the game type. Use \code{"one"} for 
#'   one-player Markov decision process or \code{"two"} for a two-player zero-sum game
#'   Defaults to \code{"one"}.
#' @param value_iteration_threshold Numeric. The convergence criterion for the 
#'   value iteration algorithm; the loop terminates when the maximum change in 
#'   state value falls below this threshold. Defaults to \code{1e-4}.
#'
#' @return A data frame containing the \code{state}, the optimal 
#'   \code{policy_runner} (the action that maximizes value), and the calculated 
#'   \code{value} of each state.
#'
#' @importFrom dplyr filter distinct mutate left_join group_by summarize arrange slice ungroup select
#' @export
solve_optimal_policy <- function(transition,
                                 players = c("one", "two"),
                                 value_iteration_threshold = 1e-4) {

  players <- match.arg(players)

  state <- transition |>
    dplyr::distinct(state = pre_state) |>
    dplyr::mutate(value = 0)
  
  max_value_change <- Inf
  
  while (max_value_change > value_iteration_threshold) {
  
    exp_value <- state |>
      dplyr::left_join(transition, by = c("state" = "pre_state")) |>
      dplyr::left_join(state, by = c("post_state" = "state"), suffix = c("_before", "_after")) |>
      # The value of terminal end-of-inning states is definitionally zero
      dplyr::mutate(value_after = ifelse(nchar(post_state) == 1, 0, value_after)) |>
      dplyr::group_by(state, value_before, action_runner, action_pitcher) |>
      dplyr::summarize(value = sum(prob * (reward + value_after)), .groups = "drop")
      
    if (players == "two") {
      # For each possible runner action, find the pitcher action which minimizes run value
      min_value <- exp_value |>
        dplyr::group_by(state, value_before, action_runner) |>
        dplyr::arrange(value) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
    } else {
      min_value <- exp_value
    }
    
    # Find the runner action which maximizes run value
    max_value <- min_value |>
      dplyr::group_by(state, value_before) |>
      dplyr::arrange(-value) |>
      dplyr::slice(1) |>
      dplyr::ungroup()
  
    max_value_change <- max_value |>
      with(max(abs(value - value_before)))
  
    state <- max_value |>
      dplyr::select(state, policy_runner = action_runner, value)
  }

  return(state)
}
