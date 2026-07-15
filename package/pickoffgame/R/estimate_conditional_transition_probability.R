#' Estimate conditional transition probabilities
#'
#' Estimates the transition probabilities between game states, conditional 
#' on specific runner outcomes (e.g., pickoff, stolen base, or no action). 
#' It combines empirical data with deterministic rules for unobserved transitions 
#' and incorporates state-specific logic for disengagement counts.
#'
#' @param data A data frame containing play-by-play baseball data. Expected 
#'   columns include \code{type}, \code{pre_state_reduced}, \code{runner_outcome}, 
#'   \code{post_state_reduced}, and \code{runs_on_play}.
#'
#' @return A data frame defining the transition model, including \code{pre_state}, 
#'   \code{runner_outcome}, \code{post_state}, the transition probability 
#'   \code{prob}, and the associated expected \code{reward}.
#'
#' @importFrom dplyr filter group_by summarize mutate ungroup n anti_join bind_rows case_when select
#' @importFrom tidyr expand_grid
#' @importFrom pickoffgame deconstruct_state apply_runner_outcome update_state
#' @export
estimate_conditional_transition_probability <- function(data) {

  # Compute empirical transition probabilities between reduced states
  transition_conditional_observed <- data |>
    # By commenting out the line below, we include non-pickoff stepoffs and automatic balls/strikes
#    dplyr::filter(type %in% c("pickoff", "pitch")) |>
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
    # Perform this mutate before the anti-join in case the anti-join wipes out all of the rows.
    # This happens when we hack this function to remove the conditioning on runner outcome.
    dplyr::mutate(
      post_state_reduced = pickoffgame::apply_runner_outcome(pre_state_reduced, runner_outcome),
      prob = 1,
      # The unobserved conditional transitions are only stolen base and pickoff attempts, and we
      # assume that these transitions do not result in runs being scored (for unobserved cases).
      reward = 0
    ) |>
    dplyr::anti_join(
      y = transition_conditional_observed,
      by = c("pre_state_reduced", "runner_outcome")
    )
  
  # This is where we append disengagements back onto the state.
  # The logic is tricky and is always worth revisiting.
  transition_conditional <- transition_conditional_observed |>
    dplyr::bind_rows(transition_conditional_unobserved) |>
    # Expand reduced state to full state by tacking on disengagements and first indicator
    tidyr::expand_grid(pre_disengagements = 0:2, pre_first = 0:1) |>
    dplyr::mutate(
      pre = pickoffgame::deconstruct_state(pre_state_reduced),
      post = pickoffgame::deconstruct_state(post_state_reduced),
      pre_state = pickoffgame::update_state(
        state = pre_state_reduced,
        new_first = pre_first,
        new_disengagements = pre_disengagements
      ),
      post_disengagements = pre_disengagements + ifelse(runner_outcome %in% c("PO+", "PO-"), 1, 0),
      pre_account_offense = stringr::str_count(pre$bases, "1") + pre$outs,
      post_account_offense = stringr::str_count(post$bases, "1") + post$outs + round(reward),
      post_first = dplyr::case_when(
        post_account_offense > pre_account_offense ~ 1,
        TRUE ~ 0
      ),
      is_runner_movement = pre$bases != post$bases,
      post_state = dplyr::case_when(
        # If the reduced post-state is a terminal end-of-inning state, do not append disengagements
        nchar(post_state_reduced) == 1 ~ post_state_reduced,
        # If the transition is end of plate appearance or runner movement, reset disengagements
        (post_first == 1) | is_runner_movement ~ pickoffgame::update_state(
          state = post_state_reduced,
          new_first = post_first,
          new_disengagements = 0
        ),
        # If third disengagement with no runner movement, advance runners and reset disengagements
        !is_runner_movement & (post_disengagements == 3) ~ pickoffgame::update_state(
          state = post_state_reduced,
          new_first = post_first,
          new_bases = paste0(0, substring(pre$bases, 1, 2)),
          new_disengagements = 0
        ),
        TRUE ~ pickoffgame::update_state(
          state = post_state_reduced,
          new_first = post_first,
          new_disengagements = post_disengagements
        )
      )
    ) |>
    # Remove impossible states (first play of the PA but there are already disengagements)
    dplyr::filter(!(pre_first == 1 & (pre$balls + pre$strikes + pre_disengagements > 0))) |>
    dplyr::select(pre_state, runner_outcome, post_state, prob, reward)

  return(transition_conditional)
}
