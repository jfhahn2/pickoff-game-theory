#' Summarize change in optimal lead distance with each disengagement
#'
#' Processes policy optimization results from bootstrap replicates to 
#' compute the average increase in a runner's lead distance (converted to inches) 
#' after the pitcher uses their first and second disengagements.
#'
#' @param policy_boot A data frame or grouped tibble containing optimization results. 
#'   Must include a \code{state} column (compatible with \code{pickoffgame::deconstruct_state}) 
#'   and a \code{policy_runner} column representing the baseline lead distance in feet. 
#'   Any existing groups on the data frame are preserved in the output.
#'
#' @return A grouped data frame (restoring the original group structure of \code{policy_boot}) 
#'   containing game state variables alongside two new formatted character columns:
#'   \itemize{
#'     \item \code{increase_1}: The mean $\pm$ standard deviation of the runner's lead 
#'           increase (in inches) moving from 0 to 1 disengagement.
#'     \item \code{increase_2}: The mean $\pm$ standard deviation of the runner's lead 
#'           increase (in inches) moving from 1 to 2 disengagements.
#'   }
#'   Full-count, two-out, two-strike states are masked with \code{"---"}.
#'
#' @importFrom dplyr group_vars mutate select all_of across group_by summarize filter n
#' @importFrom tidyr pivot_wider
#' @importFrom glue glue
#' @importFrom pickoffgame deconstruct_state
#' @export
summarize_lead_increase <- function(policy_boot) {

  old_group_vars <- dplyr::group_vars(policy_boot)
  new_group_vars <- c(old_group_vars, "first", "pre_bases", "pre_outs", "pre_balls", "pre_strikes")

  lead_increase <- policy_boot |>
    dplyr::mutate(
      state = pickoffgame::deconstruct_state(state),
      first = state$first,
      pre_bases = state$bases,
      pre_outs = state$outs,
      pre_balls = state$balls,
      pre_strikes = state$strikes,
      pre_disengagements = state$disengagements
    ) |>
    dplyr::select(
      dplyr::all_of(old_group_vars),
      bag, first, pre_bases, pre_outs, pre_balls, pre_strikes, pre_disengagements, policy_runner
    ) |>
    tidyr::pivot_wider(names_from = pre_disengagements, values_from = policy_runner) |>
    dplyr::mutate(`0` = 12 * (`1` - `0`), `1` = 12 * (`2` - `1`)) |>    # report inches
    dplyr::group_by(
        dplyr::across(dplyr::all_of(new_group_vars))
    ) |>
    dplyr::summarize(
      dplyr::across(.cols = c(`0`, `1`), .fns = c(mean = mean, sd = sd)),
      .groups = "drop"
    ) |>
    dplyr::filter(pre_bases == 100, !(first == 0 & pre_balls == 0 & pre_strikes == 0)) |>
    dplyr::mutate(
      increase_1 = glue::glue(
        "{sprintf('%.1f', `0_mean`)} $\\pm$ {sprintf('%.1f', `0_sd`)}"
      ),
      increase_1 = ifelse(pre_balls == 3 & pre_strikes == 2 & pre_outs == 2, "---", increase_1),
      increase_2 = glue::glue(
        "{sprintf('%.1f', `1_mean`)} $\\pm$ {sprintf('%.1f', `1_sd`)}"
      ),
      increase_2 = ifelse(pre_balls == 3 & pre_strikes == 2 & pre_outs == 2, "---", increase_2)
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(old_group_vars)))

  return(lead_increase)
}
