#' Predict runner outcome probabilities
#' 
#' Predicts stolen base and pickoff attempt and success probabilties from fitted lme4
#' models and converts these conditional probabilities to overall runner outcome
#' probabilities (successful pickoff P+, unsuccessful pickoff P-, successful steal S+,
#' unsuccessful steal S-, or no runner involvement N).
#'
#' Calculates the probabilities of various runner outcomes (pickoff success/failure, 
#' stolen base success/failure, or no action) based on a grid of game states, 
#' a list of fitted regression models, and optional player-specific percentiles.
#'
#' @param runner_outcome_grid A data frame containing game states and potential 
#'   actions. Must include columns such as \code{is_1b_only}, 
#'   \code{is_full_count_two_outs}, \code{action_pitcher}, \code{pre_state}, 
#'   and \code{action_runner}.
#' @param fit_runner_outcome A named list of fitted model objects (e.g., from 
#'   \code{lme4} or \code{glm}) corresponding to \code{po_attempt}, 
#'   \code{po_success}, \code{sb_attempt}, and \code{sb_success}.
#' @param percentile_players A list of tables of representative players corresponding
#'   to skill percentiles, from \code{\link{extract_percentile_players}}. Each table
#'   in the list corresponds to one of the lme4 models for runner outcomes and has a
#'   row of representative players for each percentile. The default, \code{NULL},
#'   creates predictions assuming average players, without using random effects.
#' @param runner_percentile Numeric. The percentile value to filter for the 
#'   runner's ability when \code{percentile_players} is provided.
#' @param battery_percentile Numeric. The percentile value to filter for the 
#'   battery's ability when \code{percentile_players} is provided.
#'
#' @return A data frame containing the \code{pre_state}, \code{action_runner}, 
#'   \code{action_pitcher}, \code{runner_outcome} (e.g., "P+", "S+"), and 
#'   the resulting probability \code{prob}.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols filter mutate case_when cross_join select
#' @export
predict_runner_outcome_prob <- function(runner_outcome_grid,
                                        fit_runner_outcome,
                                        percentile_players = NULL,
                                        runner_percentile = NULL,
                                        battery_percentile = NULL) {

  representative_players <- list()

  if (is.null(percentile_players)) {
    for (runner_outcome in names(fit_runner_outcome)) {
      representative_players[[runner_outcome]] <- tibble::tibble(
        sprint_speed_centered = 0,
        arm_strength_centered = 0
      )
    }
    re_form <- NA

  } else {
    for (runner_outcome in names(fit_runner_outcome)) {
      representative_players[[runner_outcome]] <- dplyr::bind_cols(
        percentile_players[[runner_outcome]]$runner |>
          dplyr::filter(pct_runner == runner_percentile),
        percentile_players[[runner_outcome]]$battery |>
          dplyr::filter(pct_battery == battery_percentile)
      )
    }
    re_form <- NULL
  }

  runner_outcome_prob <- runner_outcome_grid |>
    dplyr::mutate(
      prob_po_attempt = dplyr::case_when(
        # our model only allows pickoffs and steals with only 1B occupied, not full count two outs
        !is_1b_only | is_full_count_two_outs ~ 0,
        action_pitcher == "pickoff" ~ 1,
        action_pitcher == "pitch" ~ 0,
        TRUE ~ predict(
          object = fit_runner_outcome$po_attempt,
          newdata = runner_outcome_grid |>
            dplyr::cross_join(representative_players$po_attempt),
          type = "response",
          re.form = re_form
        )
      ),
      prob_po_success = predict(
        object = fit_runner_outcome$po_success,
        newdata = runner_outcome_grid |>
          dplyr::cross_join(representative_players$po_success),
        type = "response",
        re.form = re_form
      ),
      prob_sb_attempt = ifelse(
        # our model only allows pickoffs and steals with only 1B occupied, not full count two outs
        test = is_1b_only & !is_full_count_two_outs,
        yes = predict(
          object = fit_runner_outcome$sb_attempt,
          newdata = runner_outcome_grid |>
            dplyr::cross_join(representative_players$sb_attempt),
          type = "response",
          re.form = re_form
        ),
        no = 0
      ),
      prob_sb_success = predict(
        object = fit_runner_outcome$sb_success,
        newdata = runner_outcome_grid |>
          dplyr::cross_join(representative_players$sb_success),
        type = "response",
        re.form = re_form
      ),
      prob = dplyr::case_when(
        runner_outcome == "P+" ~ prob_po_attempt * prob_po_success,
        runner_outcome == "P-" ~ prob_po_attempt * (1 - prob_po_success),
        runner_outcome == "S+" ~ (1 - prob_po_attempt) * prob_sb_attempt * prob_sb_success,
        runner_outcome == "S-" ~ (1 - prob_po_attempt) * prob_sb_attempt * (1 - prob_sb_success),
        runner_outcome == "N"  ~ (1 - prob_po_attempt) * (1 - prob_sb_attempt)
      )
    ) |>
    dplyr::filter(prob > 0) |>
    dplyr::select(pre_state, action_runner, action_pitcher, runner_outcome, prob)

  return(runner_outcome_prob)
}
