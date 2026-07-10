#' Prepare runner outcome data for modeling
#'
#' Filters and transforms raw play-by-play baseball data into a standardized format 
#' required for fitting runner outcome models (such as pickoff and stolen base models).
#' This handles specific game-state exclusions and normalizes disengagement rules 
#' across different seasons.
#'
#' @param data A data frame containing play-by-play baseball data. Expected 
#'   columns include \code{pre_runner_1b_id}, \code{is_1b_only}, 
#'   \code{is_full_count_two_outs}, \code{is_runner_going}, \code{runner_id}, 
#'   \code{year}, and \code{pre_disengagements}.
#'
#' @return A processed data frame filtered to relevant base-stealing contexts, with 
#'   \code{pre_disengagements} and \code{year} converted to factors.
#'
#' @details 
#' The function applies the following data preparation logic:
#' \itemize{
#'   \item \bold{Context Filtering:} Restricts data to plays where only first base is occupied 
#'         (\code{is_1b_only}) and excludes automatic-running situations (\code{is_full_count_two_outs}).
#'   \item \bold{Sample Inclusion:} Retains only runners with a baseline of at least 3 stolen 
#'         base attempts within the dataset to ensure statistical stability.
#'   \item \bold{Identity Verification:} Ensures the active runner matches the player recorded on first base.
#'   \item \bold{Disengagement Rule Normalization:} Sets pre-2023 disengagements to \code{0} 
#'         (reflecting the era before limits) and handles the structural reset for cases 
#'         where a pitcher exceeds 3 disengagements.
#' }
#'
#' @importFrom dplyr group_by filter ungroup mutate case_when
#' @export
prep_runner_outcome_data <- function(data) {
   data |>
    dplyr::group_by(pre_runner_1b_id) |>
    dplyr::filter(
      # For modeling purposes, we consider only plays in which only first base is occupied
      is_1b_only,
      # Exclude full count with two outs because runners because these are not really steal attempts
      !is_full_count_two_outs,
      # We include only runners who attempted at least three stolen bases in our sample
      sum(is_runner_going & !is_going_interrupt) >= 3,    # TODO: Can we relax this filter?
      # The runner taking the lead from first base should match the runner on first base
      runner_id == pre_runner_1b_id | runner_id == 0
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      pre_balls = as.factor(pre_balls),
      pre_strikes = as.factor(pre_strikes),
      pre_disengagements = dplyr::case_when(
        # Before 2023, there was no limit on disengagements, equivalent to zero pre-disengagments
        year < 2023 ~ 0,
        # If the pitcher reaches three disengagements (rare), runners advance, and the counter resets
        pre_disengagements >= 3 ~ pre_disengagements %% 3,
        TRUE ~ pre_disengagements
      ) |>
        as.factor(),
      year = as.factor(year)
    )
}
