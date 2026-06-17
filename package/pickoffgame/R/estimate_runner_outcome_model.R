#' Estimate logistic mixed-effects models for runner outcomes
#'
#' Fits a series of Generalized Linear Mixed Models (GLMMs) to estimate the 
#' probabilities of pickoff (PO) attempts/success and stolen base (SB) 
#' attempts/success. The models account for game-state context, player-specific 
#' random effects, and environmental factors like disengagement counts.
#'
#' @param data A data frame containing play-by-play baseball data. Required 
#'   columns include \code{pre_runner_1b_id}, \code{is_1b_only}, 
#'   \code{is_full_count_two_outs}, \code{is_sb_attempt}, \code{runner_id}, 
#'   \code{year}, \code{pre_disengagements}, \code{is_po_attempt}, 
#'   \code{is_po_success}, \code{is_stolen_base}, \code{pre_outs}, 
#'   \code{pre_balls}, \code{pre_strikes}, \code{lead_distance_centered}, 
#'   \code{sprint_speed_centered}, \code{arm_strength_centered}, 
#'   \code{pitcher_id}, \code{runner_id}, and \code{catcher_id}.
#' @param verbose Logical. If \code{TRUE}, logs estimation progress to the console. 
#'   Defaults to \code{FALSE}.
#'
#' @return A named list containing four fitted \code{glmerMod} objects:
#'   \itemize{
#'     \item \code{po_attempt}: GLMM for the probability of a pickoff attempt.
#'     \item \code{po_success}: GLMM for the probability of a successful pickoff.
#'     \item \code{sb_attempt}: GLMM for the probability of a stolen base attempt.
#'     \item \code{sb_success}: GLMM for the probability of a successful stolen base.
#'   }
#'
#' @importFrom lme4 glmer
#' @importFrom dplyr group_by filter mutate ungroup
#' @importFrom logger log_info
#' @export
estimate_runner_outcome_model <- function(data, verbose = FALSE) {

  data_glmer <- data |>
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

  if (verbose) {
    logger::log_info("  Estimating GLMM for PO attempt probability")    # 2 minutes
  }
  fit_po_attempt <- lme4::glmer(
    formula = is_po_attempt ~ year + pre_outs + pre_balls + pre_strikes + pre_disengagements +
      lead_distance_centered + (1 | pitcher_id),
    data = data_glmer,
    family = binomial()
  )
  
  if (verbose) {
    logger::log_info("  Estimating GLMM for PO success probability")    # 0 minutes
  }
  fit_po_success <- lme4::glmer(
    formula = is_po_success ~ lead_distance_centered + (1 | pitcher_id),
    data = data_glmer |>
      dplyr::filter(is_po_attempt),
    family = binomial()
  )
 
  if (verbose) {
    logger::log_info("  Estimating GLMM for SB attempt probability")    # 7 minutes
  }
  fit_sb_attempt <- lme4::glmer(
    is_sb_attempt ~ pre_outs + pre_balls + pre_strikes + pre_disengagements +
      sprint_speed_centered + (1 | runner_id) + (1 | pitcher_id) + arm_strength_centered + (1 | catcher_id),
    data = data_glmer |>
      dplyr::filter(!is_po_attempt),
    family = binomial()
  )
  
  if (verbose) {
    logger::log_info("  Estimating GLMM for SB success probability")    # 1 minute
  }
  fit_sb_success <- lme4::glmer(
    is_stolen_base ~ year + lead_distance_centered +
      sprint_speed_centered + (1 | runner_id) + (1 | pitcher_id) + arm_strength_centered + (1 | catcher_id),
    data = data_glmer |>
      dplyr::filter(is_sb_attempt),
    family = binomial
  )

  return(
    list(
      po_attempt = fit_po_attempt,
      po_success = fit_po_success,
      sb_attempt = fit_sb_attempt,
      sb_success = fit_sb_success
    )
  )
}
