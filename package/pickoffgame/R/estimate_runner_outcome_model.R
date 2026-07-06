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
#' @param bootstrap_index integer, optional, indicating the index of the bootstrap sample
#'   to use. If NULL (default), game model is estimated using original data.
#' @param bootstrap_simulate optimal list of bootstrap parametric simulations from
#'   \code{glmmTMB:::simulate.glmmTMB}, ignored if \code{bootstrap_index} is NULL.
#' @param bootstrap_fit_original optional list of original fitted \code{glmmTMB} objects
#'   for refitting, ignored if \code{bootstrap_index} is NULL.
#' @param verbose Logical. If \code{TRUE}, logs estimation progress to the console. 
#'   Defaults to \code{FALSE}.
#' @param include_po_attempt logical, should pickoff attempt model be estimated?
#' @param include_po_success logical, should pickoff success model be estimated?
#' @param include_sb_attempt logical, should stolen base attempt model be estimated?
#' @param include_sb_success logical, should stolen base success model be estimated?
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
estimate_runner_outcome_model <- function(data,
                                          bootstrap_index = NULL,
                                          bootstrap_simulate = NULL,
                                          bootstrap_fit_original = NULL,
                                          verbose = FALSE,
                                          include_po_attempt = TRUE,
                                          include_po_success = TRUE,
                                          include_sb_attempt = TRUE,
                                          include_sb_success = TRUE) {

  fit_po_attempt <- NULL
  fit_po_success <- NULL
  fit_sb_attempt <- NULL
  fit_sb_success <- NULL

  if (include_po_attempt) {
    if (verbose) {
      logger::log_info("  Estimating GLMM for PO attempt probability")    # 2 minutes
    }
    if (is.null(bootstrap_index)) {
      fit_po_attempt <- glmmTMB::glmmTMB(
        formula = is_po_attempt ~ year + pre_outs + pre_balls + pre_strikes + pre_disengagements +
          lead_distance_centered + (1 | pitcher_id),
        data = data,
        family = binomial()
      )
    } else {
      fit_po_attempt <- glmmTMB::refit(
        object = bootstrap_fit_original$po_attempt,
        newresp = bootstrap_simulate$po_attempt[[glue::glue("sim_{bootstrap_index}")]]
      )
    }
  }
  
  if (include_po_success) {
    if (verbose) {
      logger::log_info("  Estimating GLMM for PO success probability")    # 0 minutes
    }
    if (is.null(bootstrap_index)) {
      fit_po_success <- glmmTMB::glmmTMB(
        formula = is_po_success ~ lead_distance_centered + (1 | pitcher_id),
        data = data |>
          dplyr::filter(is_po_attempt),
        family = binomial()
      )
    } else {
      fit_po_success <- glmmTMB::refit(
        object = bootstrap_fit_original$po_success,
        newresp = bootstrap_simulate$po_success[[glue::glue("sim_{bootstrap_index}")]]
      )
    }
  }

  if (include_sb_attempt) {
    if (verbose) {
      logger::log_info("  Estimating GLMM for SB attempt probability")    # 7 minutes
    }
    if (is.null(bootstrap_index)) {
      fit_sb_attempt <- glmmTMB::glmmTMB(
        is_sb_attempt ~ pre_outs + pre_balls + pre_strikes + pre_disengagements +
          sprint_speed_centered + (1 | runner_id) + (1 | pitcher_id) + arm_strength_centered + (1 | catcher_id),
        data = data |>
          dplyr::filter(!is_po_attempt),
        family = binomial()
      )
    } else {
      fit_sb_attempt <- glmmTMB::refit(
        object = bootstrap_fit_original$sb_attempt,
        newresp = bootstrap_simulate$sb_attempt[[glue::glue("sim_{bootstrap_index}")]]
      )
    }
  }
  
  if (include_sb_success) {
    if (verbose) {
      logger::log_info("  Estimating GLMM for SB success probability")    # 1 minute
    }
    if (is.null(bootstrap_index)) {
      fit_sb_success <- glmmTMB::glmmTMB(
        is_sb_success ~ year + lead_distance_centered +
          sprint_speed_centered + (1 | runner_id) + (1 | pitcher_id) + arm_strength_centered + (1 | catcher_id),
        data = data |>
          dplyr::filter(is_sb_attempt),
        family = binomial
      )
    } else {
      fit_sb_success <- glmmTMB::refit(
        object = bootstrap_fit_original$sb_success,
        newresp = bootstrap_simulate$sb_success[[glue::glue("sim_{bootstrap_index}")]]
      )
    }
  }

  return(
    list(
      po_attempt = fit_po_attempt,
      po_success = fit_po_success,
      sb_attempt = fit_sb_attempt,
      sb_success = fit_sb_success
    )
  )
}
