#' Estimate logistic mixed-effects models for runner outcomes
#'
#' Fits a series of Generalized Linear Mixed Models (GLMMs) to estimate the 
#' probabilities of pickoff (PO) attempts/success and stolen base (SB) 
#' attempts/success. The models account for game-state context, player-specific 
#' random effects, and environmental factors like disengagement counts.
#'
#' @param data A data frame containing play-by-play baseball data. Required 
#'   columns include \code{pre_runner_1b_id}, \code{is_1b_only}, 
#'   \code{is_full_count_two_outs}, \code{is_going_interrupt}, \code{runner_id}, 
#'   \code{year}, \code{pre_disengagements}, \code{is_pickoff_attempt}, 
#'   \code{is_pickoff_success}, \code{is_stolen_base}, \code{pre_outs}, 
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
#' @param include_pickoff_attempt logical, should pickoff attempt model be estimated?
#' @param include_pickoff_success logical, should pickoff success model be estimated?
#' @param include_runner_going logical, should runner going model be estimated?
#' @param include_going_interrupt logical, should batter interruption model be estimated?
#' @param include_stolen_base logical, should stolen base success model be estimated?
#'
#' @return A named list containing four fitted \code{glmerMod} objects:
#'   \itemize{
#'     \item \code{pickoff_attempt}: GLMM for the probability of a pickoff attempt.
#'     \item \code{pickoff_success}: GLMM for the probability of a successful pickoff.
#'     \item \code{runner_going}: GLMM for the probability of the runner going.
#'     \item \code{going_interrupt}: GLMM for the probability of a batter interruption.
#'     \item \code{stolen_base}: GLMM for the probability of a successful stolen base.
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
                                          include_pickoff_attempt = TRUE,
                                          include_pickoff_success = TRUE,
                                          include_runner_going = TRUE,
                                          include_going_interrupt = TRUE,
                                          include_stolen_base = TRUE) {

  fit_pickoff_attempt <- NULL
  fit_pickoff_success <- NULL
  fit_runner_going <- NULL
  fit_going_interrupt <- NULL
  fit_stolen_base <- NULL

  if (include_pickoff_attempt) {
    if (verbose) {
      logger::log_info("  Estimating GLMM for PO attempt probability")
    }
    if (is.null(bootstrap_index)) {
      fit_pickoff_attempt <- glmmTMB::glmmTMB(
        formula = is_pickoff_attempt ~ year + pre_outs + pre_balls + pre_strikes + pre_disengagements +
          lead_distance_centered + (1 | pitcher_id),
        data = data,
        family = binomial()
      )
    } else {
      fit_pickoff_attempt <- glmmTMB::refit(
        object = bootstrap_fit_original$pickoff_attempt,
        newresp = bootstrap_simulate$pickoff_attempt[[glue::glue("sim_{bootstrap_index}")]]
      )
    }
  }
  
  if (include_pickoff_success) {
    if (verbose) {
      logger::log_info("  Estimating GLMM for PO success probability")
    }
    if (is.null(bootstrap_index)) {
      fit_pickoff_success <- glmmTMB::glmmTMB(
        formula = is_pickoff_success ~ year + lead_distance_centered + (1 | pitcher_id),
        data = data |>
          dplyr::filter(is_pickoff_attempt),
        family = binomial()
      )
    } else {
      fit_pickoff_success <- glmmTMB::refit(
        object = bootstrap_fit_original$pickoff_success,
        newresp = bootstrap_simulate$pickoff_success[[glue::glue("sim_{bootstrap_index}")]]
      )
    }
  }

  if (include_runner_going) {
    if (verbose) {
      logger::log_info("  Estimating GLMM for runner going probability")
    }
    if (is.null(bootstrap_index)) {
      fit_runner_going <- glmmTMB::glmmTMB(
        is_runner_going ~ year + pre_outs + pre_balls + pre_strikes + pre_disengagements +
          sprint_speed_centered + (1 | runner_id) + (1 | pitcher_id) + arm_strength_centered + (1 | catcher_id),
        data = data |>
          dplyr::filter(!is_pickoff_attempt),
        family = binomial()
      )
    } else {
      fit_runner_going <- glmmTMB::refit(
        object = bootstrap_fit_original$runner_going,
        newresp = bootstrap_simulate$runner_going[[glue::glue("sim_{bootstrap_index}")]]
      )
    }
  }

  if (include_going_interrupt) {
    if (verbose) {
      logger::log_info("  Estimating GLMM for SB attempt probability")
    }
    if (is.null(bootstrap_index)) {
      fit_going_interrupt <- glmmTMB::glmmTMB(
        is_going_interrupt ~ year + pre_outs + pre_balls + pre_strikes + pre_disengagements +
          sprint_speed_centered + (1 | runner_id) + (1 | pitcher_id) + arm_strength_centered + (1 | catcher_id),
        data = data |>
          dplyr::filter(is_runner_going),
        family = binomial()
      )
    } else {
      fit_going_interrupt <- glmmTMB::refit(
        object = bootstrap_fit_original$going_interrupt,
        newresp = bootstrap_simulate$going_interrupt[[glue::glue("sim_{bootstrap_index}")]]
      )
    }
  }
  
  if (include_stolen_base) {
    if (verbose) {
      logger::log_info("  Estimating GLMM for SB success probability")
    }
    if (is.null(bootstrap_index)) {
      fit_stolen_base <- glmmTMB::glmmTMB(
        is_stolen_base ~ year + lead_distance_centered +
          sprint_speed_centered + (1 | runner_id) + (1 | pitcher_id) + arm_strength_centered + (1 | catcher_id),
        data = data |>
          dplyr::filter(is_runner_going & !is_going_interrupt),
        family = binomial
      )
    } else {
      fit_stolen_base <- glmmTMB::refit(
        object = bootstrap_fit_original$stolen_base,
        newresp = bootstrap_simulate$stolen_base[[glue::glue("sim_{bootstrap_index}")]]
      )
    }
  }

  return(
    list(
      pickoff_attempt = fit_pickoff_attempt,
      pickoff_success = fit_pickoff_success,
      runner_going = fit_runner_going,
      going_interrupt = fit_going_interrupt,
      stolen_base = fit_stolen_base
    )
  )
}
