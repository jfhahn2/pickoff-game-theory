#' Execute full analysis pipeline
#'
#' Runs the end-to-end analysis workflow for the pickoff game model. 
#' This master function handles data preparation, model validation (optional), 
#' fitting of generalized linear mixed models (GLMMs), player skill extraction, 
#' and the calculation of optimal strategies using both single-player Markov 
#' decision processes (MDP) and two-player zero-sum game (ZSG).
#'
#' @param game_state A data frame containing raw play-by-play baseball tracking data.
#' @param bootstrap_index integer, optional, indicating the index of the bootstrap sample
#'   to use. If NULL (default), game model is estimated using original data.
#' @param bootstrap_resample optional tibble from \code{rsample::bootstraps} containing
#'   bootstrap samples, ignored if \code{bootstrap_index} is NULL.
#' @param bootstrap_simulate optimal list of bootstrap parametric simulations from
#'   \code{glmmTMB:::simulate.glmmTMB}, ignored if \code{bootstrap_index} is NULL.
#' @param bootstrap_fit_original optional list of original fitted \code{glmmTMB} objects
#'   for refitting, ignored if \code{bootstrap_index} is NULL.
#'  @param estimate_glmms Logical. Should GLMM models be estimated? If \code{FALSE},
#'   the function attempts to read the file file output/models/fit_runner_outcome.rds.
#'   Defaults to \code{TRUE}.
#' @param validate_glmms Logical. If \code{TRUE}, performs cross-validation on the
#'   runner outcome models. Defaults to \code{FALSE}.
#'
#' @return A named list containing five components:
#'   \itemize{
#'     \item \code{runner_outcome_model_validation}: Results from cross-validation, 
#'           or \code{NULL} if skipped.
#'     \item \code{fit_runner_outcome}: The five fitted \code{glmmTMB} objects for runner actions.
#'     \item \code{policy_mdp}: Optimal runner policy evaluated in a single-player environment.
#'     \item \code{policy_zsg}: Optimal runner/pitcher policy evaluated as a two-player zero-sum game.
#'     \item \code{policy_mdp_skill}: A consolidated data frame containing optimal single-player 
#'           policies across a 3x3 grid of runner and battery skill percentiles (10th, 50th, 90th).
#'   }
#'
#' @importFrom logger log_info
#' @importFrom tibble tibble
#' @importFrom dplyr mutate bind_rows
#' @export
run_analysis_pipeline <- function(game_state,
                                  bootstrap_index = NULL,
                                  bootstrap_resample = NULL,
                                  bootstrap_simulate = NULL,
                                  bootstrap_fit_original = NULL,
                                  estimate_glmms = TRUE,
                                  validate_glmms = FALSE) {

  data_glmm <- prep_runner_outcome_data(data = game_state, estimate_glmms = estimate_glmms)

  if (!is.null(bootstrap_index)) {
    # If we're doing bootstrapping, replace the dataset with a bootstrap resample
    game_state <- rsample::analysis(bootstrap_resample$splits[[bootstrap_index]])
  }

  if (validate_glmms) {
    logger::log_info("Validating runner outcome models")    # 7 minutes

    set.seed(42)
    runner_outcome_model_validation <- validate_runner_outcome_model(data_glmm)

  } else {
    runner_outcome_model_validation <- NULL
  }

  # FIT RUNNER OUTCOME MODELS ----

  if (estimate_glmms) {
    logger::log_info("Fitting runner outcome models")         # 2 minutes
    fit_runner_outcome <- data_glmm |>
      estimate_runner_outcome_model(
        bootstrap_index = bootstrap_index,
        bootstrap_simulate = bootstrap_simulate,
        bootstrap_fit_original = bootstrap_fit_original
      )

  } else {
    runner_outcome_file <- "output/models/fit_runner_outcome.rds"
    if (!file.exists(runner_outcome_file)) {
      stop("If not estimating the GLMMs yourself, you need output/models/fit_runner_outcome.rds")
    }
    fit_runner_outcome <- readRDS(runner_outcome_file)
  }

  percentile_players <- list()
  for (outcome in names(fit_runner_outcome)) {
    percentile_players[[outcome]] <- extract_percentile_players(
      object = fit_runner_outcome[[outcome]],
      data = data_glmm,
      flip = outcome %in% c("pickoff_attempt", "pickoff_success", "going_interrupt")
    )
  }

  # ESTIMATE GAME MODELS ----
  logger::log_info("Estimating game models")                # 10 minutes

  policy_observed <- data_glmm |>
    dplyr::filter(year == 2023, !is.na(lead_distance)) |>
    dplyr::group_by(pre_state) |>
    dplyr::summarize(lead_distance = round(mean(lead_distance), 1))

  policy_mrp <- estimate_game_model(
    data = game_state,
    fit_runner_outcome = fit_runner_outcome,
    players = "one",
    fixed_policy = policy_observed
  )

  policy_mdp <- estimate_game_model(
    data = game_state,
    fit_runner_outcome = fit_runner_outcome,
    players = "one"
  )

  policy_zsg <- estimate_game_model(
    data = game_state,
    fit_runner_outcome = fit_runner_outcome,
    players = "two"
  )

  policy_mdp_skill <- tibble::tibble()
  for (runner_percentile in c(0.1, 0.5, 0.9)) {
    for (battery_percentile in c(0.1, 0.5, 0.9)) {
      policy_mdp_skill <- estimate_game_model(
        data = game_state,
        fit_runner_outcome = fit_runner_outcome,
        players = "one",
        percentile_players = percentile_players,
        runner_percentile = runner_percentile,
        battery_percentile = battery_percentile
      ) |>
        dplyr::mutate(
          pct_runner = runner_percentile,
          pct_battery = battery_percentile,
          .before = 1
        ) |>
        dplyr::bind_rows(policy_mdp_skill)
    }
  }

  return(
    list(
      data_glmm = data_glmm,
      runner_outcome_model_validation = runner_outcome_model_validation,
      fit_runner_outcome = fit_runner_outcome,
      policy_mrp = policy_mrp,
      policy_mdp = policy_mdp,
      policy_zsg = policy_zsg,
      policy_mdp_skill = policy_mdp_skill
    )
  )
}
