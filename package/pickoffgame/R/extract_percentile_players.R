#' Extract representative players at specified percentiles
#'
#' Identifies runner and battery combinations (pitcher/catcher) that correspond 
#' to specific performance percentiles based on fixed effects and random effects 
#' extracted from a fitted lme4 model.
#'
#' @param object A fitted model object (e.g., \code{glmerMod}) containing 
#'   random effects for \code{runner_id}, \code{pitcher_id}, and/or \code{catcher_id},
#'   and possibly fixed effects for \code{sprint_speed_centered} and \code{arm_strength_centered}.
#' @param data A table containing the data used to train \code{object}, including 
#'   \code{runner_id}, \code{pitcher_id}, \code{catcher_id}, \code{sprint_speed_centered}, 
#'   and \code{arm_strength_centered}.
#' @param percentile A numeric vector of percentiles to extract. Defaults to 
#'   \code{c(0.1, 0.5, 0.9)}.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{runner}: A data frame of \code{runner_id} and \code{sprint_speed_centered} 
#'     at the specified percentiles.
#'     \item \code{battery}: A data frame of \code{pitcher_id}, \code{catcher_id}, and 
#'     \code{arm_strength_centered} at the specified percentiles.
#'   }
#'
#' @importFrom lme4 ranef fixef
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename left_join mutate arrange slice n select
#' @export
extract_percentile_players <- function(object, data, percentile = c(0.1, 0.5, 0.9)) {

  ranef <- glmmTMB::ranef(object)$cond

  if ("runner_id" %in% names(ranef)) {

    ranef_runner <- ranef$runner_id |>
      tibble::as_tibble(rownames = "runner_id") |>
      dplyr::rename(ranef_runner = `(Intercept)`)

    data <- data |>
      dplyr::left_join(ranef_runner, by = "runner_id")

  } else {
    data$ranef_runner <- 0
  }

  if ("pitcher_id" %in% names(ranef)) {

    ranef_pitcher <- ranef$pitcher_id |>
      tibble::as_tibble(rownames = "pitcher_id") |>
      dplyr::rename(ranef_pitcher = `(Intercept)`)

    data <- data |>
      dplyr::left_join(ranef_pitcher, by = "pitcher_id")

  } else {
    data$ranef_pitcher <- 0
  }

  if ("catcher_id" %in% names(ranef)) {

    ranef_catcher <- ranef$catcher_id |>
      tibble::as_tibble(rownames = "catcher_id") |>
      dplyr::rename(ranef_catcher = `(Intercept)`)

    data <- data |>
      dplyr::left_join(ranef_catcher, by = "catcher_id")

  } else {
    data$ranef_catcher <- 0
  }

  sprint_speed_effect <- glmmTMB::fixef(object)$cond["sprint_speed_centered"] |>
    dplyr::coalesce(0)

  arm_strength_effect <- glmmTMB::fixef(object)$cond["arm_strength_centered"] |>
    dplyr::coalesce(0)

  percentile_runner <- data |>
    dplyr::mutate(runner_effect = ranef_runner + sprint_speed_effect * sprint_speed_centered) |>
    dplyr::arrange(runner_effect) |>
    dplyr::slice(round(percentile * dplyr::n())) |>
    dplyr::select(runner_id, sprint_speed_centered) |>
    dplyr::mutate(pct_runner = percentile, .before = 1)

  percentile_battery <- data |>
    dplyr::mutate(
      battery_effect = ranef_pitcher + ranef_catcher + arm_strength_effect * arm_strength_centered
    ) |>
    dplyr::arrange(-battery_effect) |>  # negative battery effects represent stronger batteries
    dplyr::slice(round(percentile * dplyr::n())) |>
    dplyr::select(pitcher_id, catcher_id, arm_strength_centered) |>
    dplyr::mutate(pct_battery = percentile, .before = 1)
  
  return(list(runner = percentile_runner, battery = percentile_battery))
}
