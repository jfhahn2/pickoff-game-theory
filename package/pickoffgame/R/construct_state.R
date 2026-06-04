#' Construct game state
#'
#' Combines base occupancy, outs, balls, strikes, and optionally disengagements 
#' into a unified string identifier for state-transition modeling.
#'
#' @param bases Character string representing base occupancy (e.g., "100" for a runner on first base).
#' @param outs Integer or character count of outs (0, 1, or 2).
#' @param balls Integer or character count of balls (0 to 3).
#' @param strikes Integer or character count of strikes (0 to 2).
#' @param disengagements Optional integer or character count of prior pitcher disengagements (0 to 2). Defaults to `NULL`.
#'
#' @return A character string formatted as "bases_outs_ballsstrikes_disengagements" 
#' (e.g., "100_1_21_0"). If `disengagements` is omitted or `NULL`, the trailing 
#' underscore and disengagement count are excluded from the output string.
#'
#' @importFrom glue glue
#' @export
construct_state <- function(bases, outs, balls, strikes, disengagements = NULL) {

  if (!is.null(disengagements)) {
    disengagement_string <- paste0("_", disengagements)
  } else {
    disengagement_string <- ""
  }

  state <- glue::glue("{bases}_{outs}_{balls}{strikes}{disengagement_string}")

  return(state)
}