#' Deconstruct game state
#'
#' Extracts bases, outs, balls, strikes and disengagements from game state string
#'
#' @param state Character string formatted as "{bases}_{outs}_{balls}{strikes}_{disengagements}"
#' (e.g., "100_1_21_0").
#'
#' @return A single-row tibble with the following columns:
#' \itemize{
#'   \item \code{bases}: Character string representing base occupancy (indices 1–3).
#'   \item \code{outs}: Integer count of outs (index 5).
#'   \item \code{balls}: Integer count of balls (index 7).
#'   \item \code{strikes}: Integer count of strikes (index 8).
#'   \item \code{disengagements}: Integer count of prior disengagements (index 10). 
#'   Returns \code{NA} if the input string does not contain a tenth character.
#' }
#'
#' @importFrom tibble tibble
#' @export
deconstruct_state <- function(state) {
  first <- substring(state, 1, 1)
  bases <- substring(state, 3, 5)
  outs <- as.integer(substring(state, 7, 7))
  balls <- as.integer(substring(state, 9, 9))
  strikes <- as.integer(substring(state, 10, 10))
  disengagements <- as.integer(substring(state, 12, 12))
  return(
    tibble::tibble(
      first = first,
      bases = bases,
      outs = outs,
      balls = balls,
      strikes = strikes,
      disengagements = disengagements
    )
  )
}