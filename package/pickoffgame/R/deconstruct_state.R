#' Deconstruct game state
#'
#' Extracts bases, outs, balls, strikes and disengagements from game state string
#'
#' @param state Character string formatted as "{bases}_{outs}_{balls}{strikes}_{disengagements}_{first}"
#' (e.g., "100_1_21_0_0").
#'
#' @return A single-row tibble with the following columns:
#' \itemize{
#'   \item \code{bases}: Character string representing base occupancy (indices 1–3).
#'   \item \code{outs}: Integer count of outs (index 5).
#'   \item \code{balls}: Integer count of balls (index 7).
#'   \item \code{strikes}: Integer count of strikes (index 8).
#'   \item \code{disengagements}: Integer count of prior disengagements (index 10). 
#'   Returns \code{NA} if the input string does not contain a tenth character.
#'   \item \code{first}: Binary 0/1 indicator of first pitch of play (index 12). 
#'   Returns \code{NA} if the input string does not contain a twelfth character.
#' }
#'
#' @importFrom tibble tibble
#' @export
deconstruct_state <- function(state) {
  bases <- substring(state, 1, 3)
  outs <- as.integer(substring(state, 5, 5))
  balls <- as.integer(substring(state, 7, 7))
  strikes <- as.integer(substring(state, 8, 8))
  disengagements <- as.integer(substring(state, 10, 10))
  first <- as.integer(substring(state, 12, 12))
  return(
    tibble::tibble(
      bases = bases,
      outs = outs,
      balls = balls,
      strikes = strikes,
      disengagements = disengagements,
      first = first
    )
  )
}