#' Update game state
#'
#' Modifies select attributes of the game state string by deconstructing 
#' the original string, replacing specified values, and reconstructing the updated identifier.
#'
#' @details
#' Arguments left as \code{NA} retain their original values from the input 
#' \code{state} string using \code{dplyr::coalesce}.
#'
#' @param state Character string formatted as "{bases}_{outs}_{balls}{strikes}_{disengagements}" 
#' (e.g., "100_1_21_0").
#' @param new_bases Optional character string to replace current base occupancy. Defaults to \code{NA}.
#' @param new_outs Optional integer to replace current out count. Defaults to \code{NA}.
#' @param new_balls Optional integer to replace current ball count. Defaults to \code{NA}.
#' @param new_strikes Optional integer to replace current strike count. Defaults to \code{NA}.
#' @param new_disengagements Optional integer to replace current disengagement count. Defaults to \code{NA}.
#'
#' @return An updated character string formatted as a standardized game state.
#'
#' @importFrom dplyr mutate coalesce
#' @export
update_state <- function(state,
                         new_first = NA,
                         new_bases = NA,
                         new_outs = NA,
                         new_balls = NA,
                         new_strikes = NA,
                         new_disengagements = NA) {
  new_state <- state |>
    deconstruct_state() |>
    dplyr::mutate(
      first = dplyr::coalesce(new_first, first),
      bases = dplyr::coalesce(new_bases, bases),
      outs = dplyr::coalesce(new_outs, outs),
      balls = dplyr::coalesce(new_balls, balls),
      strikes = dplyr::coalesce(new_strikes, strikes),
      disengagements = dplyr::coalesce(new_disengagements, disengagements)
    ) |>
    with(construct_state(first, bases, outs, balls, strikes, disengagements))

  return(new_state)
}