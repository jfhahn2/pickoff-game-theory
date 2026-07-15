#' Determine the next reduced game state from runner outcome based on manual rules
#'
#' Applies a specific runner outcome to a reduced pre-state string, updating 
#' base occupancy and out counts to determine the resulting reduced post-state.
#'
#' @details
#' This function transitions states for plays involving a runner on first base:
#' \itemize{
#'   \item \code{"PO+"} (Successful Pickoff): Bases clear (\code{"000"}), outs increment by 1.
#'   \item \code{"PO-"} (Failed Pickoff): Runner remains on first (\code{"100"}), outs unchanged.
#'   \item \code{"SB+"} (Stolen Base): Runner advances to second (\code{"010"}), outs unchanged.
#'   \item \code{"SB-"} (Caught Stealing): Bases clear (\code{"000"}), outs increment by 1.
#' }
#' If the play results in a third out, the function returns a terminal state identifier \code{"0"}.
#'
#' @param pre_state_reduced Character string representing the initial reduced state 
#' formatted as "bases_outs_ballsstrikes" (e.g., "100_1_21").
#' @param runner_outcome Character string indicating the outcome: \code{"PO+"} (successful pickoff), 
#' \code{"PO-"} (failed pickoff), \code{"SB+"} (stolen base), or \code{"SB-"} (caught stealing).
#'
#' @return A character string representing the updated reduced post-state, or \code{"0"} 
#' if the out count reaches 3.
#'
#' @importFrom dplyr mutate case_when pull
#' @export
apply_runner_outcome <- function(pre_state_reduced, runner_outcome) {
  deconstruct_state(pre_state_reduced) |>
    dplyr::mutate(
      bases = dplyr::case_when(
        runner_outcome == "PO+" ~ "000",
        runner_outcome == "PO-" ~ "100",
        runner_outcome == "SB+" ~ "010",
        runner_outcome == "SB-" ~ "000",
        runner_outcome == "GI" ~ "100"
      ),
      outs = outs + (runner_outcome %in% c("PO+", "SB-")),
      post_state_reduced = ifelse(outs == 3, "0", construct_state(bases, outs, balls, strikes))
    ) |>
    dplyr::pull(post_state_reduced)
}
