#' Load raw data
#'
#' Reads season-specific CSV files for plays, pitches, lead distances, player events, 
#' runner sprint speeds, and catcher arm strengths from a local directory structure.
#'
#' @details
#' This data-loading function filters and subsets data frames during ingestion:
#' \itemize{
#'   \item \strong{Pitch:} Selects only \code{play_id} and \code{description}.
#'   \item \strong{Lead Distance:} Deduplicates rows and isolates records where \code{base == "1st Base"}.
#'   \item \strong{Sprint Speed:} Selects player identifiers, metrics, and renames the name column.
#'   \item \strong{Arm Strength:} Selects player identifiers, arm metrics, and stolen base attempts.
#' }
#'
#' @param year Integer or character string indicating the season year to load (e.g., 2023).
#'
#' @return A named list containing six data frames:
#' \itemize{
#'   \item \code{event}: indexing and player info for each plate appearance
#'   \item \code{lead_distance}: deduplicated first base runner lead distances
#'   \item \code{pitch}: pitch descriptions with play identifiers
#'   \item \code{play}: complete play-by-play data frame
#'   \item \code{sprint_speed}: runner sprint speeds
#'   \item \code{arm_strength}: catcher arm strenghts
#' }
#'
#' @importFrom data.table fread
#' @importFrom glue glue
#' @importFrom dplyr select distinct filter
#' @export
read_data <- function(year) {

  play <- data.table::fread(glue::glue("input/data/play/{year}.csv"))

  pitch <- data.table::fread(glue::glue("input/data/pitch/{year}.csv")) |>
    dplyr::select(play_id, description)

  lead_distance_file <- glue::glue("input/data/lead_distance/{year}.csv")
  if (!file.exists(lead_distance_file)) {
    warning(glue::glue("{lead_distance_file} not found. Proceeding without raw lead distance."))
    lead_distance <- NULL
  } else {
    lead_distance <- data.table::fread(lead_distance_file) |>
      dplyr::distinct() |>                                # sometimes rows are duplicated
      dplyr::filter(base == "1st Base") |>
      dplyr::mutate(runner_id_statcast = as.character(runner_id)) |>    # may not match statsapi
      dplyr::select(play_id, runner_id_statcast, lead_distance)
  }

  event <- data.table::fread(glue::glue("input/data/event/{year}.csv"))

  sprint_speed <- data.table::fread(glue::glue("input/data/sprint_speed/{year}.csv")) |>
    dplyr::select(player_id, player_name = `last_name, first_name`, sprint_speed, competitive_runs)

  arm_strength <- data.table::fread(glue::glue("input/data/arm_strength/{year}.csv")) |>
    dplyr::select(player_id, player_name, arm_strength, sb_attempts)

  return(
    list(
      event = event,
      lead_distance = lead_distance,
      pitch = pitch,
      play = play,
      sprint_speed = sprint_speed,
      arm_strength = arm_strength
    )
  )
}