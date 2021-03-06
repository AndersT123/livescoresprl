#' Get request for given match day
#'
#' @param key a string with your rapid api key
#' @param matchday a string with the mathday "1"-"38"
#' @return a httr::GET() response.
#' @export
get_match_request <- function(matchday, key) {
  stopifnot(is.character(matchday), length(matchday) == 1)
  url = "https://heisenbug-premier-league-live-scores-v1.p.rapidapi.com/api/premierleague"
  headers = c('x-rapidapi-host' = "heisenbug-premier-league-live-scores-v1.p.rapidapi.com",
              'x-rapidapi-key' = key)
  r <- httr::GET(url,
           query = list(
             "matchday" = matchday,
             "season" = "2020-21"
           ),
           httr::add_headers(headers))
  r
}

get_team_attr <- function(content, team, attr) {
  purrr::map_depth(content, 2,
                   function(x) x[[team]][[attr]]) %>%
    purrr::flatten()
}
get_match_attr <- function(content, attr) {
  purrr::map_depth(content, 2,
                   function(x) x[[attr]]) %>%
    purrr::flatten()
}

#' Retrieve home teams for given matchday
#'
#' @param content the value from a httr::content() call to a httr::GET() response.
#' @return a character vector of team names.
#' @export
get_home_teams <- function(content) {
  get_team_attr(content, "team1", "teamName") %>%
    purrr::flatten_chr()
}

#' Title
#'
#' Description
#'
#' @inheritParams get_home_teams
#' @export
get_away_teams <- function(content) {
  get_team_attr(content, "team2", "teamName") %>%
    purrr::flatten_chr()
}

#' Title
#'
#' Description
#'
#' @inheritParams get_home_teams
#' @export
get_home_scores <- function(content) {
  get_team_attr(content, "team1", "teamScore") %>%
    purrr::flatten_int()
}

#' Title
#'
#' Description
#'
#' @inheritParams get_home_teams
#' @export
get_away_scores <- function(content) {
  get_team_attr(content, "team1", "teamScore") %>%
    purrr::flatten_int()
}

#' Title
#'
#' Description
#'
#' @inheritParams get_home_teams
#' @export
get_match_timestatus <- function(content) {
  get_match_attr(content, "time") %>%
    purrr::flatten_chr()
}

#' Title
#'
#' Description
#'
#' @inheritParams get_home_teams
#' @param as_datetime DO NOT USE YET.
#' @export
get_match_kickoff <- function(content, as_datetime = FALSE) {
  out <- get_match_attr(content, "when") %>%
    purrr::flatten_chr()
  if (as_datetime) {
    # Return as POSIXt
  }
  out
}
