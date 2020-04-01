###########################################################################
# Author: Sebastian Carl                                                  #
# Purpose: Function for scraping team rosters and player IDs from NFL.com #
# Code Style Guide: styler::tidyverse_style()                                       #
###########################################################################

#' Get team rosters for multiple seasons and teams
#' 
#' Given team IDs and years, return a dataset with each
#' player the NFL has listed as part of the roster.
#' 
#' @param teamIds A string vector containing the IDs for NFL Team(s) (see details for more information)
#' @param season A 4-digit year associated with a given NFL season
#' @details To find team associated Team IDs use the \code{\link{nflteams}} dataset 
#' stored in this package!
#' @return Data frame where each individual row represents a player in 
#' the roster of the given team and season listed by the NFL
#' containing the following information:
#' \itemize{
#' \item{team.season}
#' \item{teamPlayers.displayName}
#' \item{teamPlayers.firstName}
#' \item{teamPlayers.middleName}
#' \item{teamPlayers.lastName}
#' \item{teamPlayers.suffix}
#' \item{teamPlayers.status}
#' \item{teamPlayers.position}
#' \item{teamPlayers.positionGroup}
#' \item{teamPlayers.nflId}
#' \item{teamPlayers.esbId}
#' \item{teamPlayers.gsisId}
#' \item{teamPlayers.birthDate}
#' \item{teamPlayers.homeTown}
#' \item{teamPlayers.collegeId}
#' \item{teamPlayers.collegeName}
#' \item{teamPlayers.jerseyNumber}
#' \item{teamPlayers.height}
#' \item{teamPlayers.weight}
# \item{teamPlayers.yearsOfExperience}
# \item{teamPlayers.teamAbbr}
# \item{teamPlayers.teamSeq}
# \item{teamPlayers.teamId}
# \item{teamPlayers.teamFullName}
#' \item{team.teamId}
#' \item{team.abbr}
#' \item{team.cityState}
#' \item{team.fullName}
#' \item{team.nick}
# \item{team.teamType}
#' \item{team.conferenceAbbr}
#' \item{team.divisionAbbr}
#' \item{teamPlayers.headshot_url}
#' \item{teamPlayers.profile_url}
#' @examples
#' # Roster for Steelers in 2018
#' get_rosters("3900", 2018) 
#' 
#' Roster for Steelers and Seahawks in 2016 & 2019
#' get_rosters(c("3900", "4600"), c("2016", "2019"))
#' @export

get_rosters <- function(teamIds, seasons) {
  rosters <-
    # The map function in purrr reports some unimportant warnings,
    # which are being suppressed
    suppressWarnings(
      purrr::pmap_df(
        # pmap needs a list of lists. It is generated as all combinations of
        # teamIDs and seasons by cross2 but needs to be transposed for pmap
        purrr::transpose(purrr::cross2(teamIds, seasons)),
        function(teamId, season) {
          grab_roster(teamId, season)
        }
      )
    ) %>%
    # for some older seasons the grab function returns less information about
    # teams and players (e.g. team.conferenceAbbr, team.divisionAbbr or
    # teamPlayers.suffix) which is the reason why we can't select the
    # variables by index on the one hand and need to catch errors while selecting
    # on the other hand (using matches)
    dplyr::select(
      tidyselect::matches(
        c(
          "team.season",
          "teamPlayers.displayName",
          "teamPlayers.firstName",
          "teamPlayers.middleName",
          "teamPlayers.lastName",
          "teamPlayers.suffix",
          "teamPlayers.status",
          "teamPlayers.position",
          "teamPlayers.positionGroup",
          "teamPlayers.nflId",
          "teamPlayers.esbId",
          "teamPlayers.gsisId",
          "teamPlayers.birthDate",
          "teamPlayers.homeTown",
          "teamPlayers.collegeId",
          "teamPlayers.collegeName",
          "teamPlayers.jerseyNumber",
          "teamPlayers.height",
          "teamPlayers.weight",

          # variables commented out because they appear to be unnecessary

          # "teamPlayers.yearsOfExperience",
          # "teamPlayers.teamAbbr",
          # "teamPlayers.teamSeq",
          # "teamPlayers.teamId",
          # "teamPlayers.teamFullName",

          "team.teamId",
          "team.abbr",
          "team.cityState",
          "team.fullName",
          "team.nick",

          # team type is 'TEAM' for 'normal teams' and 'PRO' for ProBowl teams
          # seems to be unnecessary to include it

          # "team.teamType",

          "team.conferenceAbbr",
          "team.divisionAbbr"
        )
      )
    ) %>%

    # Some Variables have empty observations. Replace them with 'NA'
    dplyr::mutate_all(na_if, "") %>%

    # add the two new variables headshot_url and profile_url
    # since this only works if the variables esbId and nflId are available
    # the expressions are wrapped in a try structure
    dplyr::mutate(
      try(expr = {
        # esbId can be used to create the static url for a players latest headshot
        teamPlayers.headshot_url <- glue::glue(
          "http://static.nfl.com/static/content/public/static/img/fantasy/transparent/200x200/{teamPlayers.esbId}.png"
        )
      }, silent = TRUE),
      try(expr = {
        # nflId can be used to create a players static profile url
        teamPlayers.profile_url <- glue::glue(
          "http://www.nfl.com/player/{tolower(teamPlayers.firstName)}{tolower(teamPlayers.lastName)}/{teamPlayers.nflId}/profile"
        )
      }, silent = TRUE)
    )
  return(rosters)
}

################################################################## 
# DO NOT EXPORT
#' Grab roster for a specific ID and season
#' 
#' This is a sub-function for the get_rosters function.
#' 
#' @param teamId (character string) Specifies the unique teamId 
#' for which the roster is scraped
#' @param season 4-digit year associated with a given NFL season

grab_roster <- function(teamId, season) {

  # If a combination of Team ID and season is passed for which there
  # is no data because the corresponding team did not exist in that season,
  # then the error must be caught so that the function does not abort.
  # Therefore the whole expression is packed into a tryCatch environment.
  # Since we want to return the dataframe 'roster_raw' we declare it as an
  # emtpty dataframe first to ensure we can return it
  roster_raw <- data.frame()

  tryCatch(
    expr = {
      roster_raw <-
        # Send a GET request for provided teamID and seaon
        httr::GET(url = glue::glue("http://www.nfl.com/feeds-rs/roster/{teamId}/{season}")) %>%
        # retrieve content as character vector
        httr::content(as = "text", encoding = "UTF-8") %>%
        # parse the character vetor
        jsonlite::fromJSON(flatten = TRUE) %>%
        # put it in a dataframe
        data.frame()

      # The message actually is shown after the work because if an error occurs
      # we want to show another message with the error function
      message(glue::glue("Scraping Roster of TeamID {teamId} for the '{season}' Season..."))
    },
    error = function(e) {
      # catching the error for non existing data with the message
      message(glue::glue("The Team with the ID '{teamId}' doesn't exist for the '{season}' season!"))
    },
    warning = function(w) {
      # Experience shows that the expression will not lead to a warning,
      # so it is not used here.
    },
    finally = {
      # Before exiting the tryCatch structure we may want to wait a few seconds
      # to spare the NFL server a little bit. FOr maximum performance it is
      # commented out righht now

      # Sys.sleep(0.1)

      return(roster_raw)
    }
  )
}