#  firstapiR_http.R version 2.0.0===============================================
#  Contains functions that retrieve data from the FIRST API server and format
#  the data as either XML, JSON, or as R data frames.


# Server URLs and other parameters
.staging_url <- "https://frc-staging-api.firstinspires.org"
.production_url <- "https://frc-api.firstinspires.org"
.first_http_api_version <- "v2.0"
.package_version <- "2.0.0"
.user_agent_name <- paste("firstapiR: Version", .package_version)
.default_season <- as.integer(format(Sys.Date(), "%Y"))



#  GetSession() ================================================================
#' Create a firstapiR session
#'
#' Every firstapiR function requires a Session object as its first parameter.
#'
#' The Session object is an R list that contains the FIRST API username and
#' authorization key, season, format, and a boolean value that specifies whether
#' to use the staging server instead of the production server.
#'
#' The \code{key} argument may be set to the value "key". If this is done, the
#' firstapiR functions will skip the HTTP request and will extract example data
#' from the R/sysdata.rda file, which is included with the firstapiR package.
#' This function is for testing and demonstrations when no internet connection
#' or valid authorization key is available. Example data frames returned by
#' firstapiR functions will have their \emph{local_test_data} attribute set to
#' \code{TRUE} and the \emph{time_downloaded} attribute will be set to the date
#' and time that the example data was downloaded from the server and stored in
#' the R/sysdata.rda file.
#'
#' Throws an error if \code{season}, \code{format}, or \code{staging} arguments
#' are incorrect.
#'
#' @param username A character vector containing the username assigned by FIRST.
#' @param key A character vector containing the authorization key assigned by
#'   FIRST, or the value "key".
#' @param season An integer vector containing the 4-digit year. Must be equal to
#'   or less than the current season and greater than or equal to 2015.
#'   Optional: defaults to the current year.
#' @param format A character vector that specifies the data format that will be
#'   returned by firstapiR functions. Can be "json", "data.frame", or "xml"
#'   (case insensitive). Optional: defaults to "data.frame".
#' @param staging A logical vector. If set to \code{TRUE}, firstapiR uses the
#'   staging URL. Optional: defaults to \code{FALSE}.
#'
#' @return A Session object containing all GetSession parameters.
#'   The class attribute is set to c("list", "Session")
#'
#' @export
#'
#' @examples
#' sn <- GetSession("myUserName", "myAuthorizationKey")
#' sn <- GetSession("myUserName", "myAuthorizationKey", season = 2015)
#' sn$format <- "xml"
GetSession <- function(username, key,
                  season = .default_season,
                  format = "data.frame",
                  staging = FALSE){
  # Check for invalid arguments
  if((season < 2015) || (season > .default_season + 1))
    stop("season must be an integer between 2015 and next year")
  if(!(tolower(format) %in% c("data.frame", "xml", "json")))
    stop("format must be 'data.frame', 'xml', or 'json'")
  if(!is.logical(staging))
    stop("staging must be either TRUE or FALSE")

  # Build Session
  session <- list(username = username,
                  key = key,
                  staging = staging,
                  season = season,
                  format = format)
  class(session) <- append(class(session), "Session")

  return(session)
}


#  GetServerStatus() ===========================================================
#' Get the status of the FIRST API server
#'
#' See the \emph{API Index} section of the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for more details.
#'
#' The FIRST API URL format is:
#'
#' \code{https://frc-api.firstinspires.org/v2.0}
#'
#' @param session A Session object created with \code{GetSession()}.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'  XML::XMLDocument object, or a data.frame with class set to
#'  c("data.frame, "Status").
#'
#' @section Columns:
#' \enumerate{
#'  \item \emph{name}: character
#'  \item \emph{version}: double
#'  \item \emph{status}: character}
#'
#' @section Attributes:
#' \itemize{
#'  \item \emph{url}: Character vector containing URL submitted to FIRST API
#'    server.
#'  \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'    R/sysdata.rda file.
#'  \item \emph{local_url}: Character vector containing URL used to download
#'    local data.
#'  \item \emph{time_downloaded}: Character vector containing the local
#'    system time that the object was downladed from the FIRST API server.
#'    Formatted an an http date and time string.
#'  \item \emph{last_modified}: Character vector containing the date and time
#'    that the data was last modified by the FIRST API server.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' status <- GetServerStatus(sn)
GetServerStatus <- function(session) {
  status <- .GetHTTP(session, "status")

  # Skip rest of function for XML or JSON results
  if(session$format != "data.frame") return(status)

  class(status) <- append(class(status), "Status")
  return(status)
}


#  GetSeason() =================================================================
#' Get high-level information for an FRC season
#'
#' Returns information for the season specified in the session list (see
#' documentation for the GetSession function for additional details.)
#'
#' See the \emph{Season Summary} section of the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for more details.
#'
#' The FIRST API URL format is:
#'
#' \code{https://frc-api.firstinspires.org/v2.0/season}
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param mod_since A character vector containing an HTTP formatted date and
#'   time. Returns \code{NA} if no changes have been made to the requested data
#'   since the date and time provided. Optional.
#' @param only_mod_since A character vector containing an HTTP formatted date
#'   and time. This function only returns data that has changed since the date
#'   and time provided. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame, "Season"). Returns a logical vector of length one with value
#'   \code{NA} if data is unchanged since date and time passed in arguments
#'   \code{mod_since} or \code{only_mod_since}.
#'
#' @section Columns:
#'   \enumerate{
#'     \item \emph{eventCount}: integer
#'     \item \emph{gameName}: factor
#'     \item \emph{kickoff}: factor
#'     \item \emph{rookieStart}: integer
#'     \item \emph{teamCount}: integer
#'     \item \emph{FRCChampionships.name}: character
#'     \item \emph{FRCChampionships.startDate}: character
#'     \item \emph{FRCChampionships.location}: character}
#'
#' @section Attributes:
#'   \itemize{
#'     \item \emph{url}: Character vector containing URL submitted to FIRST API
#'       server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.
#'     \item \emph{local_url}: Character vector containing URL used to download
#'       local data.
#'     \item \emph{time_downloaded}: Character vector containing the local
#'       system time that the object was downladed from the FIRST API server.
#'       Formatted an an http date and time string.
#'     \item \emph{last_modified}: Character vector containing the date and time
#'       that the data was last modified by the FIRST API server.
#'     \item \emph{mod_since}: Character vector containing the value passed to
#'       the \code{mod_since argument}, or NULL if no argument was passed.
#'       Formatted as an http date and time string.
#'     \item \emph{only_mod_since}: Character vector containing the value passed
#'       to the \code{only_mod_since} argument, or NULL if no argument was
#'       passed. Formatted as an http date and time string.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key", season = 2015, staging = TRUE)
#' summary <- GetSeason(sn)
GetSeason <- function(session, mod_since = NULL, only_mod_since = NULL) {
  season <- .GetHTTP(session, "", mod_since, only_mod_since)

  # Skip rest of function for empty, XML, or JSON results
  if(is.na(season) || session$format != "data.frame") return(season)

  class(season) <- append(class(season), "Season")
  return(season)
}


#  GetDistricts() ==============================================================
#' Get a list of FIRST districts
#'
#' This function returns a list of all current districs, including their titles
#' and codes. District codes are used as parameters for several other FIRST API
#' functions.
#'
#' See the \emph{District Listings} section of the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for more details.
#'
#' The FIRST API URL format is:
#'
#' \code{https://frc-api.firstinspires.org/v2.0/season/districts}
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param mod_since A character vector containing an HTTP formatted date and
#'   time. Returns \code{NA} if no changes have been made to the requested data
#'   since the date and time provided. Optional
#' @param only_mod_since A character vector containing an HTTP formatted date
#'   and time. This function only returns data that has changed since the date
#'   and time provided. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to c("data.frame,
#'   "Districts"). Returns a logical vector of length one with value \code{NA}
#'   if data is unchanged since date and time passed in arguments
#'   \code{mod_since} or \code{only_mod_since}.
#'
#' @section Columns:
#'   \enumerate{
#'     \item \emph{code}: character
#'     \item \emph{name}: character
#'     \item \emph{districtCount}: integer}
#'
#' @section Attributes:
#'   \itemize{
#'     \item \emph{url}: Character vector containing URL submitted to FIRST API
#'       server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.
#'     \item \emph{local_url}: Character vector containing URL used to download
#'       local data.
#'     \item \emph{time_downloaded}: Character vector containing the local
#'       system time that the object was downladed from the FIRST API server.
#'       Formatted an an http date and time string.
#'     \item \emph{last_modified}: Character vector containing the date and time
#'       that the data was last modified by the FIRST API server.
#'     \item \emph{mod_since}: Character vector containing the value passed to
#'       the \code{mod_since argument}, or NULL if no argument was passed.
#'       Formatted as an http date and time string.
#'     \item \emph{only_mod_since}: Character vector containing the value passed
#'       to the \code{only_mod_since} argument, or NULL if no argument was
#'       passed. Formatted as an http date and time string.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' districts <- GetDistricts(sn)
GetDistricts <- function(session, mod_since = NULL, only_mod_since = NULL) {
  url <- "districts"
  districts <- .GetHTTP(session, url, mod_since, only_mod_since)

  # Skip rest of function for empty, XML, or JSON results
  if(is.na(districts) || session$format != "data.frame") return(districts)

  # Shorten the column names to reduce amount of typing required.
  names(districts) <- .TrimColNames(names(districts))

  class(districts) <- append(class(districts), "Districts")
  return(districts)
}


#  GetEvents() =================================================================
#' Get information about FRC events
#'
#' See the \emph{Event Listings} section of the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for more details.
#'
#' \code{GetEvents} will accept either the \code{team} or \code{district}
#' parameters, neither parameter, or both parameters. If neither \code{team} nor
#' \code{district} are specified, \code{GetEvents} returns all FRC events for
#' the competition season. If \code{team} is specified, the results are filtered
#' to only the events in which the FRC team participated. Similarly, if
#' \code{district} is specified, the results are filtered to only the events
#' that occurred within the specified district. If \code{exclude_district} is
#' set to TRUE, then only non-district events are returned. The \code{district}
#' and \code{exclude_district} events may not be specified at the same time.
#'
#' Throws an error if \code{team} is specified and any other arguments are
#' specified, or if both the \code{district} and \code{exclude_district}
#' arguments are specified.
#'
#' The FIRST API URL format is:
#'
#'   \code{https://frc-api.firstinspires.org/v2.0/season/events?
#'   teamNumber=team&districtCode=district&excludeDistrict=district}
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param event A character vector containing a FIRST API event code. If event
#'   is specified, \code{GetEvents()} will return results only for the specified
#'   event. Optional.
#' @param team An integer vector containing a team number. Optional
#' @param district A character vector containing the FIRST API district code
#'   (see \code{GetDistricts()}). If \code{district} is specified,
#'   \code{GetTeams()} will filter results to only the events in the specified
#'   district.
#' @param exclude_district A logical vector. If set to \code{TRUE}, district
#'   events are excluded from results. Optional.
#' @param mod_since A character vector containing an HTTP formatted date and
#'   time. Returns \code{NA} if no changes have been made to the requested data
#'   since the date and time provided. Optional.
#' @param only_mod_since A character vector containing an HTTP formatted date
#'   and time. This function only returns data that has changed since the date
#'   and time provided. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to c("data.frame,
#'   "Events"). Returns a logical vector of length one with value \code{NA} if
#'   data is unchanged since date and time passed in arguments \code{mod_since}
#'   or \code{only_mod_since}.
#'
#' @section Columns:
#'   \enumerate{
#'     \item \emph{code}: character
#'     \item \emph{divisionCode}: character
#'     \item \emph{name}: character
#'     \item \emph{type}: factor ('Regional', 'DistrictEvent',
#'        'DistrictChampionship', 'ChampionshipSubdivision',
#'        'ChampionshipDivision', 'Championship', 'Offseason')
#'     \item \emph{districtCode}: factor ('CHM', 'FIM', 'IN', 'MAR', 'NC',
#'       'PCH', 'PNW')
#'     \item \emph{venue}: character
#'     \item \emph{city}: character
#'     \item \emph{stateprov}: factor
#'     \item \emph{country}: factor
#'     \item \emph{timezone}: factor
#'     \item \emph{dateStart}: character
#'     \item \emph{dateEnd}: character
#'     \item \emph{eventCount}: integer}
#'
#' @section Attributes:
#'   \itemize{
#'     \item \emph{url}: Character vector containing URL submitted to FIRST API
#'       server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.
#'     \item \emph{local_url}: Character vector containing URL used to download
#'       local data.
#'     \item \emph{time_downloaded}: Character vector containing the local
#'       system time that the object was downladed from the FIRST API server.
#'       Formatted an an http date and time string.
#'     \item \emph{last_modified}: Character vector containing the date and time
#'       that the data was last modified by the FIRST API server.
#'     \item \emph{mod_since}: Character vector containing the value passed to
#'       the \code{mod_since argument}, or NULL if no argument was passed.
#'       Formatted as an HTTP date and time string.
#'     \item \emph{only_mod_since}: Character vector containing the value passed
#'       to the \code{only_mod_since} argument, or NULL if no argument was
#'       passed. Formatted as an HTTP date and time string.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' frc_data <- GetEvents(sn, team = 5803)
#' frc_data <- GetEvents(sn, district = 'PNW')
#' frc_data <- GetEvents(sn, team = 360, exclude_district = TRUE)
GetEvents <- function(session, event = NULL, team = NULL,
                      district = NULL, exclude_district = NULL,
                      mod_since = NULL, only_mod_since = NULL) {
  # Check for unallowed combinations of arguments.
  if(!is.null(event) && (!is.null(team) || !is.null(district) ||
                              !is.null(exclude_district)))
    stop("If you specify an event, you cannot specify any other arguments.")
  if(!is.null(district) && !is.null(exclude_district))
    stop("You cannot specify both the district and exclude_district arguments.")

  event_args <- list(eventCode = event, teamNumber = team,
                    districtCode = district, excludeDistrict = exclude_district)

  url <- .AddHTTPArgs("events", event_args)

  # Send HTTP request
  events <- .GetHTTP(session, url, mod_since, only_mod_since)

  # Skip rest of function for empty, XML, or JSON results
  if(is.na(events) || session$format != "data.frame") return(events)

  # Shorten the column names to reduce amount of typing required.
  names(events) <- .TrimColNames(names(events))

  # Convert categorical coluns to factor data types.
  events <- .FactorColumns(events, c("type", "districtCode", "stateprov",
                                     "country", "timezone"))

  class(events) <- append(class(events), "Events")
  return(events)
}


#  GetTeams() ==================================================================
#' Get details on FRC teams
#'
#' Provides lists of FRC teams for specified events, districts, and states. With
#' no parameters (except for \code{session}), \code{GetTeams} will provide a
#' list of all FRC teams.
#'
#' Because the length of the \code{GetTeams} response can be several thousand
#' lines long, the FIRST API server will break up its response into several
#' pages when the number of teams in the response exceeds 65. For the data frame
#' format, \code{GetTeams} will send a request to the FIRST API server and
#' determine from the first response whether additional HTTP requests are
#' necessary to retrieve all requested data. \code{GetTeams} will then merge all
#' responses into a single data frame. For XML and JSON formats, the user will
#' have to call \code{GetTeams} for each page of data, specifying the page with
#' the \code{page} argument.
#'
#' See the \emph{Team Listings} section of the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for more details.
#'
#' The FIRST API URL format is:
#'
#' \code{https://frc-api.firstinspires.org/v2.0/season/teams&eventCode=event
#' ?districtCode=district?state=state?page=2}
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param team An integer vector containing a team number. Optional.
#' @param event Character A FIRST API event code (see \code{GetEvents()}). If
#'   event is specified, \code{GetTeams()} will filter results to all teams
#'   particpating in the specified event. Optional.
#' @param district A character vector containing a FIRST API district code
#'   (see \code{GetDistricts()}). If specified, the FIRST API server will filter
#'   the response to only the teams in the specified district. Optional.
#' @param state A character vector containing a state name, spelled out entirely
#'   (i.e., 'Idaho', \emph{not} 'ID'). If state is specified, \code{GetTeams()}
#'   will filter results to all teams in the specified state. Optional.
#' @param page An integer vector that specifyies which page of results should be
#'   returned. Optional. Use only for XML or JSON formats.
#' @param mod_since A character vector containing an HTTP formatted date and
#'   time. Returns \code{NA} if no changes have been made to the requested data
#'   since the date and time provided. Optional.
#' @param only_mod_since A character vector containing an HTTP formatted date
#'   and time. This function only returns data that has changed since the date
#'   and time provided. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to c("data.frame,
#'   "Teams"). Returns a logical vector of length one with value \code{NA} if
#'   data is unchanged since date and time passed in arguments \code{mod_since}
#'   or \code{only_mod_since}.
#'
#' @section Columns:
#'   \enumerate{
#'     \item \emph{team}: character
#'     \item \emph{nameFull}: character
#'     \item \emph{nameShort}: character
#'     \item \emph{city}: character
#'     \item \emph{stateProv}: factor
#'     \item \emph{country}: factor
#'     \item \emph{website}: character
#'     \item \emph{rookieYear}: integer
#'     \item \emph{robotName}: character
#'     \item \emph{districtCode}: factor
#'     \item \emph{teamCountTotal}: integer
#'     \item \emph{teamCountPage}: integer
#'     \item \emph{pageCurrent}: integer
#'     \item \emph{pageTotal}: integer}
#'
#' @section Attributes:
#'   \itemize{
#'     \item \emph{url}: Character vector containing URL submitted to FIRST API
#'       server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.
#'     \item \emph{local_url}: Character vector containing URL used to download
#'       local data.
#'     \item \emph{time_downloaded}: Character vector containing the local
#'       system time that the object was downladed from the FIRST API server.
#'       Formatted an an http date and time string.
#'     \item \emph{last_modified}: Character vector containing the date and time
#'       that the data was last modified by the FIRST API server.
#'     \item \emph{mod_since}: Character vector containing the value passed to
#'       the \code{mod_since argument}, or NULL if no argument was passed.
#'       Formatted as an http date and time string.
#'     \item \emph{only_mod_since}: Character vector containing the value passed
#'       to the \code{only_mod_since} argument, or NULL if no argument was
#'       passed. Formatted as an http date and time string.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetTeams(sn, state = "California")
#' GetTeams(sn, district = "FIM")
#' GetTeams(sn, event = "CMP-CARVER")
GetTeams <- function (session, team = NULL, event = NULL, district = NULL,
                      state = NULL, page = NULL, mod_since = NULL,
                      only_mod_since = NULL) {
  # Check for unallowed combinations of arguments.
  if(!is.null(team) && (!is.null(event) || !is.null(district) ||
                         !is.null(state)))
    stop("If you specify a team, you cannot specify any other arguments")
  if(session$format == "data.frame" && !is.null(page)) {
    page <- NULL
    warning("Do not specify GetTeams page argument for data frame format")
  }

  # Assemble URL
  team_args <- list(teamNumber = team, eventCode = event,
                    districtCode = district, state = state, page = page)
  url <- .AddHTTPArgs("teams", team_args)

  # FIRST teams API can return multiple pages, and each page requires a separate
  # HTTP request, so results will be stored in a list containing one list item
  # for each page.
  teams <- list()

  # Send HTTP request and get first page of data.
  teams[[1]] <- .GetHTTP(session, url, mod_since, only_mod_since)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(teams) || session$format != "data.frame") return(teams[[1]])

  # Get total number of pages
  pages <- teams[[1]]$pageTotal[1]

  # If results consist of more than one page, send an HTTP request to get each
  # page, storing each page of results in the list.
  if(pages > 1) {
    team_attr <- attributes(teams[[1]])
    for(page in 2:pages) {
      team_args$page <- page
      url <- .AddHTTPArgs("teams", team_args)
      teams[[page]] = .GetHTTP(session, url)
    }

    # For data frames, merge all pages into one data frame.
    if(session$format == "data.frame") {
      teams_df <- teams[[1]]
      for(page in 2:pages) {
        teams_df <- merge(teams_df, teams[[page]], all = TRUE)
      }
      teams <- teams_df

      # Replace attributes that were stripped due to merging
      attr(teams, "url") <- team_attr$url
      attr(teams, "local_test_data") <- team_attr$local_test_data
      attr(teams, "local_url") <- team_attr$local_url
      attr(teams, "time_downloaded") <- team_attr$time_downloaded
      attr(teams, "last_modified") <- team_attr$last_modified
      attr(teams, "mod_since") <- team_attr$mod_since
      attr(teams, "only_mod_since") <- team_attr$only_mod_since
    }
  } else
    teams <- teams[[1]]

  # Shorten the column names to reduce amount of typing required.
  names(teams) <- .TrimColNames(names(teams))
  names(teams)[names(teams == "teamNumber")] <- "team"

  # Convert categorical coluns to factor data types.
  teams <- .FactorColumns(teams, c("districtCode", "stateProv", "country"))

  class(teams) <- append(class(teams), "Teams")
  return(teams)
}


#  GetSchedule() ===============================================================
#' Get the match schedule for a specific event
#'
#' Returns either the qualification schedule or the playoff schedule, based on
#' the value of the \code{level} argument. The \code{start} and \code{end}
#' arguments allow filtering of results to specific matches.
#'
#' The data frame returned by \code{GetSchedule()} is in team shape, i.e., each
#' row contains data for a single team and there are six rows per match. Use
#' \code{ToAllianceShape()} or \code{ToMatchShape} to convert the data frame to
#' a three-teams-per-row shape or a six-teams-per-row shape.
#'
#' See the \emph{Event Schedule} section of the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for more details.
#'
#' The FIRST API URL format is:
#'
#' \code{https://frc-api.firstinspires.org/v2.0/season/schedule/event?
#' tournamentLevel=level&teamNumber=team&start=start&end=end}
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param event A character vector containing a FIRST API event code
#'   (see \code{GetEvents}).
#' @param level A character vector containing either \emph{"qual"} or
#'   \emph{"playoff"}. Defaults to \emph{"qual"}. Optional.
#' @param team An integer vector containing a team number. Optional.
#' @param start An integer vector containing the earliest match to return.
#'   Optional.
#' @param end An integer vector containing the latest match to return. Optional.
#' @param mod_since A character vector containing an HTTP formatted date and
#'   time. Returns \code{NA} if no changes have been made to the requested data
#'   since the date and time provided. Optional.
#' @param only_mod_since A character vector containing an HTTP formatted date
#'   and time. This function only returns data that has changed since the date
#'   and time provided. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to c("data.frame,
#'   "Schedule"). Returns a logical vector of length one with value \code{NA} if
#'   data is unchanged since date and time passed in arguments \code{mod_since}
#'   or \code{only_mod_since}.
#'
#' @section Columns:
#'   \enumerate{
#'     \item \emph{match}: integer
#'     \item \emph{description}: character
#'     \item \emph{level}: factor
#'     \item \emph{field}: character
#'     \item \emph{start}: character
#'     \item \emph{team}: factor
#'     \item \emph{alliance}: factor (Red, Blue)
#'     \item \emph{station}: factor (red1, red2, red3, blue1, blue2, blue3)
#'     \item \emph{surrogate}: logical}
#'
#' @section Attributes:
#'  \enumerate{
#'     \item \emph{url}: Character vector containing URL submitted to FIRST API
#'       server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.
#'     \item \emph{local_url}: Character vector containing URL used to download
#'       local data.
#'     \item \emph{shape}: Character vector that specifies the row and column
#'       arrangement of the data frame. Initially set to \emph{team},
#'       indicating there is one team per row and six rows per match. The
#'       functions \code{ToAllianceShape()} and \code{ToMatchShape} will reshape
#'       the data frame to have either three teams per row or six teams per row
#'       and will set the \emph{shape} attribute to \emph{alliance} or
#'       \emph{match}.
#'     \item \emph{time_downloaded}: Character vector containing the local
#'       system time that the object was downladed from the FIRST API server.
#'       Formatted an an http date and time string.
#'     \item \emph{last_modified}: Character vector containing the date and time
#'       that the data was last modified by the FIRST API server.
#'     \item \emph{mod_since}: Character vector containing the value passed to
#'       the \code{mod_since argument}, or NULL if no argument was passed.
#'       Formatted as an http date and time string.
#'     \item \emph{only_mod_since}: Character vector containing the value passed
#'       to the \code{only_mod_since} argument, or NULL if no argument was
#'       passed. Formatted as an http date and time string.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetSchedule(sn, "PNCMP")
#' GetSchedule(sn, "PNCMP", start=5, end=10)
#' GetSchedule(sn, "WAAMV", level='playoff')
#' GetSchedule(sn, "PNCMP", team=4911, end=25)
GetSchedule <- function (session, event, level = "qual", team = NULL,
                         start = NULL, end = NULL, mod_since = NULL,
                         only_mod_since = NULL) {
  # Build URL
  sched_args <- list(tournamentLevel = level, teamNumber = team, start = start,
                     end = end)
  url <- .AddHTTPArgs(paste("schedule", event, sep = "/"), sched_args)

  # Send HTTP request
  sched <- .GetHTTP(session, url, mod_since, only_mod_since)

  # Skip remainder of function for empty results or XML, and JSON formats.
  if(is.na(sched) || session$format != "data.frame") return(sched)

  # Delete 'Schedule.' from the beginning of column names.
  names(sched) <- .TrimColNames(names(sched))

  # The FIRST API returns nested schedule data nested data. The remainder of
  # this function is necessary to either extract the nested data into new
  # columns or to add rows so that the scheule data can be saved as csv data.

  # Add columns for team number, station, and surrogate
  sched["team"] <- vector(mode = "integer", length = nrow(sched))
  sched["alliance"] <- vector(mode = "character", length = nrow(sched))
  sched["station"] <- vector(mode = "character", length = nrow(sched))
  sched["surrogate"] <- vector(mode = "logical", length = nrow(sched))

  # Extract teams and delete nested teams column.
  teams <- sched$Teams
  sched$Teams <- NULL

  # Expand the matches data frame so there are six rows per match.
  sched <- sched[sort(rep(1:nrow(sched), 6)), ]

  # Fill in team and station data.
  for(mtch in 1:length(teams)) {
    for(tm in 1:6) {
      mrow <- (mtch-1)*6 + tm
      sched$team[[mrow]] <- teams[[mtch]][["teamNumber"]][[tm]]
      sched$station[[mrow]] <- teams[[mtch]][["station"]][[tm]]
      sched$surrogate[[mrow]] <- teams[[mtch]][["surrogate"]][[tm]]
      #Vrow.names(sched)[mrow] <- paste(mtch, sched$station[[mrow]], sep = ".")
    }
  }

  # Set column names to shorter, easier to type values
  names(sched)[names(sched) == "tournamentLevel"] <- "level"
  names(sched)[names(sched) == "startTime"] <- "start"
  names(sched)[names(sched) == "matchNumber"] <- "match"

  # Fill in alliance data
  sched$alliance <- substr(sched$station, 1, nchar(sched$station) - 1)

  # Transform categorical columns into factors.
  sched <- .FactorColumns(sched, c("team", "station", "field",
                                   "level", "alliance"))
  # Set row and column order
  sched <- sched[order(sched$match, sched$station), ]
  cols.order <- c("match", "description", "level", "field", "start",
                 "team", "alliance", "station", "surrogate")
  sched <- .SetColumnOrder(sched, cols.order)

  row.names(sched) <- tolower(paste(sched$match, sched$station, sep = "."))

  attr(sched, "shape") <- "team"
  class(sched) <- append(class(sched), "Schedule")
  return(sched)
}


#  GetHybridSchedule() =========================================================
#' Get the match schedule and results
#'
#' For matches that have been played, \code{GetHybridSchedule} returns the teams
#' assigned to the match and the match results. If the mtach has not yet been
#' played, the assigned teams and schedule data are returned, but the result
#' fields are blank.
#'
#' The data frame returned by \code{GetHybridSchedule()} is in team shape,
#' i.e., each ' row contains data for a single team and there are six rows per
#' match. Use ' \code{ToAllianceShape()} or \code{ToMatchShape} to convert the
#' data frame to ' a three-teams-per-row shape or a six-teams-per-row shape.
#'
#' See the \emph{Hybrid Schedule} section of the FIRST API documentation for
#' more details.
#'
#' The FIRST API URL format is:
#'
#' \code{https://frc-api.firstinspires.org/v2.0/season/schedule/event/level/
#' hybrid?start=start&end=end}
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param event A character vector containing a FIRST API event code
#'   (see \code{GetEvents}).
#' @param level A character vector containing either \emph{"qual"} or
#'   \emph{"playoff"}. Optional: defaults to \emph{"qual"}.
#' @param start An integer vector containing the earliest match to return.
#'   Optional.
#' @param end An integer vector containing the latest match to return. Optional.
#' @param mod_since A character vector containing an HTTP formatted date and
#'   time. Returns \code{NA} if no changes have been made to the requested data
#'   since the date and time provided. Optional.
#' @param only_mod_since A character vector containing an HTTP formatted date
#'   and time. This function only returns data that has changed since the date
#'   and time provided. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to c("data.frame,
#'   "HybridSchedule"). Returns a logical vector of length one with value
#'   \code{NA} if data is unchanged since date and time passed in arguments
#'   \code{mod_since} or \code{only_mod_since}.
#'
#' @section Columns:
#'   \enumerate{
#'      \item \emph{match}: integer
#'      \item \emph{description}: character
#'      \item \emph{level}: factor
#'      \item \emph{start}: character
#'      \item \emph{actualStart}: character
#'      \item \emph{team}: factor
#'      \item \emph{station}: factor (red1, red2, red3, blue1, blue2, blue3)
#'      \item \emph{surrogate}: logical
#'      \item \emph{disqualified}: logical
#'      \item \emph{scoreFinal, scoreAuto, scoreFoul}: integer}
#'
#' @section Attributes:
#'   \itemize{
#'     \item \emph{url}: Character vector containing URL submitted to FIRST API
#'       server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.
#'     \item \emph{local_url}: Character vector containing URL used to download
#'       local data.
#'     \item \emph{shape}: Character vector that specifies the row and column
#'       arrangement of the data frame. Initially set to \emph{team},
#'       indicating there is one team per row and six rows per match. The
#'       functions \code{ToAllianceShape()} and \code{ToMatchShape} will reshape
#'       the data frame to have either three teams per row or six teams per row
#'       and will set the \emph{shape} attribute to \emph{alliance} or
#'       \emph{match}.
#'     \item \emph{time_downloaded}: Character vector containing the local
#'       system time that the object was downladed from the FIRST API server.
#'       Formatted an an http date and time string.
#'     \item \emph{last_modified}: Character vector containing the date and time
#'       that the data was last modified by the FIRST API server.
#'     \item \emph{mod_since}: Character vector containing the value passed to
#'       the \code{mod_since argument}, or NULL if no argument was passed.
#'       Formatted as an http date and time string.
#'     \item \emph{only_mod_since}: Character vector containing the value passed
#'       to the \code{only_mod_since} argument, or NULL if no argument was
#'       passed. Formatted as an http date and time string.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetHybridSchedule(sn, event = "ORPHI")
#' GetHybridSchedule(sn, event = "WAELL", level = "playoff", start = 3, end = 6)
GetHybridSchedule <- function(session, event, level = "qual", start = NULL,
                              end = NULL, mod_since = NULL,
                              only_mod_since = NULL) {
  # Check for prohibited combinations of arguments
  # Not required because GetSchedule has no prohibited combinations.

  # Build URL
  sched_args <- list(start = start, end = end)
  url <- .AddHTTPArgs(paste("schedule", event, level, "hybrid", sep = "/"),
                      sched_args)

  # Send HTTP request
  sched <- .GetHTTP(session, url, mod_since, only_mod_since)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(sched) || session$format != "data.frame") return(sched)

  # Delete 'Schedule.' from the beginning of column names.
  names(sched) <- .TrimColNames(names(sched))

  # The FIRST API returns nested schedule data nested data. The remainder of
  # this function is necessary to either extract the nested data into new
  # columns or to add rows so that the scheule data can be saved as csv data.

  # Add columns for team number, station, and surrogate
  sched["teamNumber"] <- vector(mode = "integer", length = nrow(sched))
  sched["station"] <- vector(mode = "character", length = nrow(sched))
  sched["surrogate"] <- vector(mode = "logical", length = nrow(sched))
  sched["disqualified"] <- vector(mode = "logical", length = nrow(sched))

  # Add combined scores columns
  sched["scoreFinal"] <- vector(mode = "integer", length = nrow(sched))
  sched["scoreFoul"] <- vector(mode = "integer", length = nrow(sched))
  sched["scoreAuto"] <- vector(mode = "integer", length = nrow(sched))

  # Extract teams and delete nested teams column.
  teams <- sched$Teams
  sched$Teams <- NULL

  # Expand the matches data frame so there are six rows per match.
  sched <- sched[sort(rep(1:nrow(sched), 6)), ]

  # Fill in team and station data.
  for(mtch in 1:length(teams)) {
    for(tm in 1:6) {
      mrow <- (mtch-1)*6 + tm
      sched$teamNumber[[mrow]] <- teams[[mtch]][["teamNumber"]][[tm]]
      sched$station[[mrow]] <- teams[[mtch]][["station"]][[tm]]
      sched$surrogate[[mrow]] <- teams[[mtch]][["surrogate"]][[tm]]
      sched$surrogate[[mrow]] <- teams[[mtch]][["dq"]][[tm]]

      # Extract red and blue scores into combined scoreing columns
      if(substr(sched$station[mrow], 1, 1) == 'R')
        score <- "scoreRed"
      else
        score <- "scoreBlue"
      sched$scoreFinal[[mrow]] <- sched[[paste(score, "Final", sep="")]][[mrow]]
      sched$scoreFoul[[mrow]] <- sched[[paste(score, "Foul", sep = "")]][[mrow]]
      sched$scoreAuto[[mrow]] <- sched[[paste(score, "Auto", sep = "")]][[mrow]]
    }
  }

  # Remove redundent score columns
  sched$scoreRedFinal <- NULL
  sched$scoreBlueFinal <- NULL
  sched$scoreRedFoul <- NULL
  sched$scoreBlueFoul <- NULL
  sched$scoreRedAuto <- NULL
  sched$scoreBlueAuto <- NULL

  # Set column names
  names(sched)[names(sched) == "tournamentLevel"] = "level"
  names(sched)[names(sched) == "matchNumber"] <- "match"
  names(sched)[names(sched) == "teamNumber"] <- "team"
  names(sched)[names(sched) == "startTime"] <- "start"
  names(sched)[names(sched) == "actualStartTime"] <- "actualStart"

  # Fill in alliance data
  sched$alliance <- substr(sched$station, 1, nchar(sched$station) - 1)

  # Transform categorical columns into factors.
  sched <- .FactorColumns(sched, c("team", "station", "level"))

  # Set row names
  row.names(sched)<- tolower(paste(sched$match, sched$station, sep = "."))

  # Set column order
  cols.order <- c("match", "description", "level", "start", "actualStart",
                  "team", "alliance", "station", "surrogate", "disqualified",
                  "scoreFinal", "scoreAuto", "scoreFoul")
  sched <- .SetColumnOrder(sched, cols.order)

  attr(sched, "shape") <- "team"
  sched <- sched[order(sched$match, sched$station), ]
  class(sched) <- append(class(sched), "HybridSchedule")
  return(sched)
}


#  GetMatchResults() ===========================================================
#' Get match scores and participating teams
#'
#' The data frame returned by \code{GetMatchResults()} is in team shape,
#' i.e., each ' row contains data for a single team and there are six rows per
#' match. Use ' \code{ToAllianceShape()} or \code{ToMatchShape} to convert the
#' data frame to ' a three-teams-per-row shape or a six-teams-per-row shape.
#'
#' See the \emph{Match Results} section of the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for more details.
#'
#' The FIRST API URL format is:
#'
#' \code{https://frc-api.firstinspires.org/v2.0/season/matches/event
#' ?tournamentLevel=level&teamNumber=team&matchNumber=match&start=start&end=end}
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param event A character vector containing a FIRST API event code
#'   (see \code{GetEvents}).
#' @param level A character vector containing either \emph{"qual"} or
#'   \emph{"playoff"}. Defaults to \emph{"qual"}. Optional.
#' @param team An integer vector containing a team number. Optional. Cannot
#'   specify \code{match} when \code{team} is specified.
#' @param match An integer vector containing a match number. Optional. If
#'   specified, \code{GetMatchResults} returns results for only the specified
#'   match. If \code{level} is not specified, returns the results for the
#'   qualification match. To get playoff match results, set \code{level} to
#'   \emph{"playoff"}. Cannot specify \code{team} when \code{match} is
#'   specified.
#' @param start An integer vector containing the earliest match to return.
#'   Optional.
#' @param end An integer vector containing the latest match to return. Optional.
#' @param mod_since A character vector containing an HTTP formatted date and
#'   time. Returns \code{NA} if no changes have been made to the requested data
#'   since the date and time provided. Optional.
#' @param only_mod_since A character vector containing an HTTP formatted date
#'   and time. This function only returns data that has changed since the date
#'   and time provided. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to c("data.frame,
#'   "MatchResults"). Returns a logical vector of length one with value
#'   \code{NA} if data is unchanged since date and time passed in arguments
#'   \code{mod_since} or \code{only_mod_since}.
#'
#' @section Columns:
#'   \enumerate{
#'      \item \emph{match}: integer
#'      \item \emph{description}: character
#'      \item \emph{level}: factor
#'      \item \emph{actualStart}: character
#'      \item \emph{postResult}: character
#'      \item \emph{team}: factor
#'      \item \emph{alliance}: factor
#'      \item \emph{station}: factor (red1, red2, red3, blue1, blue2, blue3)
#'      \item \emph{disqualified}: logical
#'      \item \emph{scoreFinal, scoreAuto, scoreFoul}: integer}
#'
#' @section Attributes:
#'   \itemize{
#'     \item \emph{url}: Character vector containing URL submitted to FIRST API
#'       server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.
#'     \item \emph{local_url}: Character vector containing URL used to download
#'       local data.
#'     \item \emph{shape}: Character vector that specifies the row and column
#'       arrangement of the data frame. Initially set to \emph{team},
#'       indicating there is one team per row and six rows per match. The
#'       functions \code{ToAllianceShape()} and \code{ToMatchShape} will reshape
#'       the data frame to have either three teams per row or six teams per row
#'       and will set the \emph{shape} attribute to \emph{alliance} or
#'       \emph{match}.
#'     \item \emph{time_downloaded}: Character vector containing the local
#'       system time that the object was downladed from the FIRST API server.
#'       Formatted an an http date and time string.
#'     \item \emph{last_modified}: Character vector containing the date and time
#'       that the data was last modified by the FIRST API server.
#'     \item \emph{mod_since}: Character vector containing the value passed to
#'       the \code{mod_since argument}, or NULL if no argument was passed.
#'       Formatted as an http date and time string.
#'     \item \emph{only_mod_since}: Character vector containing the value passed
#'       to the \code{only_mod_since} argument, or NULL if no argument was
#'       passed. Formatted as an http date and time string.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetMatchResults(sn, "PNCMP", level="qual")
#' GetMatchResults(sn, "PNCMP", team="2990")
#' GetMatchResults(sn, "WAAMV", match=2, level="playoff")
#' GetMatchResults(sn, "CMP-ARCHIMEDES", level="qual", start=10, end=20)
GetMatchResults <- function(session, event, level = "qual", team = NULL,
                            match = NULL, start = NULL, end = NULL,
                            mod_since = NULL, only_mod_since = NULL) {
  # Check for unallowed combinations of arguments.
  if((!is.null(match) || !is.null(start) || !is.null(end)) && is.null(level))
    stop("You must specify the level when you specify match, start, or end.")
  if(!is.null(team) && !is.null(match))
    stop("You cannot specify both a team and match number.")
  if(!is.null(match) && (!is.null(start) || !is.null(end)))
    stop("You cannot specify start or end if you specify match.")

  # Assemble URL
  result_args <- list(tournamentLevel = level, teamNumber = team,
                    matchNumber = match, start = start, end = end)
  url <- .AddHTTPArgs(paste("matches", event, sep = "/"), result_args)

  # Send HTTP request and get data.
  matches <- .GetHTTP(session, url, mod_since, only_mod_since)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(matches) || session$format != "data.frame") return(matches)

  # Delete 'Matches.' from the beginning of column names.
  names(matches) <- substr(names(matches), 9, 100)

  # The FIRST API returns nested schedule data. The remainder of this function
  # is necessary to extract the nested data into new rows so that the scheule
  # data can be saved as csv data.

  # Add columns for each operating station
  matches["teamNumber"] <- vector(mode = "integer", length = nrow(matches))
  matches["station"] <- vector(mode = "character", length = nrow(matches))
  matches["disqualified"] <- vector(mode = "logical", length = nrow(matches))

  # Add combined scores columns
  matches["scoreFinal"] <- vector(mode = "integer", length = nrow(matches))
  matches["scoreFoul"] <- vector(mode = "integer", length = nrow(matches))
  matches["scoreAuto"] <- vector(mode = "integer", length = nrow(matches))

  # Extract teams and delete nested teams column.
  teams <- matches$Teams
  matches$Teams <- NULL

  # Expand the matches data frame so there are six rows per match.
  xMatches <- matches[sort(rep(1:nrow(matches), 6)), ]

  # Fill in team and station data.
  for(mtch in 1:length(teams)) {
    for(tm in 1:6) {
      mrow <- (mtch-1)*6 + tm
      xMatches$teamNumber[[mrow]] <- teams[[mtch]][["teamNumber"]][[tm]]
      xMatches$station[[mrow]] <- teams[[mtch]][["station"]][[tm]]
      xMatches$disqualified[[mrow]] <- teams[[mtch]][["dq"]][[tm]]

      # Extract red and blue scores into combined scoreing columns
      if(substr(xMatches$station[mrow], 1, 1) == 'R')
        score <- "scoreRed"
      else
        score <- "scoreBlue"

      xMatches$scoreFinal[[mrow]] <- xMatches[[paste(score, 'Final', sep="")]][[mrow]]
      xMatches$scoreFoul[[mrow]] <- xMatches[[paste(score, 'Foul', sep = "")]][[mrow]]
      xMatches$scoreAuto[[mrow]] <- xMatches[[paste(score, 'Auto', sep = "")]][[mrow]]
    }
  }

    # Remove redundent score columns
    xMatches$scoreRedFinal <- NULL
    xMatches$scoreBlueFinal <- NULL
    xMatches$scoreRedFoul <- NULL
    xMatches$scoreBlueFoul <- NULL
    xMatches$scoreRedAuto <- NULL
    xMatches$scoreBlueAuto <- NULL

    matches <- xMatches


  # Convert categorical data into factors
  matches$tournamentLevel <- factor(matches$tournamentLevel)

  # Set column names to shorter, easier to type values.
  names(matches)[names(matches) == "tournamentLevel"] <- "level"
  names(matches)[names(matches) == "matchNumber"] <- "match"
  names(matches)[names(matches) == "teamNumber"] <- "team"
  names(matches)[names(matches) == "actualStartTime"] <- "actualStart"
  names(matches)[names(matches) == "postResultTime"] <- "postResult"

  # Fill in alliance data
  matches$alliance <- substr(matches$station, 1, nchar(matches$station) - 1)

  # Set row names to match.station
  row.names(matches) <- tolower(paste(matches$match,
                                      matches$station, sep = "."))

  # Set column order
  cols.order <- c("match", "description", "level", "actualStart", "postResult",
                  "team", "alliance", "station", "disqualified",
                  "scoreFinal", "scoreAuto", "scoreFoul")
  matches <- .SetColumnOrder(matches, cols.order)

  matches <- matches[order(matches$match, matches$station), ]
  attr(matches, "shape") <- "team"
  class(matches) <- append(class(matches), "MatchResults")
  return(matches)
}


#  GetScores ====================================================================
#' Get detailed match scores
#'
#' The results vary depending on the season requested. The 2016 data fields are
#' listed here. See the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for data fields for prior seasons.
#' The data frame contains two rows for each match, one for blue and the other
#' for red.
#'
#' \code{GetScores()} contains both the blue and red alliance scores for each
#' match, but it does not list the teams assigned to each alliance. Use R's
#' \code{merge()} function to merge the data frames returned by
#' \code{GetMatchResults} and \code{GetScores()} to create a data frame that
#' contains both team numbers and detailed scores:
#' \code{merge(match_results_df, scores_df)} should do the trick.
#'
#' See the \emph{Detailed Scores} section of the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for more details.
#'
#'The FIRST API URL format is:
#'
#' \code{https://frc-api.firstinspires.org/v2.0/season/matches/event/level?
#' teamNumber=team&matchNumber=match&start=start&end=end}
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param event A character vector containing a FIRST API event code
#'   (see \code{GetEvents}).
#' @param level A character vector containing either \emph{"qual"} or
#'   \emph{"playoff"}. Defaults to \emph{"qual"}. Optional.
#' @param team An integer vector containing a team number. Optional. Cannot
#'   specify \code{match} when \code{team} is specified.
#' @param match An integer vector containing a match number. Optional. If
#'   specified, \code{GetMatchResults} returns results for only the specified
#'   match. If \code{level} is not specified, returns the results for the
#'   qualification match. To get playoff match results, set \code{level} to
#'   \emph{"playoff"}. Cannot specify \code{team} when \code{match} is
#'   specified.
#' @param start An integer vector containing the earliest match to return.
#'   Optional.
#' @param end An integer vector containing the latest match to return. Optional.
#' @param mod_since A character vector containing an HTTP formatted date and
#'   time. Returns \code{NA} if no changes have been made to the requested data
#'   since the date and time provided. Optional.
#' @param only_mod_since A character vector containing an HTTP formatted date
#'   and time. This function only returns data that has changed since the date
#'   and time provided. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to c("data.frame,
#'   "Scores"). Returns a logical vector of length one with value \code{NA} if
#'   data is unchanged since date and time passed in arguments \code{mod_since}
#'   or \code{only_mod_since}.
#'
#' @section Columns:
#'   \enumerate{
#'      \item \emph{level}: character
#'      \item \emph{match}: integer
#'      \item \emph{audienceGroup}: character
#'      \item \emph{alliance}: character
#'      \item \emph{robot1Auto, robot2Auto, robot3Auto}: character
#'      \item \emph{autoBouldersLow, autoBouldersHigh}: integer
#'      \item \emph{teleopBouldersLow, teleopBouldersHigh}: integer
#'      \item \emph{towerFaceA, towerFaceB, towerFaceC}: character
#'      \item \emph{towerEndStrength}: integer
#'      \item \emph{teleopTowerCaptured, teleopDefensesBreached}: logical
#'      \item \emph{position1, position2, position3, position4,
#'        position5}: character
#'      \item \emph{position1Crossings, position2Crossings, position3Crossings,
#'        position4Crossings, position5Crossings}: integer
#'      \item \emph{foulCount, techFoulCount}: integer
#'      \item \emph{autoPoints, autoReachPoints, autoCrossingPoints,
#'        autoBoulderPoints}: integer
#'      \item \emph{teleopPoints, teleopCrossingPoints, teleopBoulderPoints,
#'        teleopChallengePoints, teleopScalePoints}: integer
#'      \item \emph{breachPoints, capturePoints}: integer
#'      \item \emph{adustPoints, foulPoints, totalPoints}: integer}
#'
#' @section Attributes:
#'   \itemize{
#'     \item \emph{url}: Character vector containing URL submitted to FIRST API
#'       server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.
#'     \item \emph{local_url}: Character vector containing URL used to download
#'       local data.
#'     \item \emph{time_downloaded}: Character vector containing the local
#'       system time that the object was downladed from the FIRST API server.
#'       Formatted an an http date and time string.
#'     \item \emph{last_modified}: Character vector containing the date and time
#'       that the data was last modified by the FIRST API server.
#'     \item \emph{mod_since}: Character vector containing the value passed to
#'       the \code{mod_since argument}, or NULL if no argument was passed.
#'       Formatted as an http date and time string.
#'     \item \emph{only_mod_since}: Character vector containing the value passed
#'       to the \code{only_mod_since} argument, or NULL if no argument was
#'       passed. Formatted as an http date and time string.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetScores(sn, event = "ARCHIMEDES")
#' GetScores(sn, event = "WAELL", start = 1, end = 10)
#' GetScores(sn, event = "WAELL", match = 15)
GetScores <- function(session, event, level = "qual", team = NULL,
                              match = NULL, start = NULL, end = NULL,
                              mod_since = NULL, only_mod_since = NULL) {
  # Check for unallowed combinations of arguments.
  if(!is.null(team) && !is.null(match))
    stop("You cannot specify both a team and match number")
  if(!is.null(match) && (!is.null(start) || !is.null(end)))
    stop("You cannot specify start or end if you specify match")

  # Assemble URL
  score_args <- list(teamNumber = team, matchNumber = match, start = start,
                     end = end)
  url <- .AddHTTPArgs(paste("scores", event, level, sep = "/"), score_args)

  # Send HTTP request and get data.
  scores <- .GetHTTP(session, url, mod_since, only_mod_since)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(scores) || session$format != "data.frame") return(scores)

  # Delete 'MatcheScores.' from the beginning of column names.
  names(scores) <- .TrimColNames(names(scores))

  # Extract nested alliances column from data frame.
  alliances <- scores$Alliances
  scores$Alliances <- NULL

  # Expand data frame to include six rows for each match.
  scores <- scores[sort(rep(1:nrow(scores), 2)), ]

  # Get names of nested Alliance columns.
  alliance_col_names <- names(alliances[[1]])
  for(col_name in alliance_col_names) {
    col_type <- if(is.integer(alliances[[1]][[col_name]]))
      "integer"
    else
      "character"
      scores[col_name] <- vector(mode = col_type, length = nrow(scores))
  }

  # Extract nested data into new columns
  for(mtch in 1:length(alliances)){
    for(col_name in alliance_col_names){
      df_row <- (mtch - 1)*2
      scores[[col_name]][[df_row + 1]] <- alliances[[mtch]][[col_name]][[1]]
      scores[[col_name]][[df_row + 2]] <- alliances[[mtch]][[col_name]][[2]]
    }
  }

  # Transform categorical columns into factors
  for(col_name in names(scores)){
    if(is.character(scores[[col_name]]))
      scores[[col_name]] <- factor(scores[[col_name]])
  }

  # Set column names to shorter, easier to type values.
  names(scores)[names(scores) == "matchLevel"] <- "level"
  names(scores)[names(scores) == "matchNumber"] <- "match"

  # Set row names to be integers.
  scores$alliance <- as.character(scores$alliance)
  row.names(scores) <- tolower(paste(scores$match, scores$alliance, sep = "."))
  scores$alliance <- factor(scores$alliance)

  class(scores) <- append(class(scores), "Scores")
  return(scores)
}


#  GetAlliances() ==============================================================
#' Get teams assigned to playoff alliances
#'
#' Returns a list of playoff alliances, including the alliance captains and
#' teams that were selected for each alliance.
#'
#' See the \emph{Event Alliances} section of the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for more details.
#'
#' The FIRST API URL format is:
#'
#' \code{https://frc-api.firstinspires.org/v2.0/season/alliances/event}
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param event A character vector containing a FIRST API event code
#'   (see \code{GetEvents}).
#' @param mod_since A character vector containing an HTTP formatted date and
#'   time. Returns \code{NA} if no changes have been made to the requested data
#'   since the date and time provided. Optional.
#' @param only_mod_since A character vector containing an HTTP formatted date
#'   and time. This function only returns data that has changed since the date
#'   and time provided. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to c("data.frame,
#'   "Alliances"). Returns a logical vector of length one with value \code{NA}
#'   if data is unchanged since date and time passed in arguments
#'   \code{mod_since} or \code{only_mod_since}.
#'
#' @section Columns:
#'   \enumerate{
#'      \item \emph{number}: integer
#'      \item \emph{name}: character
#'      \item \emph{captain, round1, round2, round3}: integer
#'      \item \emph{backup, backupReplaced}: integer
#'      \item \emph{count}: integer}
#'
#' @section Attributes:
#'   \itemize{
#'     \item \emph{url}: Character vector containing URL submitted to FIRST API
#'       server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.
#'     \item \emph{local_url}: Character vector containing URL used to download
#'       local data.
#'     \item \emph{time_downloaded}: Character vector containing the local
#'       system time that the object was downladed from the FIRST API server.
#'       Formatted an an http date and time string.
#'     \item \emph{last_modified}: Character vector containing the date and time
#'       that the data was last modified by the FIRST API server.
#'     \item \emph{mod_since}: Character vector containing the value passed to
#'       the \code{mod_since argument}, or NULL if no argument was passed.
#'       Formatted as an http date and time string.
#'     \item \emph{only_mod_since}: Character vector containing the value passed
#'       to the \code{only_mod_since} argument, or NULL if no argument was
#'       passed. Formatted as an http date and time string.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetAlliances(sn, "WAAMV")
GetAlliances <- function (session, event, mod_since = NULL,
                          only_mod_since = NULL) {
  # Assemble URL
  url <- paste("alliances/", event, sep="")

  # Send HTTP request
  alliances <- .GetHTTP(session, url, mod_since, only_mod_since)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(alliances) || session$format != "data.frame") return(alliances)

  # Remove prefix from column names.
  names(alliances) <- .TrimColNames(names(alliances))

  class(alliances) <- append(class(alliances), "Alliances")
  return(alliances)
}


#  GetRankings() ===============================================================
#' Get team rankings
#'
#' The results vary depending on the season requested. The 2016 data fields are
#' listed here. See the FIRST API documentation for data fields for prior
#' seasons.
#'
#' See the \emph{Event Rankings} section of the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for more details.
#'
#' The FIRST API URL format is:
#'
#' \code{https://frc-api.firstinspires.org/v2.0/season/rankings/event?teamNumber=team&top=top}
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param event A character vector containing a FIRST API event code
#'   (see \code{GetEvents}).
#' @param team An integer vector containing a team number. Optional.
#' @param top An integer vector specifying the number of teams to return,
#'   starting with the top ranked team.
#' @param mod_since A character vector containing an HTTP formatted date and
#'   time. Returns \code{NA} if no changes have been made to the requested data
#'   since the date and time provided. Optional.
#' @param only_mod_since A character vector containing an HTTP formatted date
#'   and time. This function only returns data that has changed since the date
#'   and time provided. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to c("data.frame,
#'   "Rankings"). Returns a logical vector of length one with value \code{NA} if
#'   data is unchanged since date and time passed in arguments \code{mod_since}
#'   or \code{only_mod_since}.
#'
#' @section Columns:
#'   \enumerate{
#'      \item \emph{rank}: integer
#'      \item \emph{teamNumber}: integer
#'      \item \emph{sortOrder1, sortOrder2, sortOrder3, sortOrder4, sortOrder5,
#'        sortOrder6}: integer or numeric.
#'      \item \emph{wins, losses, ties}: integer
#'      \item \emph{qualAverage}: numeric
#'      \item \emph{dq}: integer
#'      \item \emph{matchesPlayed}: integer}
#'
#' @section Attributes:
#'   \itemize{
#'     \item \emph{url}: Character vector containing URL submitted to FIRST API
#'       server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.
#'     \item \emph{local_url}: Character vector containing URL used to download
#'       local data.
#'     \item \emph{time_downloaded}: Character vector containing the local
#'       system time that the object was downladed from the FIRST API server.
#'       Formatted an an http date and time string.
#'     \item \emph{last_modified}: Character vector containing the date and time
#'       that the data was last modified by the FIRST API server.
#'     \item \emph{mod_since}: Character vector containing the value passed to
#'       the \code{mod_since argument}, or NULL if no argument was passed.
#'       Formatted as an http date and time string.
#'     \item \emph{only_mod_since}: Character vector containing the value passed
#'       to the \code{only_mod_since} argument, or NULL if no argument was
#'       passed. Formatted as an http date and time string.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetRankings(sn, "WAAMV")
#' GetRankings(sn, "PNCMP", team = 1983)
#' GetRankings(sn, "ARCHIMEDES", top = 5)
GetRankings <- function (session, event, team = NULL, top = NULL,
                         mod_since = NULL, only_mod_since = NULL) {
  # Check for unallowed combinations of arguments.
  if(!is.null(team) && !is.null(top))
    stop("You cannot specify both the team and top argument")

  # Assemble URL
  rank_args <- list(teamNumber = team, top = top)
  url <- .AddHTTPArgs(paste("rankings", event, sep = "/"), rank_args)

  # Send HTTP request and get data.
  rankings <- .GetHTTP(session, url, mod_since, only_mod_since)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(rankings) || session$format != "data.frame") return(rankings)

  # Delete 'Rankings.' from the beginning of column names.
  names(rankings) <- .TrimColNames(names(rankings))

  # Standardize column names across different firstapiR functions
  names(rankings)[names(rankings) == "teamNumber"] <- "team"

  class(rankings) <- append(class(rankings), "Rankings")
  return(rankings)
}


#  GetAwards() =================================================================
#' Get the awards that were presented at an event
#'
#' See the \emph{Event Awards} section of the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for more details.
#'
#' The FIRST API URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/awards/event/team}
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param event A character vector containing a FIRST API event code
#'   (see \code{GetEvents}). Must specify the \code{event} argument, the
#'   \code{team} argument, or both.
#' @param team An integer vector containing a team number. Optional.  Must
#'   specify the \code{event} argument, the \code{team} argument, or both.
#' @param mod_since A character vector containing an HTTP formatted date and
#'   time. Returns \code{NA} if no changes have been made to the requested data
#'   since the date and time provided. Optional.
#' @param only_mod_since A character vector containing an HTTP formatted date
#'   and time. This function only returns data that has changed since the date
#'   and time provided. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object or a data.frame with class set to c("data.frame,
#'   "Awards"). Returns a logical vector of length one with value \code{NA} if
#'   data is unchanged since date and time passed in arguments \code{mod_since}
#'   or \code{only_mod_since}.
#'
#' @section Columns:
#'   \enumerate{
#'      \item \emph{awardId}: integer
#'      \item \emph{teamId}: integer
#'      \item \emph{eventId}: integer
#'      \item \emph{eventDivisionId}: logical
#'      \item \emph{eventCode}: character
#'      \item \emph{name}: character
#'      \item \emph{series}: integer
#'      \item \emph{teamNumber}: integer
#'      \item \emph{fullTeamName}: character
#'      \item \emph{person}: character}
#'
#' @section Attributes:
#'   \itemize{
#'     \item \emph{url}: Character vector containing URL submitted to FIRST API
#'       server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.
#'     \item \emph{local_url}: Character vector containing URL used to download
#'       local data.
#'     \item \emph{time_downloaded}: Character vector containing the local
#'       system time that the object was downladed from the FIRST API server.
#'       Formatted an an http date and time string.
#'     \item \emph{last_modified}: Character vector containing the date and time
#'       that the data was last modified by the FIRST API server.
#'     \item \emph{mod_since}: Character vector containing the value passed to
#'       the \code{mod_since argument}, or NULL if no argument was passed.
#'       Formatted as an http date and time string.
#'     \item \emph{only_mod_since}: Character vector containing the value passed
#'       to the \code{only_mod_since} argument, or NULL if no argument was
#'       passed. Formatted as an http date and time string.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetAwards(sn, "PNCMP")
#' GetAwards(sn, team = 360)
#' GetAwards(sn, "PNCMP", 360)
GetAwards <- function(session, event = NULL, team = NULL,
                      mod_since = NULL, only_mod_since = NULL) {
  # Check for incorrect combinations of arguments.
  if(is.null(event) && is.null(team))
    stop("You must specify either a team number or event code")

  # Assemble URL -- GetAwards URL format is different from other functions.
  url <- "awards"
  if(!is.null(event))
    url <- paste(url, event, sep = "/")
  if(!is.null(team))
    url <- paste(url, team, sep = "/")

  # Send HTTP request
  awards <- .GetHTTP(session, url, mod_since, only_mod_since)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(awards) || session$format != "data.frame") return(awards)

  # Remove column name prefix
  names(awards) <- .TrimColNames(names(awards))

  # Standardize column names across different firstapiR functions
  names(awards)[names(awards) == "teamNumber"] <- "team"

  class(awards) <- append(class(awards), "Awards")
  return(awards)
}


#  GetAwardsList() =============================================================
#' Get a list of all available awards for a season
#'
#' See the \emph{Awards Listing} section of the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for more details.
#'
#' The FIRST API URL format is:
#'
#' \code{https://frc-api.firstinspires.org/v2.0/season/awards/list}
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param mod_since A character vector containing an HTTP formatted date and
#'   time. Returns \code{NA} if no changes have been made to the requested data
#'   since the date and time provided. Optional.
#' @param only_mod_since A character vector containing an HTTP formatted date
#'   and time. This function only returns data that has changed since the date
#'   and time provided. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to c("data.frame,
#'   "AwardsList"). Returns a logical vector of length one with value \code{NA}
#'   if data is unchanged since date and time passed in arguments
#'   \code{mod_since} or \code{only_mod_since}.
#'
#' @section Columns:
#'   \enumerate{
#'      \item \emph{awardId}: integer
#'      \item \emph{eventType}: character
#'      \item \emph{description}: character
#'      \item \emph{forPerson}: logical}
#'
#' @section Attributes:
#'   \itemize{
#'     \item \emph{url}: Character vector containing URL submitted to FIRST API
#'       server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.
#'     \item \emph{local_url}: Character vector containing URL used to download
#'       local data.
#'     \item \emph{time_downloaded}: Character vector containing the local
#'       system time that the object was downladed from the FIRST API server.
#'       Formatted an an http date and time string.
#'     \item \emph{last_modified}: Character vector containing the date and time
#'       that the data was last modified by the FIRST API server.
#'     \item \emph{mod_since}: Character vector containing the value passed to
#'       the \code{mod_since} argument, or NULL if no argument was passed.
#'       Formatted as an http date and time string.
#'     \item \emph{only_mod_since}: Character vector containing the value passed
#'       to the \code{only_mod_since} argument, or NULL if no argument was
#'       passed. Formatted as an http date and time string.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetAwardsList(sn)
GetAwardsList <- function(session, mod_since = NULL, only_mod_since = NULL) {
  # Assemble URL
  url <- "awards/list"

  # Send HTTP request
  alist <- .GetHTTP(session, url, mod_since, only_mod_since)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(alist) || session$format != "data.frame") return(alist)

  # Remove column name prefix
  names(alist) <- .TrimColNames(names(alist))

  class(alist) <- append(class(alist), "AwardsList")
  return(alist)
}
