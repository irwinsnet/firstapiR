#  firstapiR_http.R FIRST API R Toolbox ========================================
#' firstapiR: R Functions for the FIRST API Server
#'
#' The R Toolbox functions will connect to the FIRST API server and download
#' data on FIRST Robotics Competition (FRC) teams, events, match results, and
#' awards.
#'
#' The FIRST API accepts formatted hypertext transfer protocol (HTTP) GET
#' requests for FRC data and provides results in either javascript object
#' notation (json) or extensible markup language (xml) format. Detailed
#' documentation for the FIRST API, including precise rules for constructing the
#' HTTP requests, is available at \url{http://docs.frcevents2.apiary.io/#}
#'
#' A username and authorization key are required for connecting to the FIRST
#' API and for using the R Kit. To obtain a username and key, join the FIRST
#' Community Developers project on TeamForge at
#' \url{https://usfirst.collab.net/sf/projects/first_community_developers/}
#'
#' These functions return R dataframes by default. Optionally, the functions
#' can also return the raw JSON or XML that is provided by the FIRST API. See
#' the apiary documentation (\url{http://docs.frcevents2.apiary.io/#}) for a
#' detailed description of all response data fields.
#'
#' These functions use version 2.0 of the FIRST API. They have not been tested
#' with version 1.0.
#'
#'@name firstapiR
NULL

# The firstapiR package requires the following R packages.
# base64enc
# httr
# jsonlite
# XML

# Server URLs and other parameters
.staging_url <- "https://frc-staging-api.firstinspires.org"
.production_url <- "https://frc-api.firstinspires.org"
.version <- "v2.0"
.default_season <- as.integer(format(Sys.Date(), "%Y"))
.package_version <- "1.0.0"


#  GetSession() ================================================================
#' Create a FIRST API session
#'
#' Every FIRST API function requires a session as its first parameter.
#'
#' The session is an R list that contains the FIRST API username, authorization
#' key, season, format, and a boolean value that specifies whether to use
#' the staging server instead of the production server.
#'
#' The key field of the Session object may be set to the value "key". If this is
#' done, the firstapiR functions will skip the HTTP request and will extract
#' example data from the R/sysdata.rda file. This function is for testing and
#' demonstrations when no internet connection or valid authorization key is
#' available. Example data frames returned by firstapiR functions will have
#' their "local_test_data" attribute set to TRUE and the "time_downloaded"
#' attribute will be set to the date and time that the example data was
#' downloaded from the server and stored in the R/sysdata.rda file.
#'
#' Throws an error if \code{season}, \code{format}, or \code{staging} arguments
#' are incorrect.
#'
#' @param username A character vector containing the username assigned by FIRST.
#' @param key A character vector containing the authorization key assigned by
#'   FIRST, or the value "key".
#' @param season An integer vector containing the 4-digit year. Defaults to the
#'   current year. Must be equal to or less than the current season and greater
#'   than or equal to 2015.
#' @param format A character vector that specifies the data format that will be
#'   returned by firstapiR functions. Can be "json", "data.frame", or "xml".
#'   Defaults to "data.frame". Case insensitive.
#' @param staging A logical vector. If set to \code{TRUE}, firstapiR uses the
#'   staging URL. Defaults to \code{FALSE}.
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
#' Get the status of the FIRST API server.
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
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame, "Season").
#'
#'   \strong{Data Frame Columns}
#'   \enumerate{
#'     \item \emph{name}: character
#'     \item \emph{version}: double
#'     \item \emph{status}: character}
#'
#'   \strong{Data Frame Attributes}
#'   \itemize{
#'     \item \emph{url}: URL submitted to FIRST API
#'     \item \emph{time_downloaded}: Local System time that the object was
#'     downladed from the FIRST API server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' status <- GetServerStatus(sn)
GetServerStatus <- function(session) {
  status <- .GetHTTP(session, "status")
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
#' @param mod_since A character vector containing an HTTP formated date and
#'   time. Use the \code{httr::http_date} and \code{httr::parse_http_date}
#'   functions to convert between HTTP date strings to POSIXt date values.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame, "Season").
#'
#'   \strong{Data Frame Columns}
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
#'   \strong{Data Frame Attributes}
#'   \itemize{
#'     \item \emph{url}: URL submitted to FIRST API
#'     \item \emph{time_downloaded}: Local System time that the object was
#'     downladed from the FIRST API server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key", season = 2015, staging = TRUE)
#' summary <- GetSeason(sn)
GetSeason <- function(session, ...) {
  season <- .GetHTTP(session, "", ...)

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
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame, "Districts").
#'
#'   \strong{Data Frame Columns}
#'   \enumerate{
#'     \item \emph{code}: character
#'     \item \emph{name}: character
#'     \item \emph{districtCount}: integer}
#'
#'   \strong{Data Frame Attributes}
#'   \itemize{
#'     \item \emph{url}: URL submitted to FIRST API
#'     \item \emph{time_downloaded}: Local System time that the object was
#'     downladed from the FIRST API server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' districts <- GetDistricts(sn)
GetDistricts <- function(session, ...) {
  url <- "districts"
  districts <- .GetHTTP(session, url, ...)

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
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame, "Events").
#'
#'   \strong{Data Frame Columns}
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
#'   \strong{Data Frame Attributes}
#'   \itemize{
#'     \item \emph{url}: URL submitted to FIRST API
#'     \item \emph{time_downloaded}: Local System time that the object was
#'     downladed from the FIRST API server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' frc_data <- GetEvents(sn, team = 5803)
#' frc_data <- GetEvents(sn, district = 'PNW')
#' frc_data <- GetEvents(sn, team = 360, exclude_district = TRUE)
GetEvents <- function(session, event = NULL, team = NULL,
                      district = NULL, exclude_district = NULL, ...) {
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
  events <- .GetHTTP(session, url, ...)

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
#' responses into a single data frame. For xml and json formats, the user will
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
#'   returned. Optional. Use only for xml or json formats.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame, "Teams").
#'
#'   \strong{Data Frame Columns}
#'   \enumerate{
#'     \item \emph{teamNumber}: character
#'     \item \emph{nameFull}: character
#'     \item \emph{nameShort}: character
#'     \item \emph{city}: character
#'     \item \emph{stateProv}: factor
#'     \item \emph{country}: factor
#'     \item \emph{website}: character
#'     \item \emph{rookieYear}: integer
#'     \item \emph{robotName}: character
#'     \item \emph{districtCode}: factor
#'     \item \emph{eamCountTotal}: integer
#'     \item \emph{teamCountPage}: integer
#'     \item \emph{pageCurrent}: integer
#'     \item \emph{pageTotal}: integer}
#'
#'   \strong{Data Frame Attributes}
#'     \itemize{
#'     \item \emph{url}: URL submitted to FIRST API
#'     \item \emph{time_downloaded}: Local System time that the object was
#'     downladed from the FIRST API server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetTeams(sn, state = "California")
#' GetTeams(sn, district = "FIM")
#' GetTeams(sn, event = "CMP-CARVER")
GetTeams <- function (session, team = NULL, event = NULL, district = NULL,
                      state = NULL, page = NULL, ...) {
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
  teams[[1]] <- .GetHTTP(session, url, ...)

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
      attr(teams, "time_downloaded") <- team_attr$time_downloaded
    }
  } else
    teams <- teams[[1]]

  # Shorten the column names to reduce amount of typing required.
  names(teams) <- .TrimColNames(names(teams))

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
#' Depending on the \code{expand_cols} argument, \code{GetSchedule} can
#' structure the resulting data frame two diferent ways. When \code{expand_cols}
#' is set to \emph{FALSE} the data frame will contain six rows for every match
#' returned. Each row will include data on one team that participates in the
#' match. This narrow (i.e., fewer columns) structure is useful when filtering
#' results to specific teams, because only one column, \emph{teamNumber}, must
#' be filtered. When \code{expand_cols} is set to \emph{TRUE} the data frame
#' will have one row per match, with all six participating teams listed in one
#' row. This wide format is useful for displaying the schedule in a table.
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
#' @param start An integer vector containing the earliest match to
#'   Optional.
#' @param end An integer vector containing the latest match to return. Optional.
#' @param expand_cols A logical value that defaults to \code{FALSE}. If
#'   \code{TRUE}, the dataframe will include one row for each scheduled match,
#'   with a different column for each team. If \code{FALSE}, there will be six
#'   rows for each match, with each row listing one assigned team and their
#'   station. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame, "Schedule").
#'
#'   \strong{Data Frame Columns}
#'   \enumerate{
#'     \item \emph{description}: character
#'     \item \emph{field}: character
#'     \item \emph{tournamentLevel}: factor
#'     \item \emph{matchNumber}: integer
#'     \item \emph{startTime}: character}
#'
#'      If expand_cols == \emph{FALSE}
#'        \enumerate{
#'          \item \emph{teamNumber}: factor
#'          \item \emph{alliance}: factor (Red, Blue)
#'          \item \emph{station}: factor (Red1, Red2, Red3, Blue1, Blue2, Blue3)
#'          \item \emph{surrogate}: logical}
#'
#'     If expand_cols == \emph{TRUE}
#'       \enumerate{
#'         \item \emph{Red1.team, Red2.team, Red3.team}: factor
#'         \item \emph{Blue1.team, Blue2.team, Blue3.team}: factor
#'         \item \emph{Red1.surrogate, Red2.surrogate, Red3.surrogate}: logical
#'         \item \emph{Blue1.surrogate, Blue2.surrogate, Blue3.surrogate}:
#'           logical}
#'
#'   \strong{Data Frame Attributes}
#'     \itemize{
#'     \item \emph{url}: URL submitted to FIRST API
#'     \item \emph{time_downloaded}: Local System time that the object was
#'     downladed from the FIRST API server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.}
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
                         start = NULL, end = NULL, expand_cols = FALSE, ...) {
  # Check for prohibited combinations of arguments
  # Not required because GetSchedule has no prohibited combinations.

  # Build URL
  sched_args <- list(tournamentLevel = level, teamNumber = team, start = start,
                     end = end)
  url <- .AddHTTPArgs(paste("schedule", event, sep = "/"), sched_args)

  # Send HTTP request
  sched <- .GetHTTP(session, url, ...)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(sched) || session$format != "data.frame") return(sched)

  # Delete 'Schedule.' from the beginning of column names.
  names(sched) <- .TrimColNames(names(sched))

  # The FIRST API returns nested schedule data nested data. The remainder of
  # this function is necessary to either extract the nested data into new
  # columns or to add rows so that the scheule data can be saved as csv data.

  if(expand_cols) {
    # Add columns for each operating station
    cols <- c("Red1", "Red2", "Red3", "Blue1", "Blue2", "Blue3")
    for(col in cols) {
      cname.team <- paste(col, "team", sep = ".")
      sched[cname.team] <- vector(mode = "character",
                                  length = length(sched$matchNumber))
      cname.surr <- paste(col, "surrogate", sep = ".")
      sched[cname.surr] <- vector(mode = "logical",
                                  length = length(sched$matchNumber))
    }

    # Extract data from nested Teams column and insert into new operating station
    # columns
    for(mtch in 1:length(sched$matchNumber)){
      for(tm in 1:6){
        station <- sched$Teams[[mtch]][["station"]][[tm]]
        team <- sched$Teams[[mtch]][["teamNumber"]][[tm]]
        surrogate <- sched$Teams[[mtch]][["surrogate"]][[tm]]

        cname.team <- paste(station, "team", sep = ".")
        cname.surrogate <- paste(station, "surrogate", sep = ".")

        sched[mtch, cname.team] <- team
        sched[mtch, cname.surrogate] <- surrogate
      }
    }
    sched$Teams <- NULL

    # Convert categorical data to factors
    sched <- .FactorColumns(sched, c("Red1.team", "Red2.team", "Red3.team",
                                     "Blue1.team", "Blue2.team", "Blue3.team",
                                     "field", "tournamentLevel"))
  } else {
    # Add columns for team number, station, and surrogate
    sched["teamNumber"] <- vector(mode = "integer", length = nrow(sched))
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
        sched$teamNumber[[mrow]] <- teams[[mtch]][["teamNumber"]][[tm]]
        sched$station[[mrow]] <- teams[[mtch]][["station"]][[tm]]
        sched$surrogate[[mrow]] <- teams[[mtch]][["surrogate"]][[tm]]
      }
    }

    # Fill in alliance data
    sched$alliance <- substr(sched$station, 1, nchar(sched$station) - 1)

    # Transform categorical columns into factors.
    sched <- .FactorColumns(sched, c("teamNumber", "station", "field",
                                     "tournamentLevel", "alliance"))
  }
  class(sched) <- append(class(sched), "Schedule")
  return(sched)
}


#  GetHybridSchedule() =========================================================
#' Get the match schedule and results.
#'
#' For matches that have been played, \code{GetHybridSchedule} returns the teams
#' assigned to the match and the match results. If the mtach has not yet been
#' played, the assigned teams and schedule data are returned, but the result
#' fields are blank.
#'
#' Depending on the \code{expand_cols} argument, \code{GetHybridSchedule} can
#' structure the resulting data frame two diferent ways. When \code{expand_cols}
#' is set to \emph{FALSE} the data frame will contain six rows for every match
#' returned. Each row will include data on one team that participates in the
#' match. This narrow (i.e., fewer columns) structure is useful when filtering
#' results to specific teams, because only one column, \emph{teamNumber}, must
#' be filtered. When \code{expand_cols} is set to \emph{TRUE} the data frame
#' will have one row per match, with all six participating teams listed in one
#' row. This wide format is useful for displaying the schedule in a table.
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
#'   \emph{"playoff"}. Defaults to \emph{"qual"}. Optional.
#' @param start An integer vector containing the earliest match to
#'   Optional.
#' @param end An integer vector containing the latest match to return. Optional.
#' @param expand_cols A logical value that defaults to \code{FALSE}. If
#'   \code{TRUE}, the dataframe will include one row for each scheduled match,
#'   with a different column for each team. If \code{FALSE}, there will be six
#'   rows for each match, with each row listing one assigned team and their
#'   station. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame", "Schedule").
#'
#'   \strong{Data Frame Columns}
#'   \enumerate{
#'      \item \emph{description}: character
#'      \item \emph{tournamentLevel}: factor
#'      \item \emph{matchNumber}: integer
#'      \item \emph{startTime}: character
#'      \item \emph{actualStartTime}: character}
#'
#'      If expand_cols == \emph{FALSE}
#'        \enumerate{
#'          \item \emph{teamNumber}: factor
#'          \item \emph{station}: factor (Red1, Red2, Red3, Blue1, Blue2, Blue3)
#'          \item \emph{scoreFinal, scoreAuto, scoreFoul}: integer
#'          \item \emph{surrogate}: logical
#'          \item \emph{dq}: logical}
#'
#'      If expand_cols == \emph{TRUE}
#'        \enumerate{
#'          \item \emph{scoreRedFoul, scoreRedAuto, scoreRedFinal}: integer
#'          \item \emph{scoreBlueFoul, scoreBlueAuto, scoreBlueFinal}: integer
#'          \item \emph{Red1.team, Red2.team, Red3.team}: factor
#'          \item \emph{Blue1.team, Blue2.team, Blue3.team}: factor
#'          \item \emph{Red1.surrogate, Red2.surrogate, Red3.surrogate}: logical
#'          \item \emph{Blue1.surrogate, Blue2.surrogate,
#'            Blue3.surrogate}: logical
#'          \item \emph{Red1.dq, Red2.dq, Red3.dq}: logical
#'          \item \emph{Blue1.dq, Blue2.dq, Blue3.dq}: logical}
#'
#'   \strong{Data Frame Attributes}
#'     \itemize{
#'     \item \emph{url}: URL submitted to FIRST API
#'     \item \emph{time_downloaded}: Local System time that the object was
#'     downladed from the FIRST API server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetHybridSchedule(sn, event = "ORPHI")
#' GetHybridSchedule(sn, event = "WAELL", level = "playoff", start = 3, end = 6)
GetHybridSchedule <- function(session, event, level = "qual", start = NULL,
                              end = NULL, expand_cols = FALSE, ...) {
  # Check for prohibited combinations of arguments
  # Not required because GetSchedule has no prohibited combinations.

  # Build URL
  sched_args <- list(start = start, end = end)
  url <- .AddHTTPArgs(paste("schedule", event, level, "hybrid", sep = "/"),
                      sched_args)

  # Send HTTP request
  sched <- .GetHTTP(session, url, ...)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(sched) || session$format != "data.frame") return(sched)

  # Delete 'Schedule.' from the beginning of column names.
  names(sched) <- .TrimColNames(names(sched))

  # The FIRST API returns nested schedule data nested data. The remainder of
  # this function is necessary to either extract the nested data into new
  # columns or to add rows so that the scheule data can be saved as csv data.

  if(expand_cols) {
    # Add columns for each operating station
    cols <- c("Red1", "Red2", "Red3", "Blue1", "Blue2", "Blue3")
    for(col in cols) {
      cname.team <- paste(col, "team", sep = ".")
      sched[cname.team] <- vector(mode = "character",
                                  length = length(sched$matchNumber))
      cname.surr <- paste(col, "surrogate", sep = ".")
      sched[cname.surr] <- vector(mode = "logical",
                                  length = length(sched$matchNumber))
      cname.dq <- paste(col, "dq", sep = ".")
      sched[cname.dq] <- vector(mode = "logical", length = nrow(sched))
    }

    # Extract data from nested Teams column and insert into new operating station
    # columns
    for(mtch in 1:length(sched$matchNumber)){
      for(tm in 1:6){
        station <- sched$Teams[[mtch]][["station"]][[tm]]
        team <- sched$Teams[[mtch]][["teamNumber"]][[tm]]
        surrogate <- sched$Teams[[mtch]][["surrogate"]][[tm]]
        dq <- sched$Teams[[mtch]][["dq"]][[tm]]

        cname.team <- paste(station, "team", sep = ".")
        cname.surrogate <- paste(station, "surrogate", sep = ".")
        cname.dq <- paste(station, "dq", sep = ".")

        sched[mtch, cname.team] <- team
        sched[mtch, cname.surrogate] <- surrogate
        sched[mtch, cname.dq] <- dq
      }
    }
    sched$Teams <- NULL

    # Convert categorical data to factors
    sched <- .FactorColumns(sched, c("Red1.team", "Red2.team", "Red3.team",
                                     "Blue1.team", "Blue2.team", "Blue3.team",
                                     "tournamentLevel"))
  } else {
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

    # Transform categorical columns into factors.
    sched <- .FactorColumns(sched, c("teamNumber", "station",
                                     "tournamentLevel"))
  }
  class(sched) <- append(class(sched), "HybridSchedule")
  return(sched)
}


#  GetMatchResults() ===========================================================
#' Get match scores and participating teams
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
#' @param start An integer vector containing the earliest match to
#'   Optional.
#' @param end An integer vector containing the latest match to return. Optional.
#' @param expand_cols A logical value that defaults to \code{FALSE}. If
#'   \code{TRUE}, the dataframe will include one row for each scheduled match,
#'   with a different column for each team. If \code{FALSE}, there will be six
#'   rows for each match, with each row listing one assigned team and their
#'   station. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame", "Schedule").
#'
#'   \strong{Data Frame Columns}
#'   \enumerate{
#'      \item \emph{actualStartTime}: character
#'      \item \emph{description}: character
#'      \item \emph{tournamentLevel}: factor
#'      \item \emph{matchNumber}: integer
#'      \item \emph{postResultTime}: character}
#'
#'      If expand_cols == \emph{FALSE}
#'        \enumerate{
#'          \item \emph{teamNumber}: factor
#'          \item \emph{station}: factor (Red1, Red2, Red3, Blue1, Blue2, Blue3)
#'          \item \emph{surrogate}: logical
#'          \item \emph{scoreFinal, scoreAuto, scoreFoul}: integer}
#'
#'      If expand_cols == \emph{TRUE}
#'        \enumerate{
#'          \item \emph{scoreRedFinal, scoreRedAuto, scoreRedFoul}: integer
#'          \item \emph{scoreBlueFinal, scoreBlueAuto, scoreBlueFoul}: integer
#'          \item \emph{Red1.team, Red2.team, Red3.team}: factor
#'          \item \emph{Blue1.team, Blue2.team, Blue3.team}: factor
#'          \item \emph{Red1.surrogate, Red2.surrogate, Red3.surrogate}: logical
#'          \item \emph{Blue1.surrogate, Blue2.surrogate,
#'            Blue3.surrogate}: logical}
#'
#'   \strong{Data Frame Attributes}
#'     \itemize{
#'     \item \emph{url}: URL submitted to FIRST API
#'     \item \emph{time_downloaded}: Local System time that the object was
#'     downladed from the FIRST API server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.}
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
                            expand_cols = FALSE, ...) {
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
  matches <- .GetHTTP(session, url, ...)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(matches) || session$format != "data.frame") return(matches)

  # Delete 'Matches.' from the beginning of column names.
  names(matches) <- substr(names(matches), 9, 100)

  # The FIRST API returns nested schedule data. The remainder of this function
  # is necessary to extract the nested data into new rows so that the scheule
  # data can be saved as csv data.
  if(expand_cols) {
    # Add columns for each operating station
    cols <- c("Red1", "Red2", "Red3", "Blue1", "Blue2", "Blue3")
    for(col in cols) {
      cname.team <- paste(col, "team", sep = ".")
      matches[cname.team] <- vector(mode = "integer", length = nrow(matches))
      cname.dq <- paste(col, "dq", sep = ".")
      matches[cname.dq] <- vector(mode = "logical", length = nrow(matches))
    }

    # Extract data from nested Teams column and insert into new operating
    # station columns
    for(mtch in 1:nrow(matches)){
      for(tm in 1:6){
        station <- matches$Teams[[mtch]][["station"]][[tm]]
        team <- matches$Teams[[mtch]][["teamNumber"]][[tm]]
        dq <- matches$Teams[[mtch]][["dq"]][[tm]]

        cname.team <- paste(station, "team", sep = ".")
        cname.dq <- paste(station, "dq", sep = ".")

        matches[mtch, cname.team] <- team
        matches[mtch, cname.dq] <- dq
      }
    }
    matches$Teams <- NULL

    # Convert categorical data to factors
    for(col in cols) {
      cname.team <- paste(col, "team", sep = ".")
      matches[[cname.team]] <- factor(matches[[cname.team]])
    }
  } else {
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
  }

  # Convert categorical data into factors
  matches$tournamentLevel <- factor(matches$tournamentLevel)

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
#' @param start An integer vector containing the earliest match to
#'   Optional.
#' @param end An integer vector containing the latest match to return. Optional.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame", "Schedule").
#'
#'   \strong{Data Frame Columns}
#'   \enumerate{
#'      \item \emph{matchLevel}: character
#'      \item \emph{matchNumber}: integer
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
#'   \strong{Data Frame Attributes}
#'     \itemize{
#'     \item \emph{url}: URL submitted to FIRST API
#'     \item \emph{time_downloaded}: Local System time that the object was
#'     downladed from the FIRST API server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetScores(sn, event = "ARCHIMEDES")
#' GetScores(sn, event = "WAELL", start = 1, end = 10)
#' GetScores(sn, event = "WAELL", match = 15)
GetScores <- function(session, event, level = "qual", team = NULL,
                              match = NULL, start = NULL, end = NULL, ...) {
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
  scores <- .GetHTTP(session, url, ...)

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

  # Set row names to be integers.
  row.names(scores) <- 1:nrow(scores)

  class(scores) <- append(class(scores), "Scores")
  return(scores)
}


#  GetAlliances() ==============================================================
#' Get teams assigned to playoff alliances
#'
#' Returns a list of playoff alliances, including the alliance captains and
#' teams that were selected for the each alliance.
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
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame", "Schedule").
#'
#'   \strong{Data Frame Columns}
#'   \enumerate{
#'      \item \emph{number}: integer
#'      \item \emph{name}: character
#'      \item \emph{captain, round1, round2, round3}: integer
#'      \item \emph{backup, backupReplaced}: integer
#'      \item \emph{count}: integer}
#'
#'   \strong{Data Frame Attributes}
#'     \itemize{
#'     \item \emph{url}: URL submitted to FIRST API
#'     \item \emph{time_downloaded}: Local System time that the object was
#'     downladed from the FIRST API server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetAlliances(sn, "WAAMV")
GetAlliances <- function (session, event, ...) {
  # Assemble URL
  url <- paste("alliances/", event, sep="")

  # Send HTTP request
  alliances <- .GetHTTP(session, url, ...)

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
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame", "Schedule").
#'
#'   \strong{Data Frame Columns}
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
#'   \strong{Data Frame Attributes}
#'     \itemize{
#'     \item \emph{url}: URL submitted to FIRST API
#'     \item \emph{time_downloaded}: Local System time that the object was
#'     downladed from the FIRST API server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetRankings(sn, "WAAMV")
#' GetRankings(sn, "PNCMP", team = 1983)
#' GetRankings(sn, "ARCHIMEDES", top = 5)
GetRankings <- function (session, event, team = NULL, top = NULL, ...) {
  # Check for unallowed combinations of arguments.
  if(!is.null(team) && !is.null(top))
    stop("You cannot specify both the team and top argument")

  # Assemble URL
  rank_args <- list(teamNumber = team, top = top)
  url <- .AddHTTPArgs(paste("rankings", event, sep = "/"), rank_args)

  # Send HTTP request and get data.
  rankings <- .GetHTTP(session, url, ...)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(rankings) || session$format != "data.frame") return(rankings)

  # Delete 'Rankings.' from the beginning of column names.
  names(rankings) <- .TrimColNames(names(rankings))

  class(rankings) <- append(class(rankings), "Rankings")
  return(rankings)
}


#  GetAwards() =================================================================
#' Get the awards that were presented to a team or at an event.
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
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame", "Schedule").
#'
#'   \strong{Data Frame Columns}
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
#'   \strong{Data Frame Attributes}
#'     \itemize{
#'     \item \emph{url}: URL submitted to FIRST API
#'     \item \emph{time_downloaded}: Local System time that the object was
#'     downladed from the FIRST API server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetAwards(sn, "PNCMP")
#' GetAwards(sn, team = 360)
#' GetAwards(sn, "PNCMP", 360)
GetAwards <- function(session, event = NULL, team = NULL, ...) {
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
  awards <- .GetHTTP(session, url, ...)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(awards) || session$format != "data.frame") return(awards)

  # Remove column name prefix
  names(awards) <- .TrimColNames(names(awards))

  class(awards) <- append(class(awards), "Awards")
  return(awards)
}


#  GetAwardsList() =============================================================
#' Get a list of all available awards for a season.
#'
#' See the \emph{Awards Listing} section of the FIRST API documentation at
#' \url{http://docs.frcevents2.apiary.io/#} for more details.
#'
#' The FIRST API URL format is:
#'
#' \code{https://frc-api.firstinspires.org/v2.0/season/awards/list}
#'
#' @param session A Session object created with \code{GetSession()}.
#'
#' @return Depending on the \code{session$format} value, returns JSON text, an
#'   XML::XMLDocument object, or a data.frame with class set to
#'   c("data.frame", "Schedule").
#'
#'   \strong{Data Frame Columns}
#'   \enumerate{
#'      \item \emph{awardId}: integer
#'      \item \emph{eventType}: character
#'      \item \emph{description}: character
#'      \item \emph{forPerson}: logical}
#'
#'   \strong{Data Frame Attributes}
#'     \itemize{
#'     \item \emph{url}: URL submitted to FIRST API
#'     \item \emph{time_downloaded}: Local System time that the object was
#'     downladed from the FIRST API server.
#'     \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'       R/sysdata.rda file.}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key")
#' GetAwardsList(sn)
GetAwardsList <- function(session, ...) {
  # Assemble URL
  url <- "awards/list"

  # Send HTTP request
  alist <- .GetHTTP(session, url, ...)

  # Skip remainder of function for empty, XML, or JSON formats.
  if(is.na(alist) || session$format != "data.frame") return(alist)

  # Remove column name prefix
  names(alist) <- .TrimColNames(names(alist))

  class(alist) <- append(class(alist), "AwardsList")
  return(alist)
}


#  .AddHTTPArgs() ==============================================================
#' Add GET parameters to a URL string.
#'
#' .AddHTTPArgs is an internal function. It takes a named list and creates a GET
#' parameter string suitable for inclusion in a URL. It adds a question mark
#' ('?') to the beginning of the string and separates each name-value pair with
#' an ampersand ('&'). List items with NULL values are ignored.
#'
#' @param url A character vector containing the portion of the FIRST API URL
#'   that follows the season parameter and forward slash, but is not a GET
#'   parameter (i.e., before the '?'). For most FIRST API commands, the url
#'   string will be the simple string that identifies the command, e.g.,
#'   "districts" or "teams". A few FIRST API commands use path parameters (i.e.,
#'   Detailed Scores) in addition to GET parameters -- in such cases the url
#'   argument must contain the path parameters separated by '/'.
#' @param http_args A list of all GET parameters that will be supplied to the
#'   URL query string. The elements of the list must have a name that that
#'   matches the name of the GET parameter (e.g., teamNumber, eventCode). Any
#'   list elements that are NULL will be skipped.
#'
#' @return A character vector containing the portion of the FIRST API URL
#'   starting with the path parameter that immediately follows the season (not
#'   including the '/') and extending to the end of the URL, including the GET
#'   query string.
#'
#' @examples
#'   # From GetTeams()
#'   team_args <- list(teamNumber = team, eventCode = event,
#'     districtCode = district, state = state, page = page)
#'  url <- .AddHTTPArgs("teams", team_args)
#'
#'  # From GetRankings()
#'  rank_args <- list(teamNumber = team, top = top)
#'  url <- .AddHTTPArgs(paste("rankings", event, sep = "/"), rank_args)
.AddHTTPArgs <- function(url, http_args) {
  res <- url

  first_arg <- TRUE

  for(idx in 1:length(http_args)) {
    # Skip NULL arguments
    if(!is.null(http_args[[idx]])) {
      res <- paste(res, if(first_arg) "?" else "&", sep = '')

      # Convert logical arguments to character values "true" or "false"
      if(is.logical(http_args[[idx]]))
         if(http_args[[idx]])
           http_args[[idx]] <- "true"
         else
           http_args[[idx]] <- "false"

      res <- paste(res, names(http_args)[idx], "=", http_args[idx], sep="")
      first_arg <- FALSE
    }
  }
  return(res)
}


#  .GetHTTP() ==================================================================
#' Send an HTTP request.
#'
#' .GetHTTP is an internal function that is not intended to be called by the
#' user. .GetHTTP is called by other FIRST API methods.
#'
#' .GETHTTP is a crucial function -- it does most of the work for the firstapiR
#' publicly accessible functions. It formulates the HTTP request from the
#' \code{url} and \code{session} arguments, including final assembly on the url
#' and creating the authorization header. It verifies there are no HTTP errors
#' on the response and checks the response content for properly formatted JSON
#' or XML text. Finally, if a data frame is requested, it converts the response
#' to an R data frame.
#'
#' .GetHTTP will thow an error if any HTTP code other than 200 is received in
#' the HTTP response. The error message will include the error code and
#' the error message from the response content. See the Response Codes section
#' of the FIRST API documentation for additional details.
#'
#' .GetHTTP will also throw an error if session$format = "data.frame" and no
#' records are returned.
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param url A character vector containing a partial FIRST API url. The url
#'   argument includes everything to the right of the season. For example,
#'   \code{'events?teamNumber=team&districtCode=district}.
#'
#' @return Returns either JSON text, an XML::XMLNode object, or an R data frame,
#'   depending on the value of session$format.
#'
#' @examples
#' sn <- GetSession("username", "key")
#' .GetHTTP(sn, "events?teamNumber=2557&districtCode=PNW")
.GetHTTP <- function (session, url, mod_since = NULL,
                      only_mod_since = NULL) {

  # Check arguments
  if(!is.null(mod_since) && !is.null(only_mod_since))
    stop("Cannot specify both mod_since and only_mod_since arguments.")

  # Build Full URL
  full_url <- paste(if(session$staging) .staging_url else .production_url,
                    .version, sep="/")
  if(url != "status")
    full_url <- paste(full_url, session$season, url, sep="/")

  # Create authorization and format headers
  raw_token <- paste(session$username, session$key, sep=':')
  token <- base64enc::base64encode(charToRaw(raw_token))
  auth_header = paste("Basic", token)

  format_header <- switch(tolower(session$format), "xml" = "application/xml",
                          "application/json")

  headers <- c(Authorization = auth_header, Accept = format_header)

  # Add modification date headers
  if(!is.null(mod_since)) headers[["If-Modified-Since"]] <-
    mod_since
  if(!is.null(only_mod_since)) headers[["FMS-OnlyModifiedSince"]] <-
    only_mod_since

  if(session$key == "key") {
    # Use local data from R/sysdata.rda if user specifies dummy key.
    # See data-raw/data.R for more info.
    content <- .GetLocalData(url, session$format)
    no_results <- FALSE
  } else {
    # Send GET request to FIRST API server
    user_agent <- paste("firstapiR: Version ", .package_version)
    r <- httr::GET(full_url, httr::add_headers(.headers = headers),
                   httr::user_agent(user_agent))

    # Check HTTP Response Status Code
    if(!(httr::status_code(r) %in% c(200, 304)))
      stop(paste(httr::status_code(r), httr::content(r, "text"), sep=": "))

    content <- httr::content(r, "text")
    last_modified <- httr::headers(r)[["Last-Modified"]]

    no_results <- (httr::status_code(r) == 304)
  }

  # Set results to FALSE if no records are returned
  if(no_results) {
    result <- NA
  } else {
    # Format results based on session$format setting.
    result <- switch(tolower(session$format),
                     "json" = jsonlite::prettify(content),
                     "xml" = {
                       raw_xml <- content
                      XML::xmlRoot(XML::xmlTreeParse(raw_xml, asText = TRUE))
                     },
                     data.frame(jsonlite::fromJSON(content)))
  }

  # Set result to FALSE if no records are returned
  if(session$format == "data.frame" && length(result) == 0)
    result <- NA

  # Set descriptive attributes.
  attr(result, "url") <- full_url
  if(session$key == "key") {
    attr(result, "local_test_data") <- TRUE
    attr(result, "local_url") <- attr(content, "url")
    attr(result, "time_downloaded") <- attr(content, "time_downloaded")
    attr(result, "last_modified") <- attr(content, "last_modified")
    attr(result, "partial") <- FALSE
  } else {
    attr(result, "local_test_data") <- FALSE
    attr(result, "local_url") <- NULL
    attr(result, "time_downloaded") <- Sys.time()
    attr(result, "last_modified") <- last_modified
    attr(result, "partial") <- no_results
  }
  attr(result, "only_mod_since") <- only_mod_since
  attr(result, "mod_since") <- mod_since

  return(result)
}


#  .TrimColNames() =============================================================
#' Remove prefixes from data frame column names
#'
#' \code{.TrimColNames} is an internal function that is not intended to be
#' called by the user. It is called by other FIRST API methods.
#'
#' .TrimColNames removes all portions of a string up to and including the first
#' period. It's intended to produce shorter column names that are easier to
#' type in R interactive sessions. Column names without periods are left
#' unchanged.
#'
#' @param col_names A character vector of column names, generally provided by
#'   the names() function.
#'
#' @return A character vector of trimmed column names, with the same length as
#'   the col_names argument.
#'
#' @examples
#' names(rankings) <- .TrimColNames(names(rankings))
.TrimColNames <- function(col_names) {
  sub("\\w+\\.", "", col_names, perl = TRUE)
}


#  .FactorColumns() ============================================================
#' Convert character columns to factors
#'
#' \code{.FactorColumns} is an internal function that is not intended to be
#' called by the user. It is called by other FIRST API methods.
#'
#' There are benefits to converting data frame columns with a limited number of
#' unique character values to factors. For example, the R barplot function will
#' automatically group values by factors, which could be a team, district, etc.
#' Factors are conceptually similar to enumerations in other programming
#' languages.
#'
#' @param df The data.frame that will have it's columns converted to factors.
#' @param cols A character vector of column names that identifies the columns
#'   that will be converted to factors.
#'
#' @return Data.frame
#'
#' @examples
#' teams <- .FactorColumns(teams, c("districtCode", "stateProv", "country"))
.FactorColumns <- function(df, cols) {
  for(fc in cols)
    df[[fc]] <- factor(df[[fc]])
  return(df)
}


#  .GetLocalData() =============================================================
#' Extract FIRST data from a local file.
#'
#' \code{.GetLocalData} is an internal function that is not intended to be
#' called by the user. It is called by other FIRST API methods.
#'
#' \code{.GetLocalData} extracts archived FIRST data from the packages local
#' \code{R/sysdata.rda} file. The data is archived as JSON text or as an
#' XML::XMLDocument object.
#'
#' Normally, \code{.GetLocalData} will not be used. However, if the user sets
#' the authorization key in the Session object to the literal value "key", then
#' \code{.GetHTTP()} will call \code{.GetLocalData()} instead of sending the
#' HTTP request. As a result, \code{.GetHTTP} and all other firstapiR functions
#' that depend on \code{.GetHTTP} will skip the HTTP request and return the
#' archived, local data.
#'
#' This feature allows users to experiment with firstapiR features even if they
#' don't have an official authorization key issued by FIRST. It also enables
#' the examples in the firstapiR documentation to work. The actual HTTP request,
#' HTTP error code check, and extraction of text data from the body of the HTTP
#' response are the only parts of the code that are bypassed.
#'
#' @param url A character vector containing the portion of the FIRST API url
#'  that follows the season path parameter, not including the '/' that follows
#'  the season.
#' @param data_format A character vector containing either "json" or "xml".
#'
#' @return While the local data will be of the same type as what was requested
#'   (i.e., if called via \code{GetTeams} a list of teams will be returned,
#'   \code{GetSchedule}) will return schedule data, etc.) arguments such as
#'   \code{team} or \code{event} will be ignored. Refer to the
#'   \code{data-raw/data.R} file to see the firstapiR commands that were used to
#'   download the data from the FIRST API server.
#'
#' @examples
#' content <- .GetLocalData(url, session$format)
.GetLocalData <- function(url, data_format) {
  # Verify R/sysdata.rda file has been installed and is available.
  if(!exists("data_time"))
    stop("Local data is not available. Check for R/sysdata.rda file.")

  # Determine API command from URL
  api_cmd_ptn <- "(\\w+)(?:[/?]|$)"
  api_mod_ptn <- "/(\\w+)(?:$|\\?)"

  if(url == "")
    cmd_type <- "season"
  else {
    cmd_mtch <- regexec(api_cmd_ptn, url, perl = TRUE)
    cmd_type <- regmatches(url, cmd_mtch)[[1]][[2]]
  }

  if(cmd_type == "schedule"){
    mod_mtch <- regexec(api_mod_ptn, url, perl = TRUE)
    mod_type <- regmatches(url, mod_mtch)[[1]][[2]]
    if(mod_type == "hybrid")
      cmd_type = "hybrid"
  }

  if(cmd_type == "awards"){
    mod_mtch <- regexec(api_mod_ptn, url, perl = TRUE)
    mod_type <- regmatches(url, mod_mtch)[[1]][[2]]
    if(mod_type == "list")
      cmd_type <- "awards_list"
  }

  # Build variable name based on command type and format.
  if(tolower(data_format) == "xml") {
    local_data <- paste(cmd_type, "xml", sep = "_")
  } else {
    local_data <- paste(cmd_type, "json", sep = "_")
  }

  # Extract and return local data.
  eval(parse(text = paste("content <-", local_data)))
  attr(content, "time_downloaded") <- data_time
  return(content)
}
