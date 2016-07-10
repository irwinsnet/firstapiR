#  OVERVIEW: FRC 1318'S FIRST API R KIT ========================================
#' FRC 1318'S FIRST API R KIT, VERSION 0.9A
#' 
#' The R Kit functions will connect to the FIRST API server and download data on 
#' FIRST Robotics Competition (FRC) teams, events, match results, and awards.
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

# The FIRST API R Kit requires the following R packages. Install these
# packages before using the R Kit. 
library("base64enc")
library("httr")
library("jsonlite")
library("XML")

# Server URLs and other parameters
.staging_url <- "https://frc-staging-api.firstinspires.org"
.production_url <- "https://frc-api.firstinspires.org"
.version <- "v2.0"
.default_season <- as.integer(format(Sys.Date(), "%Y"))


#  GetSession() ================================================================
#' Create a FIRST API session.
#' 
#' Every FIRST API function requires a session as its first parameter.
#' 
#' The session is an R list that contains the FIRST API username, authorization
#' key, season, format, and a boolean value that specifies whether to use
#' the staging server instead of the production server.
#'
#' @param username Character The username assigned by FIRST.
#' @param key Character The authorization key assigned by FIRST.
#' @param season Integer The 4-digit year. Defaults to the year specified in the
#' .default_season variable at the top if this file. Must be a 4-digit number
#'   that is equal to or less than the current year and greater than or equal to
#'   the current year plus one.
#' @param format Character The data format that will be returned by the
#'   functions. Can be "json", "data.frame", or "xml". Defaults to "data.frame".
#'   Case insensitive.
#' @param staging Logical Set to TRUE to use the staging URL. Defaults to
#'   \code{FALSE}.
#' 
#' Throws an error if season, format, or staging arguments are not allowed
#' values.
#'
#' @return A list containing all GetSession parameters. class: 'Session'
#' 
#' @export
#'
#' @examples
#' sn <- GetSession('myUserName', 'myAuthorizationKey')
#' sn <- GetSession('myUserName', 'myAuthorizationKey', season = 2015)
#' sn$format <- 'xml'
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
  class(session) <- "Session"
  
  return(session)
}


#  GetSeason() =================================================================
#' Get high-level information on a particular FRC season.
#' 
#' Returns information for the season specified in the session list (see
#' documentation for the GetSession function for additional details.)
#' 
#' See the \emph{Season Summary} section of the FIRST API documentation for
#' additional details. The URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season}
#' 
#' @param session Session A session list created with \code{GetSession()}.
#' 
#' @return JSON or XML formatted text, or an R data frame.
#'  data.frame column names and classes:
#'    eventCount: integer
#'    gameName: factor
#'    kickoff: factor
#'    rookieStart: integer
#'    teamCount: integer
#'    FRCChampionships.name: character
#'    FRCChampionships.startDate: character
#'    FRCChampionships.location: character
#'    
#'  @export
#' 
#'  @examples
#'  sn <- GetSession(username, key, season = 2015)
#'  summary <- GetSeason(sn)
GetSeason <- function(session) {
  season <- .GetHTTP(session, "")
  
  
}


#  GetDistricts() ==============================================================
#' Get a list of FIRST districts.
#' 
#' This function returns a list of all current districs, including their titles 
#' and codes. District codes are used as parameters for several other FIRST API
#' functions.
#' 
#' See the \emph{District Listings} section of the FIRST API documentation. The 
#' URL format is: \code{https://frc-api.firstinspires.org/v2.0/season/districts}
#' 
#' @param session Session A session list created with \code{GetSession()}.
#'   
#' @return A data.frame, json list, or xml list.
#'    data.frame column names and classes:
#'      code: character
#'      name: character
#'      districtCount: integer
#' @exp
#' 
#' @examples
#' sn <- GetSession(username, key)
#' districts <- GetDistricts(sn)
GetDistricts <- function(session) {
  url <- "districts"
  districts <- .GetHTTP(session, url)
  
  # Skip rest of function for XML or JSON results
  if(session$format != "data.frame") return(districts)
  
  # Shorten the column names to reduce amount of typing required.
  names(districts) <- .TrimColNames(names(districts))
  
  return(districts)
}


#  GetEvents() =================================================================
#' Get details for multiple events.
#' 
#' Returns details for multiple FRC events.
#' 
#' See the \emph{Event Listings} section of the FIRST API documentation. The URL
#' format is: \code{https://frc-api.firstinspires.org/v2.0/season/events? 
#' teamNumber=team&districtCode=district&excludeDistrict=district}
#' 
#' GetEvents will accept either the \code{team} or \code{district} parameters, 
#' neither parameter, or both parameters. If neither \code{team} nor
#' \code{district} are specified, \code{GetEvents} returns all FRC events for
#' the competition season. If \code{team} is specified, the results are filtered
#' to only the events in which the FRC team participated. Similarly, if
#' \code{district} is specified, the results are filtered to only the events
#' that occurred within the specified district. If \code{exclude_district} is
#' set to TRUE, then only non-district events are returned. The \code{district}
#' and \code{exclude_district} events may not be specified at the same time.
#' 
#' @param session Session A session list created with \code{GetSession()}.
#' @param event Character A FIRST API event code. If event is
#'   specified, \code{GetEvents()} will return results only for the specified
#'   event. Optional.
#' @param team Integer Team number. Optional
#' @param district Characger The FIRST API district code (see
#'   \code{GetDistricts()}). If disctrict is specified, \code{GetTeams()} will
#'   filter results to all teams in the specified district.
#' @param exclude_district Logical If set to \code{TRUE}, district events are
#'   excluded from results. Optional.
#'   
#' Throws an error if team argument is specified and any other arguments are
#' specified, or if both the district and exclude_district arguments are
#' specified.
#'   
#' @return A data.frame, json list, or xml list.
#'    data.frame column names and classes:
#'      code: character
#'      divisionCode: character
#'      name: character
#'      type: factor ('Regional', 'DistrictEvent',
#'        'DistrictChampionship', 'ChampionshipSubdivision',
#'        'ChampionshipDivision', 'Championship', 'Offseason')
#'      districtCode: factor ('CHM', 'FIM', 'IN', 'MAR', 'NC', 'PCH',
#'        'PNW')
#'      venue: character
#'      city: character
#'      stateprov: factor
#'      country: factor
#'      timezone: factor
#'      dateStart: character
#'      dateEnd', character
#'      eventCount: integer
#' @export
#' 
#' @examples
#' sn <- GetSession(username, key)
#' frc_data <- GetEvents(sn, team = 5803))
#' frc_data <- GetEvents(sn, team = 360, district = 'PNW')
#' frc_data <- GetEvents(sn, district = FALSE))
GetEvents <- function(session, event = NULL, team = NULL,
                      district = NULL, exclude_district = NULL) {
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
  events <- .GetHTTP(session, url)
  
  # Skip rest of function for XML or JSON results
  if(session$format != "data.frame") return(events)
  
  # Shorten the column names to reduce amount of typing required.
  names(events) <- .TrimColNames(names(events))

  # Convert categorical coluns to factor data types.
  events <- .FactorColumns(events, c("type", "districtCode", "stateprov",
                                     "country", "timezone"))
  
  return(events)
}


#  GetTeams() ==================================================================
#' Get details for many teams.
#' 
#' The FIRST team listing API response will be broken up into several pages if
#' the number of teams in the response exceeds 65, with a separate HTTP request
#' required for each page. If the dataframe format is specified, then
#' \code{GetTeams()} makes this transparent to the user. \code{GetTeams()} will
#' determine the number of pages in the response, conduct an HTTP request to
#' obtain each page, and merge all pages into a single dataframe. For the xml
#' and json formats, \code{GetTeams()} will return a single page of teams.
#' 
#' See the \emph{Team Listings} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/teams&eventCode=event
#' ?districtCode=district?state=state?page=2}
#'
#' @param session Session A session list created with \code{GetSession()}.
#' @param team Integer Team number. Optional
#' @param event Character A FIRST API event code (see \code{GetEvents()}). If
#'   event is specified, \code{GetTeams()} will filter results to all teams
#'   particpating in the specified event. Optional.
#' @param district Character The FIRST API district code (see
#'   \code{GetDistricts()}). If disctrict is specified, \code{GetTeams()} will
#'   filter results to all teams in the specified district.
#' @param state Character A state name, spelled out entirely (i.e., 'Idaho',
#'   \emph{not} 'ID'). If state is specified, \code{GetTeams()} will filter
#'   results to all teams in the specified state.
#' @param page Integer Page number specifying which page of results should be
#'   returned. Optional. Use only for xml or json formats.
#'
#' @return If the data.frame format is specified (i.e., \code{session$format ==
#'   'data.frame'}), returns all teams in a single data frame. If the json or
#'   xml formates are specified, returns a single page of json or xml responses.
#'    data.frame column names and classes:
#'      teamNumber: character
#'      nameFull: character
#'      nameShort: character
#'      city: character
#'      stateProv: factor
#'      country: factor
#'      website: character
#'      rookieYear: integer
#'      robotName: character
#'      districtCode: factor
#'      teamCountTotal: integer
#'      teamCountPage: integer
#'      pageCurrent: integer
#'      pageTotal: integer
#' @export
#'
#' @examples
#' sn <- GetSession(username, key)
#' GetTeams(state = 'California')
#' GetTeams(district = 'FIM')
#' GetTeams(event = 'CMP-CARVER')
GetTeams <- function (session, team = NULL, event = NULL, district = NULL,
                      state = NULL, page = NULL) {
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
  teams[[1]] <- .GetHTTP(session, url)
  
  # Skip remainder of function for XML or JSON formats.
  if(session$format != "data.frame") return(teams[[1]])
  
  # Get total number of pages
  pages <- teams[[1]]$pageTotal[1]
  
  # If results consist of more than one page, send an HTTP request to get each
  # page, storing each page of results in the list.
  if(pages > 1) {
    for(page in 2:pages) {
      team_args$page <- page
      url <- .AddHTTPArgs("teams", team_args)
      teams[[page]] = .GetHTTP(session, url)
    }
    
    # For data frames, merge all pages into one data frame.
    if(session$format == 'data.frame') {
      teams_df <- teams[[1]]
      for(page in 2:pages) {
        teams_df <- merge(teams_df, teams[[page]], all = TRUE)
      }
      teams <- teams_df
    }
  } else teams <- teams[[1]] # For xml and json, return the list of pages.
  
  # Shorten the column names to reduce amount of typing required.
  names(teams) <- .TrimColNames(names(teams))

  # Convert categorical coluns to factor data types.
  teams <- .FactorColumns(teams, c("districtCode", "stateProv", "country"))
  
  return(teams)
}


#  GetSchedule() ===============================================================
#' Get the match schedule for a specific event.
#' 
#' See the \emph{Event Schedule} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/schedule/event?
#' tournamentLevel=level&teamNumber=team&start=start&end=end}
#'
#' @param session Session A session list created with \code{GetSession()}.
#' @param event Character The FIRST API event code.
#' @param level Character Either \code{qual} or \code{playoff}. Defaults to
#'   \code{qual}.
#' @param team Integer team number. Optional
#' @param start Integer Earliest match to reuturn
#' @param end Integer Latest match to return
#' @param expand_rows A logical value. Optional, defaults to FALSE. If FALSE, the
#'   dataframe will include one row for each scheduled match, with a different
#'   column for each team. If TRUE, there will be six rows for each match, with
#'   each row listing one assigned team and their station.
#'
#' @return A data.frame, json list, or xml list.
#'    data.frame column names and classes:
#'      description: character
#'      field: character
#'      tournamentLevel: factor
#'      matchNumber: integer      
#'      startTime: character
#'      
#'      If expand.rows == FALSE
#'        Red1.team, Red2.team, Red3.team: factor
#'        Blue1.team, Blue2.team, Blue3.team: factor
#'        Red1.surrogate, Red2.surrogate, Red3.surrogate: logical
#'        Blue1.surrogate, Blue2.surrogate, Blue3.surrogate: logical
#'      If expand.rows == TRUE
#'        teamNumber: factor
#'        station: factor (Red1, Red2, Red3, Blue1, Blue2, Blue3)
#'        surrogate: logical
#'      
#' @export
#'
#' @examples
#' sn <- GetSession(username, key)
#' GetSchedule(sn, 'PNCMP')
#' GetSchedule(sn, 'PNCMP', start=5, end=10)
#' GetSchedule(sn, 'WAAMV', level='playoff')
#' GetSchedule(sn, 'PNCMP', team=4911, end=25)
GetSchedule <- function (session, event, level = 'qual', team = NULL,
                         start = NULL, end = NULL, expand_rows = FALSE) {
  # Check for prohibited combinations of arguments
  # Not required because GetSchedule has no prohibited combinations.
  
  # Build URL
  sched_args <- list(tournamentLevel = level, teamNumber = team, start = start,
                     end = end)
  url <- .AddHTTPArgs(paste("schedule", event, sep = "/"), sched_args)
  
  # Send HTTP request
  sched <- .GetHTTP(session, url)
  
  if(session$format != 'data.frame') return(sched)  
  
  # Delete 'Schedule.' from the beginning of column names.
  names(sched) <- .TrimColNames(names(sched))
  
  # The FIRST API returns nested schedule data nested data. The remainder of 
  # this function is necessary to either extract the nested data into new
  # columns or to add rows so that the scheule data can be saved as csv data.
  
  if(expand_rows) {
    # Add columns for team number, station, and surrogate
    sched['teamNumber'] <- vector(mode = "integer", length = nrow(sched))
    sched['station'] <- vector(mode = "character", length = nrow(sched))
    sched['surrogate'] <- vector(mode = "logical", length = nrow(sched))
    
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
    
    # Transform categorical columns into factors.
    sched <- .FactorColumns(sched, c("teamNumber", "station", "field",
                                     "tournamentLevel"))
    
  } else {
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
    }
  return(sched)
}


#  GetHybridSchedule() =========================================================
#' Get the match schedule and results.
#' 
#' For matches that have been played, returns the teams assigned to the match
#' and the match results. If the mtach has not yet been played, the assigned
#' teams and schedule data is returned, but the resutls fields are blank.
#' 
#' See the \emph{Hybrid Schedule} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/schedule/event/level/
#' hybrid?start=start&end=end}
#' 
#' @param session Session A session list created with \code{GetSession()}.
#' @param event Character The FIRST API event code.
#' @param level Character Either \code{qual} or \code{playoff}. Defaults to
#'   \code{qual}. 
#' @param expand_rows Logical Defaults to FALSE. If FALSE, the dataframe will
#'   include one row for each scheduled match, with a different column for each
#'   team. If TRUE, there will be six rows for each match, with each row listing
#'   one assigned team and their station. Optional
#'   
#' @return A data.frame, json list, or xml list.
#'    data.frame column names and classes:
#'      description: character
#'      tournamentLevel: factor
#'      matchNumber: integer      
#'      startTime: character
#'      actualStartTime: character
#'      
#'      If expand.rows == FALSE
#'        scoreRedFoul, scoreRedAuto, scoreRedFinal: integer
#'        scoreBlueFoul, scoreBlueAuto, scoreBlueFinal: integer
#'        Red1.team, Red2.team, Red3.team: factor
#'        Blue1.team, Blue2.team, Blue3.team: factor
#'        Red1.surrogate, Red2.surrogate, Red3.surrogate: logical
#'        Blue1.surrogate, Blue2.surrogate, Blue3.surrogate: logical
#'        Red1.dq, Red2.dq, Red3.dq, Blue1.dq, Blue2.dq, Blue3.dq: logical
#'      If expand.rows == TRUE
#'        teamNumber: factor
#'        station: factor (Red1, Red2, Red3, Blue1, Blue2, Blue3)
#'        scoreFinal, scoreAuto, scoreFoul: integer
#'        surrogate: logical
#'        dq: logical
#'        
#' @export
#' 
#' @examples
#' sn <- GetSession(username, key)
#' GetHybridSchedule(sn, event = "ORPHI)
#' GetHybridSchedule(sn, level = "playoff", start = 3, end = 6)
GetHybridSchedule <- function(session, event, level = 'qual', start = NULL,
                              end = NULL, expand_rows = FALSE) {
  # Check for prohibited combinations of arguments
  # Not required because GetSchedule has no prohibited combinations.
  
  # Build URL
  sched_args <- list(start = start, end = end)
  url <- .AddHTTPArgs(paste("schedule", event, level, "hybrid", sep = "/"),
                      sched_args)
  
  # Send HTTP request
  sched <- .GetHTTP(session, url)
  
  if(session$format != 'data.frame') return(sched)  
  
  # Delete 'Schedule.' from the beginning of column names.
  names(sched) <- .TrimColNames(names(sched))
  
  # The FIRST API returns nested schedule data nested data. The remainder of 
  # this function is necessary to either extract the nested data into new
  # columns or to add rows so that the scheule data can be saved as csv data.
  
  if(expand_rows) {
    # Add columns for team number, station, and surrogate
    sched['teamNumber'] <- vector(mode = "integer", length = nrow(sched))
    sched['station'] <- vector(mode = "character", length = nrow(sched))
    sched['surrogate'] <- vector(mode = "logical", length = nrow(sched))
    sched['disqualified'] <- vector(mode = "logical", length = nrow(sched))
    
    # Add combined scores columns
    sched['scoreFinal'] <- vector(mode = "integer", length = nrow(sched))
    sched['scoreFoul'] <- vector(mode = "integer", length = nrow(sched))
    sched['scoreAuto'] <- vector(mode = "integer", length = nrow(sched))
    
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
          score <- 'scoreRed'
        else
          score <- 'scoreBlue'
        sched$scoreFinal[[mrow]] <- sched[[paste(score, 'Final', sep="")]][[mrow]]
        sched$scoreFoul[[mrow]] <- sched[[paste(score, 'Foul', sep = "")]][[mrow]]
        sched$scoreAuto[[mrow]] <- sched[[paste(score, 'Auto', sep = "")]][[mrow]]
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
    
  } else {
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
  }
  return(sched)
  
}


#  GetMatchResults() ============================================================
#' Get Match results
#' 
#' See the \emph{Match Results} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/matches/event
#' ?tournamentLevel=level&teamNumber=team&matchNumber=match&start=start&end=end}
#'
#' @param session Session A session list created with \code{GetSession()}.
#' @param event Character A FIRST API event code (see \code{GetEvents()}).
#' @param level Character Either \code{qual} or \code{playoff}. Defaults to
#'   \code{qual}.
#' @param team Integer Team number. Optional. Returns only results for
#'   matches in wich the team participated. Cannot specify \code{match} when
#'   \code{team} is specified.
#' @param match Integer Returns results for only the specified match. Must
#'   specify level when \code{match} is specified. Cannot specify \code{team}
#'   when \code{match} is specified.
#' @param start Integer Eearliest match to return. Optional. Must specify level
#'   when \code{start} is specified.
#' @param end Integer Latest match to return. Optional. Must specify level when
#'   \code{end} is specified.
#' @param expand_rows A logical value. Optional, defaults to FALSE. If FALSE, the
#'   dataframe will include one row for each completed match, with a different
#'   column for each team. If TRUE, there will be six rows for each match, with
#'   each row listing one assigned team and their station.
#'
#' @return A data.frame, json list, or xml list.
#'    data.frame column names and classes:
#'      actualStartTime: character
#'      description: character
#'      tournamentLevel: factor
#'      matchNumber: integer
#'      postResultTime: character
#'      
#'      If expand_rows == FALSE
#'        scoreRedFinal, scoreRedAuto, scoreRedFoul: integer
#'        scoreBlueFinal, scoreBlueAuto, scoreBlueFoul: integer
#'        Red1.team, Red2.team, Red3.team: factor
#'        Blue1.team, Blue2.team, Blue3.team: factor
#'        Red1.surrogate, Red2.surrogate, Red3.surrogate: logical
#'        Blue1.surrogate, Blue2.surrogate, Blue3.surrogate: logical
#'      If expand.rows == TRUE
#'        teamNumber: factor
#'        station: factor (Red1, Red2, Red3, Blue1, Blue2, Blue3)
#'        surrogate: logical
#'        scoreFinal, scoreAuto, scoreFoul: integer
#'        
#' @export
#'
#' @examples
#' sn <- GetSession(username, key)
#' GetMatchResults(sn, 'PNCMP', level='qual')
#' GetMatchResults(sn, 'PNCMP', team='2990')
#' GetMatchResults(sn, 'WAAMV', match=2, level='playoff')
#' GetMatchResults(sn, 'CMP-ARCHIMEDES', level='qual', start=10, end=20)
GetMatchResults <- function(session, event, level = NULL, team = NULL,
                            match = NULL, start = NULL, end = NULL,
                            expand_rows = FALSE) {
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
  matches <- .GetHTTP(session, url)

  if(session$format != 'data.frame') return(matches)
  
  # Delete 'Matches.' from the beginning of column names.
  names(matches) <- substr(names(matches), 9, 100) 
  
  # The FIRST API returns nested schedule data. The remainder of this function
  # is necessary to extract the nested data into new rows so that the scheule
  # data can be saved as csv data.
  if(expand_rows) {
    # Add columns for each operating station
    matches['teamNumber'] <- vector(mode = "integer", length = nrow(matches))
    matches['station'] <- vector(mode = "character", length = nrow(matches))
    matches['disqualified'] <- vector(mode = "logical", length = nrow(matches))
    
    # Add combined scores columns
    matches['scoreFinal'] <- vector(mode = "integer", length = nrow(matches))
    matches['scoreFoul'] <- vector(mode = "integer", length = nrow(matches))
    matches['scoreAuto'] <- vector(mode = "integer", length = nrow(matches))
    
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
          score <- 'scoreRed'
        else
          score <- 'scoreBlue'
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
  } else {
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
  }
  
  # Convert categorical data into factors
  matches$tournamentLevel <- factor(matches$tournamentLevel)
  
  return(matches)
}


#  GetScores ====================================================================
#' Get detailed match scores.
#' 
#' The results vary depending on the season requested. The 2016 data fields are
#' listed here. See the FIRST API documentation for data fields for prior
#' seasons. The data frame contains two rows for each match, one for blue
#' and the other for red.
#'
#' See the \emph{Detailed Scores} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/matches/event/level?
#' teamNumber=team&matchNumber=match&start=start&end=end}
#' 
#' @param session Session A session list created with \code{GetSession()}.
#' @param event Character A FIRST API event code (see \code{GetEvents()}).
#' @param level Character Either \code{qual} or \code{playoff}. Defaults to
#'   \code{qual}.
#' @param team Integer Team number. Optional. Returns only results for
#'   matches in wich the team participated. Cannot specify \code{match} when
#'   \code{team} is specified.
#' @param match Integer Returns results for only the specified match. Must
#'   specify level when \code{match} is specified. Cannot specify \code{team}
#'   when \code{match} is specified.
#' @param start Integer Eearliest match to return. Optional. Must specify level
#'   when \code{start} is specified.
#' @param end Integer Latest match to return. Optional. Must specify level when
#'   \code{end} is specified.
#'   
#' @return A data.frame, json list, or xml list.
#'    data.frame column names and classes (2016):
#'      matchLevel: character
#'      matchNumber: integer
#'      audienceGroup: character
#'      alliance: character
#'      robot1Auto, robot2Auto, robot3Auto: character
#'      autoBouldersLow, autoBouldersHigh: integer
#'      teleopBouldersLow, teleopBouldersHigh: integer
#'      towerFaceA, towerFaceB, towerFaceC: character
#'      towerEndStrength: integer
#'      teleopTowerCaptured, teleopDefensesBreached: logical
#'      position1, position2, position3, position4, position5: character
#'      position1Crossings, position2Crossings, position3Crossings,
#'        position4Crossings, position5Crossings: integer
#'      foulCount, techFoulCount: integer
#'      autoPoints, autoReachPoints, autoCrossingPoints,
#'        autoBoulderPoints: integer
#'      teleopPoints, teleopCrossingPoints, teleopBoulderPoints,
#'        teleopChallengePoints, teleopScalePoints: integer
#'      breachPoints, capturePoints: integer
#'      adustPoints, foulPoints, totalPoints: integer
#'      
#' @export
#' 
#' @examples
#' sn <- GetSession(username, key)
#' GetScores(sn, event = "ARCHIMEDES")
#' GetScores(sn, event = "WAELL", start = 1, end = 10)
#' GetScores(sn, event = "WAELL", match = 15)
GetScores <- function(session, event, level = 'qual', team = NULL,
                              match = NULL, start = NULL, end = NULL) {
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
  scores <- .GetHTTP(session, url)
  
  if(session$format != 'data.frame') return(scores)
  
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
  
  return(scores)
}


#  GetAlliances() ==============================================================
#' Get playoff alliances
#' 
#' Returns a list of playoff alliances, including the alliance captains and
#' teams that were selected for the each alliance.
#' 
#' See the \emph{Event Alliances} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/alliances/event}
#'
#' @param session Session A session list created with \code{GetSession()}.
#' @param event Character Case insensitive event code (see \code{GetEvents()}).
#'
#' @return A data.frame, json list, or xml list.
#'    data.frame column names and classes:
#'      number: integer
#'      name: character
#'      captain, round1, round2, round3: integer
#'      backup, backupReplaced: integer
#'      count: integer
#'    
#' @export
#'
#' @examples
#' sn <- GetSession(username, key)
#' GetAlliances(sn, 'WAAMV')
GetAlliances <- function (session, event) {
  # Assemble URL
  url <- paste('alliances/', event, sep="")
  
  # Send HTTP request
  alliances <- .GetHTTP(session, url)
  
  # Skip further processing if result is not a data frame.
  if(session$format != 'data.frame') return(sched)
  
  # Remove prefix from column names.
  names(alliances) <- .TrimColNames(names(alliances))
  
  return(alliances)
}


#  GetRankings() ===============================================================
#' Get team rankings
#' 
#' The results vary depending on the season requested. The 2016 data fields are
#' listed here. See the FIRST API documentation for data fields for prior
#' seasons.
#' 
#' See the \emph{Event Rankings} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/rankings/event?
#' teamNumber=team&top=top}
#'
#' @param session Session A session list created with \code{GetSession()}.
#' @param event Character, Case insensitive event code (see \code{GetEvents()}).
#' @param team
#' @param session Session A session list created with \code{GetSession()}.
#' @param event Character Case insensitive event code (see \code{GetEvents()}).
#' @param team Integer team number. Optional
#' @param top Integer Number of teams to return.
#'
#' @return A data.frame, json list, or xml list.
#'    data.frame column names and classes (2016):
#'      rank: integer
#'      teamNumber: integer
#'      sortOrder1, sortOrder2, sortOrder3, sortOrder4, sortOrder5,
#'        sortOrder6: integer or numeric.
#'      wins. losses, ties: integer
#'      qualAverage: numeric
#'      dq: integer
#'      matchesPlayed: integer
#' 
#' @export
#'
#' @examples
#' sn <- GetSession(username, key)
#' GetRankings(sn, 'WAAMV')
#' GetRankings(sn, "PNCMP", team = 1983)
#' GetRankings(sn, "ARCHIMEDES", top = 5)
GetRankings <- function (session, event, team = NULL, top = NULL) {
  # Check for unallowed combinations of arguments.
  if(!is.null(team) && !is.null(top))
    stop("You cannot specify both the team and top argument")
  
  # Assemble URL
  rank_args <- list(teamNumber = team, top = top)
  url <- .AddHTTPArgs(paste("rankings", event, sep = "/"), rank_args)
  
  # Send HTTP request and get data.
  rankings <- .GetHTTP(session, url)
  
  # Delete 'Rankings.' from the beginning of column names.
  names(rankings) <- .TrimColNames(names(rankings))
  
  return(rankings)
  
}


#  GetAwards() =================================================================
#' Get the awards that were presented to a team or at an event.
#' 
#' See the \emph{Event Awards} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/awards/event/team}
#'
#' @param session Session A session list created with \code{GetSession()}.
#' @param event Character Case insensitive event code (see \code{GetEvents()}).
#' @param team Integer Team number.
#'
#' @return A data.frame, json list, or xml list.
#'    data.frame column names and classes:
#'      awardId: integer
#'      teamId: integer
#'      eventId: integer
#'      eventDivisionId: logical
#'      eventCode: character
#'      name: character
#'      series: integer
#'      teamNumber: integer
#'      fullTeamName: character
#'      person: character
#'
#' @export
#'
#' @examples
#' sn <- GetSession(username, key)
#' GetAwards(sn, 'PNCMP')
#' GetAwards(sn, team = 360)
#' GetAwards(sn, event = 'PNCMP, 360)
GetAwards <- function(session, event = NULL, team = NULL) {
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
  awards <- .GetHTTP(session, url)
  
  # Remove column name prefix
  names(awards) <- .TrimColNames(names(awards))
  
  return(awards)
}


#  GetAwardsList() =============================================================
#' Get a list of all available awards for a season.
#' 
#' See the \emph{Awards Listing} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/awards/list}
#'
#' @param session Session A list created with \code{GetSession()}.
#'
#' @return A data.frame, json list, or xml list.
#'    data.frame column names and classes:
#'      awardId: integer
#'      eventType: character
#'      description: character
#'      forPerson: logical
#'      
#' @export
#'
#' @examples
#' sn <- GetSession(username, key)
#' GetAwardsList(sn)
GetAwardsList <- function(session) {
  # Assemble URL
  url <- 'awards/list'
  
  # Send HTTP request
  alist <- .GetHTTP(session, url)
  
  # Remove column name prefix
  names(alist) <- .TrimColNames(names(alist))
  
  return(alist)
}


#  .AddHTTPArgs() ==============================================================
#' Add GET parameters to URL string.
#' 
#' @param url Character The portion of the FIRST API URL that follows the
#'   season parameter and forward slash, but is not a GET parameter (i.e.,
#'   before the '?'). For most FIRST API commands, the url string will be the
#'   simple string that identifies the command, e.g., "districts" or "teams".
#'   A few FIRST API commands use path parameters (i.e., Detailed Scores) in
#'   addition to GET parameters -- in such cases the url argument must contain
#'   the path parameters separated by '/'.
#' @param http_args List A list of all GET parameters that will be supplied to
#'   the URL query string. The elements of the list must have a name that
#'   that matches the name of the GET parameter (e.g., teamNumber, eventCode).
#'   Any list elements that are NULL will be skipped.
#'   
#' @return Character A string containing a portion of the FIRST API URL,
#'   starting with the path parameter immediately following the season (not
#'   including the '/') and extending to the end of the URL, including the
#'   GET query string.
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
      res <- paste(res, if(first_arg) '?' else '&', sep = '')
      
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
#' .GetHTTP will thow an error if any HTTP code other than 200 is received in
#' the HTTP response. The error message will include the error code and
#' the error message from the response content. See the Response Codes section
#' of the FIRST API documentation for additional details.
#' 
#' .GetHTTP will also throw an error if session$format = "data.frame" and no
#' records are returned.
#'
#' @param session List A session list created with \code{GetSession()}.
#' @param url Character A partial FIRST API URL. The url argument includes
#' everything to the right of the season. For example,
#'   \code{'events?teamNumber=team&districtCode=district}. 
#'
#' @return Returns either JSON text, an XML::XMLNode object, or an R data frame,
#'   depending on the value of session$format.
#'
#' @examples
#' sn <- GetSession(username, key)
#' .GetHTTP(sn, 'events?teamNumber=2557&districtCode=PNW)
.GetHTTP <- function (session, url) {
  # Build Full URL
  url <- paste(if(session$staging) .staging_url else .production_url,
                    .version, session$season, url, sep="/")
  
  # Create authorization and format headers
  raw.token <- paste(session$username, session$key, sep=':')
  token <- base64encode(charToRaw(raw.token))
  auth.header = paste('Basic', token)
  format.header <- switch(tolower(session$format), 'xml' = 'application/xml',
                          'application/json')
  headers <- c(Authorization = auth.header, Accept = format.header)
  
  # Send GET request
  user_agent <- "FRC1318's FIRST_API R Functions, v0.9"
  r <- GET(url, add_headers(.headers = headers), user_agent(user_agent))

  # Check HTTP Response Status Code
  if(status_code(r) != 200) stop(paste(status_code(r), content(r, "text"),
                                       sep=": "))

  # Format results based on session$format setting.
  result <- switch(tolower(session$format),
                   'json' = prettify(content(r, "text")),
                   'xml' = {
                     raw_xml <- (content(r, "text"))
                     xmlRoot(xmlTreeParse(raw_xml, asText = TRUE))
                   },
                   data.frame(fromJSON(content(r, "text"))))
  
  if(session$format == "data.frame" && length(result) == 0)
    stop("No records returned.")

  return(result)
}


#  .TrimColNames() =============================================================
#' Remove prefixes from data frame column names.
#' 
#' .TrimColNames removes all portions of a string up to and including the first
#' period. It's intended to produce shorter column names that are easier to
#' type in R interactive sessions. Column names without periods are left
#' unchanged.
#' 
#' @param col_names Character A character vector of column names, generally
#'   provided by the names() function.
#'   
#' @return Character A character vector of trimmed column names, with the same
#' length as the col_names argument.
#' 
#' @examples
#' names(rankings) <- .TrimColNames(names(rankings))
.TrimColNames <- function(col_names) {
  sub("\\w+\\.", "", col_names, perl = TRUE)
}


#  .FactorColumns() ============================================================
#' Convert character columns to factors.
#' 
#' There are benefits to converting data frame columns with a limited number of
#' unique character values to factors. Factors are conceptually similar to
#' enumerations in other programming languages.
#' 
#' @param df Data.frame The data.frame that will have it's columns converted to
#'   factors.
#' @param cols Character A vector of column names that identifies the columns
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
