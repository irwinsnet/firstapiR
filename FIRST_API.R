# FIRST API OVERVIEW ===========================================================
#' FIRST API Functions
#' 
#' These functions will connect to the FIRST API server and download data on 
#' FIRST Robotics Competition (FRC) teams, events, match results, and awards.
#' 
#' The FIRST API accepts formatted hypertext transfer protocol (HTTP) GET 
#' requests for FRC data and provides results in either javascript object 
#' notation (json) or extensible markup language (xml) format. Detailed 
#' documentation for the FIRST API, including precise rules for constructing the
#' HTTP requests, is available at \url{http://docs.frcevents2.apiary.io/#}
#' 
#' A username and authorization key are required for connecting to the FIRST
#' API. To obtain a username and key, join the FIRST Community Developers
#' project on TeamForge at
#' \url{https://usfirst.collab.net/sf/projects/first_community_developers/}
#' 
#' These functions return R dataframes by default. Optionally, the functions
#' can also return the raw JSON or XML that is provided by the FIRST API. See
#' the apiary documentation (\url{http://docs.frcevents2.apiary.io/#}) for a
#' detailed description of all response data fields.
#' 
#' These functions use version 2.0 of the FIRST API. They have not been tested
#' with version 1.0.


#todo: Extract add optional parameters code to private function??
#todo: Check results from all functions and shorten compound column names
# where appropriate.
#todo: Set appropriate columns to factors.

library("base64enc")
library("httr")
library("jsonlite")
library("XML")

# Server URLs and other parameters
.staging_url = "https://frc-staging-api.firstinspires.org"
.production_url = "https://frc-api.firstinspires.org"
.version = "v2.0"


# GetSession() =================================================================
#' Create a FIRST API session list.
#' 
#' Every FIRST API function requires a session list as it's first parameter.
#' 
#' The session list returned by this function contains a named element for each
#' required or optional paramter. The name is the same as the parameter name.
#'
#' @param username The username assigned by FIRST.
#' @param key The authorization key assigned by FIRST.
#' @param season The 4-digit year. Defaults to 2016. Must be a 4-digit number
#'   that is equal to or less than the current year and greater than or equal to
#'   2015.
#' @param format The data format that will be returned by the functions. Can be
#'   "json", "data.frame", or "xml". Defaults to "data.frame". Case insensitive.
#' @param staging Set to TRUE to use the staging URL. Defaults to \code{FALSE}.
#'
#' @return A list containing all GetSession parameters.
#' @export
#'
#' @examples
#' sn <- GetSession('myUserName', 'myAuthorizationKey)
#' sn <- GetSession('myUserName', 'myAuthorizationKey, season = 2015)
#' sn$format <- 'xml'
GetSession <- function(username, key,
                  season = 2016,
                  format = 'data.frame',
                  staging = FALSE){
  session <- list(username = username,
                  key = key,
                  staging = staging,
                  season = season,
                  format = format)
}


# GetSeason() ==================================================================
#' Get high-level information on a particular FRC season.
#' 
#' Returns information for the season specified in the session list (see
#' documentation for the GetSession function for additional details.)
#' 
#' See the \emph{Season Summary} section of the FIRST API documentation for
#' additional details. The URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season}
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
#'  @exp
#' 
#'  @examples
#'  sn <- GetSession(username, key, season = 2015)
#'  summary <- GetSeason(sn)
GetSeason <- function(session) {
  .GetHTTP(session, "")
}


# GetDistricts() ===============================================================
#' Get a list of FIRST districts.
#' 
#' This function returns a list of all current districs, including their titles 
#' and codes. District codes are used as parameters for several other FIRST API
#' functions.
#' 
#' See the \emph{District Listings} section of the FIRST API documentation. The 
#' URL format is: \code{https://frc-api.firstinspires.org/v2.0/season/districts}
#' 
#' @param session A session list created with \code{GetSession()}.
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
  url <- 'districts'
  districts <- .GetHTTP(session, url)
  
  # Skip rest of function for XML or JSON results
  if(sn$format != "data.frame") return(districts)
  
  # Shorten the column names to reduce amount of typing required.
  names(districts) <- .TrimColNames(names(districts))
  
  return(districts)
}


# GetEvents() ==================================================================
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
#' that occurred within the specified district. If \code{district} is set to
#' \code{FALSE}, no district events will be included in the results (setting
#' \code{district} to \code{FALSE} corresponds to setting excludeDistrict to
#' \code{TRUE} in the HTTP request).
#' 
#' @param session A session list created with \code{GetSession()}.
#' @param team Four digit team number. Optional
#' @param district The district code (see \code{GetDistricts()}). Optional.
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
                      district = NULL, excludeDistrict = NULL) {
  # Check for unallowed combinations of arguments.
  if(!is.null(event) && (!is.null(team) || !is.null(district) ||
                              !is.null(excludeDistrict)))
    stop("If you specify an event, you cannot specify any other arguments.")
  if(!is.null(district) && !is.null(excludeDistrict))
    stop("You cannot specify both the district and excludeDistrict arguments.")
  
  event_args <- list(eventCode = event, teamNumber = team,
                    districtCode = district, excludeDistrict = excludeDistrict)
  
  url <- .AddHTTPArgs("events", event_args)

  # Send HTTP request
  events <- .GetHTTP(session, url)
  
  # Skip rest of function for XML or JSON results
  if(sn$format != "data.frame") return(events)
  
  # Shorten the column names to reduce amount of typing required.
  names(events) <- .TrimColNames(names(events))

  # Convert categorical coluns to factor data types.
  events <- .FactorColumns(events, c("type", "districtCode", "stateprov",
                                     "country", "timezone"))
  
  return(events)
}


# GetTeams() ===================================================================
#' Get details for many teams.
#' 
#' The FIRST team listing API response will be broken up into several pages if
#' the number of teams in the response exceeds 65, with a separate HTTP request
#' required for each page. If the dataframe format is specified, then
#' \code{GetTeams()} makes this transparent to the user. \code{GetTeams()} will
#' determine the number of pages in the response, conduct an HTTP request to
#' obtain each page, and merge all pages into a single dataframe. For the xml
#' and json formats, \code{GetTeams()} will return a list of xml or json
#' responses.
#' 
#' See the \emph{Team Listings} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/teams&eventCode=event
#' ?districtCode=district?state=state?page=2}
#'
#' @param session A session list created with \code{GetSession()}.
#' @param event A FIRST API event code (see \code{GetEvents()}). If event is
#'   specified, \code{GetTeams()} will filter results to all teams
#'   particpating in the specified event. Optional.
#' @param district The FIRST API district code (see \code{GetDistricts()}). If
#'   disctrict is specified, \code{GetTeams()} will filter results to all teams
#'   in the specified district.
#' @param state A state name, spelled out entirely (i.e., 'Idaho', \emph{not}
#'   'ID'). If state is specified, \code{GetTeams()} will filter results to all
#'   teams in the specified state.
#'
#' @return If the data.frame format is specified (i.e., \code{session$format ==
#'   'data.frame'}), returns all teams in a single data frame. If the json or
#'   xml formates are specified, returns a list of json or xml responses, with
#'   the length of the list being the same as the number of pages in the
#'   response.
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
    stop("If you specify a team, you cannot specify any other arguments.")
  
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
  if(sn$format != "data.frame") return(teams[[1]])
  
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
    if(sn$format == 'data.frame') {
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


# GetSchedule() ================================================================
#' Get the match schedule for a specific event.
#' 
#' See the \emph{Event Schedule} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/schedule/event?
#' tournamentLevel=level&teamNumber=team&start=start&end=end}
#'
#' @param session A session list created with \code{GetSession()}.
#' @param event The FIRST API event code.
#' @param level Either \code{qual} or \code{playoff}. Defaults to \code{qual}.
#' @param expand.rows A logical value. Optiona, defaults to FALSE. If FALSE, the
#'   dataframe will include one row for each scheduled match, with a different
#'   column for each team. If TRUE, there will be six rows for each match, with
#'   each row listing one assigned team and their station.
#'
#' @return A data.frame, json list, or xml list.
#'    data.frame column names and classes:
#'      description: character
#'      field: factor
#'      tournamentLevel: factor
#'      startTime: character
#'      matchNumber: integer
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
                         start = NULL, end = NULL, expand.rows = FALSE) {
  # Build URL
  url <- paste('schedule/', event, '?tournamentLevel=', level, sep="")
  
  # Add optional arguments
  if(!is.null(team)) url <- paste(url, '&teamNumber=', team, sep = '')
  if(!is.null(start)) url <- paste(url, '&start=', start, sep = '')
  if(!is.null(end)) url <- paste(url, '&end=', end, sep = '')
  
  # Send HTTP request
  sched <- .GetHTTP(session, url)
  
  if(session$format != 'data.frame') return(sched)  
  
  # Delete 'Schedule.' from the beginning of column names.
  names(sched) <- substr(names(sched), 10, 100)
  
  # The FIRST API returns nested schedule data nested data. The remainder of 
  # this function is necessary to either extract the nested data into new
  # columns or to add rows so that the scheule data can be saved as csv data.
  
  if(expand.rows) {
    # Add columns for team number, station, and surrogate
    sched['teamNumber'] <- vector(mode = "integer", length = nrow(sched))
    sched['station'] <- vector(mode = "character", length = nrow(sched))
    sched['surrogate'] <- vector(mode = "logical", length = nrow(sched))
    
    # Extract teams and delete nested teams column.
    teams <- sched$Teams
    sched$Teams <- NULL
    
    # Expand the matches data frame so there are six rows per match.
    xSched <- sched[sort(rep(1:nrow(sched), 6)), ]
    
    # Fill in team and station data.
    for(mtch in 1:length(teams)) {
      for(tm in 1:6) {
        mrow <- (mtch-1)*6 + tm
        xSched$teamNumber[[mrow]] <- teams[[mtch]][["teamNumber"]][[tm]]
        xSched$station[[mrow]] <- teams[[mtch]][["station"]][[tm]]
        xSched$surrogate[[mrow]] <- teams[[mtch]][["surrogate"]][[tm]]
      }
    }
    
    xSched$teamNumber <- factor(xSched$teamNumber)
    xSched$station <- factor(xSched$station)
    
    sched <- xSched
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
    for(col in cols) {
      cname.team <- paste(col, "team", sep = ".")
      sched[[cname.team]] <- factor(sched[[cname.team]])
    }
  }
  # Convert categorical columns to factors
  sched$field <- factor(sched$field)
  sched$tournamentLevel <- factor(sched$tournamentLevel)
  
  return(sched)
}


# GetMatchResults() ============================================================
#' Get Match results
#' 
#' Returns six rows for each match, one row for each team that participated
#' in the match.
#' 
#' See the \emph{Match Results} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/matches/event
#' ?tournamentLevel=level&teamNumber=team&matchNumber=match&start=start&end=end}
#'
#' @param session A session list created with \code{GetSession()}.
#' @param event A FIRST API event code (see \code{GetEvents()}).
#' @param level Either \code{qual} or \code{playoff}. Defaults to \code{qual}.
#' @param team Four digit team number. Optional. Returns only results for
#'   matches in wich the team participated. Cannot specify \code{match} when
#'   \code{team} is specified.
#' @param match An integer representing the match number. Optional. Returns
#'   results for only the specified match. Must specify level when \code{match}
#'   is specified. Cannot specify \code{team} when \code{match} is specified.
#' @param start An intger representing the earliest match to return. Optional.
#'   Must specify level when \code{start} is specified.
#' @param end An intger representing the latest match to return. Optional.
#'   Must specify level when \code{end} is specified.
#' @param expand.rows A logical value. Optiona, defaults to FALSE. If FALSE, the
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
#'      scoreRedFinal: integer
#'      scoreRedFoul: integer
#'      scoreRedAuto: integer
#'      scoreBlueFinal: integer
#'      scoreBlueFoul: integer
#'      scoreBlueAuto: integer
#'      postResultTime: character
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
                            expand.rows = FALSE) {
  # Check for unallowed combinations of arguments.
  if((!is.null(match) || !is.null(start) || !is.null(end)) && is.null(level))
    stop("You must specify the level when you specify match, start, or end.")
  if(!is.null(team) && !is.null(match))
    stop("You cannot specify both a team and match number.")
  if(!is.null(match) && (!is.null(start) || !is.null(end)))
    stop("You cannot specify start or end if you specify match.")

  # Assemble URL
  url <- paste('matches', event, sep='/')
  
  # Add optional parameters
  if(!is.null(level)) {
    url <- paste(url, if(first_arg) '?' else '&', sep = '')
    url <- paste(url, 'tournamentLevel=', team, sep = '')
    first_arg <- FALSE
  }
  if(!is.null(team)) {
    url <- paste(url, if(first_arg) '?' else '&', sep = '')
    url <- paste(url, 'teamNumber=', team, sep = '')
    first_arg <- FALSE
  }
  if(!is.null(match)) {
    url <- paste(url, if(first_arg) '?' else '&', sep = '')
    url <- paste(url, 'matchNumber=', team, sep = '')
    first_arg <- FALSE
  }
  if(!is.null(start)) {
    url <- paste(url, if(first_arg) '?' else '&', sep = '')
    url <- paste(url, 'start=', team, sep = '')
    first_arg <- FALSE
  }
  if(!is.null(end)) {
    url <- paste(url, if(first_arg) '?' else '&', sep = '')
    url <- paste(url, 'end=', team, sep = '')
    first_arg <- FALSE
  }
  
  # Send HTTP request and get data.
  matches <- .GetHTTP(session, url)

  if(session$format != 'data.frame') return(matches)
  
  # Delete 'Matches.' from the beginning of column names.
  names(matches) <- substr(names(matches), 9, 100) 
  
  # The FIRST API returns nested schedule data. The remainder of this function
  # is necessary to extract the nested data into new rows so that the scheule
  # data can be saved as csv data.
  if(expand.rows) {
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


# GetScores ====================================================================

GetScores <- function(session, event, level = 'qual', team = NULL,
                              match = NULL, start = NULL, end = NULL) {
  # Check for unallowed combinations of arguments.
  if(!is.null(team) && !is.null(match))
    stop("You cannot specify both a team and match number.")
  if(!is.null(match) && (!is.null(start) || !is.null(end)))
    stop("You cannot specify start or end if you specify match.")
  
  # Assemble URL
  url <- paste('scores', event, level, sep='/')
  
  # Add optional parameters
  if(!is.null(team)) {
    url <- paste(url, if(first_arg) '?' else '&', sep = '')
    url <- paste(url, 'teamNumber=', team, sep = '')
    first_arg <- FALSE
  }
  if(!is.null(match)) {
    url <- paste(url, if(first_arg) '?' else '&', sep = '')
    url <- paste(url, 'matchNumber=', team, sep = '')
    first_arg <- FALSE
  }
  if(!is.null(start)) {
    url <- paste(url, if(first_arg) '?' else '&', sep = '')
    url <- paste(url, 'start=', team, sep = '')
    first_arg <- FALSE
  }
  if(!is.null(end)) {
    url <- paste(url, if(first_arg) '?' else '&', sep = '')
    url <- paste(url, 'end=', team, sep = '')
    first_arg <- FALSE
  }
  
  # Send HTTP request and get data.
  scores <- .GetHTTP(session, url)
  
  if(session$format != 'data.frame') return(scores)
  
  # Delete 'MatcheScores.' from the beginning of column names.
  names(scores) <- substr(names(scores), 13, 100)
  
  xScores <- scores[sort(rep(1:nrow(scores), 2)), ]
  
  xScores
  
  # alliances <- scores$Alliances
  # Although it appears to be a data.frame, alliances is actually a list of 
  # separate data.frames, each consisting of a single column, and each column is
  # a generic list instead of an atomic vector. This is due to how the
  # 'jsonlite' package converts nested json text to data.frames. The following
  # code converts alliances to a single data.frame consisting of atomic vectors.
  # all.df <- data.frame()
  # for(col in 1:length(alliances[[1]]))
  #   all.df[col] <- unlist(alliances[[1]][col])

  # for(col in 1:length(alliances[[1]])) {
  #   col_name <- names(alliances[[1]][col])
  #   col_mode <- if(is.integer(alliances[[1]][col, 1])) 'integer' else 'character'
  #   col_mode <- 'integer'
  #   xScores[col_name] <- vector(mode = col_mode, length = nrow(xScores))
  # }
  # xScores$alliance <- factor(c('Red', 'Blue'))
  
  # all.df
  #xScores
}


# GetAlliances() ===============================================================
#' Get playoff alliances
#' 
#' Returns a list of playoff alliances, including the alliance captains and
#' teams that were selected for the each alliance.
#' 
#' See the \emph{Event Alliances} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/alliances/event}
#'
#' @param session A session list created with \code{GetSession()}.
#' @param event Case insensitive event code (see \code{GetEvents()}).
#'
#' @return A data frame, XML text, or JSON text, depending on the
#' sesssion$format setting.
#' @export
#'
#' @examples
#' sn <- GetSession(username, key)
#' GetAlliances(sn, 'WAAMV')
GetAlliances <- function (session, event) {
  # Assemble URL
  url <- paste('alliances/', event, sep="")
  
  # Send HTTP request
  .GetHTTP(session, url)
}


# GetAwards() ==================================================================
#' Get the awards that were presented to a team or at an event.
#' 
#' See the \emph{Event Awards} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/awards/event/team}
#'
#' @param session A session list created with \code{GetSession()}.
#' @param event Case insensitive event code (see \code{GetEvents()}).
#' @param team Four digit team number.
#'
#' @return A data frame, XML text, or JSON text, depending on the
#' sesssion$format setting.
#' @export
#'
#' @examples
#' sn <- GetSession(username, key)
#' GetAwards(sn, 'PNCMP')
#' GetAwards(sn, team = 360)
#' GetAwards(sn, 'PNCMP, 360)
GetAwards <- function(session, event = NULL, team = NULL) {
  # Assemble URL
  url <- 'awards'
  
  # Add optional parameters
  first_arg <- TRUE
  if(!is.null(event)) {
    url <- paste(url, event, sep = '/')
    first_arg <- FALSE
  }
  if(!is.null(team)) {
    url <- paste(url, team, sep = '/')
    first_arg <- FALSE
  }
  
  # Send HTTP request
  .GetHTTP(session, url)
}


# GetAwardsList() ==============================================================
#' Get a list of all available awards for a season.
#' 
#' See the \emph{Awards Listing} section of the FIRST API documentation. The
#' URL format is:
#' \code{https://frc-api.firstinspires.org/v2.0/season/awards/list}
#'
#' @param session A session list created with \code{GetSession()}.
#'
#' @return A data frame, XML text, or JSON text, depending on the
#' sesssion$format setting.
#' @export
#'
#' @examples
#' sn <- GetSession(username, key)
#' GetAwardsList(sn)
GetAwardsList <- function(session) {
  # Assemble URL
  url <- 'awards/list'
  
  # Send HTTP request
  .GetHTTP(session, url)
}


# .AddHTTPArgs() ==================================================================
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


# .GetHTTP() ===================================================================
#' Send an HTTP request
#' 
#' .GetHTTP is an internal function that is not intended to be called by the
#' user. .GetHTTP is called by other FIRST API methods. 
#' 
#' .GetHTTP will thow an error if any HTTP code other than 200 is received in
#' the HTTP response:
#'    400: Incorrect URL syntax
#'    401: Invalid Authorization Header
#'    404: Nonexistent Event Code
#'    500: Internal Server Error
#'    501: 501: Request Did Not Match API Pattern
#'    503: Server is Temporarily Unavailable
#'
#' @param session A session list created with \code{GetSession()}.
#' @param url A partial FIRST API URL. The url argument includes everything to
#'   the right of the season. For example,
#'   \code{'events?teamNumber=team&districtCode=district}.
#'
#' @return Returns either JSON text, an XML::XMLNode object, or an R data frame,
#'   depending on the value of session$format.
#' @export
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
  r <- GET(url, add_headers(.headers = headers))

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

  return(result)
}


# .TrimColNames() ==============================================================
.TrimColNames <- function(col_names) {
  sub("\\w+\\.", "", col_names, perl = TRUE)
}


# .FactorColumns() =============================================================
.FactorColumns <- function(df, cols) {
  for(fc in cols)
    df[[fc]] <- factor(df[[fc]])
  return(df)
}
