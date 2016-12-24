# firstapiR_util.R version 2.0.0================================================
# Contains functions that manipulate data frames containing data downloaded
# from the FIRST API server.


#  ToMatchShape()===============================================================
#' Convert a Schedule, Hybrid, or MatchResults data frame to match shape.
#'
#' The firstapiR functions \code{GetSchedule()}, \code{GetHybridSchedule()}, and
#' \code{GetMatchResults} return a data frame in team shape, i.e., with one team
#' listed in every row, and six rows per match. R documentation often refers to
#' data frames in this format as \emph{narrow} data frames. \code{ToMatchShape}
#' will convert these narrow data frames to a wide data frame (it's wide because
#' it has more columns) with all six teams listed in one row and only one row
#' per match.
#'
#' ToMatchShape will only convert data frames that are in team shape, with their
#' shape attribute set to "team". To convert a data frame that is in alliance
#' shape to match shape, first use \code{ToTeamShape()} to convert the data
#' frame back to team shape, and then use \code{ToMatchShape()} to convert it to
#' match shape.
#'
#' FirstapiR proivdes functions to reshape data frames because different shapes
#' are useful for different purposes. For example, team shape is useful for
#' extracting data for a single team because it's only necessary to subset the
#' data frame on the team column. Match shape is useful for displaying
#' schedules. Alliance shape is useful for calculating offensive power rating
#' (OPR).
#'
#' This function throws an error if df does not inherit from
#' \emph{matchResults}, \emph{schedule}, or \emph{hybridSchedule}, or if the
#' data frame's shape attribute is missing or is anything other than
#' \emph{team}.
#'
#' @param df A firstapiR data frame. The data frame must inherit from the
#'   \emph{schedule}, \emph{hybridSchedule}, or emph{matchResults} class data
#'   frames that are returned by firstapiR functions. The data frame must also
#'   be in team shape and have the shape attribute set to "team".
#'
#' @return A data frame of class \emph{matchResults}, \emph{schedule}, or
#'   \emph{hybridSchedule} (depending on the class of the df argument) in match
#'   shape, with the shape attribute set to "match".
#'
#' @seealso \code{\link{GetMatchResults}}, \code{\link{GetHybridSchedule}},
#'   \code{\link{GetSchedule}}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("myUserName", "key", season = 2016)
#' matchResults_teamshape <- GetMatchResults(sn, event = "PNCMP")
#' matchResults_matchshape <- ToMatchShape(matchResults_teamshape)
ToMatchShape <- function(df) {
  # Verify input data frame in team format
  if(!("shape" %in% names(attributes(df))))
    stop("df must have a shape attribute.")
  if(attr(df, "shape") != "team") stop("df must be in team shape.")

  # Check type of input data frame
  if(inherits(df, "Schedule"))
    var.cols <- c("team", "surrogate")
   else if(inherits(df, "MatchResults"))
    var.cols <- c("team", "disqualified", "scoreFinal", "scoreAuto",
                  "scoreFoul")
  else if(inherits(df, "HybridSchedule"))
    var.cols <- c("team", "surrogate", "disqualified", "scoreFinal",
                  "scoreAuto", "scoreFoul")
  else
    stop(paste("The df argument must inherit from 'Schedule', 'HybridSchedule'",
                "or MatchResults'.", sep = ""))

  # From team to alliance format for a schedule
  df$alliance <- NULL
  alli.df <- stats::reshape(df, direction = "wide", idvar = "match",
                     timevar = "station",
                     v.names = var.cols)

  # Arrange Columns
  if(inherits(df, "MatchResults") | inherits(df, "HybridSchedule")) {
    # Remove '1' at end of "score____.(red|blue)1" columns.
    names(alli.df) <- sub("(score\\w+\\.\\D+)1", "\\1", names(alli.df))

    # Set "score____.(red|blue)(2|3)" columns to NULL.
    alli.df[grep("score\\w+\\.\\D+[23]", names(alli.df))] <- NULL
  }

  # Set column order
  att.reshape <- attr(alli.df, "reshapeWide")
  if(inherits(df, "MatchResults"))
    alli.df <- alli.df[, c("match", "description", "level", "actualStart",
                           "postResult", "scoreFinal.Blue",
                           "scoreAuto.Blue", "scoreFoul.Blue", "scoreFinal.Red",
                           "scoreAuto.Red", "scoreFoul.Red", "team.Blue1",
                           "disqualified.Blue1", "team.Blue2",
                           "disqualified.Blue2", "team.Blue3",
                           "disqualified.Blue3", "team.Red1",
                           "disqualified.Red1", "team.Red2",
                           "disqualified.Red2", "team.Red3",
                           "disqualified.Red3")]
  if(inherits(df, "HybridSchedule"))
    alli.df <- alli.df[, c("match", "description", "level", "start",
                           "actualStart", "scoreFinal.Blue",
                           "scoreAuto.Blue", "scoreFoul.Blue", "scoreFinal.Red",
                           "scoreAuto.Red", "scoreFoul.Red", "team.Blue1",
                           "surrogate.Blue1", "disqualified.Blue1","team.Blue2",
                           "surrogate.Blue2", "disqualified.Blue2",
                           "team.Blue3", "surrogate.Blue3",
                           "disqualified.Blue3",  "team.Red1", "surrogate.Red1",
                           "disqualified.Red1",  "team.Red2",
                           "surrogate.Red2", "disqualified.Red2", "team.Red3",
                           "surrogate.Red3", "disqualified.Red3")]
  if(inherits(df, "Schedule"))
    alli.df <- alli.df[, c("match", "description", "level", "field", "start",
                           "team.Blue1", "surrogate.Blue1", "team.Blue2",
                           "surrogate.Blue2", "team.Blue3", "surrogate.Blue3",
                           "team.Red1", "surrogate.Red1", "team.Red2",
                           "surrogate.Red2", "team.Red3", "surrogate.Red3")]
  attr(alli.df, "reshapeWide") <- att.reshape

  # Set row names to match number
  row.names(alli.df) <- as.character(alli.df$match)

  # Set shape attribute and copy attributes from input data frame.
  attr(alli.df, "shape") <- "match"
  alli.df <- .PreserveAttributes(alli.df, attributes(df))

  # Set row order
  alli.df <- alli.df[order(alli.df$match), ]

  return(alli.df)
}


#  ToAllianceShape()===============================================================
#' Convert a Schedule, Hybrid, or MatchResults data frame to match shape.
#'
#' The firstapiR functions \code{GetSchedule()}, \code{GetHybridSchedule()}, and
#' \code{GetMatchResults} return a data frame in team shape, i.e., with one team
#' listed in every row, and six rows per match. R documentation often refers to
#' data frames in this format as \emph{narrow} data frames. \code{ToMatchShape}
#' will convert these narrow data frames to a wide data frame (it's wide because
#' it has more columns) with three teams per row and two rows per match, one row
#' for the blue alliance and one row for the red alliance.
#'
#' ToMatchShape will only convert data frames that are in team shape, with their
#' shape attribute set to "team". To convert data frame that are in alliance
#' shape to match shape, first use \code{ToTeamShape} to convert the data frame
#' back to team shape, and then use \code{ToMatchShape} to convert it to match
#' shape.
#'
#' FirstapiR proivdes functions to reshape data frames because different shapes
#' are useful for different purposes. For example, team shape is useful for
#' extracting data for a single team because it's only necessary to subset the
#' data frame on the team column. Match shape is useful for displaying
#' schedules. Alliance shape is useful for calculating offensive power rating
#' (OPR).
#'
#' This function throws an error if df does not inherit from
#' \emph{matchResults}, \emph{schedule}, or \emph{hybridSchedule}, or if the
#' data frame's shape attribute is missing or is anything other than
#' \emph{team}.
#'
#' @param df A firstapiR data frame. The data frame must inherit from the
#'   \emph{schedule}, \emph{hybridSchedule}, or emph{matchResults} class data
#'   frames that are returned by firstapiR functions. The data frame must also
#'   be in team shape and have the shape attribute set to "team".
#'
#' @return A data frame of class \emph{schedule}, \emph{hybridSchedule}, or
#'   \emph{matchResults} (depending on the class of the df argument) in match
#'   shape, with the shape attribute set to "alliance".
#'
#' @seealso \code{\link{GetMatchResults}}, \code{\link{GetHybridSchedule}},
#'   \code{\link{GetSchedule}}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("myUserName", "key", season = 2016)
#' matchResults_teamshape <- GetMatchResults(sn, event = "PNCMP")
#' matchResults_allianceshape <- ToAllianceShape(matchResults_teamshape)
ToAllianceShape <- function(df) {
  # Verify input data frame in team format
  if(!("shape" %in% names(attributes(df))))
    stop("df must have a shape attribute.")
  if(attr(df, "shape") != "team") stop("df must be in team shape.")

  # Strip 'Red' or 'Blue' from values in station column.
  stn <- as.character(df$station)
  df$station <- substr(stn, nchar(stn), nchar(stn))

  # Check type of input data frame
  if(inherits(df, "Schedule"))
    var.cols <- c("team", "surrogate")
  if(inherits(df, "MatchResults"))
    var.cols <- c("team", "disqualified")
  if(inherits(df, "HybridSchedule"))
    var.cols <- c("team", "surrogate", "disqualified")

  # Create output data frame in match (wide) format.
  mtch.df <- stats::reshape(df, direction = "wide", idvar = c("match", "alliance"),
                     timevar = "station", v.names = var.cols)

  # Set row names to m.color where m is match number, color is red or blue.
  row.names(mtch.df) <- tolower(paste(mtch.df$match,
                                      mtch.df$alliance, sep = "."))

  # Set shape attribute and copy attributes from input data frame.
  attr(mtch.df, "shape") <- "alliance"
  mtch.df <- .PreserveAttributes(mtch.df, attributes(df))

  # Set the row order of the data frame.
  mtch.df <- mtch.df[order(mtch.df$match, mtch.df$alliance), ]

  return(mtch.df)
}


#  ToTeamShape()===============================================================
#' Convert a Schedule, Hybrid, or MatchResults data frame to team shape.
#'
#' The firstapiR functions \code{GetSchedule()}, \code{GetHybridSchedule()}, and
#' \code{GetMatchResults} return a data frame in team shape, i.e., with one team
#' listed in every row, and six rows per match. R documentation often refers to
#' data frames in this format as \emph{narrow} data frames. The functions
#' \code{ToMatchShape} and \code{ToAllianceShape} will convert data frames in
#' the narrow team shape to the wide (they're called wide because they have more
#' columns) alliance or match shapes. \code{ToTeamShape} converts these wide
#' data frames back to narrow data frames in team shape.
#'
#' \code{ToTeamShape} will only convert data frames that are in match or
#' alliance shape, with their shape attribute set to \emph{match} or
#' \emph{alliance}.
#'
#' FirstapiR proivdes functions to reshape data frames because different shapes
#' are useful for different purposes. For example, team shape is useful for
#' extracting data for a single team because it's only necessary to subset the
#' data frame on the team column. Match shape is useful for displaying
#' schedules. Alliance shape is useful for calculating offensive power rating
#' (OPR).
#'
#' This function throws an error if df does not inherit from
#' \emph{matchResults}, \emph{schedule}, or \emph{hybridSchedule}, or if the
#' data frame's shape attribute is missing or is anything other than
#' \emph{team}.
#'
#' @param df A firstapiR data frame. The data frame must inherit from the
#'   \emph{schedule}, \emph{hybridSchedule}, or \emph{matchResults} class data
#'   frames returned by firstapiR functions. The data frame must also be in team
#'   shape and have the shape attribute set to "match" or "alliance".
#'
#' @return A data frame of class \emph{schedule}, \emph{hybridSchedule}, or
#'   \emph{matchResults} (depending on the class of the df argument) in match
#'   shape, with the shape attribute set to "match".
#'
#' @seealso \code{\link{GetMatchResults}}, \code{\link{GetHybridSchedule}},
#'   \code{\link{GetSchedule}}
#'
#' @export
#'
#' @examples
#' sn <- GetSession("myUserName", "key", season = 2016)
#' matchResults_teamshape <- GetMatchResults(sn, event = "PNCMP")
#' matchResults_matchshape <- ToMatchShape(matchResults_teamshape)
#' matchResults_back_to_teamshape <- ToTeamShape(matchResults_matchshape)
ToTeamShape <- function(df) {
  # Check for valid input
  if(!("shape" %in% names(attributes(df))))
    stop("df must have a shape attribute.")
  if(!(attr(df, "shape") %in% c("alliance", "match")))
    stop("df argument must be a data frame in alliance or match shape")

  # Replace columns for data frames in match shape.
  if((attr(df, "shape") == "match") & (!inherits(df, "Schedule"))) {
    # Add the digit 1 to the end of each score column name.
    score.cols <- grep("score\\w+\\.(Blue|Red)", names(df))
    names(df)[score.cols] <- paste(names(df)[score.cols], 1, sep = "")

    # Add new score columns for stations red2/3 and blue2/3 and copy data from
    #   red1 and blue1 score columns.
    new.cols <- c(sub("(score\\D+)\\d", "\\12" , names(df[score.cols])),
                  sub("(score\\D+)\\d", "\\13" , names(df[score.cols])))
    for(new.col in new.cols) {
      df[[new.col]] <- df[[sub("(score\\D+)[23]", "\\11", new.col)]]
    }
  }

  # Reshape data frame. Assumes df has reshapeWide attribute from prior call to
  #   reshape() function.
  team.df <- stats::reshape(df)

  if(attr(df, "shape") == "match") {
    # Rebuild alliance column
    al <- as.character(team.df$station)
    team.df$alliance <- substr(al, 1, nchar(al) - 1)

  } else if(attr(df, "shape") == "alliance") {
    # Rebuild station column
    stations <- sub("^\\d+\\.\\D+\\.", "", row.names(team.df))
    team.df$station <- paste(df$alliance, stations, sep = "")
  }

  # Set row names to [match].[station]
  row.names(team.df) <- tolower(paste(team.df$match,
                                      team.df$station, sep = "."))

  # Retrieve attributes from input data frame
  team.df <- .PreserveAttributes(team.df, attributes(df))

  # Set column order
  if("Schedule" %in% class(team.df))
  team.df <- .SetColumnOrder(team.df, c("match", "description", "level",
                                        "field", "start", "team", "alliance",
                                        "station", "surrogate"))
  if("HybridSchedule" %in% class(team.df))
    team.df <- .SetColumnOrder(team.df, c("match", "description", "level",
                                          "start", "actualStart", "team",
                                          "alliance", "station", "surrogate",
                                          "disqualified", "scoreFinal",
                                          "scoreAuto", "scoreFoul"))
  if("MatchResults" %in% class(team.df))
    team.df <- .SetColumnOrder(team.df, c("match", "description", "level",
                                          "actualStart", "postResult", "team",
                                          "alliance", "station", "disqualified",
                                          "scoreFinal", "scoreAuto",
                                          "scoreFoul"))

  # Sort, assign shape attribute, and return result.
  team.df <- team.df[order(team.df$match, team.df$station), ]
  attr(team.df, "shape") <- "team"
  return(team.df)
}


# GetAll() =====================================================================
#' Download all data for a specific FRC competition.
#'
#' Downloads all data for a specific FRC competition and returns the data in a
#' list of data frames, or character vectors containing XML or JSON text,
#' depending on the format setting in the session variable. The list's named
#' elements are listed below in the \emph{Value} section.
#'
#' @param session A Session object created with \code{GetSession()}.
#' @param event A character vector containing a FIRST API event code
#'   (see \code{GetEvents}).
#'
#' @return A list containing the following firstapiR elements which are data
#'   frames, XML character vectors, or JSON character vectors:
#'   \enumerate{
#'     \item \emph{season}: Season class. See \link{GetSeason}.
#'     \item \emph{event}: Events class. See \link{GetEvents}.
#'     \item \emph{teams}: Teams class, listing all teams in the competition.
#'       See \link{GetTeams}.
#'     \item \emph{matches_qual}: MatchResults class, for qualification matches.
#'       See \link{GetMatchResults}.
#'     \item \emph{matches_playoff}: MatchResults class, for playoffs. See
#'       \link{GetMatchResults}.
#'     \item \emph{schedule_qual}: Schedule class, for qualification matches.
#'       See \link{GetSchedule}.
#'     \item \emph{schedule_playoff}: Schedule class, for playoffs. See
#'       \link{GetSchedule}.
#'     \item \emph{scores_qual}: Scores class, for qualification matches. See
#'       \link{GetScores}.
#'     \item \emph{scores_playoff}: Scores class, for playoffs. See
#'       \link{GetScores}.
#'     \item \emph{rankings}: Rankings class. See \link{GetRankings}.
#'     \item \emph{alliances}: Alliances class. See \link{GetAlliances}.
#'     \item \emph{awards}: Awards class. See \link{GetAwards}.}
#' \code{GetAll} will omit qualification-level elements and rankings if the
#' \code{event} argument is set to "CMP", which corresponds to the finals on
#' Einstein.
#'
#' @seealso \link{GetEvents}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sn <- GetSession("myUserName", "key", season = 2016)
#' archimedes <- GetAll(sn, "ARCHIMEDES")
#' SaveData(archimedes)
#' }
GetAll <- function(session, event) {
  cmp <- (event == "CMP")
  evt <- list()
  evt$season <- GetSeason(session)
  evt$event <- GetEvents(session, event)
  evt$teams <- GetTeams(session, event = event)
  if(!cmp) evt$schedule.qual <- GetSchedule(session, event, level = "qual")
  evt$schedule.playoff <- GetSchedule(session, event, level = "playoff")
  if(!cmp) evt$hybrid.qual <- GetHybridSchedule(session, event, level = "qual")
  evt$hybrid.playoff <- GetHybridSchedule(session, event, level = "playoff")
  if(!cmp) evt$matches.qual <- GetMatchResults(session, event, level = "qual")
  evt$matches.playoff <- GetMatchResults(session, event, level = "playoff")
  if(!cmp) evt$scores.qual <- GetScores(session, event, level = "qual")
  evt$scores.playoff <- GetScores(session, event, level = "playoff")
  if(!cmp) evt$results.qual <- MergeResults(evt$hybrid.qual, evt$scores.qual)
  evt$results.playoff <- MergeResults(evt$hybrid.playoff,
                                             evt$scores.playoff)
  if(!cmp) evt$rankings <- GetRankings(session, event)
  evt$alliances <- GetAlliances(session, event)
  evt$awards <- GetAwards(session, event)

  return(evt)
}


# MergeResults() ================================================================
#' Merge HybridSchedule and Scores data into a single data frame.
#'
#' \code{MergeResults()} uses R's merge function to combine the results of the
#' \code{GetHybridSchedule()} and \code{GetScores(})) functions into a single
#' data frame. While the \emph{DetailedScores} data frame contains every match
#' detail that gets stored in the field management system, it doesn't list the
#' teams that participated in the match. For each row of scores it only listss
#' the match number and the alliance (red or blue). \code{MergeResults()} makes
#' it easy to link detailed match performance data to specific FRC teams.
#'
#' @param hybrid.df A data frame with class \emph{HybridSchedule()}, obtained
#'   from the function \code{GetHybridSchedule}. The data frame must be in team
#'   shape.
#' @param scores.df A data frame with class \emph{Scores}, obtained from the
#'   function \code{GetScores()}.
#'
#' @return A data frame containing both the teams assigned to a match and their
#'   detailed scores. The data frame is in team shape, with six rows per match
#'   and one team listed per row. The class is ("Results", "data.frame").
#'
#' @section Errors:
#'   \code{MergeResults()} throws an error if:
#'   \itemize{
#'     \item{hybrid.df does not have class \emph{HybridSchedule} or
#'       is not in team shape}
#'     \item{scores.df does not have class \emph{DetailedScores}}
#'     \item{hybrid.df or scores.df urls are not valid FIRST API urls}
#'     \item{scores.df and hybrid.df seasons, events, or match levels are not
#'       equal}
#'   }
#'
#' @export
#'
#' @examples
#' sn <- GetSession("username", "key", season = 2016)
#' hybrid_WAELL_qual <- GetHybridSchedule(sn, event = "WAELL")
#' scores_WAELL_qual <- GetScores(sn, event = "WAELL")
#' results_WAELL_qual <- MergeResults(hybrid_WAELL_qual, scores_WAELL_qual)
MergeResults <- function(hybrid.df, scores.df) {

  # Check arguments
  urls <- character(2)

  if(!inherits(hybrid.df, "HybridSchedule"))
    stop("hybrid.df argument must be data frame of class 'HybridSchedule'")
  else
    urls[1] <- attr(hybrid.df, "url")
    if(attr(hybrid.df, "shape") != "team")
      stop("hybrid.df argument must be in team shape.")
  if(!inherits(scores.df, "Scores"))
    stop("scores.df argument must be data frame of class 'Scores'")
  else
    urls[2] <- attr(scores.df, "url")

  # Verify data frames are from single compeition.

  # URLs have valid format
  if(!all(grepl(paste("^https://\\.*frc-api.firstinspires.org/v2.0/20\\d\\d/",
                      "(schedule|scores)/[^/]+/(qual|playoff)(/hybrid)?$",
                      sep = ""),
                urls)))
     stop("Data frames have invalid url parameters.")

  # Data frames are from same season
  urls_season <- regmatches(urls, regexpr("\\d{4}", urls))
  if(urls_season[1] != urls_season[2])
    stop("Data frames are from different seasons.")

  # Data frames are from same event
  urls_event <- sub(".*/(schedule|scores)/([^/]+)/.*", "\\2", urls)
  if(urls_event[1] != urls_event[2])
    stop("Data frames are from different events.")

  # Data frames are from same match level
  urls_lvl <- sub(".*/(schedule|scores)/[^/]+/(qual|playoff).*", "\\2", urls)
  if(urls_lvl[1] != urls_lvl[2])
    stop("Data frames are not from same match level (qual or playoff).")

  # Remove duplicate columns from MatchResults data frame
  hybrid.df$scoreAuto <- NULL
  hybrid.df$scoreFoul <- NULL
  hybrid.df$scoreFinal <- NULL

  # Merge data frames
  merged.df <- merge(hybrid.df, scores.df)

  # Convert columns to factors
  merged.df$alliance <- factor(merged.df$alliance)
  merged.df$station <- factor(merged.df$station)

  # Sort results and set row names to lvl.match.station
  merged.df <- merged.df[order(merged.df$match, merged.df$station), ]
  row.names(merged.df) <- tolower(paste(tolower(substr(levels(merged.df$level[1]),
                                        1, 1)),
                                merged.df$match, merged.df$station, sep="."))

  # Add attributes
  attr(merged.df, "url") <- urls
  class(merged.df) <- append(class(merged.df), "Results")
  return(merged.df)
}


# SaveData =====================================================================
#' Saves data to an RDS data file via a file save dialog.
#'
#' Displays the tcltk package's \emph{save file dialog}. Once the user enters or
#' selects a file name, \code{SaveData()} will save the value passed in
#' parameter x to an RDS data file. The dialog will initially display the
#' current working directory (see \code{getwd()} and \code{setwd()}).
#'
#' SaveData uses the \code{saveRDS()} function, from R's base package, to save
#' R data. RDS files can only contain a single object. To save more than one
#' object in single file, just put the objects in a list.
#'
#' Throws an error if the selected file does not end in "RDS"
#' (case-insensitive). Prints a message if the user cancels the dialog without
#' selecting a file.
#'
#' @param x An R object that will be saved to an RDS file.
#'
#' @return NULL (invisibly), or NA if the user cancels the dialog without
#'   selecting a file.
#'
#' @seealso \code{\link{saveRDS}}, \code{\link{getwd}}, \code{\link{setwd}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sn <- GetSession("username", "key", season = 2016)
#' sched <- GetSchedule(sn, "ORPHI")
#' SaveData(sched)
#' }
SaveData <- function(x) {
  extensions = "{ {RDS Files} {.RDS} }"
  filename <- tcltk::tclvalue(tcltk::tkgetSaveFile(title = "Save R Data",
                                                   filetypes = extensions,
                                                   defaultextension = "RDS"))
  if(filename == "") {
    message("User canceled file save dialog.")
    return(NA)
  }
  if(!grepl(".+\\.rds$", tolower(filename)))
    stop("File extension must be 'RDS'.")
  saveRDS(x, file = filename)
}


# ReadData =====================================================================
#' Reads data from an RDS data file via a file open dialog.
#'
#' Displays the tcltk package's \emph{read file dialog}. Once the user enters or
#' selects a file name, \code{ReadData()} will read and return the value stored
#' in the corresponding RDS data file. The dialog will initially display the
#' current working directory (see \code{getwd()} and \code{setwd()}).
#'
#' ReadData uses the \code{readRDS()} function, from R's base package, to read
#' R data. The data is stored in the RDS file as an unnamed object, so the user
#' must assign the return value to a named variable.
#'
#' Throws an error if the selected file does not end in "RDS"
#' (case-insensitive). Prints a message if the user cancels the dialog without
#' selecting a file.
#'
#' @return The R object stored in the RDS data file, or NA if the user cancels
#'   the dialog without selecting a file.
#'
#' @seealso \code{\link{readRDS}}, \code{\link{getwd}}, \code{\link{setwd}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my.data <- ReadData()
#' }
ReadData <- function() {
  extensions = "{ {RDS Files} {.RDS} }"
  filename <- tcltk::tclvalue(tcltk::tkgetOpenFile(filetypes = extensions,
                                                   defaultextension = "RDS",
                                                   title = "Open an RDS Data File"))
  if(filename == "") {
    message("User canceled file save dialog.")
    return(NA)
  }
  if(!grepl(".+\\.rds$", tolower(filename)))
    stop("File extension must be 'RDS'.")
  readRDS(filename)
}
