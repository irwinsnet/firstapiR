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
#' shape attribute set to "team". To convert data frame that are in alliance
#' shape to match shape, first use \code{ToTeamShape} to convert the data frame
#' back to team shape, and then use \code{ToMatchShape} to convert it to match
#' shape.
#'
#' FirstapiR proivdes functions to reshape data frames because different shapes
#' are useful for different purposes. For example, team shape is useful for
#' extracting data for a single team because it's only necessary to subset the
#' data frame on the single team column. Match shape is useful for displaying
#' schedules. Alliance shape is useful for calculating offensive power rating
#' (OPR).
#'
#' This function throws an error if df does not inherit from
#' \emph{matchResults}, \emph{schedule}, or \emph{hybridSchedule}, or if the
#' data frame's shape attribute is missing or is anything other than
#' \emph{team}.
#'
#' @param username A firstapiR data frame. The data frame must inherit from
#'   the schedule, hybridSchedule, or matchResults class data frames returned
#'   by firstapiR functions. The data frame must also be in team shape and have
#'   the shape attribute set to "team".
#'
#' @return A data frame of class schedule, hybridSchedule, or matchResults
#'   (depending on the class of the df argument) in match shape, with the shape
#'   attribute set to "match".
#'
#' @export
#'
#' @examples
#' sn <- GetSession("myUserName", "key")
#' matchResults <- GetMatchResults(sn, event = "PNCMP")
#' matchResults.m <- ToMatchShape(matchResults)
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
  alli.df <- reshape(df, direction = "wide", idvar = "match",
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
                           "postResult", "scoreFinal.blue",
                           "scoreAuto.blue", "scoreFoul.blue", "scoreFinal.red",
                           "scoreAuto.red", "scoreFoul.red", "team.blue1",
                           "disqualified.blue1", "team.blue2",
                           "disqualified.blue2", "team.blue3",
                           "disqualified.blue3", "team.red1",
                           "disqualified.red1", "team.red2",
                           "disqualified.red2", "team.red3",
                           "disqualified.red3")]
  if(inherits(df, "HybridSchedule"))
    alli.df <- alli.df[, c("match", "description", "level", "start",
                           "actualStart", "scoreFinal.blue",
                           "scoreAuto.blue", "scoreFoul.blue", "scoreFinal.red",
                           "scoreAuto.red", "scoreFoul.red", "team.blue1",
                           "surrogate.blue1", "disqualified.blue1","team.blue2",
                           "surrogate.blue2", "disqualified.blue2",
                           "team.blue3", "surrogate.blue3",
                           "disqualified.blue3",  "team.red1", "surrogate.red1",
                           "disqualified.red1",  "team.red2",
                           "surrogate.red2", "disqualified.red2", "team.red3",
                           "surrogate.red3", "disqualified.red3")]
  if(inherits(df, "Schedule"))
    alli.df <- alli.df[, c("match", "description", "level", "field", "start",
                           "team.blue1", "surrogate.blue1", "team.blue2",
                           "surrogate.blue2", "team.blue3", "surrogate.blue3",
                           "team.red1", "surrogate.red1", "team.red2",
                           "surrogate.red2", "team.red3", "surrogate.red3")]
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
#' it has more columns) with all six teams listed in one row and only one row
#' per match.
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
#' data frame on the single team column. Match shape is useful for displaying
#' schedules. Alliance shape is useful for calculating offensive power rating
#' (OPR).
#'
#' This function throws an error if df does not inherit from
#' \emph{matchResults}, \emph{schedule}, or \emph{hybridSchedule}, or if the
#' data frame's shape attribute is missing or is anything other than
#' \emph{team}.
#'
#' @param username A firstapiR data frame. The data frame must inherit from
#'   the schedule, hybridSchedule, or matchResults class data frames returned
#'   by firstapiR functions. The data frame must also be in team shape and have
#'   the shape attribute set to "team".
#'
#' @return A data frame of class schedule, hybridSchedule, or matchResults
#'   (depending on the class of the df argument) in match shape, with the shape
#'   attribute set to "alliance".
#'
#' @export
#'
#' @examples
#' sn <- GetSession("myUserName", "key")
#' matchResults <- GetMatchResults(sn, event = "PNCMP")
#' matchResults.a <- ToAllianceShape(matchResults)
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
  mtch.df <- reshape(df, direction = "wide", idvar = c("match", "alliance"),
                     timevar = "station", v.names = var.cols)

  # Set row names to m.color where m is match number, color is red or blue.
  row.names(mtch.df) <- paste(mtch.df$match, mtch.df$alliance, sep = ".")

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
#' the narrow team shape to the wide match or alliance shapes (they're called
#' wide because they have more columns). \code{ToTeamShape} converts these wide
#' data frames back to narrow data frames in team shape.
#'
#' /code{ToTeamShape} will only convert data frames that are in match or
#' alliance shape, with their shape attribute set to \emph{match} or
#' \emph{alliance}.
#'
#' FirstapiR proivdes functions to reshape data frames because different shapes
#' are useful for different purposes. For example, team shape is useful for
#' extracting data for a single team because it's only necessary to subset the
#' data frame on the single team column. Match shape is useful for displaying
#' schedules. Alliance shape is useful for calculating offensive power rating
#' (OPR).
#'
#' This function throws an error if df does not inherit from
#' \emph{matchResults}, \emph{schedule}, or \emph{hybridSchedule}, or if the
#' data frame's shape attribute is missing or is anything other than
#' \emph{team}.
#'
#' @param username A firstapiR data frame. The data frame must inherit from
#'   the schedule, hybridSchedule, or matchResults class data frames returned
#'   by firstapiR functions. The data frame must also be in team shape and have
#'   the shape attribute set to "team".
#'
#' @return A data frame of class schedule, hybridSchedule, or matchResults
#'   (depending on the class of the df argument) in match shape, with the shape
#'   attribute set to "match".
#'
#' @export
#'
#' @examples
#' sn <- GetSession("myUserName", "key")
#' matchResults <- GetMatchResults(sn, event = "PNCMP")
#' matchResults.m <- ToMatchShape(matchResults)
#' matchResults.m.t <- ToTeamShape(matchResults.m)
ToTeamShape <- function(df) {
  # Check for valid input
  if(!("shape" %in% names(attributes(df))))
    stop("df must have a shape attribute.")
  if(!(attr(df, "shape") %in% c("alliance", "match")))
    stop("df argument must be a data frame in alliance or match shape")

  # Replace columns for data frames in match shape.
  if(attr(df, "shape") == "match") {
    names(df)[names(df) == "scoreFinal.blue"] <- "scoreFinal.blue1"
    names(df)[names(df) == "scoreAuto.blue"] <- "scoreAuto.blue1"
    names(df)[names(df) == "scoreFoul.blue"] <- "scoreFoul.blue1"
    names(df)[names(df) == "scoreFinal.red"] <- "scoreFinal.red1"
    names(df)[names(df) == "scoreFoul.red"] <- "scoreFoul.red1"
    names(df)[names(df) == "scoreAuto.red"] <- "scoreAuto.red1"
    df$scoreFinal.blue2 <- df$scoreFinal.blue1
    df$scoreFinal.blue3 <- df$scoreFinal.blue1
    df$scoreAuto.blue2 <- df$scoreAuto.blue1
    df$scoreAuto.blue3 <- df$scoreAuto.blue1
    df$scoreFoul.blue2 <- df$scoreFoul.blue1
    df$scoreFoul.blue3 <- df$scoreFoul.blue1
    df$scoreFinal.red2 <- df$scoreFinal.red1
    df$scoreFinal.red3 <- df$scoreFinal.red1
    df$scoreAuto.red2 <- df$scoreAuto.red1
    df$scoreAuto.red3 <- df$scoreAuto.red1
    df$scoreFoul.red2 <- df$scoreFoul.red1
    df$scoreFoul.red3 <- df$scoreFoul.red1
  }

  # Reshape data frame. Assumes df has reshapeWide attribute from prior call to
  #   reshape() function.
  team.df <- reshape(df)

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
  row.names(team.df) <- paste(team.df$match, team.df$station, sep = ".")

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


#  .PreserveAttributes()===============================================================
#' Reassign firstapiR attributes to a data frame.
#'
#' This is an internal function. Many actions in R will strip attributes from
#' data frames. This helper function replaces those attributes.
#'
#' This function throws an error if the list passed via the \emph{att} argument
#' does not contain elements named \emph{local_test_data},
#' \emph{time_downloaded}, or \emph{last_modified}.
#'
#' @param df A data frame that may have had its attributes removed.
#' @param att A list containing the attributes from the data frame before the
#'   attributes were stripped. This character vector should be created by
#'   calling \code{attribues} on the original data frame.
#'
#' @return A data frame with the attributes \emph{local_test_data},
#' \emph{time_downloaded}, and \emph{last_modified}.
#'
#' @examples
#' new.df <- .PreserveAttributes(old.df)
.PreserveAttributes <- function(df, att){
  attr(df, "url") <- att$url
  attr(df, "local_test_data") <- att$local_test_data
  attr(df, "time_downloaded") <- att$time_downloaded
  attr(df, "last_modified") <- att$last_modified

  if("local_url" %in% names(att))
    attr(df, "local_url") <- att$local_url
  return(df)
}
