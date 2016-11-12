
#' Transform a data frame so there is one row per team.
#'
#' @export
ToTeamShape <- function(df) {
  # Check for valid input
  if(!(attr(df, "shape") %in% c("alliance", "match")))
    stop("df argument must be a data frame in alliance or match shape")

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

  # Sort, assign shape attribute, and return result.
  team.df <- team.df[order(team.df$match, team.df$station), ]
  attr(team.df, "shape") <- "team"
  return(team.df)
}


#' Transform a data frame so there is one row per alliance.
#'
#' @export
ToMatchShape <- function(df, revert.team = FALSE) {
  # Verify input data frame in team format
  if(attr(df, "shape") != "team") stop("df must be in team shape.")

  # Check type of input data frame
  if(inherits(df, "Schedule"))
    var.cols <- c("team", "surrogate")
  if(inherits(df, "MatchResults"))
    var.cols <- c("team", "disqualified", "scoreFinal", "scoreAuto",
                  "scoreFoul")
  if(inherits(df, "HybridSchedule"))
    var.cols <- c("team", "surrogate", "disqualified", "scoreFinal",
                  "scoreAuto", "scoreFoul")

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
    alli.df <- alli.df[, c("match", "description", "level", "actualStartTime",
                           "postResultTime", "scoreFinal.blue",
                           "scoreAuto.blue", "scoreFoul.blue", "scoreFinal.red",
                           "scoreAuto.red", "scoreFoul.red", "team.blue1",
                           "disqualified.blue1", "team.blue2",
                           "disqualified.blue2", "team.blue3",
                           "disqualified.blue3", "team.red1",
                           "disqualified.red1", "team.red2",
                           "disqualified.red2", "team.red3",
                           "disqualified.red3")]
  if(inherits(df, "HybridSchedule"))
    alli.df <- alli.df[, c("match", "description", "level", "startTime",
                           "actualStartTime", "scoreFinal.blue",
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


#' Transform a data frame so there is one row per match.
#'
#' @export
ToAllianceShape <- function(df) {
  # Verify input data frame in team format
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

.PreserveAttributes <- function(df, att){
  attr(df, "url") <- att$url
  attr(df, "local_test_data") <- att$local_test_data
  attr(df, "time_downloaded") <- att$time_downloaded
  attr(df, "last_modified") <- att$last_modified
  return(df)
}

