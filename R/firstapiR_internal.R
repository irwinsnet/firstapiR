#  firstapiR_internal.R version 2.0.0 ==========================================
#  Contains internal functions that are called by other firstapiR functions.


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
                    .first_http_api_version, sep="/")
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
    r <- httr::GET(full_url, httr::add_headers(.headers = headers),
                   httr::user_agent(.user_agent_name))

    # Check HTTP Response Status Code
    if(!(httr::status_code(r) %in% c(200, 304)))
      stop(paste(httr::status_code(r), httr::content(r, "text"), sep=": "))

    content <- httr::content(r, "text")
    last_modified <- httr::headers(r)[["Last-Modified"]]

    no_results <- (httr::status_code(r) == 304)
  }

  # Set results to NA if no records are returned
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

  # Set result to NA if no records are returned
  if(session$format == "data.frame" && length(result) == 0)
    result <- NA

  # Set descriptive attributes.
  attr(result, "url") <- full_url
  if(session$key == "key") {
    attr(result, "local_test_data") <- TRUE
    attr(result, "local_url") <- attr(content, "url")
    attr(result, "time_downloaded") <- attr(content, "time_downloaded")
    attr(result, "last_modified") <- attr(content, "last_modified")
  } else {
    attr(result, "local_test_data") <- FALSE
    attr(result, "local_url") <- NULL
    attr(result, "time_downloaded") <- httr::http_date(Sys.time())
    attr(result, "last_modified") <- last_modified
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


#  .SetColumnOrder() ===========================================================
#' Set the column order of a data frame.
#'
#' \code{.SetColumnOrder()} is an internal function that is not intended to be
#' called by the user. It is called by other FIRST API methods.
#'
#' @param df A data frame
#' @param col.order A character vector containing the names of the columns in
#'   \code{df}, in the desired column order.
#'
#' @return A data frame identical to the the data frame provided in the
#'   \code{df} argument, except with the columns in the order specified in the
#'   \code{col.order} argument. The returned data frame will ahve the same
#'   attributes as the data frame provided in the \code{df} argument.
#'
#' @examples
#' my.df <- data.frame(col3 = 1:3, col1 = 4:6, col2: 7:9)
#' ordered.df <- .SetColumnOrder(my.df, c("col1", "col2", "col3"))
.SetColumnOrder <- function(df, col.order) {
  df.att <- attributes(df)
  df.att$names <- NULL
  df.att$row.names <- NULL
  df.att$class <- NULL
  df.sorted <- df[, col.order]
  for(idx in 1:length(df.att))
    attr(df.sorted, names(df.att)[[idx]]) <- df.att[[idx]]
  return(df.sorted)
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
  # Remove names, class, and row.names attributes
  strip.names <- append(names(attributes(df)), c("reshapeLong", "reshapeWide"))
  att <- att[!(names(att) %in% strip.names)]
  # Append remaining attributes to data frame attributes.
  attributes(df) <- append(attributes(df), att)
  return(df)
}
