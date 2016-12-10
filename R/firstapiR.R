#  firstapiR.R version 2.0.0 ===================================================
#' firstapiR 2.0.0: R Functions for the FIRST API Server
#'
#' The firstapiR package provides functions that connect to the FIRST API server
#' and download data on FIRST Robotics Competition (FRC) teams, events, match
#' results, and awards.FirstapiR also contains utility functions for reshaping
#' data frames into wide or long formats, and to save FRC data to local files.
#'
#' The FIRST API accepts formatted hypertext transfer protocol (HTTP) GET
#' requests and provides FRC data in either javascript object notation (JSON) or
#' extensible markup language (XML) format. Detailed documentation for the FIRST
#' API, including precise rules for constructing the HTTP requests, is available
#' at \url{http://docs.frcevents2.apiary.io/#}
#'
#' A username and authorization key are required for connecting to the FIRST API
#' and for using firstapiR. To obtain a username and key, join the \emph{FIRST
#' Community Developers} project on TeamForge at
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

# FirstapiR's R code is split into two files.
#   * firstapiR_http.R contains functions that generate HTTP requests, send
#     them to the FIRST API server, and format the returned data as either XML,
#     JSON, or as R data frames.
#   * firstapiR_http.R contains utility functions for working with FRC data
#     after it has been downloaded.

# The firstapiR package requires the following R packages.
#   * base64enc
#   * httr
#   * jsonlite
#   * XML


# TODO: Revise lines for changing column names so they don't depend on column
#   order. DONE
# TODO: Remove expand_cols arg form all functions. DONE.
# TODO: Shorten column names where applicable. DONE
# TODO: Add Shape attribute. DONE
# TODO: Document shape attribute. DONE
# TODO: Reorder attribute documentation to match data frame order. DONE
# TODO: Add function to download entire event. DONE
# TODO: Add function to save data to files. DONE
# TODO: Add function to merge MatchResults and DetailedScores. DONE
# TODO: Document all functions
# TODO: Move .PreserveAttributes function from firstapiR_util to firstapiR_http
# TODO: Make sure .PreserverColumns is setting the attributes correctly.
# TODO: Add data from 2016 championships
# TODO: modify GetAll test to use local data.
# TODO: Finish url checking on Merge function.
# TODO: Alliance and Station columns back to title case.
