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

# FirstapiR's R code is split into three files.
#   * firstapiR_main.R contains functions that generate HTTP requests, send
#     them to the FIRST API server, and format the returned data as either XML,
#     JSON, or as R data frames.
#   * firstapiR_util.R contains utility functions for manipulating FRC data
#     after it has been downloaded.
#  *  firstapiR_internal contains internal helper functions that are only
#     available within the firstapiR package.
#  *

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
# TODO: Move .PreserveAttributes function DONE
# TODO: Make sure .PreserveColumns is setting the attributes correctly. DONE
# TODO: Add data from 2016 championships DONE
# TODO: modify GetAll test to use local data. DONE.
# TODO: Finish url checking on Merge function. DONE

#' Standard firstapiR Attributes
#'
#' Objects returned by firstapiR functions that begin with \emph{Get} have the
#' following attributes:
#'
#' \itemize{
#'   \item \emph{url}: Character vector containing URL submitted to FIRST API
#'     server.
#'   \item \emph{local_test_data}: \code{TRUE} if data was extracted from
#'     R/sysdata.rda file.
#'   \item \emph{local_url}: Character vector containing URL used to download
#'     local data.
#'   \item \emph{time_downloaded}: Character vector containing the local
#'     system time that the object was downladed from the FIRST API server.
#'     Formatted an an http date and time string.
#'   \item \emph{last_modified}: Character vector containing the date and time
#'     that the data was last modified by the FIRST API server.
#'   \item \emph{mod_since}: Character vector containing the value passed to
#'     the \code{mod_since argument}, or NULL if no argument was passed.
#'     Formatted as an http date and time string.
#'   \item \emph{only_mod_since}: Character vector containing the value passed
#'     to the \code{only_mod_since} argument, or NULL if no argument was
#'     passed. Formatted as an http date and time string.}
#'
#' Use the \code{attr()} function to retrieve the value of these attributes.
#'
#' @name standard_attributes
NULL

# FIRST CHAMPIONSHIP DATA - 2016 ===============================================
#' 2016 FRC Championship Data
#'
#' firstapiR includes datasets for the 2016 FRC Championship. Each data set was
#' produced by the \code{firstapiR::GetAll()}and consists of a list of data
#' frames that were returned by the various event-related Get functions. See the
#' documentation for \link{\code{GetAll()}} for details on the contents of these
#' datasets.
#'
#' There is a data set for every championship division, and for the final
#' championship matches on the Einstein field. The einstein2016 dataset is
#' smaller than the other datasets because it doesn't have qualification or
#' alliance selection data. Each dataset is stored in it's own rda file in the
#' package's data directory.
#'
#' @seealso \link{\code{GetAll()}}
#'
#' @docType data
#'
#' @name FRC_Data
NULL

#' @rdname FRC_Data
"archimedes2016"

#' @rdname FRC_Data
"carson2016"

#' @rdname FRC_Data
"carver2016"

#' @rdname FRC_Data
"curie2016"

#' @rdname FRC_Data
"galileo2016"

#' @rdname FRC_Data
"hopper2016"

#' @rdname FRC_Data
"newton2016"

#' @rdname FRC_Data
"tesla2016"

#' @rdname FRC_Data
"einstein2016"
