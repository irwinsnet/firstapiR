# firstapiR_tcltk.R version 2.0.0 ==============================================
# Contains functions that use the tcltk package to display graphical user
#   interfaces.


#' Saves data to an RDS data file via a file save dialog.
#'
#' Displays the tcltk package's \emph{save file dialog}. Once the user enters or
#' selects a file name, \code{SaveData()} will save the value passed in
#' parameter x to an RDS data file. The dialog will initially display whatever
#' folder the current working directory (see \code{getwd()} and \code{setwd()}).
#'
#' SaveData uses the \code{saveRDS()} function, from R's base package, to save
#' R data. RDS files can only contain a single object. To save more than one
#' object in single file, just put them in a list.
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
#' @export
#'
#' @examples
#' \dontrun{
#' sn <- GetSession("username", "key")
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


#' Reads data from an RDS data file via a file open dialog.
#'
#' Displays the tcltk package's \emph{save file dialog}. Once the user enters or
#' selects a file name, \code{ReadData()} will read and return the value stored
#' in the corresponding RDS data file. The dialog will initially display
#' whatever folder the current working directory (see \code{getwd()} and
#' \code{setwd()}).
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
