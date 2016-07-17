#  OVERVIEW: FRC 1318'S FIRST API R Toolbox ====================================
#' Analysis Module, Version 1.0
#' 

Points_boxplot <- function(match_results, rankings, event,
                           start = 1, stop = 10) {
  # Select teams
  # teams <- rankings$teamNumber[start:stop]
  
  # Create a data frame with data only for selected teams
  f_results <- match_results[match_results$teamNumber %in%
                               rankings$teamNumber[start:stop], ]
  
  # Remove unselected teams from the data frame teamNumber column
  f_results$teamNumber <- factor(f_results$teamNumber, levels =
                                   rankings$teamNumber[start:stop])
  
  boxplot(f_results[["scoreFinal"]] ~ f_results$teamNumber, f_results)
}