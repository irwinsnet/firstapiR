## ----districtCodes-------------------------------------------------------
library(firstapiR)
sn <- GetSession("username", "key")
districts <- GetDistricts(sn)

## ----eventCodes----------------------------------------------------------
sn <- GetSession("username", "key")
PNW_events <- GetEvents(sn, district = "PNW")
PNW_events[, c("code", "name")]
PNW_events[, c("code", "dateStart")]

## ------------------------------------------------------------------------
mr <- firstapiR::GetMatchResults(sn, "PNCMP")

# Put the teams in order by average score
mr$teamNumber <- reorder(mr$teamNumber, -mr$scoreFinal, mean)

# Filter out all but the top 8 teams
top8 <- mr$teamNumber[1:8]
mr8 <- mr[mr$teamNumber %in% top8, ]
mr8$teamNumber <- droplevels(mr8$teamNumber)

# Boxplot the top 8 teams
boxplot(scoreFinal~teamNumber, mr8, las = 2, main = "Qual Scores, Top 8 Teams",
        sub = "2016 PNW District Championships")

## ------------------------------------------------------------------------
sched <- firstapiR::GetSchedule(sn, event = "ORPHI", expand_cols = TRUE)

# Display the first 12 rows of the data frame, skipping the first three colums
sched[1:3, 4:17]

## ----eval = FALSE--------------------------------------------------------
#  # Create an HTTP date-time string set to midnight GMT yesterday
#  mod_date <- httr::http_date(as.POSIXct(Sys.Date() - 1))
#  
#  # Request recently changed data from the server
#  match_results <- firstapiR::GetMatchResults(sn, event = "PNCMP",
#                                              mod_since = mod_date)
#  
#  # Assuming there have been no updates to the data since yesterday, this
#  #   returns TRUE
#  is.na(match_results)
#  
#  # The value passed to mod_since returned as an attribute, even when the
#  #   result is NA
#  print(attr(match_results, "mod_since"))
#  

## ----eval = FALSE--------------------------------------------------------
#  # Create an HTTP date-time string set to midnight GMT yesterday
#  mod_date <- httr::http_date(as.POSIXct(Sys.Date() - 1))
#  
#  # Request recently changed data from the server
#  match_results <- firstapiR::GetMatchResults(sn, event = "PNCMP",
#                                              only_mod_since = mod_date)
#  
#  # Assuming there have been no updates to the data since yesterday, this
#  #   returns TRUE
#  is.na(match_results)
#  
#  # The value passed to mod_since returned as an attribute, even when the
#  #   result is NA
#  print(attr(match_results, "only_mod_since"))
#  

