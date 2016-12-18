## ----districtCodes-------------------------------------------------------
library(firstapiR)
sn <- GetSession("username", "key")
districts <- GetDistricts(sn)

## ----eventCodes----------------------------------------------------------
sn <- GetSession("username", "key")
PNW_events <- GetEvents(sn, district = "PNW")
PNW_events[, c("code", "name")]
PNW_events[, c("code", "dateStart")]

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

## ------------------------------------------------------------------------
sn <- firstapiR::GetSession("username", "key")
match_results <- firstapiR::GetMatchResults(sn, "PNCMP", level = "qual")
match_results[1:12, c("match", "alliance", "station", "team", "scoreAuto",
                      "scoreFinal")]

## ------------------------------------------------------------------------
agg_scores <- aggregate(match_results[c("scoreFinal", "scoreAuto",
                                        "scoreFoul")],
                        list("team" = match_results$team), mean)
agg_scores[order(agg_scores$scoreFinal, decreasing = TRUE), ][1:10, ]


## ------------------------------------------------------------------------
attr(match_results, "shape")

## ------------------------------------------------------------------------
mresults_alliance <- ToAllianceShape(match_results)
mresults_alliance[1:4, c("match", "alliance", "team.1", "team.2", "team.3",
                         "scoreAuto", "scoreFinal")]

## ------------------------------------------------------------------------
attr(mresults_alliance, "shape")

## ------------------------------------------------------------------------
mresults_match <- ToMatchShape(match_results)
mresults_match[1:2, c("match", "team.Red1", "team.Red2", "team.Red3",
                      "team.Blue1", "team.Blue2", "team.Blue3",
                      "scoreFinal.Blue", "scoreFinal.Red")]

## ----eval = FALSE--------------------------------------------------------
#  SaveData(GetAlliances(sn, event = "WAAMV"))
#  evt_alliances <- ReadData()
#  

