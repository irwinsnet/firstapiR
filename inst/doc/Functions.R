## ----districtCodes-------------------------------------------------------
library(firstapiR)
sn <- GetSession("username", "key")
districts <- GetDistricts(sn)

## ----eventCodes----------------------------------------------------------
sn <- GetSession("username", "key")
PNW_events <- GetEvents(sn, district = "PNW")
PNW_events[, c("code", "name")]
PNW_events[, c("code", "dateStart")]

## ----modified_since_1, eval = FALSE--------------------------------------
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

## ----modified_since_2, eval = FALSE--------------------------------------
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

## ----team_shape_1--------------------------------------------------------
sn <- firstapiR::GetSession("username", "key")
match_results <- firstapiR::GetMatchResults(sn, "PNCMP", level = "qual")
match_results[1:12, c("match", "alliance", "station", "team", "scoreAuto",
                      "scoreFinal")]

## ----team_shape_2--------------------------------------------------------
agg_scores <- aggregate(match_results[c("scoreFinal", "scoreAuto",
                                        "scoreFoul")],
                        list("team" = match_results$team), mean)
agg_scores[order(agg_scores$scoreFinal, decreasing = TRUE), ][1:10, ]


## ----team_shape_3--------------------------------------------------------
attr(match_results, "shape")

## ----alliance_shape_1----------------------------------------------------
mresults_alliance <- firstapiR::ToAllianceShape(match_results)
mresults_alliance[1:4, c("match", "alliance", "team.1", "team.2", "team.3",
                         "scoreAuto", "scoreFinal")]

## ----alliance_shape_2----------------------------------------------------
attr(mresults_alliance, "shape")

## ----match_shape---------------------------------------------------------
mresults_match <- firstapiR::ToMatchShape(match_results)
mresults_match[1:2, c("match", "team.Red1", "team.Red2", "team.Red3",
                      "team.Blue1", "team.Blue2", "team.Blue3",
                      "scoreFinal.Blue", "scoreFinal.Red")]

## ----save_load_data, eval = FALSE----------------------------------------
#  SaveData(firstapiR::GetAlliances(sn, event = "WAAMV"))
#  evt_alliances <- firstapiR::ReadData()
#  

## ----GetAll, eval = FALSE------------------------------------------------
#  # Get all data for the 2016 district competition at Auburn Mountainview HS
#  sn <- firstapiR::GetSession("username", "key")
#  all_data <- firstapiR::GetAll(sn, event = "WAAMV")

## ----preloaded_data_1----------------------------------------------------
# Show elements in curie2016 list:
names(firstapiR::curie2016)

## ----preloaded_data_2----------------------------------------------------
# Show alliances for Curie Subdivision (first 8 columns)
firstapiR::curie2016$alliances[1:8]

## ----merge_results-------------------------------------------------------
# Merge MatchResults and Scores data frames
curie_results <- firstapiR::MergeResults(firstapiR::curie2016$hybrid.qual,
                                         firstapiR::curie2016$scores.qual)
# Show structure of resulting data frame
str(curie_results)

## ----merge_rows----------------------------------------------------------
# Number qualification matches
nrow(firstapiR::curie2016$matches.qual)

# Number playoff matches
nrow(firstapiR::curie2016$matches.playoff)

# Merge qualification and playoff data into a single table
all_matches <- rbind(firstapiR::curie2016$matches.qual,
                     firstapiR::curie2016$matches.playoff)

# Total Matches
nrow(all_matches)

