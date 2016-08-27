# data-raw/data.R
#
# This file downloads FIRST API data and saves it to the R/sysdata.rda file. The
# data is saved in both XML and JSON text formats and is used returned by
# firstapiR functions when the authorization key is set to "key". This local
# data allows users to test firstapiR functions without an authorization key
# or internet connection.

data_time <- Sys.time()

sn_j <- firstapiR::GetSession(username, key, format = "json", season = 2016)
sn_x <- firstapiR::GetSession(username, key, format = "xml", season = 2016)

status_json <- firstapiR::GetServerStatus(sn_j)
status_xml <- firstapiR::GetServerStatus(sn_x)
url <- attr(status_xml, "url")
status_xml <- toString(status_xml)
attr(status_xml, "url") <- url

season_json <- firstapiR::GetSeason(sn_j)
season_xml <- firstapiR::GetSeason(sn_x)
url <- attr(season_xml, "url")
season_xml <- toString(season_xml)
attr(season_xml, "url") <- url

districts_json <- firstapiR::GetDistricts(sn_j)
districts_xml <- toString(firstapiR::GetDistricts(sn_x))
url <- attr(districts_xml, "url")
attr(districts_xml, "url") <- url

events_json <- firstapiR::GetEvents(sn_j, district = "PNW")
events_xml <- toString(firstapiR::GetEvents(sn_x, district = "PNW"))
url <- attr(events_xml, "url")
attr(events_xml, "url") <- url

teams_json <- firstapiR::GetTeams(sn_j, state = "Idaho", page = 1)
teams_xml <- toString(firstapiR::GetTeams(sn_x, state = "Idaho", page = 1))
url <- attr(teams_xml, "url")
attr(teams_xml, "url") <- url

schedule_json <- firstapiR::GetSchedule(sn_j, event = "ORPHI")
schedule_xml <- toString(firstapiR::GetSchedule(sn_x, event = "ORPHI"))
url <- attr(schedule_xml, "url")
attr(schedule_xml, "url") <- url

hybrid_json <- firstapiR::GetHybridSchedule(sn_j, event = "WAELL")
hybrid_xml <- toString(firstapiR::GetHybridSchedule(sn_x, event = "WAELL"))
url <- attr(hybrid_xml, "url")
attr(hybrid_xml, "url") <- url

matches_json <- firstapiR::GetMatchResults(sn_j, event = "PNCMP")
matches_xml <- toString(firstapiR::GetMatchResults(sn_x, event = "PNCMP"))
url <- attr(matches_xml, "url")
attr(matches_xml, "url") <- url

scores_json <- firstapiR::GetScores(sn_j, event = "ARCHIMEDES")
scores_xml <- toString(firstapiR::GetScores(sn_x, event = "ARCHIMEDES"))
url <- attr(scores_xml, "url")
attr(scores_xml, "url") <- url

alliances_json <- firstapiR::GetAlliances(sn_j, "WAAMV")
alliances_xml <- toString(firstapiR::GetAlliances(sn_x, "WAAMV"))
url <- attr(alliances_xml, "url")
attr(alliances_xml, "url") <- url

rankings_json <- firstapiR::GetRankings(sn_j, "WAAMV")
rankings_xml <- toString(firstapiR::GetRankings(sn_x, "WAAMV"))
url <- attr(rankings_xml, "url")
attr(rankings_xml, "url") <- url

awards_json <- firstapiR::GetAwards(sn_j, "PNCMP")
awards_xml <- toString(firstapiR::GetAwards(sn_x, "PNCMP"))
url <- attr(awards_xml, "url")
attr(awards_xml, "url") <- url

awards_list_json <- firstapiR::GetAwardsList(sn_j)
awards_list_xml <- toString(firstapiR::GetAwardsList(sn_x))
url <- attr(awards_list_xml, "url")
attr(awards_list_xml, "url") <- url


devtools::use_data(status_json, status_xml,
                   season_json, season_xml,
                   districts_json, districts_xml,
                   events_json, events_xml,
                   teams_json, teams_xml,
                   schedule_json, schedule_xml,
                   hybrid_json, hybrid_xml,
                   matches_json, matches_xml,
                   scores_json, scores_xml,
                   alliances_json, alliances_xml,
                   rankings_json, rankings_xml,
                   awards_json, awards_xml,
                   awards_list_json, awards_list_xml,
                   data_time,
                   internal = TRUE, overwrite = TRUE)
