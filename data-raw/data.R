# data-raw/data.R
#
# This file downloads FIRST API data and saves it to the R/sysdata.rda file. The
# data is saved in both XML and JSON text formats and is used returned by
# firstapiR functions when the authorization key is set to "key". This local
# data allows users to test firstapiR functions without an authorization key
# or internet connection.

ConvertXMLDocToText <- function(xml_doc) {
  url <- attr(xml_doc, "url")
  last_modified <- attr(xml_doc, "last_modified")
  xml_text <- toString(xml_doc)
  attr(xml_text, "url") <- url
  attr(xml_text, "last_modified") <- last_modified
  print(last_modified)
  return(xml_text)
}

data_time <- Sys.time()

sn_j <- firstapiR::GetSession(username, key, format = "json", season = 2016)
sn_x <- firstapiR::GetSession(username, key, format = "xml", season = 2016)

status_json <- firstapiR::GetServerStatus(sn_j)
status_xml <- ConvertXMLDocToText(firstapiR::GetServerStatus(sn_x))

season_json <- firstapiR::GetSeason(sn_j)
season_xml <- ConvertXMLDocToText(firstapiR::GetSeason(sn_x))

districts_json <- firstapiR::GetDistricts(sn_j)
districts_xml <- ConvertXMLDocToText(firstapiR::GetDistricts(sn_x))

events_json <- firstapiR::GetEvents(sn_j, district = "PNW")
events_xml <- ConvertXMLDocToText(firstapiR::GetEvents(sn_x, district = "PNW"))

teams_json <- firstapiR::GetTeams(sn_j, state = "Idaho", page = 1)
teams_xml <- ConvertXMLDocToText(
  firstapiR::GetTeams(sn_x, state = "Idaho", page = 1))

schedule_json <- firstapiR::GetSchedule(sn_j, event = "ORPHI")
schedule_xml <- ConvertXMLDocToText(
  firstapiR::GetSchedule(sn_x, event = "ORPHI"))

hybrid_json <- firstapiR::GetHybridSchedule(sn_j, event = "WAELL")
hybrid_xml <- ConvertXMLDocToText(
  firstapiR::GetHybridSchedule(sn_x, event = "WAELL"))

matches_json <- firstapiR::GetMatchResults(sn_j, event = "PNCMP")
matches_xml <- ConvertXMLDocToText(
  firstapiR::GetMatchResults(sn_x, event = "PNCMP"))

scores_json <- firstapiR::GetScores(sn_j, event = "ARCHIMEDES")
scores_xml <- ConvertXMLDocToText(
  firstapiR::GetScores(sn_x, event = "ARCHIMEDES"))

alliances_json <- firstapiR::GetAlliances(sn_j, "WAAMV")
alliances_xml <- ConvertXMLDocToText(firstapiR::GetAlliances(sn_x, "WAAMV"))

rankings_json <- firstapiR::GetRankings(sn_j, "WAAMV")
rankings_xml <- ConvertXMLDocToText(firstapiR::GetRankings(sn_x, "WAAMV"))

awards_json <- firstapiR::GetAwards(sn_j, "PNCMP")
awards_xml <- ConvertXMLDocToText(firstapiR::GetAwards(sn_x, "PNCMP"))

awards_list_json <- firstapiR::GetAwardsList(sn_j)
awards_list_xml <- ConvertXMLDocToText(firstapiR::GetAwardsList(sn_x))


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
