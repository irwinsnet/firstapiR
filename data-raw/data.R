data_time <- Sys.time()

sn_j <- firstapiR::GetSession(username, key, format = "json", season = 2016)
sn_x <- firstapiR::GetSession(username, key, format = "xml", season = 2016)

season_json <- firstapiR::GetSeason(sn_j)
season_xml <- toString(firstapiR::GetSeason(sn_x))

districts_json <- firstapiR::GetDistricts(sn_j)
districts_xml <- firstapiR::GetDistricts(sn_x)

events_json <- firstapiR::GetEvents(sn_j, district = "PNW")
events_xml <- toString(firstapiR::GetEvents(sn_x, team = 360,
                                            exclude_district = TRUE))

teams_json <- firstapiR::GetTeams(sn_j, district = "PNW", page = 1)
teams_xml <- firstapiR::GetTeams(sn_x, state = "Idaho", page = 1)

schedule_json <- firstapiR::GetSchedule(sn_j, event = "ORPHI")
schedule_xml <- firstapiR::GetSchedule(sn_x, event = "PNCMP",
                                        level = "playoff")

hybrid_json <- firstapiR::GetHybridSchedule(sn_j, event = "ORPHI")
hybrid_xml <- firstapiR::GetHybridSchedule(sn_x, event = "ORPHI")

matches_json <- firstapiR::GetMatchResults(sn_j, event = "PNCMP")
matches_xml <- firstapiR::GetMatchResults(sn_x, event = "PNCMP")

scores_json <- firstapiR::GetScores(sn_j, event = "ARCHIMEDES")
scores_xml <- firstapiR::GetScores(sn_x, event = "ARCHIMEDES")

alliances_json <- firstapiR::GetAlliances(sn_j, "WAAMV")
alliances_xml <- firstapiR::GetAlliances(sn_x, "WAAMV")

rankings_json <- firstapiR::GetRankings(sn_j, "WAAMV")
rankings_xml <- firstapiR::GetRankings(sn_x, "WAAMV")

awards_json <- firstapiR::GetAwards(sn_j, "PNCMP", 360)
awards_xml <- firstapiR::GetAwards(sn_x, "PNCMP", 360)

awards_list_json <- firstapiR::GetAwardsList(sn_j)
awards_list_xml <- firstapiR::GetAwardsList(sn_x)


devtools::use_data(season_json, season_xml,
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
