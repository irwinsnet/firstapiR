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

devtools::use_data(season_json, season_xml,
                   districts_json, districts_xml,
                   events_json, events_xml,
                   teams_json, teams_xml,
                   data_time,
                   internal = TRUE, overwrite = TRUE)
