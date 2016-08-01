data_time <- Sys.time()

sn_j <- GetSession(username, key, format = "json", season = 2016)
sn_x <- GetSession(username, key, format = "xml", season = 2016)

season_json <- GetSeason(sn_j)
season_xml <- toString(GetSeason(sn_x))

districts_json <- GetDistricts(sn_j)
districts_xml <- GetDistricts(sn_x)

devtools::use_data(season_json, season_xml,
                   districts_json, districts_xml,
                   data_time,
                   internal = TRUE, overwrite = TRUE)
