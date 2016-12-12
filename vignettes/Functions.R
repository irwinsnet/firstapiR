## ----districtCodes-------------------------------------------------------
library(firstapiR)
sn <- GetSession("username", "key")
districts <- GetDistricts(sn)

## ----eventCodes----------------------------------------------------------
sn <- GetSession("username", "key")
PNW_events <- GetEvents(sn, district = "PNW")
PNW_events[, c("code", "name")]
PNW_events[, c("code", "dateStart")]

