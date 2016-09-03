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

