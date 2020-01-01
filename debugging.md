## Debugging Notes

1. In firstapiR_main, line 606, expected that the dataframe would have a column named "Teams", but instead
column is named "teams". Appears that FIRST changed from title to lower case for this label name.
2. Also in firstapiR_main, in GetMatchResults and GetScores, converted first character of column names to
   lowercase to adjust for capitialization changes made by FIRST.
3. Made same change ("Teams" to "teams") at the following locations:
  * GetHybridSchedule(), lines 751, 752
  * GetMatchResults(), lines 924, 925
4. Many changes made to tests.
  * Error messages have changed slightly since 2016. Tests that check for expected errors no longer check the exact text returned by the message, just that an error occurred.
  * Some conditions that used to result in a 304 message (Not Modidified) now return code 200 with no data.
  * Session objects were not defaulting to current year for season, but tests require season to be set
    to 2016 to pass.
  
