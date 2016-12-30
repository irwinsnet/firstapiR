## ----install_v2.0.0, eval = FALSE----------------------------------------
#  devtools::install_github("irwinsnet/firstapiR")

## ----install_v1.0.0, eval= FALSE-----------------------------------------
#  devtools::install_github("irwinsnet/firstapiR", ref = "v1.0.0")

## ----GetSession_1--------------------------------------------------------
# Create a Session object
sn <- firstapiR::GetSession("username", "key")

# Display the Session object.
sn

## ----GetSession_2--------------------------------------------------------
# Create a Session and specify XML format
sn <- firstapiR::GetSession("username", "key", format = "xml")

# Change the season to 2015
sn$season <- 2015

# Display the Session object
sn

## ----Districts-----------------------------------------------------------
# Create a Session and use it in a firstapiR function
sn <- firstapiR::GetSession("username", "key")
dist <- firstapiR::GetDistricts(sn)

# Display the class of the object returned by GetDistricts()
class(dist)

# Display the data frame
dist

## ----Subsetting_1--------------------------------------------------------
# Retrieve the item in th 6th row of the column named "code"
dist$code[[6]]

## ----Subsetting_2--------------------------------------------------------
# Retrieve the name of any district with code == "PNW"
dist$name[dist$code == "PNW"]

## ----structure-----------------------------------------------------------
# Display the structure and attributes of the dist object
str(dist)

