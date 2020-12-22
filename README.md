# firstapiR: An R Package for Using the FIRST API Server

The firstapiR package provides R functions that download FIRST Robotics 
Competition (FRC) data from the FIRST API server via hypertext transfer protocol
(HTTP) requests. The firstapiR package formats the resulting data as JSON text,
XML text, or as R data frames.

### Version 2.0.1

[Full documentation for version 2.0.1 of the firstapiR package is available here](https://irwinsnet.github.io/firstapiR/Getting_Started).

[Guidance for version
1.0.0 of firstapiR is located here
](http://irwinsnet.github.io/projects/firstapiR/Getting_Started_v100).


### FIRST - *F*or *I*nspiration and *R*ecognition in *S*cience and *T*echnology

FIRST engages kids in kindergarten through high school in exciting, 
mentor-based, research and robotics programs that help them become science and 
technology leaders, as well as well-rounded contributors to society. Learn more 
about FIRST by [visiting their website.](http://www.firstinspires.org/)

FRC is one of four FIRST programs. In FRC, high-school students, in cooperation
with adult mentors, design and build robots and operate them in competitions.
The competitions have detailed rules ([check out the most recent game manual 
](http://www.firstinspires.org/resource-library/frc/competition-manual-qa-system))
and an extensive amount of data is generated at each competition. All 
scheduling, team, scoring, and ranking data is stored in FIRST servers and is 
made available to users via the FIRST API.


### Getting Access to the FIRST API

To see what the FIRST API can do, visit the [FIRST API documentation page 
](http://docs.frcevents2.apiary.io/#). The documentation page provides detailed 
information on the formats for HTTP request and authorization headers, as well
as detailed descriptions of the data provided by each type of API call.

To use the FIRST API (via firstapiR functions or via HTTP requests that you
build yourself), you must have a username and authorization key issued by FIRST.
To get a username and authorization key, go to the [FIRST Community Developers
page on TeamForge 
](https://usfirst.collab.net/sf/projects/first_community_developers/). Follow 
the instructions on the page to join the project and FIRST will email you a 
username and authorization key. Make sure you keep your authorization key secret
- don't post the key anywhere that is publicly accessible.


### Learning R

R is a functional language intended for statistical computing, data analysis and
plotting. I initially considered building this package in Python, but in the end
I wanted to learn to use R, so here we are. R seems a bit quirky at first, 
especially compared to other languages like Java or Python, but it has some
useful, built-in features for working with data. R has an extensive array of
free add-in packages for manipulating and visualizing data, plus there is an
excellent and free IDE available for R, RStudio.

If you are new to R, experimenting with firstapiR isn't the worst way to get 
started, but there are some resources you should check out:

1. Download R from [the main R
website](https://cran.r-project.org/mirrors.html).
1. Download and use the free version of [RStudio]( 
https://rstudio.com/products/rstudio/download/).
1. To learn R, I recommend starting with the
[Quick R Website ](http://www.statmethods.net/)
1. Next, check out the R
introduction on the [main R documentation page 
](https://cran.r-project.org/manuals.html)
1. Everything written by [Hadley
Wickham](http://hadley.nz/) is excellent. His [Advanced R
website](http://adv-r.had.co.nz/), [R package website 
](http://r-pkgs.had.co.nz/) and his [testthat 
](https://cran.r-project.org/web/packages/testthat/index.html) and [devtools 
](https://cran.r-project.org/web/packages/devtools/index.html) packages were 
essential to the development of the firstapiR package.


### Installing the firstapiR package

1. Install the devtools package. This is easy to do in RStudio - just select
_Tools->Install Packages..._ from the menu bar, type "devtools" in the
package name box and hit _Install_.

1. The devtools package provides a function for installing R packages from
GitHub. Type the following command at the RStudio console prompt:
```{r install_v2.0.1, eval = FALSE}
devtools::install_github("irwinsnet/firstapiR")
```

If you need the prior version of firstapiR, use this command instead:
```{r install_v1.0.0, eval= FALSE}
devtools::install_github("irwinsnet/firstapiR", ref = "v1.0.0")
```


## Differences between Versions 1.0.0 and 2.0.1

1. Version 1.0.0 of firstapiR is still available from github per the firstapiR
installation instructions above.

1. The main difference from version 1.0.0 is that there are three widths of
*Schedule*, *HybridSchedule*, and *MatchResult* data frames. The three
widths are *team*, *alliance*, and *match*. Team data frames have one row per
team, requiring six rows to contain all of the data for a single match. The 
*station* column specifies the station to which the team was assigned. Alliance 
data frames have three teams per row, with one row for the blue alliance and 
another for the red alliance. The *alliance* column contains *Red* or *Blue*,
depending on the alliance. Finally, match data frames have six teams per row
and one row per match.

1. The functions `GetSchedule()`, `GetHybridSchedule()`, and `GetMatchResults()`
no longer accept the `expand_cols` argument. These functions can only return a
data frame in team shape. Users can convert between team, alliance, and match 
data frames with the functions `ToAllianceShape()`, `ToMatchShape()`, and 
`ToTeamShape()`.

1. I've added a `MergeReults()` function which merges the *HybridSchedule* and
*Scores* data frames into a single data frame, providing team numbers and
detailed performance data all in one table.

1. I've added the function `GetAll()`, which downloads everything about a single
competition and saves it in a single list.

1. I've added functions for saving FIRST competition data locally in R data
files. The functions `SaveData()` and `ReadData()` display file open and save
dialogs for saving and loading data to and from *.RDS* files.

1. To give users without an authorization key some data to play with, firstapiR
now includes all performance data for the 2016 FIRST Robotics Championships.
The data is available in R lists named `archimedes2016`, `carver2016`,
`curie2016`, etc.

1. I've fixed a few inconsistencies. Some character columns were changed to
factors, some column names were shortened to reduce excess typing, row names are
more consistent, etc.
