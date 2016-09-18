# firstapiR: An R Package for Using the FIRST API Server

The firstapiR package provides R functions that download FIRST Robotics 
Competition (FRC) data from the FIRST API server via hypertext transfer protocol
(HTTP) requests. The firstapiR functions format the data as JSON text, XML text,
or as R data frames.

### Installing R

1. Download and install R from the [Comprehensive R Archive Network's (CRAN)
website](https://www.r-project.org/). R is available for Windows, Mac, and
Linux.

1. No other tools are reqired to use the firstapiR package. You can use all 
firstapiR functions with just the basic R download from step 1. However, I 
recormmend that you download and install the free version of RStudio from the
[Rstudio website](https://www.rstudio.com/). The remainder of these
instructions assume that you are using RStudio -- but it shouldn't be difficult
to interpret these instructions for other R environments.


### Installing the firstapiR package

1. Install the devtools package. This is easy to do in RStudio - just select
_Tools->Install Packages..._ from the menu bar, type "devtools" in the
package name box and hit _Install_.

1. The devtools package provides a function for installing R packages from
GitHub. Type the following command at the RStudio console prompt:
```R
devtools::install_github("irwinsnet/firstapiR")
```

The `install_github` command should automatically install any dependent packages. 
If R complains about some package being missing, just install it from the CRAN 
repository using RStudio's install packages feature (see step 1) and then
repeat step 2.


### Getting Access to the FIRST API

The firstapiR package does contain some example data from the FIRST API server,
so you can experiment with firstapiR without a FIRST API server account. See the
[Getting Started Vignette](inst/doc/Getting_Started.html) for guidance.

To use firstapiR to download data from the FIRST API server, you must have a
username and authorization key for the FIRST API server. To get a username and
authorization key, go to the [FIRST Community Developers page on TeamForge 
](https://usfirst.collab.net/sf/projects/first_community_developers/). Follow 
the instructions on the page to join the project and FIRST will email you a 
username and authorization key. Make sure you keep your authorization key
secret -- don't post the key anywhere that is publicly accessible.

Detailed documentation on the FIRST API is available at the [FIRST API
documentation page](http://docs.frcevents2.apiary.io/#). The documentation page
provides detailed information on the URL formats and headers that are required
for HTTP requests to the FIRST API server, as well as the HTTP responses.


### Learning More About the FIRST API Server

Documentation for firstapiR is automatically downloaded when you install the
firstapiR package and is available offline. To learn more about firstapiR:

* Read the (Getting Started Vignette)[
http://htmlpreview.github.io/?https://github.com/irwinsnet/firstapiR/blob/master/inst/doc/Getting_Started.html]. 
* Read the (Functions Overview Vignette)[http://htmlpreview.github.io/?https://github.com/irwinsnet/firstapiR/blob/master/inst/doc/Functions.html]. 
* Read the help documentation that is provided for each function. 

You can read the vignettes directly in RStudio if firstapiR is installed. Click 
on the firstapiR package in the RSudio _Packages_ window, which brings up an
index to all firstapiR help resources. Click on the _Userguides, package
vignettes..._ link to navigate to the _Getting Started Vignette_.


### FIRST - *F*or *I*nspiration and *R*ecognition in *S*cience and *T*echnology

FIRST engages kids in kindergarten through high school in exciting, 
mentor-based, research and robotics programs that help them become science and 
technology leaders, as well as well-rounded contributors to society. Learn more 
about FIRST by [visiting their website.](http://www.firstinspires.org/)

FRC is one of four FIRST programs. In FRC, high-school students, in cooperation
with adult mentors, design and build robots and operate them in competitions.
The competitions have detailed rules ([check out the 2016 game
manual](http://www.firstinspires.org/resource-library/frc/competition-manual-qa-system))
and an extensive amount of data is generated at each competition. All 
scheduling, team, scoring, and ranking data is stored in FIRST servers and is 
made available to users via the FIRST API.
