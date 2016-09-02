## firstapiR: FIRST API R Toolbox

The firstapiR package provides R functions that download FIRST Robotics 
Competition (FRC) data from the FIRST API server via hypertext transfer protocol
(HTTP) requests. The firstapiR functions format the data as JSON text, XML text,
or as R data frames.

### FIRST - *F*or *I*nspiration and *R*ecognition in *S*cience and *T*echnology

FIRST engages kids in kindergarten through high school in exciting, 
mentor-based, research and robotics programs that help them become science and 
technology leaders, as well as well-rounded contributors to society. Learn more 
about FIRST by [visiting their website.](http://www.firstinspires.org/)

FRC is one of four FIRST programs. In FRC, high-school students, in cooperation
with adult mentors, design and build robots and operate them in competitions.
The competitions have detailed rules ([check out the 2016 game manual 
](http://www.firstinspires.org/resource-library/frc/competition-manual-qa-system))
and an extensive amount of data is generated at each competition. All 
scheduling, team, scoring, and ranking data is stored in FIRST servers and is 
made available to users via the FIRST API.

### Getting Access to the FIRST API

To see what the FIRST API can do, visit the [FIRST API documentation page 
](http://docs.frcevents2.apiary.io/#). The documentation page provides detailed 
information on the formats for HTTP request and authorization headers, as well
as detailed descriptions of the data provided by each type of API call.

To use the FIRST API (via firatapiR functions or via HTTP requests that you
build yourself), you must have a username and authorization key issued by FIRST.
To get a username and authorization key, go to the [FIRST Community Developers
page on TeamForge 
](https://usfirst.collab.net/sf/projects/first_community_developers/). Follow 
the instructions on the page to join the project and FIRST will email you a 
username and authorization key. Make sure you keep your authorization key secret
- don't post the key anywhere that is publicly accessible.
