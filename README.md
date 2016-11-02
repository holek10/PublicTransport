# Real-time Public Transport Tracker

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

This repository contains the source code for an interactive app that display live location of public modes of transportation (trams and buses) for city of Wroclaw, Poland.

## About
The app is build using [R](http://www.r-project.org) and [Shiny](http://shiny.rstudio.com) web framework (with [shinydashboard](https://rstudio.github.io/shinydashboard/) package), utilizing [Leaflet](http://www.leafletjs.com) maps with custom tile from [Mapbox] (http://www.mapbox.com).  
I took the inspiration from two other interactive apps from RStudio folks:
- [Twin Cities Buses](https://gallery.shinyapps.io/086-bus-dashboard/) (source code: https://github.com/rstudio/shiny-examples/tree/master/086-bus-dashboard)
- [SuperZip example](http://shiny.rstudio.com/gallery/superzip-example.html) (source code: https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example)

## Data source
Publicly available data from [Municipal Transport Company](http://mpk.wroc.pl/jak-jezdzimy/mapa-pozycji-pojazdow) offering (not so straighforward) API for vehicle tracking.  
The data is fetched from a [live feed](http://pasazer.mpk.wroc.pl/position.php) which gathers GPS location from all vehicles around Wroclaw area.  
Example POST request takes the following form (where '104' is tram line number):
```
 curl --data "busList[bus][]=104" http://pasazer.mpk.wroc.pl/position.php
```
which gives the following array with coordinates (y = longitude , x = latitude) and unique vehicle identifier (k).
```
{name: "104", type:"bus", y:16.948914, x:51.15738, k:"8667099"}
```
In R we can use `Rcurl` and `jsonlite` packages to get the same output 
```R
library(Rcurl)
library(jsonlite)
busline <- 104
names(busline) <- "busList[bus][]"
dat <- postForm("http://pasazer.mpk.wroc.pl/position.php", .params = busline)
fromJSON(dat)
  name type        y        x       k
1  104  bus 16.94891 51.15739 8667099
2  104  bus 17.01786 51.11339 8667137
3  104  bus 16.95299 51.14373 8667158
```

## How to run the app
To run the app locally you should install required packages: **shiny**,  **shinydashboard**,  **leaflet**, **RCurl**, **jsonlite** and **XML** in R, for example: 
```R
if (!require('shiny')) install.packages("shiny")
```
and use the function `runGithub()` with specified repository name under my username:
```R
shiny::runGitHub("PublicTransport", "holek10")
```
Once the app is loaded select bus or tram line for the left hand side to see the real-time position on the map.  
You can zoom-in the map and/or adjust refresh interval.

## Licensing 
The app is provided for free under GNU General Public License




