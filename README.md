[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=AS69FWTKL5NTL&item_name=Make+more+cool+stuff.&currency_code=USD&source=url)

Bitcoin Donations: 1DGR8GSGn5fNUPjYksXMeYJEgF7kxkgCw4

Various data visualizations of F1 race data.

Data sourced from: http://ergast.com/mrd/

# Getting Started

* Requires R version 3.6.* https://www.r-project.org/
* Requires RStudio https://www.rstudio.com/
* Requires Rtools (will be prompted to install this later)
* Requires imagemagick be installed: https://imagemagick.org/script/download.php
** This does not work on windows unless you use a bash emu. I recommend cmder: https://cmder.net/
* Start by running libsSetup.R to get the packages installed used by these scripts.
* Then run the helperFunctions.R to load in the data and add functions used to assist some metrics.

Then you can run any other script.

# Updating the Data

Download the most recent data from the data source, and replace all existing files in the "data" directory with the new CSVs.

# Visuals

A few functions are present to generate aesthetics for the plots. 

## Planned features

* Race weather indicators
** Rain or shine
** Air and track temperature
* F1 rule set
** Engine type(s) allowed
** Refueling allowed indicator

## Word Map generator
This function will generate a map of the world, with a target over the geo-coordinates of the circuit. 
It outputs a raster grob object of the map to annotate a viz with. 

`australiaCircuitWorldMap <- generateCircuitWorldMap(circuit_Id = 1)`

## Get Circuit Image
This function accepts a circuit ID and a transparency value. 
It outputs a raster grob object of the track found on the wikipedia page for the track, with a transparency defined as a value between 0 and 1, 0 being transparent. 
It currently relies on the PNG images stored locally in the repository as opposed to GET-ing them directly from the wiki. 
This is done because I currently only read PNG files, and have not prepared the project to dynamically read other image types. 

`australiaCircuitImage <- getTrackImage(circuit_Id = 1, transparencyVal = 0.2)`

## Lap Times Density
This metric is designed to analyze lap times for a single circuit, and visualize how lap times change over time. 
Laps for a race where the driver pitted, and the out lap after, are removed in order to only evaluate "race laps".

![](lapTimeDensity_Monaco.gif)

This script outputs 3 separate animations. 
The final graphic is a composite of all 3. 
Each animation transitions it's state based on the year of a Grand Prix.
The first (top) is a [density estimation](https://en.wikipedia.org/wiki/Density_estimation) plot of all lap times on the circuit. 
The second (bottom left) is a [boxplot](https://en.wikipedia.org/wiki/Box_plot) of the same lap times. 
And the third (bottom right) is simply a line graph of the median lap times.

There is a separate shell script in the "gifFiles" directory to join the 3 animations together. 
The shell script relies on imagemagick to perform the joining, and therefore must be installed if you want to join the gifs together. 
Otherwise, the R script does what it's intended to do, which is output 3 separate gifs.

## Qualifying Times Density
Same as Lap Times Density, but uses qualifying times data instead of race lap times data.

[![Donate](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=AS69FWTKL5NTL&item_name=Make+more+cool+stuff.&currency_code=USD&source=url)
