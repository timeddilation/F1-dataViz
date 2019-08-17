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

Download the most recent data from the data source, and replace all existing files in the "data" direcory with the new CSVs.