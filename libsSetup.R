myLibs <- c("data.table",
            "ggplot2",
            "lubridate",
            "reshape2",
            "hms",
            "plotly",
            "dplyr",
            "ggthemes",
            "gganimate",
            "gifski",
            "png",
            "transformr",
            "rstudioapi",
            "devtools",
            "magick",
            "ggmap",
            "maps")
install.packages(myLibs)
rm(myLibs)

# just choose NONE is it asks you to update something
devtools::install_github("clauswilke/ggtext")
