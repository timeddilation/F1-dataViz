# requires imagemagick be installed: https://imagemagick.org/script/download.php
magick convert spread.gif -repage 960x100 -coalesce \
          null: \( median.gif -coalesce \) \
          -geometry +480+0 -layers Composite    spreadMedian.gif
		  
magick convert density.gif -repage 960x540 -coalesce \
          null: \( spreadMedian.gif -coalesce \) \
          -geometry +0+440 -layers Composite    lapTimesDensity.gif
		  
rm -f spread.gif median.gif density.gif spreadMedian.gif