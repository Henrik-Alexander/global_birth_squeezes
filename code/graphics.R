#####
# Project: Global birth squeezes
# Purpose: Graphics
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 29.09.2025
######

library(ggplot2)

# Set the graphic scheme
theme_set(theme_test(base_size=12, base_family="serif"))
theme_update(panel.grid.major=element_line(linewidth=0.2, linetype="dotted", colour="lightgrey"),
             axis.text=element_text(colour="black"),
             strip.background = element_rect(fill="white"),
             strip.text = element_text(face="bold"),
             legend.position="bottom", 
             panel.spacing = unit(0.5, "cm"),
             plot.margin = margin(0.1, 0.5, 0.1, 0.1, "cm")
             )

### END ###########################