###
# Project: Global birth squeezes
# Purpose: META-file
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 26/09/2025
##

# Set the meta-parameters ==============================

install_packages <- FALSE


# Create the folder structure ==========================

folders <- c("code", "raw", "data", "results", "text", "R.utils")
lapply(folders, function(folder) if(!dir.exists(folder)) dir.create(folder))


# Install the packages =================================

if(install_packages) {
  packages <- c("tidyverse", "data.table")
  install.packages(packages)
}

# 


### END ################################################