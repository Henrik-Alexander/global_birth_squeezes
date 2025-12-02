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

folders <- c("code", "raw", "data", "results", "text")
lapply(folders, function(folder) if(!dir.exists(folder)) dir.create(folder))


# Install the packages =================================

if(install_packages) {
  packages <- c("tidyverse", "data.table", "R.utils", "gg3D")
  install.packages(packages)
  remotes::install_github("AckerDWM/gg3D")
}

# 

<<<<<<< HEAD


# Run the code =========================================

# 1. Load the data
source("code/00_load_data.R")

# 2. Clean and prepare the data
source("code/01_clean_data.R")

# 3. Analyze the population sex ratios
source("code/02_demographic_analysis.R")

# 4. Estimate the male TFR
source("code/03_male_tfr_approach.R")

# 5. Estimate the age grap approach
source("code/04_age_gap_approach.R")
=======
>>>>>>> parent of 712dd5e (Include Christian's estimation|)

### END ################################################