###
# Project: Global birth squeezes
# Purpose: META-file
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 26/09/2025
##

library(gglot2)
library(readxl)
library(data.table)
library(stringr)

# Load the files
raw_files <- list.files("raw", full.names=T)


# Clean the SRB =================================

# Load the demographic indicators compats
dt_wpp_demo <- read_xlsx(raw_files[str_detect(raw_files, "DEMOGRAPHIC_INDICATORS")], skip=16)


# Select the SRB and the location
dt_wpp_srb <- dt_wpp_demo[, c("Index", "Variant", "Region, subregion, country or area *", "Year", "Sex Ratio at Birth (males per 100 female births)")]


# Clean the life-tables ========================


# Clean the fertility data =====================




### END #########################################