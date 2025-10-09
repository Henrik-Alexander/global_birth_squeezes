###
# Project: Global birth squeezes
# Purpose: Load and clean wpp data
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 26/09/2025
##

rm(list=ls())

library(ggplot2)
library(readxl)
library(data.table)
library(stringr)

# Load the files
raw_files <- list.files("raw", full.names=T)

# Clean the population data =====================

# Load the population data
dt_wpp_pop <- fread("raw/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv")

# Select the columns
dt_wpp_pop <- dt_wpp_pop[, c("Variant", "Location", "LocID", "Time", "AgeGrp", "AgeGrpSpan", "PopMale", "PopFemale")]

# Rename the columns
setnames(dt_wpp_pop, old=names(dt_wpp_pop), new=c("variant", "region", "location_code", "year", "age", "n", "pop_male", "pop_female"))

# Transform the age variable in the population data
dt_wpp_pop[, age:=as.numeric(str_remove_all(age, "\\+"))]

# Save the data
save(dt_wpp_pop, file="data/wpp_pop.Rda")
rm(dt_wpp_pop)

# Clean the SRB =================================

# Load the demographic indicators compats
dt_wpp_demo <- as.data.table(read_xlsx(raw_files[str_detect(raw_files, "DEMOGRAPHIC_INDICATORS")], skip=16))

# Select the SRB and the location
dt_wpp_pop <- dt_wpp_pop[, c("Variant", "Location", "LocID", "Time", "AgeGrp", "AgegrpSpan", "PopMale", "PopFemale")]

# Set the names
setnames(dt_wpp_srb, old=names(dt_wpp_srb), new=c("variant", "region", "location_code", "year", "srb"))

# Make the srb column numeric
dt_wpp_srb[, srb:=as.numeric(srb)]

# Save the data
save(dt_wpp_srb, file="data/wpp_srb.Rda")

# Clean the life-tables ========================

# Load the individual life tables
dt_lt_est_m <- fread(raw_files[str_detect(raw_files, "Life_Table_Complete_Medium_Male_1950-2023")])
dt_lt_proj_m <- fread(raw_files[str_detect(raw_files, "Life_Table_Complete_Medium_Male_2024-2100")])
dt_lt_est_f <- fread(raw_files[str_detect(raw_files, "Life_Table_Complete_Medium_Female_1950-2023")])
dt_lt_proj_f <- fread(raw_files[str_detect(raw_files, "Life_Table_Complete_Medium_Female_2024-2100")])

# Bind the projection and estimation life tables together
dt_lt_m <- rbindlist(list(dt_lt_est_m, dt_lt_proj_m))
dt_lt_f <- rbindlist(list(dt_lt_est_f, dt_lt_proj_f))

# Add the column for sex
dt_lt_m <- dt_lt_m[, c("Variant", "Location", "LocID", "Time", "Sex",  "AgeGrpStart", "AgeGrpSpan", "px")]
dt_lt_f <- dt_lt_f[, c("Variant", "Location", "LocID", "Time", "Sex",  "AgeGrpStart", "AgeGrpSpan", "px")]

# Rename the columns
setnames(dt_lt_m, old=names(dt_lt_m), new=c("variant", "region", "location_code", "year", "sex", "age", "n", "px"))
setnames(dt_lt_f, old=names(dt_lt_f), new=c("variant", "region", "location_code", "year", "sex", "age", "n", "px"))

# Merge the two life tables
dt_wpp_lt <- merge(dt_lt_m, dt_lt_f, suffixes=c("_m", "_f"), by=c("region", "location_code", "year", "age", "n"))

# Save the life tables
save(dt_wpp_lt, file="data/wpp_lt_both.Rda")

# Clean the location data ======================

# Load the location files
wpp_location <- fread(raw_files[str_detect(raw_files, "LOCATIONS")])

# Select the needed columns
wpp_location <- wpp_location[, .(Location, LocTypeName, GeoRegName, SDGRegName, SDGSubRegName)]

# Rename the columns
setnames(wpp_location, old = names(wpp_location), new=c("region", "location_type", "region_name", "sdg_region", "sdg_subregion"))

# Save the data
save(wpp_location, file = "data/wpp_location.Rda")

# Clean the birth data =========================

# Load the birth counts
wpp_births <- fread("raw/WPP2024_Fertility_by_Age1.csv")

# Select the most important variables
wpp_births <- wpp_births[, c("Variant", "Location", "LocID", "Time", "AgeGrp",  "AgeGrpSpan", "ASFR", "Births")]

# Rename the variables
setnames(wpp_births, old=names(wpp_births), new=c("variant", "region", "location_code", "year", "age_mother", "n", "asfr", "births"))

# Save the data
save(wpp_births, file="data/wpp_births.Rda")

### END #########################################