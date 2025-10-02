###
# Project: Global birth squeezes
# Purpose: Download files
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 29/09/2025
##

# Functions ==========================================

load_unzip <- function(path) {
  # Temporary file direction
  temp <- tempfile()
  
  # Download the file
  download.file(path, temp, quite = T)
  
  # Load the data
  tmp <- fread(temp)
  file.remove(temp)
  
  return(tmp)
}


# Load the life tables ===============================

# Set the paths
path_lt_est_f <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv.gz"
path_lt_prj_f <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv.gz"
path_lt_est_m <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv.gz"
path_lt_prj_m <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv.gz"

# Collect the paths
paths_lt <- mget(ls(pattern="^path_"))

# Download and unzip the files
for (i in seq_along(paths_lt)) {
  
  cat("File", i, "out of", length(paths_lt), "\n")
  
  # Temporary file direction
  temp <- tempfile()
  
  # Download the file
  download.file(paths_lt[[i]], temp, quite = T)
  
  # Load the data
  tmp <- fread(temp)
  
  # Write the file to raw
  if(str_detect(names(paths_lt)[i], "_f$")) {
    fwrite(tmp, file=file.path("raw", str_sub(paths_lt[[i]], 87, 141)))
  } else {
    fwrite(tmp, file=file.path("raw", str_sub(paths_lt[[i]], 87, 139)))
  }
    
  
  file.remove(temp)
  
}

## 2. Load the birth counts ----------------------------------

# Load the birth counts
path_wpp_births <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Fertility_by_Age1.csv.gz"

# Temporary file direction
temp <- tempfile()

# Download the file
download.file(path_wpp_births, temp, quite = T)

# Load the data
tmp <- fread(temp)

# Save the file
fwrite(tmp, file="raw/WPP2024_Fertility_by_Age1.csv")
rm(tmp)
file.remove(temp)


## Load the population data ---------------------------------

# Download the population data
path_wpp_pop <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv.gz"
path_wpp_pop2 <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Population1JanuaryBySingleAgeSex_Medium_2024-2100.csv.gz"

# Temporary file direction
dt_wpp_pop1 <- load_unzip(path_wpp_pop)
dt_wpp_pop2 <- load_unzip(path_wpp_pop2)

# Combine the data
dt_wpp_pop <- rbindlist(list(dt_wpp_pop1, dt_wpp_pop2))

# Save the file
fwrite(dt_wpp_pop, file="raw/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv")
rm(dt_wpp_pop)
file.remove(temp)

### END ###############################################