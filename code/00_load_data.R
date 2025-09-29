###
# Project: Global birth squeezes
# Purpose: Download files
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 29/09/2025
##

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


### END ###############################################