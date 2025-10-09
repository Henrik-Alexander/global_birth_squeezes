###
# Project: Global birth squeezes
# Purpose: Analysis
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 29/09/2025
##

rm(list = ls())

# Load the packages
library(sf)
library(tidyverse)
library(data.table)
library(gridExtra)

# Load the results
load("data/parity_age_period.Rda")
load("data/wpp_srb.Rda")

## Functions ---------------------------------------------

locid_m49 <- function(locid) {
  loc <- as.character(locid)
  len <- nchar(loc)
  ifelse(len==1, paste0("00", loc), ifelse(len==2, paste0("0", loc), loc))
}

## Load the map data--------------------------------------

# Set the path to the map-data
path_map <- "U:/data/UN/maps/25M_simplified/UN_Geodata_simplified_25m_shape"
files_map <- list.files(path_map, pattern=".shp$")

# Load the different layers
maps <- lapply(list.files(path_map, pattern=".shp$", full.names = T), read_sf)
names(maps) <- str_sub(files_map, 1, 4)

# UN caption
un_caption <- "The boundaries and names shown and the designations used on this map do not imply official endorsement or acceptance by the United Nations."

# Split the boundaries
# three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
# to do that, we create a separate dataframe with each containing boundaries of the same type
boundaries <- split(maps$BNDL, maps$BNDL$bdytyp)

# Transform the location id
parity_age[year==2023, m49_cd:=locid_m49(location_code)]
dt_wpp_srb[year==2023, m49_cd:=locid_m49(location_code)]

# Merge data with base map
map_parity_age <- merge(maps$BNDA, parity_age[year==2023, ], by="m49_cd", all.x=T, all.y=T)
map_wpp_srb <- merge(maps$BNDA, dt_wpp_srb[year==2023, ], by="m49_cd", all.x=T, all.y=T)

# Create the plotting function
plot_data_map <- function(year, data) {
  
  # Create the ordered variable
  data$N <- ordered(data$N)
  
  # Which year
  data <- data[data$Time==year, ]
  
  # Plot the oceans
  p  <- ggplot() +
    # Plot the country bodies
    geom_sf(data=maps[[1]], fill = "white", colour=NA) +
    geom_sf(data=data, aes(fill=N), colour=NA) +
    # Plot the country boundaries
    # three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
    # to do that, we create a separate dataframe with each containing boundaries of the same type
    # SOLID: 0 is coastlines; 1 is international boundaries; 
    geom_sf(data=boundaries$`0`, colour="black", linewidth=0.1) +
    geom_sf(data=boundaries$`1`, colour="black", linewidth=0.1) + 
    # DASHED: Undetermined boundary lines (e.g. Sudan and South Sudan, State of Palestine)
    geom_sf(data=boundaries$`3`, colour="black", linetype="dashed", linewidth=0.1) +   
    # DOTTED: Jammu and Kashmir line of control
    geom_sf(data=boundaries$`4`, colour="black", linetype="dotted", linewidth=0.1) + 
    # Plot the lakes
    geom_sf(data=maps[["WBYA"]], fill="lightblue1", colour=NA, linewidth=0.1) +
    # Change the axis
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    # Create the plotting
    scale_fill_viridis_d(option="B", direction=1, na.value = "white") +
    # Add the labs
    labs(caption=un_caption) +
    # Create the background
    theme(
      panel.background = element_rect(fill='white'),
      axis.text = element_blank(),
      legend.position="bottom",
      axis.ticks = element_blank()
    ) +
    guides(fill = guide_legend())
  
  return(p)
}

# Water colour: "lightblue1" or "white"
water <- "white"

# Merge the map with 
ggplot() +
  # Plot the country bodies
  geom_sf(data=maps[[1]], fill = "lightblue1", colour=NA) +
  geom_sf(data=map_parity_age, aes(fill=parity_age), colour=NA) +
  # Plot the country boundaries
  # three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
  # to do that, we create a separate dataframe with each containing boundaries of the same type
  # SOLID: 0 is coastlines; 1 is international boundaries; 
  geom_sf(data=boundaries$`0`, colour="black", linewidth=0.1) +
  geom_sf(data=boundaries$`1`, colour="black", linewidth=0.1) + 
  # DASHED: Undetermined boundary lines (e.g. Sudan and South Sudan, State of Palestine)
  geom_sf(data=boundaries$`3`, colour="black", linetype="dashed", linewidth=0.1) +   
  # DOTTED: Jammu and Kashmir line of control
  geom_sf(data=boundaries$`4`, colour="black", linetype="dotted", linewidth=0.1) + 
  # Plot the lakes
  geom_sf(data=maps[["WBYA"]], fill="lightblue1", colour=NA, linewidth=0.1) +
  # Change the axis
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # Create the plotting
  scale_fill_viridis_c("Parity age", option="B", direction=-1, na.value = "white", breaks=seq(0, 100, by=10)) +
  # Add the labs
  labs(caption=un_caption) +
  # Create the background
  theme(
    panel.background = element_rect(fill='lightblue1'),
    axis.text = element_blank(),
    legend.position="bottom",
    axis.ticks = element_blank()
  ) +
  guides(fill = guide_legend(nrow=1))

ggsave(filename="results/worldmap_parity_age_2024.pdf", height=15, width=25, unit="cm")



# Merge the map with 
ggplot() +
  # Plot the country bodies
  geom_sf(data=maps[[1]], fill = "black", colour=NA) +
  geom_sf(data=map_wpp_srb, aes(fill=srb), colour=NA) +
  # Plot the country boundaries
  # three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
  # to do that, we create a separate dataframe with each containing boundaries of the same type
  # SOLID: 0 is coastlines; 1 is international boundaries; 
  geom_sf(data=boundaries$`0`, colour="black", linewidth=0.1) +
  geom_sf(data=boundaries$`1`, colour="black", linewidth=0.1) + 
  # DASHED: Undetermined boundary lines (e.g. Sudan and South Sudan, State of Palestine)
  geom_sf(data=boundaries$`3`, colour="black", linetype="dashed", linewidth=0.1) +   
  # DOTTED: Jammu and Kashmir line of control
  geom_sf(data=boundaries$`4`, colour="black", linetype="dotted", linewidth=0.1) + 
  # Plot the lakes
  geom_sf(data=maps[["WBYA"]], fill=water, colour=NA, linewidth=0.1) +
  # Change the axis
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # Create the plotting
  scale_fill_viridis_c("Sex ratio at birth", option="B", direction=-1) +
  # Add the labs
  #labs(caption=un_caption) +
  # Create the background
  theme(
    panel.background = element_rect(fill=water),
    axis.text = element_blank(),
    legend.position="bottom",
    axis.ticks = element_blank(),
    legend.key.width=unit(2, "cm")
  )
ggsave("results/srb_world_map_2023.svg")

### END ###############################################