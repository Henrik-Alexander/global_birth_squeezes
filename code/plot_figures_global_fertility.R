library(tidyverse)
library(sf)


source("code/graphics.R")

# Load the locations
load("data/wpp_location.Rda")

# Load the data
load("data/wpp_tfr.Rda")

## Functions ================================================

locid_m49 <- function(locid) {
  loc <- as.character(locid)
  len <- nchar(loc)
  ifelse(len==1, paste0("00", loc), ifelse(len==2, paste0("0", loc), loc))
}


# Global TFR ================================================

file_path <- "U:/projects/33_decomposing_fertility_decline"

# Extract the intervals
wpp_tfr_intervals <- wpp_tfr[str_detect(wpp_tfr$variant, "PI$"), ]
wpp_tfr_intervals <- wpp_tfr_intervals |> select(-births) |> pivot_wider(names_from="variant", values_from = "tfr")

# Plot the trend in the world TFR
ggplot() +
    geom_hline(yintercept=2.1) +
    geom_vline(xintercept=2024, linetype="dotted") +
    geom_ribbon(data=subset(wpp_tfr_intervals, region=="World"), aes(x=year, ymin =`Lower 80 PI`/1000, ymax=`Upper 80 PI`/1000), alpha=0.2, fill="black") +
    geom_ribbon(data=subset(wpp_tfr_intervals, region=="World"), aes(x=year, ymin =`Lower 95 PI`/1000, ymax=`Upper 95 PI`/1000), alpha=0.2, fill="black") +
    geom_line(data=subset(wpp_tfr, region=="World"&str_detect(variant, "Medium")), aes(x=year, y=tfr/1000, group=variant), linewidth=1.5) +
    geom_line(data=subset(wpp_tfr_intervals, region=="World"), aes(x=year, y=`Median PI`/1000),  linewidth=1.5, colour="black", linetype="dotted") +
    scale_x_continuous("Year", n.breaks=20, expand=c(0, 0)) +
    scale_y_continuous("Total Fertility Rate", n.breaks=10) +
  guides(colour="none")
ggsave(file=file.path(file_path, "figures/world_tfr_development.pdf"), height=15, width=20, unit="cm")

# Number of countries with below-replacement fertility ======

# Merge lcoations with tfr data
wpp_tfr <- merge(wpp_tfr, wpp_location)

# Aggregate the counts
wpp_replacement_tfr <- wpp_tfr |> 
  filter(location_type=="Country/Area") |> 
  mutate(low_fertility=ifelse(tfr/1000<2.1, "below replacement fertility", "above replacement fertility"))


# Aggregate the share of replacement fertility
wpp_replacement_tfr |> 
  filter(year < 2024) |> 
  group_by(year, low_fertility) |> 
  count() |> 
  ggplot(aes(x=year, y=n, fill=low_fertility)) +
  geom_col() +
  scale_x_continuous("Year", expand=c(0, 0), n.breaks=20) +
  scale_y_continuous("Number of countries", expand=c(0, 0), n.breaks=10) +
  geom_hline(yintercept = length(unique(wpp_location$region[wpp_location$location_type=="Country/Area"]))/2) +
  scale_fill_viridis_d("", option="E")
ggsave(file=file.path(file_path, "figures/world_tfr_below_replacement.pdf"), height=15, width=20, unit="cm")


# World map of fertility levels =============================

# Set the path to the map-data
path_map <- "U:/data/UN/maps/25M_simplified/UN_Geodata_simplified_25m_shape"
files_map <- list.files(path_map, pattern=".shp$")

# Load the different layers
maps <- lapply(list.files(path_map, pattern=".shp$", full.names = T), read_sf)
names(maps) <- str_sub(files_map, 1, 4)

# Split the boundaries
# three styles of boundaries should be mapped: standard solid line, dashed line for undetermined boundaries, dotted for selected disputed boundaries
# to do that, we create a separate dataframe with each containing boundaries of the same type
boundaries <- split(maps$BNDL, maps$BNDL$bdytyp)

# Transform the location id
wpp_replacement_tfr$m49_cd <- locid_m49(wpp_replacement_tfr$location_code)

# Merge data with base map
map_tfr_world <- merge(maps$BNDA, wpp_replacement_tfr[wpp_replacement_tfr$year==2023, ], by="m49_cd", all.x=T, all.y=T)

# Water colour: "lightblue1" or "white"
water <- "white"

# Merge the map with 
ggplot() +
  # Plot the country bodies
  geom_sf(data=maps[[1]], fill = water, colour=NA) +
  geom_sf(data=map_tfr_world, aes(fill=tfr/1000), colour=NA) +
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
  scale_fill_viridis_c("Parity age", option="B", direction=1, na.value = "white", breaks=seq(0, 6, by=0.5)) +
  # Add the labs
  # Create the background
  theme(
    panel.background = element_rect(fill=water),
    axis.text = element_blank(),
    legend.position="bottom",
    axis.ticks = element_blank(),
    legend.key.width=unit(2, "cm")
  )

ggsave(file=file.path(file_path, "figures/world_map_tfr.pdf"), height=15, width=25, unit="cm")



# Merge the map with 
ggplot() +
  # Plot the country bodies
  geom_sf(data=maps[[1]], fill = water, colour=NA) +
  geom_sf(data=map_tfr_world, aes(fill=low_fertility), colour=NA) +
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
  scale_fill_viridis_d("", option="E") +
  # Add the labs
  # Create the background
  theme(
    panel.background = element_rect(fill=water),
    axis.text = element_blank(),
    legend.position="bottom",
    axis.ticks = element_blank()
    )
ggsave(file=file.path(file_path, "figures/world_map_below_replacement.pdf"), height=15, width=25, unit="cm")

### END ###############################################