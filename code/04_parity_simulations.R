###
# Project: Global birth squeezes
# Purpose: Analysis
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 04/10/2025
##

# Functions ================================

# Load the data ============================

# Load the population data
load("data/wpp_pop.Rda")

# Simulations =============================

simulate_opportunities <- function(men=1000, women=1000, places=500, quota=0.5) {
  if(men < places * (1-quota) | women < places * quota) warning("More places than people!")
  chance_men <-  places * (1-quota) / men
  chance_women <- places * quota / women
  cat("Chance women =", chance_women, "\n")
  cat("Chance men =", chance_men, "\n")
  cat("relative change of women to men=", round(100*(chance_women-chance_men) / (chance_women), 2), "%\n")
}

# Simulate excess men
simulate_opportunities(men=1000)
simulate_opportunities(men=106, women=100, places=50)

# Function to obtain a fair quota
obtain_fair_quota <- function(men=1000, women=1000, places=NULL){
  # Estimate the total number of places
  total <- men+women
  
  if (is.null(places)) {
    places <- trunc(0.1*total)
  }
  # Create the share places
  share_places <- places/total
  # Estimate the men and women places
  men_places <- share_places * men
  wom_places <- share_places * women
  
  # Estimate the quota
  cat("Estimate places=", men_places + wom_places, "\n")
  
  # Estimate the quota
  cat("Quota:", wom_places/places)
  
}

# Plot the world population structure ------

dt_wpp_pop[, age_group:=fcase(age <= 14, "0-14", 
                                             age >=15 & age <= 64, "15-64",
                                             age >= 65, "65+")]


# Aggregate the world population
sr_age <- dt_wpp_pop[, .(sr=sum(pop_male)/sum(pop_female)), by=.(region, year, age_group, variant)]

# Plot the trend in the sex ratio
scaling <- 1
cutoff <- 2025
ggplot() + 
  geom_vline(xintercept=cutoff, linetype="dotted") +
  geom_hline(yintercept=1*scaling) +
  geom_line(data=subset(sr_age, region=="World" & year>=cutoff), aes(x=year, y=sr*scaling, group=age_group, colour=age_group), linetype="dotted", linewidth=1.2) +
  geom_line(data=subset(sr_age, region=="World" & year<=cutoff), aes(x=year, y=sr*scaling, group=age_group, colour=age_group)) +
  scale_x_continuous("Year", expand=c(0, 0), breaks=seq(1950, 2100, by=10)) +
  scale_y_continuous("Population sex ratio (men/women)", expand=c(0.1, 0), trans="log10", n.breaks=10) +
  scale_colour_brewer("Age group:", palette="Set1")

ggsave(filename="results/population_sr_age.svg", height=15, width=25, unit="cm")
  
  
ggplot() + 
  geom_vline(xintercept=cutoff, linetype="dotted") +
  geom_hline(yintercept=1*scaling) +
  geom_line(data=subset(sr_age, region=="Rwanda" & year>=cutoff), aes(x=year, y=sr*scaling, group=age_group, colour=age_group), linetype="dotted", linewidth=1.2) +
  geom_line(data=subset(sr_age, region=="Rwanda" & year<=cutoff), aes(x=year, y=sr*scaling, group=age_group, colour=age_group)) +
  scale_x_continuous("Year", expand=c(0, 0), breaks=seq(1950, 2100, by=10)) +
  scale_y_continuous("Population sex ratio (men/women)", expand=c(0.1, 0), trans="log10", n.breaks=10) +
  scale_colour_brewer("Age group:", palette="Set1")

### END ####################################