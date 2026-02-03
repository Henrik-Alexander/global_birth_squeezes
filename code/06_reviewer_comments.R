###
# Project: Global birth squeezes
# Purpose: Alternative decomposition figure
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 02/02/2025
##

# Goal: Distinguish into the mortality and the SRB component

library(data.table)
library(tidyverse)

# Load the data
load("data/decomp_pop.Rda")

# Pick a reference year
ref_year <- 1950
ages <- seq(20, 50, by = 15)
years <- seq(1960, 2100, by = 10)

# Select the country examples
countries_examples <- c("China", "Republic of Korea", "India", "Rwanda", "Guatemala")

# Estimate the component -----------------------------------------------------

# Create a new data frame for the decomposition
decomp <- df

# Estimate the mortality component
decomp[, ratio_mortality := cumprod(px_m)/cumprod(px_f), by = .(region, location_code, year)]
decomp[, ratio_birth := srb/100]
decomp[, ratio_pop := ratio_mortality * ratio_birth]

# Select the important columns
decomp <- decomp[!is.na(ratio_birth), .(ratio_pop, ratio_mortality, ratio_birth, region, year, age)]

# Plot the results -----------------------------------------------------------

# Reshape longer
decomp_long <- melt(decomp, id.vars = c("region", "year", "age"))
decomp_ref <- decomp_long[year==ref_year, ]
decomp_ref[, year:=NULL]
decomp_long <- merge(decomp_long, decomp_ref, by=c("region", "age", "variable"), suffixes = c("", "_ref"), all.x=TRUE, all.y=FALSE, allow.cartesian=TRUE)

# Select the data
pop_ratio <- subset(decomp_long, year %in% years & age %in% ages & region %in% countries_examples & variable == "ratio_pop")
decomp_long <- subset(decomp_long, year %in% years & age %in% ages & region %in% countries_examples & variable != "ratio_pop")
decomp_long[, variable:=str_to_title(str_replace(variable, "ratio_", "Contribution "))]

# Plot the data longitudinally
ggplot(data=decomp_long, aes(x=age, fill=variable, group=variable)) +
  geom_hline(yintercept = 1) +
  geom_col(aes(y=value), position=position_dodge()) +
  geom_point(data=pop_ratio, aes(y=value)) +
  #geom_hline(data=decomp_long, aes(yintercept=value, colour=variable, group=variable), linewidth=1.3) +
  facet_grid(region ~ age) +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  scale_y_log10(n.breaks=10) +
  theme(panel.spacing.x=unit(1, "cm"))

# Plot the final version of the figure
ggplot(data=decomp_long, aes(x=year)) +
  geom_hline(yintercept = 1) +
  geom_col(aes(y=value,  group=variable, fill=variable), position=position_dodge(), alpha=0.7) +
  geom_line(data=pop_ratio, aes(y=value, colour="Population sex ratio")) +
  geom_point(data=pop_ratio, aes(y=value, colour="Population sex ratio")) +
  #geom_hline(data=decomp_long, aes(yintercept=value, colour=variable, group=variable), linewidth=1.3) +
  facet_grid(region ~  age) +
  scale_fill_viridis_d(end=0.8) +
  scale_color_manual(values="black") +
  scale_x_continuous("Year", n.breaks=10, expand=c(0, 0)) +
  scale_y_log10("Sex Ratio", n.breaks=8) +
  theme(panel.spacing.x=unit(0.1, "cm"),
        legend.title = element_blank())
ggsave(filename="results/plot_decomp_sex_ratio_contribution.pdf", height=20, width=20, unit="cm")


## Plot the distribution of fertility ------------------------------------------

load("data/wpp_male_tfr_prediction.Rda")

# Estimate the ratio
wpp_tfr_pop[, tfr_ratio := fit / tfr]

# Plot the results
ggplot(subset(wpp_tfr_pop, variant_tfr=="Medium"), aes(x=year, y=tfr_ratio, group=year)) +
  geom_jitter() +
  geom_hline(yintercept=1) +
  geom_boxplot()


### END ########################################################################