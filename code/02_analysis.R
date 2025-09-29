###
# Project: Global birth squeezes
# Purpose: Analysis
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 29/09/2025
##

library(data.table)
library(ggplot2)

# 1. Data preperation =========================

# Load the data
load("data/wpp_srb.Rda")
load("data/wpp_lt_both.Rda")
load("data/wpp_location.Rda")

# Merge the data
df <- merge(dt_wpp_lt, dt_wpp_srb, by=c("region", "location_code", "year"), all.x=TRUE, all.y=TRUE)

# 2. Plot the data ============================

# Estimate the number of survival
df[, sx_m := srb*cumprod(px_m), by = .(region, location_code, year)]
df[, sx_f := 100*cumprod(px_f), by = .(region, location_code, year)]

# Plot the parity age
df[, female_skewed := ifelse(sx_f>sx_m, 1, 0)]

# Create the parity age
parity_age <- df[female_skewed==1, .(parity_age=min(age, na.rm = T)-0.5), by = .(region, location_code, year)]

# Merge with the location data
parity_age <- merge(parity_age, wpp_location, by="region")

# Plot the parity age
ggplot(data=parity_age, aes(x=year, y=parity_age)) +
  geom_line(aes(group=region, colour=sdg_region)) +
  geom_smooth(se=FALSE, linewidth=2) +
  scale_x_continuous("Year", expand=c(0, 0), breaks=seq(1950, 2030, by=10)) +
  scale_y_continuous("Parity age", breaks=seq(0, 100, by=10)) +
  facet_wrap(~ sdg_region)
ggsave(filename="results/parity_age_globally.pdf", height=20, width=25, unit="cm")

# Plot China and Taiwan
ggplot(data=subset(parity_age, region %in% c("China", "Dem. People's Republic of Korea", "China, Taiwan Province of China" )), aes(x=year, y=parity_age)) +
  geom_line(aes(group=region, colour=region), linewidth=2) +
  scale_x_continuous("Year", expand=c(0, 0), breaks=seq(1950, 2030, by=10)) +
  scale_y_continuous("Parity age", breaks=seq(0, 100, by=10)) +
  facet_wrap(~ region)
ggsave(filename="results/parity_age_asian_examples.pdf", height=15, width=25, unit="cm")


# Plot China and Taiwan
ggplot(data=subset(df, region %in% c("China", "Dem. People's Republic of Korea", "China, Taiwan Province of China" )), aes(x=age, y=sx_m/sx_f)) +
  geom_hline(yintercept = 1) +
  geom_line(aes(group=year), colour="grey", linewidth=0.2) +
  geom_line(data=subset(df, year %in% seq(1950, 2023, by=10) & region %in% c("China", "Dem. People's Republic of Korea", "China, Taiwan Province of China" )), aes(x=age, y=sx_m/sx_f, colour=factor(year), group=year), linewidth=1.4) +
  scale_x_continuous("Age", expand=c(0, 0), breaks=seq(0, 100, by=10)) +
  scale_y_continuous("Sex ratio of life table survivors accounting for SRB", breaks=seq(0, 2, by=0.1), expand=c(0, 0)) +
  scale_colour_viridis_d("Year") +
  facet_wrap(~ region)
ggsave(filename="results/lifetable_sr_asian_examples.pdf", height=15, width=25, unit="cm")

### END #######################################