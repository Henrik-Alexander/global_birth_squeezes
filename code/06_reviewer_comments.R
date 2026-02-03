###
# Project: Global birth squeezes
# Purpose: Reviewer comments (Round 1)
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 02/02/2025
##

rm(list = ls())

library(data.table)
library(tidyverse)

# Comment R. 1. C. 2 ===========================================================

# Estimate levels of childlessness
# Register data: Norway and Finland
# Survey data: Rest

# Load the GGS harmonized birth histories
ggs_births <- fread("raw/GGS/HarmonizedHistoriesII_2026_01_21.csv")

## Load the data from Tanturri 2015
childlessness_tanturri <- fread("raw/childlessness-worldwide.csv")

# Load the fertility data
load("data/wpp_male_tfr_prediction.Rda")

# Filter the coutnries
tfr_wpp <- wpp_tfr_pop[region %in% childlessness_tanturri$Country, ]

# Aggregate the data and filter
tfr_wpp_1990s <- tfr_wpp[year %in% 1990:1999, .(tfr_ratio = mean(fit) / mean(tfr)), by = .(region)]


# Comment R. 1. C. 4 ===========================================================

# Goal: Replot figure 3
# Distinguish into the mortality and the SRB component


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

# Reviewer 2 ===================================================================

# Comment R. 2 C. 2 ============================================================ 

# Plot the gender gap in mortality
lt_future_f <- fread("raw/WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv")
lt_future_m <- fread("raw/WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv")

# Select the columns
lt_future_f <- lt_future_f[, .(Location, Time, Age=AgeGrpStart, Sex, lx)]
lt_future_m <- lt_future_m[, .(Location, Time, Age=AgeGrpStart, Sex, lx)]

# Bind the data together
lt_future <- rbindlist(list(lt_future_f, lt_future_m))

# Keep the unique versions
lt_future <- unique(lt_future, by = c("Location", "Time", "Age", "Sex"))

# Reshape the data long-to-wide
lt_future <- lt_future <- dcast(lt_future, formula = "Location + Time + Age ~ Sex", value.var = "lx")

# Estimate the ratio of survivors
lt_future[, pop_ratio := Male / Female]

# Plot the development for Sub-Saharan Africa
ggplot(data=subset(lt_future, Location %in% c("Sub-Saharan Africa", "High-income countries") & Age < 50), aes(x=Age, y=pop_ratio*1.05, colour=Time, group=Time)) +
  geom_hline(yintercept = 1) +
  geom_line() +
  scale_colour_viridis_c() +
  scale_x_continuous(expand = c(0, 0), n.breaks = 10) +
  scale_y_log10("Population sex ratio") +
  facet_wrap(~Location)
ggsave("results/pop_ratio_ssa.pdf", width=15, height=10, unit="cm")


# Comment R. 2. C. 2 ===========================================================

# Goal: Replot figure 3
# Distinguish into the mortality and the SRB component
load("data/wpp_male_tfr_prediction.Rda")

# Estimate the ratio
wpp_tfr_pop[, tfr_ratio := fit / tfr]

# Plot the results
ggplot(subset(wpp_tfr_pop, variant_tfr=="Medium"), aes(x=year, y=tfr_ratio, group=year)) +
  geom_jitter() +
  geom_hline(yintercept=1) +
  geom_boxplot()


# Plot the distributions
tfr_distribution <- wpp_tfr_pop[, .(max_tfr_ratio = max(tfr_ratio), 
                min_tfr_ratio = min(tfr_ratio), 
                median_tfr_ratio = median(tfr_ratio),
                lower80_tfr_ratio = quantile(tfr_ratio, prob = (1-0.8)/2),
                lower95_tfr_ratio = quantile(tfr_ratio, prob = (1-0.95)/2),
                upper80_tfr_ratio = quantile(tfr_ratio, prob = 1-(1-0.8)/2),
                upper95_tfr_ratio = quantile(tfr_ratio, prob = 1 - (1-0.95)/2)), by = .(year)]


# Plot the distribution
ggplot(tfr_distribution, aes(x=year)) +
  geom_line(aes(y=median_tfr_ratio, linetype = "Median"), linewidth = 1.5) +
  geom_line(aes(y=max_tfr_ratio, linetype = "Maximum")) +
  geom_line(aes(y=min_tfr_ratio, linetype = "Minimum")) +
  geom_ribbon(aes(ymin = lower80_tfr_ratio, ymax = upper80_tfr_ratio, alpha = "80%")) +
  geom_ribbon(aes(ymin = lower95_tfr_ratio, ymax = upper95_tfr_ratio, alpha = "95%")) +
  geom_hline(yintercept = 1, colour = "red", linetype = "dotted") +
  geom_vline(xintercept = 2025, colour = "red", linetype = "dotted") +
  scale_alpha_discrete("Quantiles: ", range = c(0.5, 0.3)) +
  scale_linetype_manual("", values = c(2, 1, 2)) +
  scale_x_continuous("Year", breaks = seq(1950, 2100, by = 10), expand = c(0, 0)) +
  scale_y_log10("TFR ratio (male TFR / female TFR)", n.breaks=12)
ggsave("results/distribution_tfr_ratio_time.pdf", height=10, width = 15, unit="cm")

### END ########################################################################