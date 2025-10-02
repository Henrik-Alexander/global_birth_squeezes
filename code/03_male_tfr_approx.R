###
# Project: Global birth squeezes
# Purpose: Analysis
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 29/09/2025
##

rm(list=ls());gc(TRUE)

library(data.table)
library(ggplot2)

# Load the graphical scheme
source("code/graphics.R")

# Load the adult sex ratios
load("data/wpp_pop_adult_sr.Rda")

# Load and prepare data =============================

# Create the female, male TFR and adult population sex ratio data
files_fert_country <- list.files("data", pattern="fertility_", full.names = TRUE)
lapply(files_fert_country, load, envir = .GlobalEnv)

# Bind the country files
fert_subnational <- rbindlist(mget(ls(pattern="^fert_")), fill=TRUE, idcol="country")

# Clear the country name
fert_subnational[, country:=str_to_title(str_remove(country, "^fert_"))]

# Re-estimate the TFR ratio
fert_subnational[, tfr_ratio:=tfr_male/tfr_female]

# Create a data column
fert_subnational[, data:="Subnational fertility data"]

# Plot the resilationship between 
ggplot(data=fert_subnational, aes(x=asr, y=tfr_ratio)) +
  geom_hline(yintercept=1, linetype="dashed") +
  geom_vline(xintercept=1, linetype="dashed") +
  geom_smooth(aes(colour=country), method="lm", se=F) +
  geom_point(aes(colour=country), alpha=0.3) +
  geom_smooth(method="lm", se=F, colour="black", linewidth=2) +
  scale_x_continuous("Sex Ratio (age 20-30)", trans="log10", n.breaks=10) +
  scale_y_continuous("TFR male/TFR female", trans="log10", n.breaks=10)

ggsave("results/regional_tfr_adult_sr.pdf", height=20, width=20, unit="cm")

### Prepare the national fertility data =============

# Load the national male TFR
load("data/male_national_fertility.Rda")

## Recode country names in the national fertility data

# Which countries need recoding?
recode_countries <- unique(fert_national$country[is.na(fert_national$pop_male)])

# U.S.A and West Germany
ccd$country[ccd$country=="U.S.A."] <- "United States of America"

# Merge the country TFR with the wpp adult population
fert_national <- merge(ccd, wpp_pop_adult, by.x=c("country", "year"), by.y=c("region", "year"))

# Estimate the regression model =====================

# Combine the national and the subnaitonal data
fert_global <- rbindlist(list(fert_subnational, fert_national), fill=TRUE)

# Plot the resilationship between 
ggplot(data=fert_global, aes(x=asr, y=tfr_ratio)) +
  geom_hline(yintercept=1, linetype="dashed") +
  geom_vline(xintercept=1, linetype="dashed") +
  facet_wrap(~ data) +
  #geom_smooth(aes(colour=country), method="lm", se=F) +
  geom_point(aes(colour=country), alpha=0.3) +
  geom_smooth(method="lm", se=F, colour="black", linewidth=1) +
  scale_x_continuous("Sex Ratio (age 20-30)", trans="log10", n.breaks=10) +
  scale_y_continuous("TFR male/TFR female", trans="log10", n.breaks=10)

# SAvew the data
ggsave("results/global_tfr_adult_sr.pdf", height=25, width=20, unit="cm")


# Estimate the regression model
model_approximation <- lm(log(tfr_male)~log(tfr_female) + log(asr), data=fert_global)


# Look at the model result
summary(model_approximation)

# Print the r-squared
summary_model_approximation <- summary(model_approximation)

# Print the fit statistics
cat("R-squared=", summary_model_approximation$r.squared, "\n")
cat("R-square adjuted=", summary_model_approximation$adj.r.squared, "\n")


### END #############################################