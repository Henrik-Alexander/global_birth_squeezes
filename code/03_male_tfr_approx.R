###
# Project: Global birth squeezes
# Purpose: Analysis
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 29/09/2025
##

rm(list=ls()); gc(TRUE)

library(data.table)
library(ggplot2)
library(stringr)
library(gg3D)

# Load the graphical scheme
source("code/graphics.R")

# Load the adult sex ratios
load("data/wpp_pop_adult_sr.Rda")

# Load the wpp locations
load("data/wpp_location.Rda")

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

# Remove very low estimates of male and female TFR
fert_subnational <- fert_subnational[tfr_female>0.5 & tfr_male>0.5, ]

### Prepare the national fertility data =============

# Load the national male TFR
load("data/male_national_fertility.Rda")

## Recode country names in the national fertility data

# Merge the country TFR with the wpp adult population
fert_national <- merge(ccd, wpp_pop_adult, by.x=c("country", "year"), by.y=c("region", "year"), all.x=TRUE)

# Which countries need recoding?
recode_countries <- unique(fert_national$country[is.na(fert_national$pop_male)])
for(country in recode_countries) {cat("ccd$country[ccd$county==", paste0("\"", country, "\""), "] <- ", paste0("\"", country, "\""), "\n")}

# U.S.A and West Germany
ccd$country[ccd$county== "Central African Republic (the)" ] <-  "Central African Republic" 
ccd$country[ccd$county== "Comoros (the)" ] <-  "Comoros" 
ccd$country[ccd$county== "Congo (the Democratic Republic of the)" ] <-  "Democratic Republic of the Congo" 
ccd$country[ccd$county== "Congo (the)" ] <-  "Congo" 
wpp_pop_adult$country[wpp_pop_adult$county== "CÃ´te d'Ivoire" ] <-  "Côte d'Ivoire" 
ccd$country[ccd$county== "Dominican Republic (the)" ] <-  "Dominican Republic" 
ccd$country[ccd$county== "East Germany" ] <-  "East Germany" 
ccd$country[ccd$county== "England and Wales" ] <-  "United Kingdom" 
ccd$country[ccd$county== "Gambia (the)" ] <-  "Gambia" 
ccd$country[ccd$county== "Hong Kong" ] <-  "China, Hong Kong SAR" 
ccd$country[ccd$county== "Korea (the Republic of)" ] <-  "Republic of Korea" 
ccd$country[ccd$county== "Lao People's Democratic Republic (the)" ] <-  "Lao People's Democratic Republic" 
ccd$country[ccd$county== "Macedonia (the former Yugoslav Republic of)" ] <-  "North Macedonia" 
ccd$country[ccd$county== "Moldova (the Republic of)" ] <-  "Republic of Moldova" 
ccd$country[ccd$county== "Netherlands (the)" ] <-  "Netherlands" 
ccd$country[ccd$county== "Niger (the)" ] <-  "Niger" 
ccd$country[ccd$county== "Palestine, State of" ] <-  "Palestine, State of" 
ccd$country[ccd$county== "Philippines (the)" ] <-  "Philippines" 
ccd$country[ccd$county== "Rumania" ] <-  "Romania" 
ccd$country[ccd$county== "Russian Federation (the)" ] <-  "Russian Federation" 
ccd$country[ccd$county== "Sudan (the)" ] <-  "Sudan" 
ccd$country[ccd$county== "Swaziland" ] <-  "Swaziland" 
ccd$country[ccd$county== "Taiwan" ] <-  "China, Taiwan Province of China" 
ccd$country[ccd$county== "Tanzania, United Republic of" ] <-  "United Republic of Tanzania" 
wpp_pop_adult$country[wpp_pop_adult$county== "TÃ¼rkiye"  ] <-  "Turkey"
ccd$country[ccd$county== "U.S.A" ] <-  "United States of America" 
ccd$country[ccd$county== "United Kingdom of Great Britain and Northern Ireland (the)" ] <-  "United Kingdom" 
ccd$country[ccd$county== "United States" ] <-  "United States of America" 
ccd$country[ccd$county== "United States of America (the)" ] <-  "United States of America" 
ccd$country[ccd$county== "West Germany" ] <-  "West Germany" 
ccd$country[ccd$county== "Yugoslavia" ] <-  "Yugoslavia" 

# Merge the country TFR with the wpp adult population
fert_national <- merge(ccd, wpp_pop_adult, by.x=c("country", "year"), by.y=c("region", "year"))

# Estimate the regression model =====================

# Combine the national and the subnaitonal data
fert_global <- rbindlist(list(fert_subnational, fert_national), fill=TRUE)

# Remove unnececary variables
fert_global <- fert_global[, .(country, year, data, tfr_male, tfr_female, tfr_ratio, asr, males, females)]

# Assign outliers
fert_global[, outlier:=ifelse(asr>3|asr<0.3, TRUE, FALSE)]

# Plot the resilationship between 
ggplot(data=fert_global, aes(x=asr, y=tfr_ratio)) +
  geom_hline(yintercept=1, linetype="dashed") +
  geom_vline(xintercept=1, linetype="dashed") +
  facet_wrap(~ data) +
  #geom_smooth(aes(colour=country), method="lm", se=F) +
  geom_text(data=subset(fert_global, asr>1.8), aes(label=country, colour=data),hjust="right", vjust="bottom") +
  geom_point(aes(colour=data), alpha=0.3) +
  geom_smooth(aes(colour=data), method="lm", formula="y~x", se=F) +
  geom_smooth(method="lm", se=F, formula="y~x", colour="black", linewidth=1, aes(colour=data)) +
  scale_x_continuous("Sex Ratio (age 20-39)", trans="log10", n.breaks=10) +
  scale_y_continuous("TFR male/TFR female", trans="log10", n.breaks=10) +
  guides(colour=guide_legend(ncol=4)) +
  theme(
    axis.text.x=element_text(angle=45, hjust=1, vjust=1)
  )
# SAvew the data
ggsave("results/global_tfr_adult_sr.pdf", height=25, width=20, unit="cm")

# Estimate the regression model
model_approximation <- lm(log(tfr_male)~log(tfr_female) + log(asr), data=fert_global)

# Look at the model fit
plot(model_approximation)

# Look at the model result
summary(model_approximation)

# Print the r-squared
summary_model_approximation <- summary(model_approximation)

# Save the model
save(model_approximation, file="results/model_tfr_approximation.Rda")

# Print the fit statistics
cat("R-squared=", summary_model_approximation$r.squared, "\n")
cat("R-square adjuted=", summary_model_approximation$adj.r.squared, "\n")

# If the coefficients are not statistical significant, rerun reduced model
if (!all(summary_model_approximation$coefficients[2:3, 4]<0.05)) {
  # Create the reduced data
  fert_global[, reduced_predictor:=log(tfr_female)-log(asr)]
  fert_global[, reduced_outcome:=log(tfr_male)-log(tfr_female)+log(asr)]
  
  # Run an empty regression
  reduced_model <- lm(log(tfr_male)~1, data=fert_global)
  
  # Create the equation
  sum(exp(coef(reduced_model)))
}

# Save the final data set
save(fert_global, file="data/fert_global_asr.Rda")

# Plot the resilationship between 
ggplot(data=fert_global, aes(x=tfr_female, y=tfr_male, colour=data)) +
  geom_abline(intercept=0, slope=1, linetype="dashed") +
  geom_point(alpha=0.3) +
  geom_abline(intercept=coef(model_approximation)[1], slope=coef(model_approximation)[2]) +
  geom_text(data=subset(fert_global, asr>1.8), aes(label=country, colour=data),hjust="right", vjust="bottom") +
  facet_wrap(~ data) +
  geom_point(aes(colour=data), alpha=0.3) +
  scale_x_continuous("TFR female/Sex ratio (20-39)", n.breaks=10, trans="log10") +
  scale_y_continuous("TFR male", n.breaks=10, trans="log10") +
  guides(colour=guide_legend(ncol=4)) +
  theme(
    axis.text.x=element_text(angle=45, hjust=1, vjust=1)
  )

ggsave(filename = "results/male_fertility_approx_tfr_fem.pdf", height=20, width=20, unit="cm")

# Plot the resilationship between 
ggplot(data=fert_global, aes(x=asr, y=tfr_male, colour=data)) +
  geom_vline(xintercept=1) +
  geom_point(alpha=0.3) +
  geom_text(data=subset(fert_global, asr>1.8), aes(label=country, colour=data),hjust="right", vjust="bottom") +
  geom_abline(intercept=coef(model_approximation)[1], slope=coef(model_approximation)[3]) +
  #facet_wrap(~ data) +
  geom_point(aes(colour=data), alpha=0.3) +
  scale_x_continuous("Sex ratio (20-39)", n.breaks=10, trans="log10") +
  scale_y_continuous("TFR male", n.breaks=10, trans="log10") +
  guides(colour=guide_legend(ncol=4)) +
  theme(
    axis.text.x=element_text(angle=45, hjust=1, vjust=1)
  )
ggsave(filename = "results/male_fertility_approx_asr.pdf", height=20, width=20, unit="cm")


# Make a 3D ggplot
make_plot <- function(theta=0, phi=0) {
ggplot(data=fert_global, aes(x=round(asr, 2), y=round(tfr_female, 2), z=round(tfr_male, 2), colour=data, shape=data)) +
  axes_3D(theta=theta, phi=phi) +
  stat_3D(theta=theta, phi=phi, alpha=0.5) +
  axis_labs_3D(theta=theta, phi=phi) +
  labs_3D(theta=theta, phi=phi, 
          labs=c("Sex ratio(20-39)", "TFR female", "TFR male"), 
          angle=c(0,0,90),
          hjust=c(0,2,2), 
          vjust=c(2,2,-2)) +
  ggtitle("Approximation of male TFR") +
  theme_void() +
  theme(legend.position = "right")
}

make_plot(120, 20)
ggsave(filename="results/3d_tfr_asr_approx.pdf")


# Approximation TFR male -------------------------------

### END #############################################