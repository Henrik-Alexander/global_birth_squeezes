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
library(tidyverse)
#library(gg3D)

# Load the graphical scheme
source("code/graphics.R")

# Load the adult sex ratios
load("data/wpp_pop_adult_sr.Rda")

# Load the wpp locations
load("data/wpp_location.Rda")

# Functions =========================================

rmsd <- function(pred, obs) {
  if(length(pred)!=length(obs)) stop("Input vectors must have the same length!")
  
  sqrt(sum((obs-pred)^2) / length(pred))
  
}

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

# Create the log data
fert_global[, log_tfr_male:=log(tfr_male)]
fert_global[, log_tfr_female:=log(tfr_female)]
fert_global[, log_asr:=log(asr)]

# Estimate the regression model
model_approximation <- lm(log_tfr_male~log_tfr_female + log_asr, data=fert_global)

# Look at the model fit
plot(model_approximation)

# Look at the model result
summary(model_approximation)

# Create the regression table
stargazer(model_approximation, 
          type="latex",
          dep.var.labels = "log TFR men",
          covariate.labels = c("log TFR women", "log SR (20-39)", "Intercept"),
          ci=T, keep.stat=c("n", "rsq", "adj.rsq"))

# Print the r-squared
summary_model_approximation <- summary(model_approximation)

# Make the examples
prediction_examples <- expand.grid("log_tfr_female"=log(c(1, 2.1, 3, 5)),
                                   "log_asr" = log(c(0.5, 1, 2)))
prediction_examples$prediction <- predict(model_approximation, prediction_examples)
prediction_examples <- exp(prediction_examples)


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

# 
# # Make a 3D ggplot
# make_plot <- function(theta=0, phi=0) {
# ggplot(data=fert_global, aes(x=round(asr, 2), y=round(tfr_female, 2), z=round(tfr_male, 2), colour=data, shape=data)) +
#   axes_3D(theta=theta, phi=phi) +
#   stat_3D(theta=theta, phi=phi, alpha=0.5) +
#   axis_labs_3D(theta=theta, phi=phi) +
#   labs_3D(theta=theta, phi=phi, 
#           labs=c("Sex ratio(20-39)", "TFR female", "TFR male"), 
#           angle=c(0,0,90),
#           hjust=c(0,2,2), 
#           vjust=c(2,2,-2)) +
#   ggtitle("Approximation of male TFR") +
#   theme_void() +
#   theme(legend.position = "right")
# }

#make_plot(120, 20)
#ggsave(filename="results/3d_tfr_asr_approx.pdf")


# 3. Approximation TFR male ============================

# Load the world tfr
load("data/wpp_tfr.Rda")
load("data/wpp_pop_adult_sr.Rda")

# Merge the TFR and the adult sex ratio
wpp_tfr_pop <- merge(x=wpp_tfr, y=wpp_pop_adult, by=c("region", "location_code", "year"),
                     suffixes = c("_tfr", "_pop"))

# Rename the TFR variable
wpp_tfr_pop[, log_tfr_female:=log(tfr)]
wpp_tfr_pop[, log_asr:=log(asr)]

# Predict the data
tfr_prediction <- predict(model_approximation, wpp_tfr_pop, se.fit=TRUE, interval="prediction")

# Attach the prediction to the orginal data
wpp_tfr_pop <- bind_cols(wpp_tfr_pop, exp(tfr_prediction$fit))

# Estiamte the relative difference
wpp_tfr_pop[, tfr_diff:=(fit-tfr)/tfr]  
wpp_tfr_pop[, tfr_diff_upr:=(upr-tfr)/tfr]
wpp_tfr_pop[, tfr_diff_lwr:=(lwr-tfr)/tfr]

# Estimate the cross-overs
wpp_tfr_pop <- wpp_tfr_pop[order(region, variant_tfr,  year), ]
wpp_tfr_pop[, cross_over:= ifelse(lag(tfr)<lag(fit)&tfr>fit, 1, 0), by=region]

# Merge with the location data
wpp_tfr_pop <- merge(wpp_tfr_pop, wpp_location, by="region")

# Clean the sdg region label
wpp_tfr_pop$sdg_region <- str_remove(wpp_tfr_pop$sdg_region, "\\(.+\\)")

# Save the data
save(wpp_tfr_pop, file="data/wpp_male_tfr_prediction.Rda")

# Estimate the numbers
wpp_tfr_pop[year%in% c(1950, 2025, 2100) & location_type=="Country/Area" & variant_tfr=="Medium", .(higher_men=mean(ifelse(tfr_diff>0, 1, 0))), by=year]

# Range of the difference
wpp_tfr_pop[location_type=="Country/Area" & variant_tfr=="Medium", .(range=100*range(tfr_diff))]

# Estimate the share of higher countries
wpp_tfr_pop[, male_birth_squeeze:=ifelse(tfr_diff>0, 1, 0)]
wpp_tfr_pop[variant_tfr=="Medium"&location_type=="Country/Area", .(.N) , by=.(year, male_birth_squeeze)] |> 
  ggplot(aes(x=year, y=N, fill=factor(male_birth_squeeze))) + 
  geom_col() +
  geom_vline(xintercept = 2024) +
  scale_fill_viridis_d("", option="D", labels=c("Higher female TFR", "Higher male TFR")) +
  scale_x_continuous("Year", breaks = seq(1950, 2100, by=10), expand = c(0, 0)) +
  scale_y_continuous("Number of countries", expand=c(0, 0), n.breaks=10) 
ggsave(filename="results/n_countries_birth_squeeze.pdf", height=15, width=25, unit="cm")  

# Plot the relative differences
ggplot(wpp_tfr_pop, aes(x=year, y=tfr_diff, group=year, colour=year)) + 
  geom_hline(yintercept=0) + 
  geom_boxplot() +
  scale_colour_viridis_c(option = "D") +
  scale_x_continuous("Year", breaks = seq(1950, 2100, by=10), expand = c(0, 0)) +
  scale_y_continuous("Relative difference (female TFR - male TFR)", expand=c(0, 0), n.breaks=10, labels=scales::percent) +
  guides(colour="none")
ggsave(filename="results/distr_relative_difference_tfr.pdf", height=15, width = 35, unit="cm")

unique(wpp_tfr_pop$year[wpp_tfr_pop$tfr_diff > 1])


# Fint the maximum and minimum tfr difference
wpp_tfr_pop[tfr_diff< -0.733]


## 3.2 Plot the approximations ------------------------

# Plot the relative difference
ggplot(subset(wpp_tfr_pop, location_type=="Country/Area" & variant_tfr=="Medium"), aes(x=year, group=region, colour=sdg_region)) +
  geom_hline(yintercept=0) +
  geom_line(aes(y=tfr_diff)) +
  scale_x_continuous("Year", n.breaks=10, expand=c(0, 0)) +
  scale_y_continuous("Relative TFR difference (TFR men - TFR women)", n.breaks=10, expand=c(0, 0), labels=scales::percent) +
  facet_wrap(~sdg_region) +
  guides(colour="none")
ggsave(filename="results/tfr_rel_difference_panel.pdf", height=20, width=20, unit="cm")

# Plot the relative difference
ggplot(subset(wpp_tfr_pop, region %in% c("China", "Republic of Korea", "India" ) & variant_tfr=="Medium"), aes(x=year, group=region, colour=region)) +
  geom_vline(data=subset(wpp_tfr_pop, region %in% c("China", "Republic of Korea", "India" ) & variant_tfr=="Medium" & cross_over==1), aes(xintercept=year, colour=region)) +
  geom_ribbon(aes(ymin=0, ymax=tfr_diff, fill=region), alpha=0.5) +
  geom_hline(yintercept=0) +
  geom_line(aes(y=tfr_diff)) +
  scale_x_continuous("Year", n.breaks=10, expand=c(0, 0)) +
  scale_y_continuous("Relative TFR difference (TFR men - TFR women)", n.breaks=15, expand=c(0, 0), labels=scales::percent) +
  facet_wrap(~region) +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  scale_fill_viridis_d(option="D") +
  scale_colour_viridis_d(option="D") + 
  guides(colour="none", fill="none")
ggsave(filename="results/tfr_rel_difference_asia.pdf", height=10, width=20, unit="cm")

# Plot sdg regions
plot_sdg_rel <- function(sdg_reg, df=wpp_tfr_pop) {
  df <- subset(df, variant_tfr=="Medium" & sdg_region==sdg_reg)
  plot_sdg <- ggplot(data=df, aes(x=year, group=region)) +
  geom_vline(data=subset(df, cross_over==1), aes(xintercept=year, colour=region)) +
  geom_ribbon(aes(ymin=0, ymax=tfr_diff, fill=region, colour=region), alpha=0.3) +
  geom_hline(yintercept=0) +
  geom_line(aes(y=tfr_diff, colour=region)) +
  scale_x_continuous("Year", n.breaks=10, expand=c(0, 0)) +
  scale_y_continuous("Relative TFR difference (TFR men - TFR women)", n.breaks=10, expand=c(0, 0), labels=scales::percent) +
  facet_wrap(~region) +
  scale_fill_viridis_d(option="D") +
  scale_colour_viridis_d(option="D") + 
  guides(colour="none", fill="none") +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
    
  ggsave(plot_sdg, filename=paste0("results/sdg_reg_tfr_diff_", sdg_reg, ".pdf"), height=25, width=25, unit="cm")
  return(plot_sdg)
}

lapply(unique(wpp_tfr_pop$sdg_region), plot_sdg_rel)


# Plot the TFR to the female TFR
ggplot(subset(wpp_tfr_pop, location_type=="Country/Area" & year %in% c(1950, 2025, 2050, 2100)), aes(x=tfr, y=fit, colour=sdg_region)) +
  geom_abline(slope=1, intercept=0) +
  #geom_line(aes(group=region),alpha=0.5, aes(group=region)) +
  geom_point(alpha=0.4) +
  #geom_linerange(aes(ymin=lwr, ymax=upr), alpha=0.5) +
  scale_x_continuous("TFR women", breaks=1:10, expand=c(0, 0)) +
  scale_y_continuous("TFR men", breaks=1:10, expand=c(0, 0)) +
  scale_colour_viridis_d("",option="D") +
  facet_wrap(~ year, ncol=1, scales="free")  +
  guides(colour=guide_legend(ncol=2, override.aes=list(alpha=1)))
ggsave(filename="results/tfr_male_female_difference.pdf", height=25, width=15, unit="cm")

# Plot for the world
plot_male_tfr <- function(country) {
  plt_tmp <- ggplot(data=subset(wpp_tfr_pop, region==country & variant_tfr=="Medium"), aes(x=year)) +
    geom_vline(xintercept=2023, linetype="dotted") +
    geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.3, fill="darkblue") +
    geom_line(aes(y=fit, colour="Male TFR"), lwd=1.2) +
    geom_line(aes(y=tfr, colour="Female TFR"), lwd=1.2) +
    scale_x_continuous("Year", expand=c(0, 0), breaks=seq(1950, 2100, by=10)) +
    scale_y_continuous("Total Fertility Rate", n.breaks=10) +
    scale_colour_manual("", values=c("darkred", "darkblue")) +
    ggtitle(paste("Mmale TFR to female TFR in", country))
  
    
  ggsave(plt_tmp, filename=paste0("results/regress_approx/reg_app_tfr_", country, ".pdf"), height=10, width=15, unit="cm")
  
  
  #return(plt_tmp)
}

# Plot the data for France
plot_male_tfr("France")

# Plot the relative difference
plot_rel_diff <- function(country) {
  plt_tmp <- ggplot(data=subset(wpp_tfr_pop, region==country & variant_tfr=="Medium"), aes(x=year)) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=2023, linetype="dotted") +
    geom_ribbon(aes(ymin=tfr_diff_lwr, ymax=tfr_diff_upr), alpha=0.3) +
    geom_line(aes(y=tfr_diff), lwd=1.2) +
    scale_x_continuous("Year", expand=c(0, 0), breaks=seq(1950, 2100, by=10)) +
    scale_y_continuous("Difference TFR men to TFR women", n.breaks=10, labels = scales::percent) +
    ggtitle(paste("Relative difference of male TFR to female TFR in", country))
  
  ggsave(plt_tmp, filename=paste0("results/regress_approx/rel_diff_reg_app", country, ".pdf"), height=15, width=15, unit="cm")
         
     # return(plt_tmp)
}

# Make all the plots
for(region in unique(wpp_tfr_pop$region)) {
  
  cat("Region:", region, "\n")
  plot_male_tfr(region)
  plot_rel_diff(region)
}

### 3.3. Cross-overs -------------------------------------

# Plot the line with the cross-overs
ggplot(subset(wpp_tfr_pop, variant_tfr=="Medium"&location_type=="Country/Area"), aes(x=year, y=fit/tfr, group=region, colour=sdg_region)) +
  geom_line(alpha=0.3) +
  geom_hline(yintercept=1) +
  geom_vline(xintercept=2023, linetype="dotted") +
  geom_point(data=subset(wpp_tfr_pop, cross_over==1&variant_tfr=="Medium"&location_type=="Country/Area"), aes(x=year-0.5, y=1),  size=1) +
  scale_x_continuous("Year", seq(1950, 2100, by=25), expand=c(0, 0)) +
  scale_y_continuous("TFR ratio", trans="log10", n.breaks=10) +
  facet_wrap(~sdg_region, scales="free_y") +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=45, vjust=1, hjust=1))
ggsave(filename="results/tfr_ratio_crossover.pdf", height=25, width=35,unit="cm")


# Plot the crossovers
ggplot(data=subset(wpp_tfr_pop, cross_over==1&variant_tfr=="Medium"&location_type=="Country/Area"), aes(x=fct_reorder(region, year), y=year, colour=sdg_region)) +
  geom_hline(yintercept=2023) +
  geom_point() +
  coord_flip() +
  scale_y_continuous("Year", breaks=seq(1950, 2100, by=10), expand=c(0, 0)) + 
  theme(
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    legend.title=element_blank()
  )
ggsave(filename="results/tfr_ratio_crossovers_country.pdf", height=50, width=25, unit="cm")


# Plot the cross-overse
ggplot(data=subset(wpp_tfr_pop, cross_over==1&variant_tfr=="Medium"&location_type=="Country/Area"), aes(x=year, fill=sdg_region, colour=sdg_region)) +
  geom_vline(xintercept=2023) +
  geom_density(bins=50, alpha=0.5) +
  scale_x_continuous("Year", breaks=seq(1950, 2100, by=10), limits=c(1950, 2100), expand=c(0, 0)) + 
  scale_y_continuous(expand=c(0, 0), n.breaks=10) +
  facet_wrap(~sdg_region) +
  scale_fill_viridis_d(option="D") +
  scale_colour_viridis_d(option="D") +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
  guides(fill="none", colour="none")
ggsave(filename="results/tfr_ratio_crossover_timing_sdg_regions.pdf", height=20, width=30, unit="cm")



### 3.3 Out of sample validation ----------------------

# Load the male national fertility database
load("data/fert_global_asr.Rda")

# Merge the male fertility database
validation <- merge(fert_global[data=="Human Fertility Collection", ], wpp_tfr_pop[variant_tfr=="Medium"], by.x=c("country", "year"), by.y=c("region", "year"), suffixes = c("_obs", "_pred"))

# Estimate the error
validation[, error:=tfr_male-fit]

# Plot the data
ggplot(data=validation, aes(x=tfr_male, colour=country)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(aes(y=fit)) +
  geom_linerange(aes(ymin=lwr, ymax=upr), alpha=0.3) +
  scale_x_continuous("TFR men observed", n.breaks=10, expand=c(0, 0)) +
  scale_y_continuous("TFR men predicted", n.breaks=10, expand=c(0, 0)) +
  guides(colour=guide_legend(nrow=2))
ggsave(filename="results/out_of_sample_validation.pdf", height=15, width=15, unit="cm")

# Plot the error distribution
ggplot(data=validation, aes(x=error)) + 
  geom_vline(xintercept=0) + 
  geom_histogram(bins = 40, colour="white") +
  scale_x_continuous("Prediction error (observed-predicted)", n.breaks=10, expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0), n.breaks=10)
ggsave(filename="results/out_of_sample_validation_error.pdf", height=15, width=15, unit="cm")

# Prediction intervals calibartion
validation[, calibrated:=ifelse(lwr<tfr_male & upr>tfr_male, 1, 0)]

# Estimate the RMSD
(rmds <- rmsd(pred=validation$fit, obs=validation$tfr_male))

### END #############################################