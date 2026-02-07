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
library(readxl)
library(HMDHFDplus)

# Select the country examples
countries_examples <- c("China", "Republic of Korea", "India", "Rwanda", "Guatemala")

# Reviewer 1 ===================================================================

## Comment R. 1. C. 2 ----------------------------------------------------------

# Estimate levels of childlessness
# Register data: Norway and Finland
# Survey data: Rest

## Load the data from Tanturri 2015
childlessness_tanturri <- fread("raw/childlessness-worldwide.csv")

# Load the fertility data
load("data/wpp_male_tfr_prediction.Rda")

# Harmonize the Country names in the childlessness data: Russia, UK, US, Coratia, Cyech Republic
childlessness_tanturri$Country[childlessness_tanturri$Country=="Coratia"] <- "Croatia"
childlessness_tanturri$Country[childlessness_tanturri$Country=="Russia"] <- "Russian Federation"
childlessness_tanturri$Country[childlessness_tanturri$Country=="US"] <- "United States of America"
childlessness_tanturri$Country[childlessness_tanturri$Country=="Czech Republic"] <- "Czechia"
childlessness_tanturri$Country[childlessness_tanturri$Country=="UK"] <- "United Kingdom"

# Filter the coutnries
tfr_wpp <- wpp_tfr_pop[region %in% childlessness_tanturri$Country & model == "model_agegap", ]

# Estimate the TFR ratios
tfr_wpp_1990s <- tfr_wpp[year %in% 1990:1999, .(tfr_ratio_1990s = mean(fit) / mean(tfr)), by=.(region)]
tfr_wpp_1995 <- tfr_wpp[year == 1995, .(tfr_ratio1995 = fit / tfr), by=.(region)]
tfr_ratio <- merge(tfr_wpp_1990s, tfr_wpp_1995, by="region")

# Merge the data
tfr_ratio <- merge(childlessness_tanturri, tfr_ratio, by.x = "Country", by.y="region", all.x=T)

ggplot(data = tfr_ratio, aes(x=tfr_ratio_1990s, y = `Male Childlessness`/`Female Childlessness`)) +
  geom_smooth(se=F, method="lm", formula = "y~x", colour="black", linewidth=1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_point(aes(colour=Country)) +
  geom_text(aes(label=Country, colour=Country), vjust=-0.8, family = "serif", size=2) +
  scale_x_log10("TFR ratio (1990s)", n.breaks=10) +
  scale_y_log10("Childlessness ratio (2010)", n.breaks=10) +
  scale_colour_viridis_d() +
  guides(colour = "none")
ggsave(filename = "results/childlessness_tfr_ratio.pdf", height=10, width=14, unit="cm")

## Comment R. 1 C. 3 -----------------------------------------------------------

# 1. Plot the components of the regression

# Load the regression models
load("results/model_tfr_approximation.Rda")

# Select the regression model
mod_coeff <- coefficients(regression_models$model_agegap)

# Filter the data
df <- wpp_tfr_pop[variant_tfr == "Medium" & model == "model_agegap", ]
df <- df[, .(region, year, tfr, fit, log_asr_agegap)]

# Create the tfr ratio
df[, tfr_ratio := fit / tfr]

# Estimate the male TFR from the regression
df[, male_tfr := exp(mod_coeff["(Intercept)"] + mod_coeff["log_tfr_female"] * log(tfr) + mod_coeff["log_asr_agegap"] * log_asr_agegap)]

# Estiamte the components
df[, tfr_component := exp(mod_coeff["log_tfr_female"] * log(tfr))]
df[, pop_component := exp(mod_coeff["log_asr_agegap"] * log_asr_agegap)]
df[, est_tfr_ratio := (exp(mod_coeff["(Intercept)"]) * tfr_component * pop_component)/tfr]

# Make the counterfactual simulation using 2025 as reference category
ref_year <- 2025
df_ref <- df[year==2025, ]
df_ref[, year:=NULL]
df_ref <- merge(df, df_ref, by = c("region"), suffixes = c("", "_ref"), all.x=TRUE, allow.cartesian = TRUE)

# Plot the counterfactual trend
ggplot(data=subset(df_ref, region %in% c("China", "India", "Republic of Korea", "Sweden", "United Kingdom", "United States of America")), aes(x=year)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 2025, linetype = "dashed", colour="black") +
  geom_line(aes(y = tfr_ratio, linetype = "Observed"), linewidth=1.2) +
  geom_line(aes(y = (exp(mod_coeff["(Intercept)"]) * tfr_component_ref * pop_component)/tfr_ref, colour = "TFR from 2025", linetype = "Counterfactual"), linewidth=1.2) +
  geom_line(aes(y = (exp(mod_coeff["(Intercept)"]) * tfr_component * pop_component_ref)/tfr, colour = "Population from 2025", linetype = "Counterfactual"), linewidth=1.2) +
  geom_ribbon(aes(ymin = (exp(mod_coeff["(Intercept)"]) * tfr_component_ref * pop_component)/tfr_ref, ymax = tfr_ratio, fill = "TFR from 2025"), alpha=0.3) +
  geom_ribbon(aes(ymin = (exp(mod_coeff["(Intercept)"]) * tfr_component * pop_component_ref)/tfr, ymax = tfr_ratio, fill = "Population from 2025"), alpha=0.3) +
  scale_colour_manual("Data:", values = c("darkblue", "red")) +
  scale_fill_manual("Data:", values = c("darkblue", "red")) +
  scale_linetype_manual("Scenario:", values = c("dotted", "solid")) +
  scale_x_continuous("Year", n.breaks=10, expand=c(0, 0)) +
  scale_y_log10("TFR Ratio (TFR men / TFR women)", n.breaks=8) +
  theme(panel.spacing.x=unit(0.1, "cm")) +
  facet_wrap(~ region, nrow=3)
ggsave(filename = "results/decomposition_tfr_ratio_change.pdf", height=25, width=20, unit="cm")



# Plot for a single case
df_long <- melt(data=df, id.vars=c("region", "year", "tfr"), measure.vars = c("tfr_component", "pop_component"))
ggplot(data = subset(df_long, region == "France"), aes(x = year)) +
  geom_line(aes(y = exp(mod_coeff["(Intercept)"])/tfr)) +
  geom_line(aes(y = value/tfr, group = variable, colour = variable)) +
  geom_line(aes(y = exp(mod_coeff["(Intercept)"] * value)/tfr, group = variable, colour=variable), linetype = "dotted")
  
## Comment R. 1. C. 4 -----------------------------------------------------------

# Goal: Replot figure 3
# Distinguish into the mortality and the SRB component

# Load the data
load("data/decomp_pop.Rda")

# Pick a reference year
ref_year <- 1950
ages <- seq(20, 50, by = 15)
years <- seq(1960, 2100, by = 10)

# Country selection
country_selection <- c("China", "India", "Guatemala", "Rwanda", "Sub-Saharan Africa")

## Estimate the component

# Create a new data frame for the decomposition
decomp <- df

# Estimate the mortality component
decomp[, ratio_mortality := cumprod(px_m)/cumprod(px_f), by = .(region, location_code, year)]
decomp[, ratio_birth := srb/100]
decomp[, ratio_pop := ratio_mortality * ratio_birth]

# Select the important columns
decomp <- decomp[!is.na(ratio_birth), .(ratio_pop, ratio_mortality, ratio_birth, region, year, age)]

## Plot the results

# Reshape longer
decomp_long <- melt(decomp, id.vars = c("region", "year", "age"))
decomp_ref <- decomp_long[year==ref_year, ]
decomp_ref[, year:=NULL]
decomp_long <- merge(decomp_long, decomp_ref, by=c("region", "age", "variable"), suffixes = c("", "_ref"), all.x=TRUE, all.y=FALSE, allow.cartesian=TRUE)

# Select the data
pop_ratio <- subset(decomp_long, year %in% years & age %in% ages & region %in% country_selection & variable == "ratio_pop")
decomp_long <- subset(decomp_long, year %in% years & age %in% ages & region %in% country_selection & variable != "ratio_pop")
decomp_long[, variable:=str_to_title(str_replace(variable, "ratio_", "Contribution "))]

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
ggsave(filename="results/plot_decomp_sex_ratio_contribution.pdf", height=25, width=20, unit="cm")

## Comment R. 1. C. 5 -----------------------------------------------------------

## Plot the relationship between TFRm/TFRw and MAC-MAF
## 1. Obtain estimates on mean age of fatherhood and mean age at childbearing
## 2. Plot the relationship

### 1. Schoumaker's data -------------------------------------------------------

# Set the path
path_schoumaker <- "U:/data/global/schoumaker_male"

# Load the male and female fertility estimates
schoumaker_tfr <- read_xlsx(path = list.files(path_schoumaker, full.names = T)[1])
schoumaker_tfr <- as.data.table(schoumaker_tfr)

# Load the projection
schoumaker_projection <- read_xlsx(path = list.files(path_schoumaker, full.names = T)[2])

# Select the data
schoumaker_tfr <- schoumaker_tfr[, .(country = `Country name`, cntry=`ISO-3`,
                                     tfr_female = as.numeric(`Female TFR (shown on Figures)`),
                                     mac_female = as.numeric(`Mean age at childbearing (shown on Figures)`),
                                     tfr_male = as.numeric(`Male TFR (shown on Figures)`),
                                     mac_male = as.numeric(`Mean age at fatherhood (shown on Figures)`),
                                     source = "Schoumaker's male fertility data")]

### 2. Male Fertility database -------------------------------------------------

# Set the path to the male fertility database
path_mfd <- "U:/data/global/male_fertility_database"
file_names <- list.files(path_mfd)

# Load the files
mfd <- lapply(file.path(path_mfd, file_names), fread)

# Bind the files together
mfd <- rbindlist(mfd)

# Estimate the mean age of childbearing
mfd <- mfd[, .(tfr_male = sum(ASFR), mac_male = sum(ASFR*Age)/sum(ASFR)), by = .(Country, Year1)]
setnames(mfd, "Country", "CNTRY")

# Load the country coCountry# Load the country codes for the HFD
hfd_countries <- HMDHFDplus::getHFDcountries()
hfd_countries <- as.data.table(hfd_countries)
hfd_countries[, link:=NULL]

# Merge the mfd data with the country names
mfd <- merge(mfd, hfd_countries, by = "CNTRY", all.x=T)

#### Load the female ASFR data -------------------------------------------------

# Load the female data
source("U:/accounts/authentification.R")

# Load for the countries the ASFRs
for(country in unique(mfd$Country)) {
  try(tmp <- readHFDweb(CNTRY = country, item = "asfrRR", username = hfd_un, password = hfd_pw))
  if (length(ls(pattern = "tmp")) != 0) {
  tmp$Country <- country
  assign(paste0("hfd_asfr_", country), tmp)
  rm(tmp)
  }
}

# Collect the data
asfr_hfd <- rbindlist(mget(ls(pattern = "hfd_asfr_")))

# Estimatme the TFR and the mean age at chidlbearing
hfd <- asfr_hfd[, .(tfr_female = sum(ASFR), mac_female = sum(ASFR*Age)/sum(ASFR)), by = .(Country, Year)]
setnames(hfd, old = "Country", new = "CNTRY")

# Merge the HFD with the male fertility database
mfd <- merge(hfd, mfd, by.x=c("Year", "CNTRY"), by.y=c("Year1", "CNTRY"), all.y=T, all.x=F, suffixes = c("_w", "_m"))

# Make column names to lower
setnames(mfd, old = names(mfd), new = tolower(names(mfd)))

# Make the source
mfd[, source := "Male Fertility Database"]

rm(list = ls(pattern = "hfd_"), tmp)

### 3. Subnational fertility data ----------------------------------------------

# Load the analysis data
load("U:/projects/4_gender_mac_diff/data/analysis_data.Rda")

# Load the fertility data
load("U:/projects/3 Regional birth squeezes/Subnational_birth_squeezes/data/fert_data_subnational.Rda")

# Merge dev and fert data
subnational_fert_data <- merge(dev, fert, by = c("country", "region", "year"))

# Make the data frame a data table
subnational_fert_data <- as.data.table(subnational_fert_data)

# Create an indicator for the data source
subnational_fert_data[, source := "Subnational male fertility data"]

### Bring the data together ----------------------------------------------------

# Bring the data together
male_fertility_data <- rbindlist(list(subnational_fert_data, schoumaker_tfr), fill = TRUE)

# Plot the relationship
ggplot(data = male_fertility_data, aes(x=tfr_male/tfr_female, y = mac_male-mac_female)) +
  geom_vline(xintercept=1, linetype="dashed", colour="red") +
  geom_point(aes(colour = source), alpha = 0.7) +
  geom_smooth(formula = "y~poly(x, 2)", method="lm", se = F, colour = "grey") +
  scale_x_log10("TFR ratio (TFR men / TFR women)", n.breaks = 10, expand=c(0.01, 0)) +
  scale_y_continuous("Average parental age gap (MAF - MAC)", n.breaks = 10) +
  scale_colour_viridis_d("Data source:")
ggsave(filename = "results/tfr_ratio_age_childbearing.pdf", height=12, width=15, unit="cm")


# Relationship with the birth rate
ggplot(data = male_fertility_data, aes(x=tfr_female, y = mac_male-mac_female)) +
  geom_vline(xintercept=1, linetype="dashed", colour="red") +
  geom_point(aes(colour = source), alpha = 0.7) +
  geom_smooth(formula = "y~poly(x, 2)", method="lm", se = F, colour = "grey") +
  scale_x_continuous("TFR women", n.breaks = 10, expand=c(0.01, 0)) +
  scale_y_continuous("Average parental age gap (MAF - MAC)", n.breaks = 10) +
  scale_colour_viridis_d("Data source:")


# Reviewer 2 ===================================================================

## Comment R. 2 C. 2 ----------------------------------------------------------- 


## Overall gender balance
wpp_pop <- read_xlsx("raw/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx", skip=16)
wpp_pop <- as.data.table(wpp_pop)
wpp_pop <- wpp_pop[, .(country = `Region, subregion, country or area *`,
                       year = as.numeric(Year),
                       pop_male = as.numeric(`Male Population, as of 1 July (thousands)`),
                       pop_female = as.numeric(`Female Population, as of 1 July (thousands)`))]

# Filter the non-missing values
wpp_pop <- wpp_pop[!is.na(year) & !is.na(pop_male), ]


# Estimate the gender balance globally
wpp_pop[country == "World", .(sex_ratio = pop_male / pop_female), by = year] |> 
  ggplot(aes(x=year, y=sex_ratio)) +
  geom_line()


# Load the life tables
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


# Comment R. 2. C. 2 -----------------------------------------------------------

# Distinguish into the mortality and the SRB component
load("data/wpp_male_tfr_prediction.Rda")

# Estimate the ratio
wpp_tfr_pop[, tfr_ratio := (fit - tfr)/tfr]

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
  geom_hline(yintercept = 0, colour = "red", linetype = "dotted") +
  geom_vline(xintercept = 2025, colour = "red", linetype = "dotted") +
  scale_alpha_discrete("Quantiles: ", range = c(0.5, 0.3)) +
  scale_linetype_manual("", values = c(2, 1, 2)) +
  scale_x_continuous("Year", breaks = seq(1950, 2100, by = 10), expand = c(0, 0)) +
  scale_y_continuous("TFR difference (male TFR - female TFR) / female TFR", n.breaks=12)
ggsave("results/distribution_tfr_ratio_time.pdf", height=10, width = 15, unit="cm")

### END ########################################################################