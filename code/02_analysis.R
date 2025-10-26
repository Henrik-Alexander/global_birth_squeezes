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

# 2. Estiamte period parity age  ============================

# Estimate the number of survival
df[, sx_m := srb*cumprod(px_m), by = .(region, location_code, year)]
df[, sx_f := 100*cumprod(px_f), by = .(region, location_code, year)]

# Plot the parity age
df[, female_skewed := ifelse(sx_f>sx_m, 1, 0)]

# Create the parity age
parity_age <- df[female_skewed==1, .(parity_age=min(age, na.rm = T)-0.5), by = .(region, location_code, year)]

# Merge with the location data
parity_age <- merge(parity_age, wpp_location, by="region")

# Save the parity age for periods
save(parity_age, file="data/parity_age_period.Rda")

## 2.2. Plot period results --------------------------------

# Plot the parity age
ggplot(data=parity_age, aes(x=year, y=parity_age)) +
  geom_line(aes(group=region, colour=sdg_region)) +
  geom_smooth(se=FALSE, linewidth=2) +
  scale_x_continuous("Year", expand=c(0, 0), breaks=seq(1950, 2030, by=10)) +
  scale_y_continuous("Parity age", breaks=seq(0, 100, by=10), expand=c(0, 0)) +
  facet_wrap(~ sdg_region) +
  labs(subtitle="Age at which men and women have the same size, reflecting the excess men at birth and excess male mortality.",
       caption="Source: author's calculation using WPP 2024.")
ggsave(filename="results/parity_age_globally.pdf", height=20, width=25, unit="cm")

# Plot China and Taiwan
ggplot(data=subset(parity_age, region %in% c("China", "Republic of Korea", "Viet Nam", "Cambodia", "Rwanda", "Guatemala")), aes(x=year, y=parity_age)) +
  geom_line(aes(group=region, colour=region), linewidth=2) +
  scale_x_continuous("Year", expand=c(0, 0), breaks=seq(1950, 2030, by=10)) +
  scale_y_continuous("Parity age", breaks=seq(0, 100, by=10)) +
  facet_wrap(~ region) +
  labs(subtitle="Age at which men and women have the same size, reflecting the excess men at birth and excess male mortality.",
       caption="Source: author's calculation using WPP 2024.")
ggsave(filename="results/parity_age_asian_examples.pdf", height=15, width=25, unit="cm")

# Estimate the aggregate
df_aggregate <- df[region %in% c("China", "Republic of Korea", "Viet Nam", "Cambodia", "Rwanda", "Guatemala"), .(sx_m=mean(sx_m, na.rm=T), sx_f=mean(sx_f, na.rm=T)), by=.(region, age)]

# Plot China and Taiwan
ggplot(data=subset(df, region %in% c("China", "Republic of Korea", "Viet Nam", "Cambodia", "Rwanda", "Guatemala")), aes(x=age, y=sx_m/sx_f)) +
  geom_hline(yintercept = 1) +
  #geom_line(aes(group=year), colour="grey", alpha=0.3, linewidth=0.2) +
  geom_line(data=subset(df, year %in% c(1950, 1970, 1995, 2020) & region %in% c("China", "Republic of Korea", "Viet Nam", "Cambodia", "Rwanda", "Guatemala")), aes(x=age, y=sx_m/sx_f, colour=factor(year), group=year), linewidth=1.4) +
  geom_line(data=df_aggregate, aes(x=age, y=sx_m/sx_f, colour="average", linetype="Country average"), linewidth=1.3, colour="black") + 
  scale_x_continuous("Age", expand=c(0, 0), breaks=seq(0, 100, by=10)) +
  scale_y_continuous("Sex ratio of life table survivors accounting for SRB", breaks=seq(0, 2, by=0.2), limits=c(0, 1.22), expand=c(0, 0)) +
  scale_colour_viridis_d("Year") +
  facet_wrap(~ region, ncol=2) +
  scale_linetype_manual("", values="dotted") + 
  labs(subtitle="Sex ratio of a synthetic cohort reflecting sex-specific mortality and sex ratio at birth.",
       caption="Source: author's calculation using WPP 2024.")
ggsave(filename="results/lifetable_sr_asian_examples.pdf", height=25, width=20, unit="cm")


# Subset the data
df |> 
  filter(region %in% c("China", "Republic of Korea", "Viet Nam", "Cambodia", "Rwanda", "Guatemala") & year %in% c(1950, 1970, 1995, 2020)) |> 
  group_by(region, year) |> 
  mutate(cross_over = ifelse(sx_m/sx_f < 1 & lag(sx_m/sx_f) > 1, 1, 0)) |> 
  filter(cross_over==1) |> 
  select(region, year, age) |> 
  pivot_wider(names_from="year", values_from="age") |> 
  View()


## Cohort perspective ===========================

# Merge the new cohort data
df_coh <- merge(dt_wpp_lt, dt_wpp_srb, by=c("region", "location_code", "year"), all.x=TRUE, all.y=TRUE)

# Remove the unnecacy columns
df_coh <- df_coh[ , .(region, location_code, year, age, n, px_m, px_f, srb)]

# Estimate the cohort
df_coh[, cohort:=year - age]
df_coh[, cohort2:=year - age + 1]

# Remove the year column
df_coh[, year:=NULL]

# Estimate the average survival probability for adjacent cohorts
df_coh <- merge(x=df_coh, y=df_coh, 
                by.x=c("age", "region", "location_code", "cohort"),
                by.y = c("age", "region", "location_code", "cohort2"),
                suffixes=c("_coh1", "_coh2"), all.x=T, all.y=F)

# Estimate the cohort survival probabilities
df_coh[ , px_m := (px_m_coh1 + px_m_coh2)/2]
df_coh[ , px_f := (px_f_coh1 + px_f_coh2)/2]

# Select the important columns
df_coh <- df_coh[!is.na(px_m) & !is.na(px_f) & !is.na(srb_coh1) & cohort >= 1950, .(region, age, location_code, cohort, srb=srb_coh1, px_f, px_m)]

# Set the keys
setkeyv(df_coh, c("region", "cohort"))

# Order by region, cohort and yeaqr
df_coh <- df_coh[order(region, cohort, age), ]

# Estimate the number of survival
df_coh[, sx_m := srb*cumprod(px_m), by = .(region, location_code, cohort)]
df_coh[, sx_f := 100*cumprod(px_f), by = .(region, location_code, cohort)]

## 2.1 Estimate the cohort parity age ------------------------------

# Plot the parity age
df_coh[, female_skewed := ifelse(sx_f>sx_m, 1, 0)]

# Create the parity age
parity_age_coh <- df_coh[female_skewed==1, .(parity_age=min(age, na.rm = T)-0.5), by = .(region, location_code, cohort)]

# Save the parity age for the cohort
save(parity_age_coh, file="data/parity_age_cohort.Rda")

## 2.2. Plot cohort results ----------------------------------------

# Plot the survival curves for the 
ggplot(data=subset(df_coh, region=="Norway" & cohort %in% seq(1950, 2000, by=10)), aes(x=age, y=sx_m/sx_f, group=cohort, colour=cohort)) +
  geom_line() +
  scale_colour_gradient(low="blue", high="red") +
  geom_hline(yintercept = 1)

# Plot the lexis diagram
ggplot(data=subset(df_coh, region=="Norway")) +
  geom_tile(aes(x=cohort+age, y=age, fill=sx_m/sx_f)) +
  geom_step(data=subset(parity_age_coh, region=="Norway"), aes(x=cohort+parity_age, y=parity_age), colour="black") +
  geom_abline(intercept=-seq(1950, 2030, by=10), slope=1, linetype="dashed", colour="grey") +
  scale_fill_gradient2(midpoint=1, high="darkred", low="darkblue", mid="white") +
  scale_x_continuous("Cohort", expand=c(0, 0), breaks=seq(1950, 2025, by=10)) +
  scale_y_continuous("Age", expand=c(0, 0), breaks = seq(0, 80, by=10)) +
  theme(legend.key.width = unit(2, "cm"))

# 3. Standardization ---------------------------------------------

# Load the two data sets
load("data/wpp_pop.Rda")
load("data/wpp_births.Rda")

# Estimate the wpp TFR for women
wpp_tfr <- wpp_births[, .(tfr=sum(asfr/1000), births=sum(births)), by=.(region, year, variant, location_code)]

# Save the WPP TFR data
save(wpp_tfr, file="data/wpp_tfr.Rda")
rm(wpp_tfr)

# Merge the data
wpp_standard <- merge(x=wpp_births, y=dt_wpp_pop,
                      by.x=c("region", "location_code", "year", "age_mother", "n"),
                      by.y=c("region", "location_code", "year", "age", "n"), 
                      suffixes=c("_births", "_pop"), all.x = TRUE)

# Estimate the rates
wpp_standard[, asfr_female:=births/pop_female]
wpp_standard[, asfr_male:=births/pop_male]

# Estimate the total fertility rates
wpp_tfr_standard <- wpp_standard[, .(tfr_obs=sum(asfr), tfr_male=sum(asfr_male), tfr_female=sum(asfr_female), asr=sum(pop_male)/sum(pop_female)), by =  .(region, year, location_code, variant_births)]

# Estimate the relative and absolute difference
wpp_tfr_standard[, abs_diff:=tfr_male-tfr_female]
wpp_tfr_standard[, rel_diff:=round(100*abs_diff/tfr_female, 2)]

# Merge with location variables
wpp_tfr_standard <- merge(wpp_tfr_standard, wpp_location, by="region")

# Save the results
save(wpp_tfr_standard, file="data/wpp_tfr_standard.Rda")

## Estimate the adult age ratio ----------------------------------

# Filter the adult population
wpp_pop_adult <- dt_wpp_pop[age%in%20:30, ]

# Estimate the adult sex ratio
wpp_pop_adult <- wpp_pop_adult[, .(pop_male=sum(pop_male), pop_female=sum(pop_female)), by=.(region, location_code, variant, year)]

# Estimate the sex ratio
wpp_pop_adult[, asr:=pop_male/pop_female]

# Save the data
save(wpp_pop_adult, file="data/wpp_pop_adult_sr.Rda")

## 3.1. Plot the results -----------------------------------------

# Plot the trend in the absolute difference
ggplot(data=subset(wpp_tfr_standard, variant_births=="Medium" & sdg_region!=""), aes(x=year, y=rel_diff/100, colour=sdg_region)) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=2024, linetype="dashed") +
  geom_line(aes(group=region), alpha=0.4) +
  #geom_smooth(method="loess", se=FALSE) +
  facet_wrap(~ sdg_region) +
  scale_y_continuous("TFR Difference", expand=c(0, 0), limits=c(-0.5, 1), labels=scales::percent) +
  scale_x_continuous("Year", expand=c(0, 0), breaks=seq(1950, 2100, by=25)) +
  guides(colour=guide_legend(ncol=2)) +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        legend.title=element_blank())
ggsave(filename="results/standardization_rel_diff_time_trend.pdf", height=20, width=25, unit="cm")  

# Plot trend in China
ggplot(data=subset(wpp_tfr_standard, region %in% c("China", "Republic of Korea", "China, Taiwan Province of China" )&variant_births=="Medium"), aes(x=year, y=rel_diff/100, colour=region)) +
  geom_vline(xintercept=2024, linetype="dashed") +
  geom_hline(yintercept=0) +
  geom_point() +
  geom_line(aes(group=variant_births)) +
  facet_wrap(~ region) +
  scale_y_continuous("% difference male to female TFR", n.breaks=8, labels=scales::percent) +
  scale_x_continuous("Year", breaks=seq(1950, 2100, by=25), expand=c(0, 0)) +
  theme(
    axis.text.x=element_text(angle=45, hjust=1, vjust=1)
  )

ggsave(filename="results/standard_east_asia_rel.pdf", height=15, width=25,unit="cm")


ggplot(data=subset(wpp_tfr_standard, location_type=="Country/Area" &variant_births=="Medium"&year%in%seq(1950, 2100, by=50)), aes(x=rel_diff/100, fill=as.factor(sdg_region))) +
  geom_vline(xintercept=0, linetype="solid") +
  geom_histogram(bins=50, colour="white") +
  facet_wrap(~ year) +
  scale_x_continuous("% difference male to female TFR", n.breaks=8, labels=scales::percent) +
  scale_y_continuous("Count", expand=c(0, 0)) +
  guides(fill=guide_legend(ncol=2)) +
  theme(legend.title=element_blank())
ggsave(filename="results/standard_distribution_time.pdf", height=15, width=25,unit="cm")


# Estimate the cross overs
wpp_tfr_standard <- wpp_tfr_standard[order(region, variant_births, year)]
wpp_tfr_standard[, cross_over:= ifelse(lag(rel_diff)>0&rel_diff<0, 1, 0), by=.(region, variant_births)]

# Plot the cross overs
# Plot the cross-overse
ggplot(data=subset(wpp_tfr_standard, cross_over==1&variant_births=="Medium"&location_type=="Country/Area"), aes(x=year, fill=sdg_region, colour=sdg_region)) +
  geom_vline(xintercept=2023) +
  geom_histogram(bins=50, alpha=0.5) +
  scale_x_continuous("Year", breaks=seq(1950, 2100, by=10), limits=c(1950, 2100), expand=c(0, 0)) + 
  scale_y_continuous(expand=c(0, 0), n.breaks=10) +
  facet_wrap(~sdg_region) +
  scale_fill_viridis_d(option="D") +
  scale_colour_viridis_d(option="D") +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
  guides(fill="none", colour="none")

### Conflict impact =======================================

# Load the military conflict data
dt_conflict <- fread("raw/UcdpPrioConflict_v25_1.xlsx", sheet=1)

### END #######################################