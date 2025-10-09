###
# Project: Global birth squeezes
# Purpose: Educational distribution
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 04/10/2025
##

rm(list=ls())

source("code/graphics.R")

library(ggplot2)
library(data.table)

# 1. Prepare the data ======================

# Load the data from the Wittgenstein centre
dt_edu_pop <- fread("raw/wcde_data_education.csv")

# Pivot wider
dt_edu_pop <- dcast(dt_edu_pop, formula = Area+Year+Age+Education~Sex, value.var = "Population")

# Estimate the sex ratio
dt_edu_pop[, sr:=Male/Female]

# Make the age variable a factor
dt_edu_pop[, Age:=factor(Age, ordered=T)]

# Create and age group variable
dt_edu_pop[, age_group:=fcase(Age%in%paste(seq(0, 20, by=5), seq(4, 24, by=5), sep="--"), "Below 25",
                              Age%in%paste(seq(25, 60, by=5), seq(29, 64, by=5), sep="--"), "25-64",
                              Age%in%c(paste(seq(65, 95, by=5), seq(69, 99, by=5), sep="--"), "100+"), "Above")]

# Aggregate by age group
dt_edu_agg <- dt_edu_pop[, .(sr=sum(Male)/sum(Female)), by=.(Area, Year, age_group, Education)]


# 2. Plot the data =========================


# Plot the trend in the sex ratio
ggplot(data=subset(dt_edu_pop, Area=="World"&!(Age%in%c("0--4", "10--14", "All"))), aes(x=Year, y=sr, colour=Education, group=Education)) +
  geom_vline(xintercept=2025, linetype="dotted") +
  geom_hline(yintercept=1) +
  geom_line(linewidth=1.3) +
  facet_wrap(~Age) +
  scale_y_continuous("Sex ratio", trans="log10") +
  scale_x_continuous(expand=c(0, 0))


# Plot the trend for age groups
ggplot(data=subset(dt_edu_agg, Area=="World"&!is.na(age_group)&Education!="Total"), aes(x=Year, y=sr, colour=Education, group=Education)) +
  geom_vline(xintercept=2025, linetype="dotted") +
  geom_hline(yintercept=1) +
  geom_line(linewidth=1.3) +
  facet_wrap(~age_group) +
  scale_y_continuous("Sex ratio", trans="log10", n.breaks=10) +
  scale_x_continuous(expand=c(0, 0), breaks=seq(1950, 2100, by=10)) +
  scale_colour_viridis_d(option="E") +
  theme(
    axis.text.x=element_text(angle=45, hjust=1, vjust=1)
  )
ggsave(filename="results/sex_ratio_world_education.pdf", width=25, height=15, unit="cm")


### END ####################################