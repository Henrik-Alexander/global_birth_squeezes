## Plot the decomposition differently


# Goal: Distinguish into the mortality and the SRB component


# Pick a reference year
ref_year <- 1950
ages <- seq(20, 50, by = 15)
years <- seq(1960, 2100, by = 20)

# Estimate the component -----------------------------------------------------

# Create a new data frame for the decomposition
decomp <- df

# Estimate the mortality component
decomp[, ratio_mortality := cumprod(px_m)/cumprod(px_f), by = .(region, location_code, year)]
decomp[, ratio_birth := srb/100]
decomp[, ratio_pop := mortality * ratio_birth]

# Select the important columns
decomp <- decomp[!is.na(ratio_birth), .(ratio_pop, ratio_mortality, ratio_birth, region, year, age)]


# Plot the results -----------------------------------------------------------

# Reshape longer
decomp_long <- melt(decomp, id.vars = c("region", "year", "age"))
decomp_ref <- decomp_long[year==ref_year, ]
decomp_ref[, year:=NULL]
decomp_long <- merge(decomp_long, decomp_ref, by=c("region", "age", "variable"), suffixes = c("", "_ref"), all.x=TRUE, all.y=FALSE, allow.cartesian=TRUE)

# Select the data
decomp_long <- subset(decomp_long, year %in% years & age %in% ages & region %in% countries_examples & variable != "ratio_pop")
decomp_long[, variable:=str_to_title(str_replace(variable, "ratio_", "Contribution "))]

ggplot(data=decomp_long, aes(x=age, fill=interaction(variable, variable), group=variable)) +
  geom_hline(yintercept = 1, linetype="dotted") +
  geom_col(aes(y=value), position=position_dodge()) +
  #geom_hline(data=decomp_long, aes(yintercept=value, colour=variable, group=variable), linewidth=1.3) +
  facet_grid(year ~  region) +
  scale_fill_viridis_d("Group:", labels=c(ref_year, "Observed")) +
  scale_colour_viridis_d("Group:", labels=c(ref_year, "Observed")) +
  scale_y_log10(n.breaks=10) +
  theme(panel.spacing.x=unit(1, "cm"))

## Plot the long-data

# Melt the data longer
decomp_long <- melt(decomp_long, id.vars = c("region", "year", "age", "variable"), variable.name="ref" )

# Plot the data
ggplot(data=subset(decomp_long, year %in% years & age %in% ages & region == "Afghanistan" & variable != ""), aes(x= variable, fill=ref, group=interaction(variable, ref))) +
  geom_bar(aes(y=value-1), alpha=0.5, position=position_dodge()) +
  facet_grid(year ~ age) +
  scale_y_continuous(name = 'values', 
                     breaks = seq(-1, 1, 0.5), 
                     labels = seq(-1, 1, 0.5) + 1) +
  coord_flip()



ggplot(data=decomp_long, aes(x=str_to_title(str_replace(variable, "ratio_", "Contribution ")), fill=ref, group=interaction(variable, ref))) +
  geom_hline(yintercept = 1, linetype="dotted") +
  geom_col(aes(y=value), position=position_dodge()) +
  geom_hline(data=decomp_long, aes(yintercept=value, colour=ref, group=ref), linewidth=1.3) +
  facet_grid(year ~ age + region) +
  coord_flip() +
  scale_fill_viridis_d("Group:", labels=c("Observed", ref_year)) +
  scale_colour_viridis_d("Group:", labels=c("Observed", ref_year)) +
  scale_y_log10(n.breaks=10) +
  theme(panel.spacing.x=unit(1, "cm"))




### END ########################################################################