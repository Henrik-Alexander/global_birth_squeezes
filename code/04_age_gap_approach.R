###
# Project: Global birth squeezes
# Purpose: Age gap approach
# Author: Christian Dudel
# E-mail: schubert@demogr.mpg.de
# Date: 21/11/2025
##



### Demographic forecasts of TFR ratios ########################################


### Packages & Settings ########################################################

library(httr)
library(data.table)
library(dtplyr)
library(tidyverse)
library(maps)
library(ggthemes)
library(readxl)
library(R.utils)


### Links ######################################################################

urlfer <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Fertility_by_Age1.csv.gz"
urlpop1 <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationExposureBySingleAgeSex_Medium_1950-2023.csv.gz"
urlpop2 <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationExposureBySingleAgeSex_Medium_2024-2100.csv.gz"
urldiff <- "https://perso.uclouvain.be/bruno.schoumaker/data/A.%20Estimates%20of%20male%20and%20female%20fertility.xlsx"

  
### Download ###################################################################  

# Create the folder
if(!dir.exists("Download")) dir.create("Download")

# Where to save it?
zipfer <- "Download/fert.csv.gz"
zippop1 <- "Download/pop1.csv.gz"
zippop2 <- "Download/pop2.csv.gz"
dirdiff <- "Download/agediff.xlsx"

# Download
if(!file.exists(zipfer)) GET(urlfer, write_disk(zipfer, overwrite = TRUE), progress() )
if(!file.exists(zippop1)) GET(urlpop1, write_disk(zippop1, overwrite = TRUE), progress() )
if(!file.exists(zippop2)) GET(urlpop2, write_disk(zippop2, overwrite = TRUE), progress() )
if(!file.exists(dirdiff)) GET(urldiff, write_disk(dirdiff, overwrite = TRUE), progress() )


### Load data ##################################################################

# Load
fer <- fread(zipfer)
pop1 <- fread(zippop1)
pop2 <- fread(zippop2)
diff <- read_excel(dirdiff)


### Edit data ##################################################################  

# Combine past and future population
pop <- rbind(pop1,pop2)

# Country and variant
fer <- fer %>% filter(Variant=="Medium")
pop <- pop %>% filter(Variant=="Medium")

# Restrict variables
fer <- fer %>% select(Time,LocID,Location,AgeGrp,ASFR,Births)
pop <- pop %>% select(Time,LocID,Location,AgeGrp,PopMale,PopFemale)

# Make age integer (causes a warning)
pop <- pop %>% mutate(AgeGrp=as.numeric(AgeGrp)) %>% na.omit

# Get counts etc. right
fer <- fer %>% mutate(ASFR=ASFR/1000,
                      Births=Births*1000)

pop <- pop %>% mutate(PopMale=PopMale * 1000,
                      PopFemale=PopFemale * 1000)

# Merge pop and fer (warning can be ignored)
all <- left_join(pop,fer)

# Set missing to zero
all <- all %>% mutate(ASFR=ifelse(is.na(ASFR),0,ASFR),
                      Births=ifelse(is.na(Births),0,Births))


### TFR for women ##############################################################

tfr_women <- all %>% group_by(Location,LocID,Time) %>% summarize(TFR=sum(ASFR))
age_women <- all %>% group_by(Location,LocID,Time) %>% summarize(MAC=sum(AgeGrp*ASFR/sum(ASFR)))


### Fixed age difference #######################################################

# What age differences to consider
agediff <- -2:20

for(ad in agediff) {

  # Age for merging
  pop <- pop %>% mutate(MergeAge=AgeGrp-ad)
  fer <- fer %>% mutate(MergeAge=AgeGrp)
  
  # Merge pop and fer
  shiftall <- left_join(pop,fer,by=c("Time","Location","LocID","MergeAge"))
  
  # Set missing to zero
  shiftall <- shiftall %>% mutate(ASFR=ifelse(is.na(ASFR),0,ASFR),
                        Births=ifelse(is.na(Births),0,Births))
  
  # Get ASFRs for men
  shiftall <- shiftall %>% mutate(ASFRm=ifelse(PopMale==0|Births==0,
                                               0,
                                               Births/PopMale))
  
  # TFR
  tfr_men <- shiftall %>% 
             # Group by country and year
             group_by(Location,LocID,Time) %>%
             # Get sum of ASFRs, Total population
             summarize(mTFR=sum(ASFRm),
                       pop=sum(PopMale)+sum(PopFemale)) |> 
             # Add age difference to the data
             mutate(TFRtype=paste0(ad)) 
             
  
  # Merge (warnings can be ignored)
  fixdiff <- left_join(tfr_men,tfr_women)

  # Combine
  if(ad==agediff[1]) results <- fixdiff else results <- rbind(results,fixdiff)

}


### Select relevant scenarios ##################################################

# Age difference between men and women
ageref <- diff |> 
          select("Country name",
                 "UN Country Code",
                 "Mean age at childbearing (shown on Figures)",
                 "Mean age at fatherhood (shown on Figures)") |> 
          rename("Location" = "Country name",
                 "LocID"="UN Country Code",
                 "MAC"="Mean age at childbearing (shown on Figures)",
                 "PAC"="Mean age at fatherhood (shown on Figures)") |> 
          mutate(PAC=as.numeric(PAC),
                 MAC=as.numeric(MAC),
                 diff=round(PAC-MAC))

# Merge with results 
results <- left_join(results,ageref,by="LocID") |> 
           rename("Location"="Location.x")

# Replace missing with global mean
globalmean <- round(mean(ageref$diff,na.rm=T))
results <- results |> mutate(diff=ifelse(is.na(diff),
                                         globalmean,
                                         diff))

# Difference to reference age difference
results <- results |> mutate(scenario=diff-as.numeric(TFRtype))

# Select scenarios based on this
results <- results |> filter(scenario%in%-2:2)


### For subset: conditional distributions ######################################

countrylist <- c("Canada","Denmark","Estonia","Finland","France","Hungary",
                 "Italy","Japan","Poland","Spain","Sweden",
                 "United States of America")

for(country in countrylist) {
  
  # Conditional distribution
  filename <- paste0("Data/",country,".rda")
  load(filename)
  
  # Temporary
  tmppop <- pop |> filter(Location==country)
  tmppop$Births <- NA
  tmpfer <- fer |> filter(Location==country)
  
  # Years 
  years <- unique(tmpfer$Time)
  age <- unique(tmpfer$AgeGrp)
  
  cagem <- as.numeric(rownames(conditional))
  cagef <- as.numeric(colnames(conditional))
  
  # Loop
  for(year in years) {
    tmpbrt <- tmpfer |> filter(Time==year & AgeGrp%in%cagef) |> pull(Births)
    tmpbrt <- (conditional%*%tmpbrt)[,1]
    tmppop$Births[tmppop$Time==year & tmppop$AgeGrp%in%cagem] <- tmpbrt
  }
  
  # Set missing to zero
  tmppop <- tmppop %>% mutate(Births=ifelse(is.na(Births),0,Births))
  
  # Get ASFRs for men
  tmppop <- tmppop %>% mutate(ASFRm=ifelse(PopMale==0|Births==0,
                                           0,
                                           Births/PopMale))
  
  # TFR
  tfr_men <- tmppop %>% 
    # Group by country and year
    group_by(Location,LocID,Time) %>%
    # Get sum of ASFRs, Total population
    summarize(mTFR=sum(ASFRm),
              pop=sum(PopMale)+sum(PopFemale)) |> 
    # Add age difference to the data
    mutate(scenario="Conditional") 
  
  # Merge (warnings can be ignored)
  condiff <- left_join(tfr_men,tfr_women)
  
  # Combine
  if(which(countrylist==country)==1) resultsc <- condiff else resultsc <- rbind(resultsc,condiff)
  
}


### Quick comparison ###########################################################  

# Calculate TFR ratio
results <- results |> mutate(TFRratio=mTFR/TFR)
resultsc <- resultsc |> mutate(TFRratio=mTFR/TFR)

# Select results
comparison0 <- results |> ungroup() |> filter(Location%in% countrylist & scenario==0) |> select(Location,TFRratio, Time) |> rename("Scenario0"="TFRratio")
comparison1 <- results |> ungroup()|> filter(Location%in% countrylist & scenario==1) |> select(Location,TFRratio, Time) |> rename("Scenario1"="TFRratio")
comparisonm1 <- results |> ungroup()|> filter(Location%in% countrylist & scenario==-1) |> select(Location,TFRratio, Time) |> rename("Scenario-1"="TFRratio")
comparisonc <- resultsc |> ungroup()|> select(Location,TFRratio, Time)|> rename("ScenarioC"="TFRratio")

# Merge
comparison <- left_join(comparison0,comparison1, by = c("Location", "Time"))
comparison <- left_join(comparison,comparisonm1, by = c("Location", "Time"))
comparison <- left_join(comparison,comparisonc, by = c("Location", "Time"))

# Generate difference between fixed difference of 0 and conditional approach
comparison$diff <- log(comparison$Scenario0)-log(comparison$ScenarioC)

# Save the data
save(comparison, file="../data/age_gap_simulation.Rda")
  
### END ########################################################################