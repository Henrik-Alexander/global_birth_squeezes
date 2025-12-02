# Global birth squeezes

Authors: Vegard Skirbekk, Thomas Spoorenberg, Christian Dudel and Henrik-Alexander Schubert

## Purpose
This project seeks to describe the in the population at reproductive age between 1950 and 2024 around the world using the World Population Prospects 2024. All code on which this analysis is based was written in the [**R**](https://www.r-project.org/) statistical programming language.

## Software and hardware
The analysis were executed in [**R**](https://www.r-project.org/) version 4.2.1 (2022-06-23 ucrt). The computing unit was platform x86_64-w64-mingw32/x64 (64-bit).
The program was running under Windows Server x64 (build 17763)

## Prerequisites
In order to run the files you need to download the data from WPP 2024 and store in the folder ./raw. The [age-specific fertility rates](https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/3_Fertility/WPP2024_FERT_F01_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx), the [single-year life tables](https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/4_Mortality/WPP2024_MORT_F06_1_SINGLE_AGE_LIFE_TABLE_ESTIMATES_BOTH_SEXES.xlsx) and the [Bundled files](https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx) can be downloaded by following the hyperlinks.

### Packages
This work would not have been possible with the scientific and programming contributions of people who developed packages and made them available free of use on [**R-Cran**](https://cran.r-project.org/). I list the packages used in this project to acknowledge the contribution of the authors and to ensure that people can download the required packages in order to fully reproduce the results. Furthermore, the interested reader can follow the link on the package name to read the vignettes.

- [`stargazer`](https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf) by Marek Hlavac
- [`tidyverse`](https://cran.r-project.org/web/packages/tidyverse/index.html) by Hadley Wickham
- [`data.table`](https://cran.r-project.org/web/packages/data.table/index.html) by Matt Dowle et al.
- [`readxl`](https://cran.r-project.org/web/packages/readxl/index.html) by Jennifer Bryan
- [`patchwork`](https://cran.r-project.org/web/packages/patchwork/index.html) by Thomas Lin Pedersen
- [`ggrepel`](https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html) by Kamil Slowikowski

# Folder structure

## ./code: 

- [Clean the data](code/00_load_data.R): The data is downloaded from the internet.
- [Load the data](code/01_clean_data.R): The data is loaded, cleaned and stored.
- [Analyze population change](code/02_demographic_analysis.R): The adult sex ratios are analyzed.
- [Estimate male TFR](code/03_male_tfr_approx.R): The TFR for men is estimated.
- [Age gap approach](code/04_age_gap_approach.R): The age gap approach is estimated


- [Functions](code/functions.R): Helper functions
- [Graphic template](code/graphics.R): Sets the graphic style


## ./raw: 

This folder contains the raw, unedited data from WPP2024.

- Fertility: [age-specific fertility rates](https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/3_Fertility/WPP2024_FERT_F01_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx)
- Mortality: [single-year life tables](https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/4_Mortality/WPP2024_MORT_F06_1_SINGLE_AGE_LIFE_TABLE_ESTIMATES_BOTH_SEXES.xlsx)
- Locations: UN location names
- WPP 2024 Summary: [Bundled files](https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx)
![Data structure of the raw folder.](raw_data.png)



## ./data:

This folder is used for storing the manipulated data created with the ./code files from the raw data under (./raw).

## ./tables:
This folder contains the tables.

## ./figures:
This folder contains the figures.