## Plot the jalovaara data

library(readxl)
library(tidyverse)

## Load the data
file_jalovaara <- "data/jalovaara2019_childlessness.xlsx"
childlessness_m <- read_xlsx(file_jalovaara, sheet=1)
names(childlessness_m) <- str_to_title(names(childlessness_m))
childlessness_f <- read_xlsx(file_jalovaara, sheet=2)

# Pivot the data
childlessness_f <- childlessness_f |> 
  pivot_longer(cols=c("Low", "Medium", "High"), names_to="education", values_to="childlessness")

childlessness_m <- childlessness_m |> 
  pivot_longer(cols=c("Low", "Medium", "High"), names_to="education", values_to="childlessness")


# Sort the variables
childlessness_f$education <- ordered(childlessness_f$education, levels=c("Low", "Medium", "High"))
childlessness_m$education <- ordered(childlessness_m$education, levels=c("Low", "Medium", "High"))

# Plot the data
ggplot(data = childlessness_m, aes(x=Cohort, y=childlessness, group=education)) +
  geom_line(aes(colour="Men (age 45)")) +
  geom_point(aes(colour="Men (age 45)"), size=3, shape=15) +
  geom_text(aes(colour="Men (age 45)", label=childlessness), vjust=2) +
  geom_line(data = childlessness_f, aes(x=Cohort, y=childlessness, colour="Women (age 40)", group=education)) +
  geom_point(data = childlessness_f, aes(x=Cohort, y=childlessness, colour="Women (age 40)"), size=4, shape=17) +
  geom_text(data = childlessness_f, aes(x=Cohort, y=childlessness, label=childlessness, colour="Women (age 40)"), vjust=-1.5) +
  facet_wrap(~education)  + 
  scale_colour_manual(values=c("navy", "darkred")) +
  scale_x_discrete(expand=c(0.02, 0)) +
  scale_y_continuous("Childlessness (%)", expand=c(0.01, 0), n.breaks=10) +
  theme(
    axis.text.x=element_text(angle=45, hjust=1, vjust=1)
  )

ggsave("results/childlessness_norge_sex_jalovaara2019.pdf", width=25, height=15, unit="cm")
### END #####################################################