install.packages("haven")

library("haven")
edu <- read_sav("sect2_education.sav")
read_sav("/Users/andrewchristensen/Documents/Access Initiative/Nigeria/NGA_2018_LSS_v01_M_SPSS/Household/sect2_education.sav")
edu <- read_sav("/Users/andrewchristensen/Documents/Access Initiative/Nigeria/NGA_2018_LSS_v01_M_SPSS/Household/sect2_education.sav")
View(edu)

aux <- read_sav("/Users/andrewchristensen/Documents/Access Initiative/Nigeria/NGA_2018_LSS_v01_M_SPSS/Household/sect_aux.sav")
View(aux)

ros <- read_sav("/Users/andrewchristensen/Documents/Access Initiative/Nigeria/NGA_2018_LSS_v01_M_SPSS/Household/sect1_roster.sav")
View(ros)


# Merge age data with education data

install.packages("dplyr")
library(dplyr)
eduros <- full_join(edu,ros)
View(eduros)

library(plyr)
plyr::count(eduros$s01q04a)

#Create variable where 1=person of secondary schooling age and 0=person not of secondary schooling age

eduros <- eduros %>%
  mutate(
    secondaryage=if_else(eduros$s01q04a=="14" | eduros$s01q04a=="15" | eduros$s01q04a=="16" | eduros$s01q04a=="17" | eduros$s01q04a=="18" | eduros$s01q04a=="19",1,0))

count(eduros$secondaryage)


