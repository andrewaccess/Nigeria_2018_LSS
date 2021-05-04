

setwd("/Users/andrewchristensen/Documents/Access Initiative/Nigeria/NGA_2018_LSS_v01_M_SPSS/Household/")
getwd()
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
    secondaryage=if_else(eduros$s01q04a=="12" | eduros$s01q04a=="13" | eduros$s01q04a=="14" | eduros$s01q04a=="15" | eduros$s01q04a=="16" | eduros$s01q04a=="17",1,0))

plyr::count(eduros$secondaryage)

#Create a table that shows the percentage of all secondary school age youth who are not currently attending 
#lower or upper secondary school

outofschooltable <- table(eduros$secondaryage,eduros$s02q12)
outofschooltable #Columns 2=currently NOT attending school, 1=currently ATTENDING school / Rows 0=Not secondary age, 1=secondary age

#Create a table that shows the out of school rate by each age

eduros <- eduros %>%
  mutate(
    age12=ifelse(eduros$s01q04a=="12",1,0))
      
plyr::count(eduros$age12)

#Create a table that shows the percentage of 12 year-olds who are not currently attending lower or upper secondary school

outofschooltable12 <- table(eduros$age12,eduros$s02q12)
outofschooltable12 #Columns 2=currently NOT attending school, 1=currently ATTENDING school / Rows 0=Not secondary age, 1=secondary age

#13 year-olds
eduros <- eduros %>%
  mutate(
    age13=ifelse(eduros$s01q04a=="13",1,0))

plyr::count(eduros$age13)

outofschooltable13 <- table(eduros$age13,eduros$s02q12)
outofschooltable13

#14 year-olds
library(dplyr)
library(haven)
eduros <- eduros %>%
  mutate(
    age14=ifelse(eduros$s01q04a=="14",1,0))

plyr::count(eduros$age14)

outofschooltable14 <- table(eduros$age14,eduros$s02q12)
outofschooltable14

#15 year-olds
library(dplyr)
library(haven)
eduros <- eduros %>%
  mutate(
    age15=ifelse(eduros$s01q04a=="15",1,0))

plyr::count(eduros$age15)

outofschooltable15 <- table(eduros$age15,eduros$s02q12)
outofschooltable15

#16 year-olds
library(dplyr)
library(haven)
eduros <- eduros %>%
  mutate(
    age16=ifelse(eduros$s01q04a=="16",1,0))

plyr::count(eduros$age16)

outofschooltable16 <- table(eduros$age16,eduros$s02q12)
outofschooltable16

#17 year-olds
library(dplyr)
library(haven)
eduros <- eduros %>%
  mutate(
    age17=ifelse(eduros$s01q04a=="17",1,0))

plyr::count(eduros$age17)

outofschooltable17 <- table(eduros$age17,eduros$s02q12)
outofschooltable17

#1.13 Identify how out of school rates vary by gender overall
secondaryagegendertable <- table(eduros$secondaryage,eduros$s02q02)
secondaryagegendertable #columns: female=2, male=1; rows: secondary school age = 1, not secondary school age = 0
# There are many more females of secondary school age than males of secondary school age, perhaps this is due to sampling procedure.

eduros <- eduros %>%
  mutate(
    secondaryagegirl=if_else(eduros$s02q02==2 & secondaryage==1 ,1,0))

plyr::count(eduros$secondaryagegirl)

eduros <- eduros %>%
  mutate(
    secondaryageboy=if_else(eduros$s02q02==1 & secondaryage==1 ,1,0))

plyr::count(eduros$secondaryageboy)

outofschooltablegirls <- table(eduros$secondaryagegirl,eduros$s02q12)
outofschooltablegirls # Rows= girl=1, boy=0, Column= ATTENDING=1, NOT ATTENDING=2

outofschooltableboys <- table(eduros$secondaryageboy,eduros$s02q12)
outofschooltableboys # Rows= boy=1, girl=0, Column= ATTENDING=1, NOT ATTENDING=2

         