### Descriptive data and table 1

# Author: Tara Jenson, based on code written by Cam Reimer, Marcia P. Jimenez & L. Paloma Rojas-Saunero 
# Created: 2/1/2024
# Last Edited: 8/16/2024

library(tidyverse)
library(rio)
library(tableone)
library(WriteXLS)

install_formats() # Install rio's ‘Suggests’ Dependencies
rm(list = ls()) # clean up your global environment



# Import data -------------------------------------------------------------

setwd("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data")


###Read in combined GSV-Mesa data file 
race_edu_depr_strata_forTables <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/race_edu_depr_strata.csv")
glimpse(race_edu_depr_strata_forTables)
head(race_edu_depr_strata_forTables,20)
dim(race_edu_depr_strata_forTables) # 5858

###set cat vars to factors 
race_edu_depr_strata_forTables$race1c <- factor(race_edu_depr_strata_forTables$race1c)
race_edu_depr_strata_forTables$gender1 <- factor(race_edu_depr_strata_forTables$gender1)
race_edu_depr_strata_forTables$agecat1c <- factor(race_edu_depr_strata_forTables$agecat1c)
race_edu_depr_strata_forTables$educ_3cat <- factor(race_edu_depr_strata_forTables$educ_3cat)
race_edu_depr_strata_forTables$ndepr_reord <- factor(race_edu_depr_strata_forTables$ndepr_reord)
race_edu_depr_strata_forTables$site1c <- factor(race_edu_depr_strata_forTables$site1c)
race_edu_depr_strata_forTables$income1 <- factor(race_edu_depr_strata_forTables$income1)
race_edu_depr_strata_forTables$income_4cat <- factor(race_edu_depr_strata_forTables$income_4cat)

glimpse(race_edu_depr_strata_forTables) # 5858

###set cat vars to factors and assign labels for levels
race_edu_depr_strata_forTables$race1c <- factor(race_edu_depr_strata_forTables$race1c, levels=c(1,2,3,4),
                          labels=c("White", 
                                   "Chinese American",
                                   "Black",
                                   "Hispanic"))

race_edu_depr_strata_forTables$educ_3cat <- factor(race_edu_depr_strata_forTables$educ_3cat, levels = c(1,2,3), 
                             labels = c("High School/GED or less", 
                                        "Some college", 
                                        "Bachelor's Degree or higher"))

race_edu_depr_strata_forTables$ndepr_reord <- factor(race_edu_depr_strata_forTables$ndepr_reord, levels = c(3,2,1), 
                               labels=c("High", "Moderate", "Low"))  # changing to NSES verbiage: least = high, mod = mod, most = low  

race_edu_depr_strata_forTables$gender1 <- factor(race_edu_depr_strata_forTables$gender1, levels = c(0,1), 
                                                       labels = c("Female", "Male"))

race_edu_depr_strata_forTables$popden_dichot <- factor(race_edu_depr_strata_forTables$popden_dichot, levels = c(0,1), 
                           labels = c("<=8,000 people per square mile", ">8,000 people per square mile"))

race_edu_depr_strata_forTables$income_4cat <- factor(race_edu_depr_strata_forTables$income_4cat, levels = c(1,2,3,4), 
                           labels = c("< $25,000", "$25,000-$49,999", "$50,000-$74,999", "$75,000+"))

race_edu_depr_strata_forTables$site1c <- factor(race_edu_depr_strata_forTables$site1c, levels = c(3,4,5,6,7,8), 
                          labels = c("WFU", "COL", "JHU", "UMN", "NWU", "UCLA"))

race_edu_depr_strata_forTables$agecat1c <- factor(race_edu_depr_strata_forTables$agecat1c, levels=c(1,2,3,4),
                                           labels=c("45 - 54 years",
                                                    "55 - 64 years",
                                                    "65 - 74 years",
                                                    "75 - 84 years"))

glimpse(race_edu_depr_strata_forTables)
sum(race_edu_depr_strata_forTables$popden_dichot =="<=8,000 people per square mile")
sum(race_edu_depr_strata_forTables$popden_dichot ==">8,000 people per square mile")
# Create Table 1 ------------------------------------------------

###overall Ns/%s and means/SDs -----------------
# cont vars: age, n'hood depr
# cat vars: race/eth, edu, depr cat sex, exam site (location)

contVars <-
  c(
    "age1c",
    "F1_PC2",
    "popdenmi_nowat",
    "green_total",
    "tree_total",
    "grass",
    "green_other"
  )
catVars <-
  c(
    "race1c",
    "educ_3cat",
    "ndepr_reord",
    "popden_dichot",
    "gender1",
    "income_4cat",
    "site1c",
    "agecat1c"
  )  

allvars <- c(contVars, catVars)
table1 <-
  CreateTableOne(
    vars = allvars,
    data = race_edu_depr_strata_forTables,
    factorVars = catVars
  )
table1
print(table1)
summary(table1)
print(table1,  formatOptions = list(big.mark = ","))


t1_out <- print(
            table1, 
            explain = TRUE,
            test = FALSE,
            quote = FALSE,
            noSpaces = TRUE,
            printToggle = FALSE,
            formatOptions = list(big.mark = ","))

## Save to a CSV file - USE THIS METHOD
write.csv(t1_out, file = "/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/tables/table1_overallNs.csv")

path <- "/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/tables/"
export(t1_out_df, here::here(path, "table1_test.csv")) # this exports only the values col because the whole object
                                                      # created by tableOne is a matrix + array sitch so export
                                                      # only prints the array of the column of values



###mean(SD) greenness for each strata of cat vars  -----------------
# cont vars: "green_total_2005_2007","tree_total_2005_2007","grass_2005_2007","green_other_2005_2007"
# strata vars:  "race1c" "educ_3cat" "n_depr" "gender1" "income_4cat" "site4c" "agecat1c" "popden_dichot"
 
contVars_greens <-
  c(
    "green_total",
    "tree_total",
    "grass",
    "green_other"
  )

#repeat this for each of the strata cat vars listed above, kick out to csv to cut/paste into main table
# "race1c","educ_3cat","ndepr_reord","popden_dichot","gender1","income_4cat","site1c","agecat1c"
table1_greens <-
  CreateTableOne(
    vars = contVars_greens,
    strata = "agecat1c" ,
    data = race_edu_depr_strata_forTables,
  )
table1_greens

t1_greens_out <- print(
  table1_greens, 
  explain = TRUE,
  test = FALSE,
  quote = FALSE,
  noSpaces = TRUE,
  printToggle = FALSE,
  formatOptions = list(big.mark = ","))

## Save to a CSV file - USE THIS METHOD
write.csv(t1_greens_out, file = "/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/tables/table1_greens_strat_by_cat_vars.csv")

# Supp Table 1 ------------------------------------------------
## strata of race/eth, edu & NSES
table(race_edu_depr_strata$strata)


