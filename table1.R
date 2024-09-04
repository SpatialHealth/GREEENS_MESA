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
gsv_mesa_popden_noNArace_edu_depr_forTables <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/gsv_mesa_popden_noNArace_edu_depr_sm.csv")
glimpse(gsv_mesa_popden_noNArace_edu_depr_forTables)
head(gsv_mesa_popden_noNArace_edu_depr_sm,20)
dim(gsv_mesa_popden_noNArace_edu_depr_sm) # 5246

###set cat vars to factors 
gsv_mesa_popden_noNArace_edu_depr_forTables$race1c <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$race1c)
gsv_mesa_popden_noNArace_edu_depr_forTables$gender1 <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$gender1)
gsv_mesa_popden_noNArace_edu_depr_forTables$educ_3cat <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$educ_3cat)
gsv_mesa_popden_noNArace_edu_depr_forTables$f1_pc2_3cat <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$f1_pc2_3cat)
gsv_mesa_popden_noNArace_edu_depr_forTables$n_depr <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$n_depr)
gsv_mesa_popden_noNArace_edu_depr_forTables$site4c <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$site4c)
gsv_mesa_popden_noNArace_edu_depr_forTables$income1 <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$income1)
gsv_mesa_popden_noNArace_edu_depr_forTables$income_4cat <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$income_4cat)


# no need for this next section as importing data already with no NA for strata vars
###subset for non-missing race x edu x f1_pc2 
#sum(is.na(gsv_mesa$race1c)) # 215
#sum(is.na(gsv_mesa$educ_3cat)) # 238
#sum(is.na(gsv_mesa$f1_pc2_3cat)) # 1265

#gsv_mesa_noNAedu_f1pc2 <- gsv_mesa %>% 
#  filter(!is.na(race1c)) %>%
#  filter(!is.na(educ_3cat)) %>% # subset to non-missing race/eth x edu x f1_pc2 strata
#  filter(!is.na(f1_pc2_3cat))
#dim(gsv_mesa_noNAedu_f1pc2) # 5536

#gsv_mesa_noNAedu_f1pc2_sm <- gsv_mesa_noNAedu_f1pc2 %>% select(idno, race1c, educ_3cat, f1_pc2_3cat,
#                                                               age1c, agecat1c, gender1, income1, income_3cat, year, green_total, 
#                                                               tree_total, green_other, grass_500, site1c, site4c, F1_PC2)


# swap in gsv_mesa_popden_noNArace_edu_depr_sm data from stratified analyses
glimpse(gsv_mesa_popden_noNArace_edu_depr_forTables) # 5246

###set cat vars to factors and assign labels for levels
gsv_mesa_popden_noNArace_edu_depr_forTables$race1c <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$race1c, levels=c(1,2,3,4),
                          labels=c("White", 
                                   "Chinese American",
                                   "Black",
                                   "Hispanic"))

gsv_mesa_popden_noNArace_edu_depr_forTables$educ_3cat <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$educ_3cat, levels = c(1,2,3), 
                             labels = c("High School/GED or less", #reference
                                        "Some college", 
                                        "Bachelor's Degree or higher"))

gsv_mesa_popden_noNArace_edu_depr_forTables$n_depr <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$n_depr, levels = c(3,2,1), 
                               labels=c("High", "Moderate", "Low"))  # changing to NSES verbiage: least = high, mod = mod, most = low  

gsv_mesa_popden_noNArace_edu_depr_forTables$gender1 <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$gender1, levels = c(0,1), 
                                                       labels = c("Female", "Male"))

gsv_mesa_popden_noNArace_edu_depr_forTables$popden_dichot <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$popden_dichot, levels = c(0,1), 
                           labels = c("<7,500 people per square mile", ">=7,500 people per square mile"))

gsv_mesa_popden_noNArace_edu_depr_forTables$income_4cat <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$income_4cat, levels = c(1,2,3,4), 
                           labels = c("< $25,000", "$25,000-$49,999", "$50,000-$74,999", "$75,000+"))

gsv_mesa_popden_noNArace_edu_depr_forTables$site4c <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$site4c, levels = c(3,4,5,6,7,8), 
                          labels = c("WFU", "COL", "JHU", "UMN", "NWU", "UCLA"))

gsv_mesa_popden_noNArace_edu_depr_forTables$agecat1c <- factor(gsv_mesa_popden_noNArace_edu_depr_forTables$agecat1c, levels=c(1,2,3,4),
                                           labels=c("45 - 54 years",
                                                    "55 - 64 years",
                                                    "65 - 74 years",
                                                    "75 - 84 years"))

glimpse(gsv_mesa_popden_noNArace_edu_depr_forTables)
sum(gsv_mesa_popden_noNArace_edu_depr_forTables$popden_dichot =="<7,500 people per square mile")
sum(gsv_mesa_popden_noNArace_edu_depr_forTables$popden_dichot ==">=7,500 people per square mile")
# Create Table 1 ------------------------------------------------

###overall Ns/%s and means/SDs -----------------
# cont vars: age, n'hood depr
# cat vars: race/eth, edu, depr cat sex, exam site (location)

contVars <-
  c(
    "age1c",
    "F1_PC2",
    "popdenmi_nowat",
    "green_total_2005_2007",
    "tree_total_2005_2007",
    "grass_2005_2007",
    "green_other_2005_2007"
  )
catVars <-
  c(
    "race1c",
    "educ_3cat",
    "n_depr",
    "popden_dichot",
    "gender1",
    "income_4cat",
    "site4c",
    "agecat1c"
  )  

allvars <- c(contVars, catVars)
table1 <-
  CreateTableOne(
    vars = allvars,
    data = gsv_mesa_popden_noNArace_edu_depr_forTables,
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
    "green_total_2005_2007",
    "tree_total_2005_2007",
    "grass_2005_2007",
    "green_other_2005_2007"
  )

#repeat this for each of the strata cat vars listed above, kick out to csv to cut/paste into main table
table1_greens <-
  CreateTableOne(
    vars = contVars_greens,
    strata = "gender1" ,
    data = gsv_mesa_popden_noNArace_edu_depr_forTables,
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


