### Descriptive data and table 1

# Author: Tara Jenson, based on code written by Cam Reimer, Marcia P. Jimenez & L. Paloma Rojas-Saunero 
# Created: 2/1/2024
# Last Edited: 2/3/2024

library(tidyverse)
library(dplyr)
library(rio)
library(tableone)
library(WriteXLS)

install_formats() # Install rio's ‘Suggests’ Dependencies
rm(list = ls()) # clean up your global environment



# Import data -------------------------------------------------------------

setwd("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data")


###Read in combined GSV-Mesa 2007 cross-sectional data file 
gsv_mesa <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/gsv_demo_census_2007.csv")
head(gsv_mesa,20)
dim(gsv_mesa) # 6814

###subset for non-missing race x edu x f1_pc2 
sum(is.na(gsv_mesa$race1c)) # 215
sum(is.na(gsv_mesa$educ_3cat)) # 238
sum(is.na(gsv_mesa$f1_pc2_3cat)) # 1265

gsv_mesa_noNAedu_f1pc2 <- gsv_mesa %>% 
  filter(!is.na(race1c)) %>%
  filter(!is.na(educ_3cat)) %>% # subset to non-missing race/eth x edu x f1_pc2 strata
  filter(!is.na(f1_pc2_3cat))
dim(gsv_mesa_noNAedu_f1pc2) # 5536

gsv_mesa_noNAedu_f1pc2_sm <- gsv_mesa_noNAedu_f1pc2 %>% select(idno, race1c, educ_3cat, f1_pc2_3cat,
                                                               age1c, agecat1c, gender1, income1, income_3cat, year, green_total, 
                                                               tree_total, green_other, grass_500, site1c, site4c, F1_PC2)

###set cat vars to factors and assign labels for levels
gsv_mesa_noNAedu_f1pc2_sm$race1c <- factor(gsv_mesa_noNAedu_f1pc2_sm$race1c, levels=c(1,2,3,4),
                          labels=c("White", 
                                   "Chinese American",
                                   "Black",
                                   "Hispanic"))

gsv_mesa_noNAedu_f1pc2_sm$educ_3cat <- factor(gsv_mesa_noNAedu_f1pc2_sm$educ_3cat, levels = c(1,2,3), 
                             labels = c("High School/GED or less", #reference
                                        "Some college", 
                                        "Bachelor's Degree or higher"))

gsv_mesa_noNAedu_f1pc2_sm$f1_pc2_3cat <- factor(gsv_mesa_noNAedu_f1pc2_sm$f1_pc2_3cat, levels = c(3,2,1), 
                               labels=c("Most deprived neighborhood", #reference
                                        "Moderately deprived neighborhood",
                                        "Least deprived neighborhood"))  


gsv_mesa_noNAedu_f1pc2_sm$gender1 <- factor(gsv_mesa_noNAedu_f1pc2_sm$gender1, levels = c(0,1), 
                           labels = c("Female", "Male"))

gsv_mesa_noNAedu_f1pc2_sm$income1 <- factor(gsv_mesa_noNAedu_f1pc2_sm$income1, levels = 1:13, 
                           labels = c("< $5,000", "$5,000-$7,999", "$8,000-$11,999", "$12,000-$15,999", "$16,000-$19,999", "$20,000-$24,999", "$25,000-$29,999", "$30,000-$34,999", "$35,000-$39,999", "$40,000-$49,000", "$50,000-$74,999", "$75,000-$99,999", "$100,000 +"))

gsv_mesa_noNAedu_f1pc2_sm$site4c <- factor(gsv_mesa_noNAedu_f1pc2_sm$site4c, levels = c(3,4,5,6,7,8), 
                          labels = c("WFU", "COL", "JHU", "UMN", "NWU", "UCLA"))

gsv_mesa_noNAedu_f1pc2_sm$agecat1c <- factor(gsv_mesa_noNAedu_f1pc2_sm$agecat1c, levels=c(1,2,3,4),
                                           labels=c("45 - 54 years",
                                                    "55 - 64 years",
                                                    "65 - 74 years",
                                                    "75 - 84 years"))


# collapse income to fewer cats & make factor
gsv_mesa_noNAedu_f1pc2_sm   <- gsv_mesa_noNAedu_f1pc2_sm  %>% 
  mutate(income_4cat = 
           case_when(
             income1 %in% c("<$5,000", "$5,000-$7,999", "$8,000-$11,999",
                            "$12,000-$15,999","$16,000-$19,999","$20,000-$24,999") ~ 1,
             income1 %in% c("$25,000-$29,999", "$30,000-$34,999",
                            "$35,000-$39,999", "$40,000-$49,999") ~ 2,
             income1 %in% c("$50,000-$74,999") ~ 3,
             TRUE ~ 4 # > than 74,999
           ))

gsv_mesa_noNAedu_f1pc2_sm$income_4cat <- factor(gsv_mesa_noNAedu_f1pc2_sm$income_4cat, levels=c(1,2,3,4),
                           labels = c("<$24,999", #Reference
                                      "$25,000-$49,999", "$50,000-$74,999", ">$75,000"))


# Create Table 1 ----------------------------------------------------------

###overall Ns/%s and means/SDs -----------------
# cont vars: age, n'hood depr
# cat vars: race/eth, edu, depr cat sex, exam site (location)

contVars <-
  c(
    "age1c",
    "F1_PC2"
  )
catVars <-
  c(
    "race1c",
    "educ_3cat",
    "f1_pc2_3cat",
    "gender1",
    "income_4cat",
    "site4c",
    "agecat1c"
  )  

allvars <- c(contVars, catVars)
table1 <-
  CreateTableOne(
    vars = allvars,
    data = gsv_mesa_noNAedu_f1pc2_sm,
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
# cont vars: green_total
# strata vars:  "race1c" "educ_3cat" "f1_pc2_3cat" "gender1" "income_4cat" "site4c" "agecat1c"
 
contVars_greens <-
  c(
    "green_total",
    "tree_total",
    "grass_500",
    "green_other"
  )

#repeat this for each of the strata cat vars listed above, kick out to csv to cut/paste into main table
table1_greens <-
  CreateTableOne(
    vars = contVars_greens,
    strata = "agecat1c" ,
    data = gsv_mesa_noNAedu_f1pc2_sm,
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



