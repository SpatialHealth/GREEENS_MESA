# GREEENS project: MAIHDA analyses - stratifying models by popn density -------------------------
# Author: Tara Jenson
# Created: 2/8/2024
# Last Edited: 

library(haven)
library(tidyverse)


# pull popn density var in from exam 4 datafile
census_df <- read_sas("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/MESAa23_CensTrctSES_20220824/MESAa23_CensTrctSES_20220824.sas7bdat")
head(census_df)
census_df_sm <- census_df %>% 
  filter(EXAM==4) %>% #select for exam 4 values only
  dplyr::select(idno,popdenmi_nowat) # keep only necess vars
head(census_df_sm)
dim(census_df_sm) #5693
head(gsv_mesa) # using gsv_mesa df created in main analyses
dim(gsv_mesa) #6814
gsv_mesa_popden <- inner_join(gsv_mesa,census_df_sm,"idno")
head(gsv_mesa_popden) 
dim (gsv_mesa_popden) # 5693   37

# distrib of pop density
summary(gsv_mesa_popden$popdenmi_nowat)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# 3.19   2859.10   7499.39  25123.66  20575.79 200363.52        12 

gsv_mesa_popden %>% 
  filter(popdenmi_nowat >= 1000) %>% 
  summarise(count = n())

5187 / 5693 # 91% of our sample is considered urban

sum(is.na(gsv_mesa_popden$popdenmi_nowat)) #12 missing

gsv_mesa_popden %>% 
  filter(!is.na(popdenmi_nowat) & popdenmi_nowat >= 1000) %>% 
  summarise(count = n())
5187/5693*100 # 91% have >1000 ppl per sq mile - Lilah confirmed this is expected

gsv_mesa_popden_nona <- gsv_mesa_popden %>% 
  filter(!is.na(popdenmi_nowat)) %>% 
  mutate(
    popden_dichot = case_when(
      popdenmi_nowat < 7500 ~ 0,   # lower pop dens < 7500
                      TRUE ~ 1     # higher pop dens > 7500, no missing values so no case for
    )  
  )
dim(gsv_mesa_popden_nona) #5681 - dropped the 12 with missing popdens
sum(gsv_mesa_popden_nona$popden_dichot ==0)
sum(gsv_mesa_popden_nona$popden_dichot ==1)
2928+2753 # all good

# now let's run the total greenness models stratified by <7500 and > 7500
###set cat vars to factors 
gsv_mesa_popden_nona$race1c <- factor(gsv_mesa_popden_nona$race1c)
gsv_mesa_popden_nona$gender1 <- factor(gsv_mesa_popden_nona$gender1)
gsv_mesa_popden_nona$educ_3cat <- factor(gsv_mesa_popden_nona$educ_3cat)
gsv_mesa_popden_nona$f1_pc2_3cat <- factor(gsv_mesa_popden_nona$f1_pc2_3cat)
gsv_mesa_popden_nona$n_depr <- factor(gsv_mesa_popden_nona$n_depr)
gsv_mesa_popden_nona$site4c <- factor(gsv_mesa_popden_nona$site4c)
gsv_mesa_popden_nona$income1 <- factor(gsv_mesa_popden_nona$income1)


###subset for non-missing race x edu x f1_pc2 
gsv_mesa_popden_noNArace_edu_depr <- gsv_mesa_popden_nona %>% 
  filter(!is.na(race1c)) %>%
  filter(!is.na(educ_3cat)) %>% # subset to non-missing edu & f1_pc2 for race/eth x edu x f1_pc2 strata
  filter(!is.na(n_depr))
dim(gsv_mesa_popden_noNArace_edu_depr) # 5535 (just one less than my orig dataset 5536

gsv_mesa_popden_noNArace_edu_depr_sm <- gsv_mesa_popden_noNArace_edu_depr %>% 
  dplyr::select(idno, race1c, educ1, educ_3cat, F1_PC2, f1_pc2_3cat, n_depr,
         age1c, agecat1c, gender1, income1, income_3cat, year, site1c, site4c, 
         green_total, tree_total, green_other, grass_500, popdenmi_nowat, popden_dichot)

head(gsv_mesa_popden_noNArace_edu_depr_sm)
dim(gsv_mesa_popden_noNArace_edu_depr_sm)


# create 2 subsets: popden_dichot == 0 and popden_dichot ==1
gsv_mesa_lowdens <-gsv_mesa_popden_noNArace_edu_depr_sm %>% 
  filter(popden_dichot==0)
dim(gsv_mesa_lowdens) # 2873
gsv_mesa_highdens <-gsv_mesa_popden_noNArace_edu_depr_sm %>% 
  filter(popden_dichot==1)
dim(gsv_mesa_highdens) # 2662
2873+2662 # 5535 - yep

# step 1 intersectional strata & size checks #################
### 1a. create intersectional strata ######################################

## prior dataset name race_edu_f1_tert_strata

race_edu_depr_strata_low <- gsv_mesa_lowdens %>%
  dplyr::group_by(race1c,educ_3cat,n_depr) %>%  # by race/eth, edu & nhood depr
  dplyr::mutate(strata=cur_group_id())               # 4 x 3 x 3 = 36 strata 
head(race_edu_depr_strata_low,50)
range(race_edu_depr_strata_low$strata) # 1-36 as expected for race/eth x edu strata

race_edu_depr_strata_low %>% 
  dplyr::select(strata, race1c, educ_3cat, n_depr, F1_PC2, f1_pc2_3cat) %>% 
  head(., 20)

strata_new_table_low <- table(race_edu_depr_strata_low$n_depr,race_edu_depr_strata_low$strata) # check the recode
strata_new_table_low


race_edu_depr_strata_high <- gsv_mesa_highdens %>%
  dplyr::group_by(race1c,educ_3cat,n_depr) %>%  # by race/eth, edu & nhood depr
  dplyr::mutate(strata=cur_group_id())               # 4 x 3 x 3 = 36 strata 
head(race_edu_depr_strata_high,50)
range(race_edu_depr_strata_high$strata)

race_edu_depr_strata_high %>% 
  dplyr::select(strata, race1c, educ_3cat, n_depr, F1_PC2, f1_pc2_3cat) %>% 
  head(., 20)

strata_new_table_high <- table(race_edu_depr_strata_high$n_depr,race_edu_depr_strata_high$strata) # check the recode
strata_new_table_high

### 2a.i: % total greenness, Simple intersectional  -----------------------------------------
### Bayesian MLM for simple intersectional model 
model1_race_edu_f1_greentotal_low <- brm(green_total~1+age1c+gender1+income1+site4c+(1|strata),
                                     data = race_edu_depr_strata_low,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=3, seed=123)

model1_race_edu_f1_greentotal_low

model1_race_edu_f1_greentotal_high <- brm(green_total~1+age1c+gender1+income1+site4c+(1|strata),
                                         data = race_edu_depr_strata_high,
                                         warmup = 5000,
                                         iter = 10000,
                                         chains=3, seed=123)

model1_race_edu_f1_greentotal_high

# step 4: RQ 2: How does the predicted greenspace outcome differ across intersectional strata? -----------------------------------------
# --> Calculate the average baseline greenspace outcome (and 95% CI) predicted by the simple intersectional model for each stratum

# Predicted average baseline greenspace per strata: race x edu x f1_pc2 (36 strata)
### 4a. % total greenness -------
# model 1 (simple/null)
pred.means.model1_race_edu_f1_greentotal_low <- model1_race_edu_f1_greentotal_low %>% 
  epred_draws(race_edu_depr_strata_low) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_greentotal_low)

pred.means.model1_race_edu_f1_greentotal_high <- model1_race_edu_f1_greentotal_high %>% 
  epred_draws(race_edu_depr_strata_high) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_greentotal_high)
