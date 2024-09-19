# GREEENS project: MAIHDA analyses - stratifying models by popn density -------------------------
# Author: Tara Jenson
# Created: 2/8/2024
# Last Edited: 8/7/2024

library(haven)
library(tidyverse)
rm(list = ls()) # clean up your global environment

# pull main analysis data
gsv_mesa_forstrat_analysis <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/mesa_gsv_noNArace_edu_depr.csv")
head(gsv_mesa_forstrat_analysis,20)
dim(gsv_mesa_forstrat_analysis) # 5858 | 20
glimpse(gsv_mesa_forstrat_analysis) 

# check site counts
gsv_mesa_forstrat_analysis %>% 
  count(site1c)

summary()
gsv_mesa_forstrat_analysis %>% 
  filter(site1c==3) %>% 
  summarise(maxpopdens = max(popdenmi_nowat))


# now let's run the total greenness models stratified by pop density (<8000 and >8000)
###set cat vars to factors 
gsv_mesa_forstrat_analysis$race1c <- factor(gsv_mesa_forstrat_analysis$race1c)
gsv_mesa_forstrat_analysis$gender1 <- factor(gsv_mesa_forstrat_analysis$gender1)
gsv_mesa_forstrat_analysis$educ_3cat <- factor(gsv_mesa_forstrat_analysis$educ_3cat)
gsv_mesa_forstrat_analysis$ndepr_terts <- factor(gsv_mesa_forstrat_analysis$ndepr_terts)
gsv_mesa_forstrat_analysis$ndepr_reord <- factor(gsv_mesa_forstrat_analysis$ndepr_reord)
gsv_mesa_forstrat_analysis$site1c <- factor(gsv_mesa_forstrat_analysis$site1c)
gsv_mesa_forstrat_analysis$income1 <- factor(gsv_mesa_forstrat_analysis$income1)

gsv_mesa_forstrat_analysis %>% 
  summarise_all(~ sum(is.na(.))) # only complete-case

glimpse(gsv_mesa_forstrat_analysis)

# create 2 subsets: popden_dichot == 0 and popden_dichot ==1
gsv_mesa_lowdens <-gsv_mesa_forstrat_analysis %>% 
  filter(popden_dichot==0)
dim(gsv_mesa_lowdens) # 2724
gsv_mesa_highdens <-gsv_mesa_forstrat_analysis %>% 
  filter(popden_dichot==1)
dim(gsv_mesa_highdens) # 3134
2724+3134 # 5858 - yep

# I out out in the high pop density model below that site 4 drops out so need to assess the Ns for those
gsv_mesa_lowdens %>% 
  count(site1c) # 7 ppl in site 4
gsv_mesa_highdens %>% 
  count(site1c)


# step 1 intersectional strata & size checks #################
### 1a. create intersectional strata ######################################

## prior dataset name race_edu_f1_tert_strata

race_edu_depr_strata_low <- gsv_mesa_lowdens %>%
  dplyr::group_by(race1c,educ_3cat,ndepr_reord) %>%  # by race/eth, edu & nhood depr
  dplyr::mutate(strata=cur_group_id())               # 4 x 3 x 3 = 36 strata 
head(race_edu_depr_strata_low,50)
range(race_edu_depr_strata_low$strata) # 1-36 as expected for race/eth x edu strata

race_edu_depr_strata_low %>% 
  dplyr::select(strata, race1c, educ_3cat, ndepr_terts, ndepr_reord, F1_PC2) %>% 
  head(., 20)

strata_new_table_low <- table(race_edu_depr_strata_low$ndepr_reord,race_edu_depr_strata_low$strata) # check the recode
strata_new_table_low


race_edu_depr_strata_high <- gsv_mesa_highdens %>%
  dplyr::group_by(race1c,educ_3cat,ndepr_reord) %>%  # by race/eth, edu & nhood depr
  dplyr::mutate(strata=cur_group_id())               # 4 x 3 x 3 = 36 strata 
head(race_edu_depr_strata_high,50)
range(race_edu_depr_strata_high$strata)

race_edu_depr_strata_high %>% 
  dplyr::select(strata, race1c, educ_3cat, ndepr_terts, ndepr_reord, F1_PC2) %>% 
  head(., 20)

strata_new_table_high <- table(race_edu_depr_strata_high$ndepr_reord,race_edu_depr_strata_high$strata) # check the recode
strata_new_table_high

### 2a.i: % trees, Simple intersectional  -----------------------------------------
### Bayesian MLM for simple intersectional model 

glimpse(race_edu_depr_strata_low)
model1_race_edu_f1_tree_total_low <- brm(tree_total~1+age1c+gender1+income1+(1|strata),
                                     data = race_edu_depr_strata_low,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=3, seed=123)

model1_race_edu_f1_tree_total_low

glimpse(race_edu_depr_strata_high)
model1_race_edu_f1_tree_total_high <- brm(tree_total~1+age1c+gender1+income1+(1|strata),
                                         data = race_edu_depr_strata_high,
                                         warmup = 5000,
                                         iter = 10000,
                                         chains=3, seed=123)

model1_race_edu_f1_tree_total_high


### 2a.ii: % grass, Simple intersectional  -----------------------------------------
### Bayesian MLM for simple intersectional model 
glimpse(race_edu_depr_strata_low)
model1_race_edu_f1_grass_low <- brm(grass~1+age1c+gender1+income1+(1|strata),
                                         data = race_edu_depr_strata_low,
                                         warmup = 5000,
                                         iter = 10000,
                                         chains=3, seed=123)

model1_race_edu_f1_grass_low

glimpse(race_edu_depr_strata_high)
model1_race_edu_f1_grass_high <- brm(grass~1+age1c+gender1+income1+(1|strata),
                                          data = race_edu_depr_strata_high,
                                          warmup = 5000,
                                          iter = 10000,
                                          chains=3, seed=123)

model1_race_edu_f1_grass_high

# step 4: RQ 2: How does the predicted greenspace outcome differ across intersectional strata? -----------------------------------------
# --> Calculate the average baseline greenspace outcome (and 95% CI) predicted by the simple intersectional model for each stratum

# Predicted average baseline greenspace per strata: race x edu x f1_pc2 (36 strata)
### 4a. % total greenness -------
# model 1 (simple/null)
# trees
pred.means.model1_race_edu_f1_tree_total_low <- model1_race_edu_f1_tree_total_low %>% 
  epred_draws(race_edu_depr_strata_low) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_tree_total_low)

pred.means.model1_race_edu_f1_tree_total_high <- model1_race_edu_f1_tree_total_high %>% 
  epred_draws(race_edu_depr_strata_high) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_tree_total_high)

# grass
pred.means.model1_race_edu_f1_grass_low <- model1_race_edu_f1_grass_low %>% 
  epred_draws(race_edu_depr_strata_low) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_grass_low)

pred.means.model1_race_edu_f1_grass_high <- model1_race_edu_f1_grass_high %>% 
  epred_draws(race_edu_depr_strata_high) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_grass_high)
