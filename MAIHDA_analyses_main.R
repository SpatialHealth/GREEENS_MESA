# GREEENS project MAIHDA models coding-------------------------
# Author: Tara Jenson
# Created: 11/27/2023
# Last Edited: 08/10/2024

library(tidyverse)
library(magrittr)
library(brm)
library(brms)
library(tidybayes)
library(WriteXLS)
library(rcompanion)
library(extraDistr)
library(haven)
library(broom)
library(broom.mixed)


##### Setup/run MAIHDA models to assess relationship and interaction
###### of intersecting social strata on measures of greenspace 

setwd("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data")

# step 0. data import and mgmt------------------------------------------------------------------
## GSV/SVI data -------
### pull 2000-2006 GSV values-------
gsv_2000_thru_2002 <- readr::read_csv(paste0("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/gsv_2000_thru_2002.csv"))
glimpse(gsv_2000_thru_2002) # 22,608 --> 7536 * 3 yrs
gsv_2000_thru_2002  %>%  
  n_distinct(idno) # weird - it's like %>% pipe isn't working??
n_distinct(gsv_2000_thru_2002$idno) # 7536

### check missingness
gsv_2000_thru_2002 %>% 
  summarise_all(~ sum(is.na(.))) # missing trends high for 0, much smaller for 500, smaller for 1000

# select only cols we want for analyses
# total greenness (all vegetation: trees_500, palm_500, grass_500, field_500, flowers_500, plant_500)
# trees: trees_500 + palm_500)
# grass: grass_500
# green_other: field_500 + flowers_500 + plant_500
gsv_2000_thru_2002_sm <- gsv_2000_thru_2002 %>% 
  dplyr::select(idno, year, trees_0, trees_500, trees_1000, palm_0, palm_500, palm_1000, 
                grass_0, grass_500, grass_1000, field_0, field_500, field_1000, 
                flowers_0, flowers_500, flowers_1000, plant_0, plant_500, plant_1000)
glimpse(gsv_2000_thru_2002_sm) # 22,608, 20 cols

### code analysis GSV measures -------
# total greenness (trees_500 + palm_500 + grass_500 + field_500 + flowers_500 + plant_500)
# trees: trees_500 + palm_500)
# grass: grass_500
# green_other: field_500 + flowers_500 + plant_500
gsv_2000_thru_2002_analysis_measures <- gsv_2000_thru_2002_sm %>% 
  mutate(green_total=trees_500 + palm_500 + grass_500 + field_500 + flowers_500 + plant_500) %>% 
  mutate(tree_total=trees_500 + palm_500) %>% 
  mutate(green_other=field_500 + flowers_500 + plant_500)
glimpse(gsv_2000_thru_2002_analysis_measures) 

### avg GSV measures 2000-2002 -------
glimpse(gsv_2000_thru_2002_analysis_measures) 
head(gsv_2000_thru_2002_analysis_measures)

gsv_2000_thru_2002_analysis_measures_avgd <- gsv_2000_thru_2002_analysis_measures %>% 
  group_by(idno) %>% 
  summarise(green_total=mean(green_total), tree_total=mean(tree_total), 
            grass=mean(grass_500), green_other=mean(green_other))
glimpse(gsv_2000_thru_2002_analysis_measures_avgd) # 7,536 
glimpse(gsv_2000_thru_2002_analysis_measures)
head(gsv_2000_thru_2002_analysis_measures_avgd,50) %>% 
  print(n=50)

gsv_2000_thru_2002_ids_only <- gsv_2000_thru_2002_analysis_measures_avgd %>% 
  dplyr::select(idno)
glimpse(gsv_2000_thru_2002_ids_only)
n_distinct(gsv_2000_thru_2002_ids_only$idno) # 7536 

## MESA exam 1 data -----
mesa_exam1 <- read_sas("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/MESAe1FinalLabel20220125.sas7bdat")
glimpse(mesa_exam1) # 6,814 obs --> not sure how there are more GSV rows (see above) than MESA 

mesa_exam1_ids_only <- mesa_exam1 %>% 
  dplyr::select(idno)
glimpse(mesa_exam1_ids_only) 
n_distinct(mesa_exam1_ids_only$idno) # 6814
7536 - 6814 # 722 diff


### check of ids in GSV data NOT in MESA data ------
in_gsv_not_in_mesa <- anti_join(gsv_2000_thru_2002_ids_only, mesa_exam1_ids_only, by="idno")
glimpse(in_gsv_not_in_mesa) # 745 (doesn't add up quite: 7536 - 6,814 = 722)

in_mesa_not_in_gsv <- anti_join(mesa_exam1_ids_only, gsv_2000_thru_2002_ids_only, by="idno")
glimpse(in_mesa_not_in_gsv) # 23 

readr::write_csv(x = in_gsv_not_in_mesa,
                 file = paste0("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/in_gsv_not_in_mesa.csv"),
                 num_threads = 3) # adding option param to change NA to.

readr::write_csv(x = in_mesa_not_in_gsv,
                 file = paste0("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/in_mesa_not_in_gsv.csv"),
                 num_threads = 3) # adding option param to change NA to.


### subset to vars of interest -----
mesa_exam1_sm <- mesa_exam1 %>% 
  dplyr::select(idno, site1c, age1c, agecat1c, race1c, gender1, educ1, income1)
glimpse(mesa_exam1_sm) # 6,814

## MESA census data for n'hood depr & popn density -------
census_df <- read_sas("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/MESAa23_CensTrctSES_20220824/MESAa23_CensTrctSES_20220824.sas7bdat")
glimpse(census_df)
head(census_df)
census_df_sm <- census_df %>% 
  filter(EXAM==1) %>% #select for exam 1 values only
  dplyr::select(idno,F1_PC2,popdenmi_nowat) # keep only necess vars
head(census_df_sm)
dim(census_df_sm) # 6191

## join exam 1 + census + GSV measures -----
mesa_exam1_plus_census <- inner_join(mesa_exam1_sm, census_df_sm, by="idno")
glimpse(mesa_exam1_plus_census) # 6,191

mesa_gsv <- inner_join(mesa_exam1_plus_census, gsv_2000_thru_2002_analysis_measures_avgd, by="idno")
glimpse(mesa_gsv) # 6,187 (only lost a couple - that's good)

## recode vars to categorical and factor -------
mesa_gsv <- mesa_gsv %>% 
  mutate(educ_3cat = case_when(
    (educ1>=0 & educ1 <=3) ~ 1,
    (educ1>=4 & educ1 <=6) ~ 2,
    (educ1==7 | educ1 ==8) ~ 3,
    TRUE ~ NA)) %>% 
  mutate(income_4cat = case_when(
    (income1 >= 1 & income1 <= 6) ~ 1, # <$25,000       
    (income1 >= 7 & income1 <= 10) ~ 2, # $25k - 49,999       
    (income1 == 11) ~ 3, # $50k - $74999         
    (income1 == 12 | income1 == 13) ~ 4, # >$75k
    TRUE ~ NA))

xtabs( ~ educ_3cat + educ1, mesa_gsv, addNA = TRUE, na.action = NULL) # looks good
xtabs( ~ income_4cat + income1, mesa_gsv, addNA = TRUE, na.action = NULL) # looks good

mesa_gsv <- mesa_gsv %>% 
  mutate(ndepr_terts = ntile(F1_PC2, 3)) %>% # higher F1_PC2 value denotes worse NSES
  mutate(
    ndepr_reord = case_when(
      ndepr_terts == 1 ~ 3,        # flipping the least-depr indicator to 3 (highest NSES)
      ndepr_terts == 2 ~ 2,        # stays same      
      ndepr_terts == 3 ~ 1,        # flipping most-depr indicator to 1 (lowest NSES)
      is.na(ndepr_terts) ~ NA      # if missing, stays missing
    )) %>% 
  mutate(ndepr_reord = as.factor(ndepr_reord)) %>% 
  mutate(ndepr_terts_fact = if_else(ndepr_reord == 3, 'High NSES', if_else(ndepr_terts == 2, 'Mod NSES', 'Low NSES'))) %>% 
  mutate(ndepr_terts_fact=as.factor(ndepr_terts_fact))

# do a check that the ndepr levels are what I think/have assigned them to be
## i.e. that low SES are highest tertile of F1_PC2 values, high SES are the lowest
## use cut_number to see what the actual tertile cutpoints are
mesa_gsv %>% 
  mutate(ndepr_terts_bins = cut_number(F1_PC2,3)) %>% 
  dplyr::select(idno, F1_PC2, ndepr_terts, ndepr_reord, ndepr_terts_fact, ndepr_terts_bins)
# yep - these all check out

xtabs( ~ ndepr_reord + ndepr_terts, mesa_gsv, addNA = TRUE, na.action = NULL) # looks good
xtabs( ~ ndepr_terts_fact + ndepr_terts, mesa_gsv, addNA = TRUE, na.action = NULL)
xtabs( ~ ndepr_terts_fact + ndepr_reord, mesa_gsv, addNA = TRUE, na.action = NULL)

head(mesa_gsv,30) %>% 
  print(n=30)
# quick viz on terts and values of low, mod & high NSES
depr_fact_plot <- ggplot(mesa_gsv, aes(x=ndepr_terts_fact, y=F1_PC2)) 
depr_fact_plot + geom_boxplot() # looks good

depr_plot <- ggplot(mesa_gsv, aes(x=ndepr_reord, y=F1_PC2)) 
depr_plot + geom_boxplot() # looks good

## dichototomize pop density
# distrib of pop density
summary(mesa_gsv$popdenmi_nowat)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.    
# 14.76  3482.89   8003.10  27289.52  22092.71 210750.81   
# will dichotomize at median 8000, ~ median

mesa_gsv %>% 
  filter(popdenmi_nowat >= 1000) %>% 
  summarise(count = n())

5721 / 5858 # 97.6% of our sample is considered urban (> 1000ppl per sqr mile) - Lilah confirmed this is expected
sum(is.na(mesa_gsv$popdenmi_nowat)) # 0 missing


mesa_gsv <- mesa_gsv %>% 
  mutate(
    popden_dichot = case_when(
      popdenmi_nowat <= 8000 ~ 0,   # lower pop dens <= 8000
      TRUE ~ 1     # higher pop dens > 8003 (no missing values)
    ))
dim(mesa_gsv) # 6187
sum(mesa_gsv$popden_dichot ==0) # 2951
sum(mesa_gsv$popden_dichot ==1) # 3236
2951+3236 # all good 6187

###set cat vars to factors 
mesa_gsv$race1c <- factor(mesa_gsv$race1c)
mesa_gsv$gender1 <- factor(mesa_gsv$gender1)
mesa_gsv$educ_3cat <- factor(mesa_gsv$educ_3cat)
mesa_gsv$site1c <- factor(mesa_gsv$site1c)
mesa_gsv$income1 <- factor(mesa_gsv$income1)
mesa_gsv$income_4cat <- factor(mesa_gsv$income_4cat)

##check site counts
mesa_gsv %>% 
  count(site1c) # looks rational

##subset for non-missing race x edu x f1_pc2 -------
mesa_gsv %>% 
  summarise_all(~ sum(is.na(.)))
mesa_gsv_noNArace_edu_depr <- mesa_gsv %>% 
  filter(!is.na(race1c)) %>%
  filter(!is.na(educ_3cat)) %>% # subset to non-missing edu & f1_pc2 for race/eth x edu x f1_pc2 strata...
  filter(!is.na(ndepr_terts)) %>% # NSES
  filter(!is.na(green_total)) %>%  # ... non-missing outcomes
  filter(!is.na(income1)) # non-missing income
dim(mesa_gsv_noNArace_edu_depr) # 5858 (exam 4 only N was 5246)
glimpse(mesa_gsv_noNArace_edu_depr)

##check site counts again 
mesa_gsv_noNArace_edu_depr %>% 
  count(site1c) # looks rational


## check covar missingness------
glimpse(mesa_gsv_noNArace_edu_depr)
mesa_gsv_noNArace_edu_depr %>% 
  summarise_all(~ sum(is.na(.))) # only complete-case

out_dir <- "/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/"
readr::write_csv(x = mesa_gsv_noNArace_edu_depr, 
                 file = paste0(out_dir, "mesa_gsv_noNArace_edu_depr.csv"), 
                 num_threads = 3) 

# step 1 intersectional strata & size checks #################
### 1a. create intersectional strata ######################################
race_edu_depr_strata <- mesa_gsv_noNArace_edu_depr %>%
  dplyr::group_by(race1c,educ_3cat,ndepr_reord) %>%  # by race/eth, edu & nhood depr
  dplyr::mutate(strata=cur_group_id())               # 4 x 3 x 3 = 36 strata 
head(race_edu_depr_strata,50)
range(race_edu_depr_strata$strata) # 1-36 as expected for race/eth x edu strata

race_edu_depr_strata %>% 
  count(strata) %>% 
  print(n=36)

# compare the new strata with the flipped depr var to the old
race_edu_depr_strata %>% 
  dplyr::select(strata, race1c, educ_3cat, ndepr_terts, ndepr_reord, ndepr_terts_fact, F1_PC2) %>% 
  head(., 100) %>% 
  print(n=100)

strata_new_table <- table(race_edu_depr_strata$ndepr_reord,race_edu_depr_strata$strata) # check the recode
strata_new_table

xtabs( ~ race1c + strata , race_edu_depr_strata, addNA = TRUE, na.action = NULL)
xtabs( ~ educ_3cat + strata , race_edu_depr_strata, addNA = TRUE, na.action = NULL)
xtabs( ~ ndepr_reord + strata , race_edu_depr_strata, addNA = TRUE, na.action = NULL)
xtabs( ~ ndepr_terts_fact + strata , race_edu_depr_strata, addNA = TRUE, na.action = NULL)

out_dir <- "/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/"
readr::write_csv(x = race_edu_depr_strata, 
                 file = paste0(out_dir, "race_edu_depr_strata.csv"), 
                 num_threads = 3) 

######## testing out doing normal score transformation of  the greenness measures to use in models
#race_edu_depr_strata$green_totalNST = blom(race_edu_depr_strata$green_total)
# use green_totalNST in the model instead

### 1b. Check that sample sizes are sufficiently large #########################
total.number.strata <- 36 # num strata we have for race x edu x f1
n.strata <- table(race_edu_depr_strata$strata) # store sample sizes of strata in an object
n.strata
n.strata.df <- as.data.frame(t(n.strata)) # convert to df
n.strata.df
n.strata.30 <- sum(n.strata.df$Freq>=30) #num strata with more than 30 individs
n.strata.30
n.strata.30/total.number.strata* 100 # 97% of strata have >30 individs

n.strata.50 <- sum(n.strata.df$Freq>=50) #num strata with more than 30 individs
n.strata.50
n.strata.50/total.number.strata* 100 # 86% of strata have >50 individs

n.strata.20 <- sum(n.strata.df$Freq>=20) #num strata with more than 30 individs
n.strata.20
n.strata.20/total.number.strata* 100 # 100% of strata have >20 individs

# double check strata chars/Ns
race_edu_depr_strata %>%  # just swap levels in/out for counts
  filter(race1c==4 & educ_3cat ==3) %>% 
  count(ndepr_reord)


# step 2. MLM by strata: race x edu x n'hood depriv-----------------------------------------
#

## Perform multilevel analysis to partition the variance between 
###     and within intersectional strata                       
###     --> include age, sex, income, site as covars

### 2a.0 decide how to set priors - just trying this out -----------
### https://vasishth.github.io/bayescogsci/book/ch-reg.html#likelihood-and-priors
### Greenness measures cannot be negative (can be 0), or >100
# setting plausible priors
#summary(race_edu_depr_strata$green_total)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.37   13.76   26.30   25.75   34.95   69.49      88 
# set alpha = normal(50,25)
#qnorm(c(.025, .975), mean = 50, sd = 25) # 1.0009 98.9991

# uninformed prior for σ
# σ ∼Normal+  (0,1000) 
#qtnorm(c(.025, .975), mean = 0, sd = 50, a = 0) # in extraDistr pkg
# 1.566899 112.070136

# prior for B
# B ~normal() - skip this for now
#prior = c(
#  prior(normal(1000, 500), class = Intercept),
#  prior(normal(0, 1000), class = sigma),
#  prior(normal(0, 100), class = b, coef = c_load)
#)

### 2a.i: % total greenness, Simple intersectional  -----------------------------------------
### Bayesian MLM for simple intersectional model 
glimpse(race_edu_depr_strata) # 5,858 ppl
model1_race_edu_f1_greentotal <- brm(green_total~1+age1c+gender1+income_4cat+site1c+(1|strata),
                                     data = race_edu_depr_strata,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=3, seed=123)

model1_race_edu_f1_greentotal # 5858  
# checked/confirmed these missing numbers

#trying out with setting priors
#model1_race_edu_f1_greentotal_priors <- brm(green_total~1+age1c+gender1+income1+site4c+(1|strata),
#                                     data = race_edu_depr_strata,
#                                     warmup = 5000,
#                                     iter = 10000,
#                                     chains=1, seed=123,
#                                     prior = c(
#                                       prior(normal(50, 25), class = Intercept),
#                                       prior(normal(0, 50), class = sigma)
#                                       )
#                                     )

#model1_race_edu_f1_greentotal_priors # no change to the estimates

# trying out with normal-transformed green_totalNST 
#model1_race_edu_f1_greentotalNST <- brm(green_totalNST~1+age1c+gender1+income1+site4c+(1|strata),
#                                     data = race_edu_depr_strata,
#                                     warmup = 5000,
#                                     iter = 10000,
#                                     chains=1, seed=123)

#model1_race_edu_f1_greentotalNST # yeah runs but the values of distrib, and thus the estimates, are entirely diff

# trying out with log-normal, link identity 
model1_race_edu_f1_greentotal_ln <- brm(green_total~1+age1c+gender1+income_4cat+site1c+(1|strata),
                                        data = race_edu_depr_strata,
                                       family = "lognormal",
                                        warmup = 5000,
                                        iter = 10000,
                                       chains=3, seed=123)

model1_race_edu_f1_greentotal_ln # it ran....but pretty close to same as with normal transformed
                                # Exponentiating estimates leave me with way smaller variances than with my orig model


# Check plots
plot(model1_race_edu_f1_greentotal, variable = "^b", regex = TRUE)
plot(model1_race_edu_f1_greentotal)



# Check Gelman-Rubin convergence diagnostic (Rhat value should be lower than 1.1/1.05 for good convergence)
model1_race_edu_f1_greentotal.rhats <- round(as.numeric(model1_race_edu_f1_greentotal$rhats), 2)
model1_race_edu_f1_greentotal.rhats

any(model1_race_edu_f1_greentotal.rhats > 1.1) # false indicates convergence good
any(model1_race_edu_f1_greentotal.rhats > 1.05) # false indicates convergence good

### 2a.ii: % trees only, Simple intersectional  -----------------------------------------
model1_race_edu_f1_trees <- brm(tree_total~1+age1c+gender1+income_4cat+site1c+(1|strata),
                                data = race_edu_depr_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=3, seed=123)

model1_race_edu_f1_trees

# Check results
summary(model1_race_edu_f1_trees)

# Check plots
plot(model1_race_edu_f1_trees, variable = "^b", regex = TRUE)

# Check Gelman-Rubin convergence diagnostic (Rhat value should be lower than 1.1/1.05 for good convergence)
model1_race_edu_f1_trees.rhats <- round(as.numeric(model1_race_edu_f1_trees$rhats), 2)
model1_race_edu_f1_trees.rhats

any(model1_race_edu_f1_trees.rhats > 1.1) # FALSE = convergence good
any(model1_race_edu_f1_trees.rhats > 1.05) # FALSE = convergence good

### 2a.iii:  % grass only, Simple intersectional  -----------------------------------------
model1_race_edu_f1_grass <- brm(grass~1+age1c+gender1+income_4cat+site1c+(1|strata),
                                data = race_edu_depr_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=3, seed=123)

model1_race_edu_f1_grass

# Check results
summary(model1_race_edu_f1_grass)

# Check plots
plot(model1_race_edu_f1_grass, variable = "^b", regex = TRUE)

# Check Gelman-Rubin convergence diagnostic (Rhat value should be lower than 1.1/1.05 for good convergence)
model1_race_edu_f1_grass.rhats <- round(as.numeric(model1_race_edu_f1_grass$rhats), 2)
model1_race_edu_f1_grass.rhats

any(model1_race_edu_f1_grass.rhats > 1.1) # convergence good
any(model1_race_edu_f1_grass.rhats > 1.05) # convergence good

### 2a.iv:  % other greenness, Simple intersectional  -----------------------------------------
model1_race_edu_f1_green_other <- brm(green_other~1+age1c+gender1+income_4cat+site1c+(1|strata),
                                data = race_edu_depr_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=3, seed=123)

model1_race_edu_f1_green_other

# Check results
summary(model1_race_edu_f1_green_other)

# Check plots
plot(model1_race_edu_f1_green_other, variable = "^b", regex = TRUE)

# Check Gelman-Rubin convergence diagnostic (Rhat value should be lower than 1.1/1.05 for good convergence)
model1_race_edu_f1_green_other.rhats <- round(as.numeric(model1_race_edu_f1_green_other$rhats), 2)
model1_race_edu_f1_green_other.rhats

any(model1_race_edu_f1_green_other.rhats > 1.1) # convergence good
any(model1_race_edu_f1_green_other.rhats > 1.05) # convergence good

### 2b.i: % total greenness, interactional model -----------------------------------------
model2_race_edu_f1_greentotal <- brm(green_total~1+relevel(race1c,ref="3")+educ_3cat+ndepr_reord+age1c+gender1+income_4cat+site1c+(1|strata),
                                     data = race_edu_depr_strata,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=3, seed=123)

model2_race_edu_f1_greentotal

### 2b.ii: % trees only, interactional model -----------------------------------------

model2_race_edu_f1_trees <- brm(tree_total~1+relevel(race1c,ref="3")+educ_3cat+ndepr_reord+age1c+gender1+income_4cat+site1c+(1|strata),
                                data = race_edu_depr_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=3, seed=123)

model2_race_edu_f1_trees

### 2b.iii: % grass only, interactional model -----------------------------------------

model2_race_edu_f1_grass <- brm(grass~1+relevel(race1c,ref="3")+educ_3cat+ndepr_reord+age1c+gender1+income_4cat+site1c+(1|strata),
                                     data = race_edu_depr_strata,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=3, seed=123)

model2_race_edu_f1_grass

### 2b.iv: % other greenness, interactional model -----------------------------------------

model2_race_edu_f1_green_other <- brm(green_other~1+relevel(race1c,ref="3")+educ_3cat+ndepr_reord+age1c+gender1+income_4cat+site1c+(1|strata),
                                data = race_edu_depr_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=3, seed=123)

model2_race_edu_f1_green_other

# step 3: RQ 1: How useful are intersectional strata for predicting baseline greenspace? -----------------------------------------
#   --> Calculate the VPC (in %)
# round(strata.var.model1/(strata.var.model1 + residual.var.model1) * 100, 2)
# strata.var.model1 = model 1 (sd intercept estimat)^2
# residual.var.model1 = model 1 (fam specific params sigma)^2
# see Keller et al. 2023 for ref
### 3a. % total green space  -----------------------------------------
# Check results
model1_race_edu_f1_greentotal
# Variance at race x edu x depr strata strata level model 1 (sd intercept estimate)^2
3.60 ^2 # 12.96

# Variance at individual level model 1 (fam specific params sigma)^2
7.73^2 # 59.7529

# Calculate VPC model 1
round(12.96/ (12.96 + 59.7529)*100,2) # 17.82%

### 3b. % trees only -----------------------------------------
model1_race_edu_f1_trees

# Variance at race x edu strata strata level model 1 (sd intercept estimate)^2
2.74 ^2 # 7.5076

# Variance at individual level model 1 (fam specific params sigma)^2
6.18^2 # 38.1924

# Calculate VPC model 1
round(7.5076/ (7.5076 + 38.1924)*100,2) # 16.43%

### 3c. % grass only  -----------------------------------------
# Check results
model1_race_edu_f1_grass
# Variance at race x edu strata strata level model 1 (sd intercept estimate)^2
0.86 ^2 # 0.7396

# Variance at individual level model 1 (fam specific params sigma)^2
2.43^2 # 5.9049

# Calculate VPC model 1
round(0.7396/ (0.7396 + 5.9049)*100,2) # 11.13%

### 3d. % other greenness  -----------------------------------------
model1_race_edu_f1_green_other

# Variance at race x edu strata strata level model 1 (sd intercept estimate)^2
0.21 ^2 # 0.0441

# Variance at individual level model 1 (fam specific params sigma)^2
0.58^2 # 0.3364

# Calculate VPC model 1
round(0.0441/ (0.0441 + 0.3364)*100,2) # 11.59%

# step 4: RQ 2: How does the predicted greenspace outcome differ across intersectional strata? -----------------------------------------
# --> Calculate the average baseline greenspace outcome (and 95% CI) predicted by the simple intersectional model for each stratum

# Predicted average baseline greenspace per strata: race x edu x f1_pc2 (36 strata)
### 4a. % total greenness -------
# model 1 (simple/null)

# https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html#see-also-1
# https://www.rdocumentation.org/packages/tidybayes/versions/0.11.1.000001/topics/point_interval
# https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/ 
# https://paul-buerkner.github.io/brms/reference/predictive_interval.brmsfit.html

# assuming normal distrib model
pred.means.model1_race_edu_f1_greentotal <- model1_race_edu_f1_greentotal %>% 
  epred_draws(race_edu_depr_strata) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_greentotal)

# lognormal model
pred.means.model1_race_edu_f1_greentotal_ln <- model1_race_edu_f1_greentotal_ln %>% 
  epred_draws(race_edu_depr_strata) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_greentotal_ln)

### 4b. % trees only -------
pred.means.model1_race_edu_f1_trees <- model1_race_edu_f1_trees %>% 
  epred_draws(race_edu_depr_strata) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_trees)

### 4c. % grass only -------
pred.means.model1_race_edu_f1_grass <- model1_race_edu_f1_grass %>% 
  epred_draws(race_edu_depr_strata) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_grass)

### 4d. % other greenness -------
pred.means.model1_race_edu_f1_green_other <- model1_race_edu_f1_green_other %>% 
  epred_draws(race_edu_depr_strata) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_green_other)

#### --> for forest plots of null/random effects model PROCEED TO "MAIHDA forest plots.R"

# step 5: RQ 3: To what extent do interx of social categories contribute incrementally to explaining greenspace inequalities? ---------------
# --> Calculate the VPC (in %) in the interactional model (VPC adjusted)
# round(strata.var.model2/(strata.var.model2 + residual.var.model2) * 100, 2)
# strata.var.model1 = model 2 (sd intercept estimat)^2
# residual.var.model1 = model 2 (fam specific params sigma)^2
# see Keller et al. 2023 for ref
## race x edu x neighborhood dep ####
### total greenspace ####
model2_race_edu_f1_greentotal
# Variance at strata strata level model 2 (sd intercept estimate)^2
2.25^2 # 5.1076

# Variance at individual level model 2 (fam specific params sigma)^2
7.73^2 # 59.7529

# Calculate adjusted VPC model 2
round(5.1076 / (5.1076 + 59.7529)*100,2) # 7.87%

# get strata level variance from model 1 (null model)
model1_race_edu_f1_greentotal
# Variance at strata strata level model 1 (sd intercept estimate)^2
3.60^2 # 12.96
# Proportional Change in Variance (PCV) = Assessment of the extent to which between-stratum 
# inequalities are explained by additive vs. interactive/residual effects
# i.e. percentage of between-strata variance that cannot be explained by main effects (in %):
# WRONG ---_> (Variance at individual level model 2 - Variance at strata strata level model 2)/Variance at individual level model 2 
# CORRECT --> pcv <− round(((strata.var.model1 - strata.var.model2) / strata.var.model1) * 100, 2)
# strata.var.model1 = model 1 sd(Intercept)^2
# strata.var.model2 = model 2 sd(Intercept)^2
# see Keller et al 2023 Table 2 - p 31
round(((12.96-5.1076)/12.96)*100,2) # PCV = 60.59%
100 - round(((12.96-5.1076)/12.96)*100,2) # 39.41%

### trees ####
model2_race_edu_f1_trees
# Variance at strata level model 2 (sd intercept estimate)^2
1.41^2 # 1.9881

# Variance at individual level model 2 (fam specific params sigma)^2
6.18^2 # 38.1924

# Calculate adjusted VPC model 2
round(1.9881 / (1.9881 + 38.1924)*100,2) # 4.95%

# get strata level variance from null model 1
model1_race_edu_f1_trees 
2.74^2 # 7.5076
# Proportional Change in Variance (PCV) = Assessment of the extent to which between-stratum 
# inequalities are explained by additive vs. interactive/residual effects
# i.e. percentage of between-strata variance that cannot be explained by main effects (in %):
round(((7.5076-1.9881)/7.5076)*100,2) # 73.52
100 - round(((7.4529-2.0164)/7.4529)*100,2) # 27.06

### grass ####
model2_race_edu_f1_grass
# Variance at race x edu strata strata level model 2 (sd intercept estimate)^2
0.77^2 # 0.5929

# Variance at individual level model 2 (fam specific params sigma)^2
2.44^2 # 5.9536

# Calculate adjusted VPC model 2
round(0.5929 / (0.5929 + 5.9536)*100,2) # 9.06%

# get null model strata variance
model1_race_edu_f1_grass
0.86^2 # 0.7396

# Proportional Change in Variance (PCV) = Assessment of the extent to which between-stratum 
# inequalities are explained by additive vs. interactive/residual effects
# i.e. percentage of between-strata variance that cannot be explained by main effects (in %):
# (Variance at individual level model 2 - Variance at strata strata level model 2)/Variance at individual level model 2 
round(((0.7396-0.5929)/0.7396)*100,2) # 19.84
100 - round(((0.7396-0.5929)/0.7396)*100,2) # 80.16

### other green ####
model2_race_edu_f1_green_other
# Variance at race x edu strata strata level model 2 (sd intercept estimate)^2
0.11^2 # 0.0121

# Variance at individual level model 2 (fam specific params sigma)^2
0.58 ^2 # 0.3364

# Calculate adjusted VPC model 2
round(0.0121 / (0.0121 + 0.3364)*100,2) # 3.47%

# get strata variance from null model 1
model1_race_edu_f1_green_other
0.21^2  # 0.0441
# Proportional Change in Variance (PCV) = Assessment of the extent to which between-stratum 
# inequalities are explained by additive vs. interactive/residual effects
# i.e. percentage of between-strata variance that cannot be explained by main effects (in %):
round(((0.0441-0.0121)/0.0441)*100,2) # 72.56%
100 - round(((0.0441-0.0121)/0.0441)*100,2) # 27.44%

# step 6: RQ 4: Are greenspace inequalities more or less pronounced in specific intersectional strata? ---------------
## for race x edu strata SKIP THIS FOR NOW SINCE PROCEEDING WITH race/edu/f1_pc strata

## for race x edu x neighborhood deprivation strata

#### Extract random effects
#bayes.random.effects <- brms::ranef(mcmc.model2) - Keller example
bayes.random.effects_green <- brms::ranef(model2_race_edu_f1_greentotal)
bayes.random.effects_trees <- brms::ranef(model2_race_edu_f1_trees)
bayes.random.effects_grass <- brms::ranef(model2_race_edu_f1_grass)
bayes.random.effects_green_other <- brms::ranef(model2_race_edu_f1_green_other)

# As data frame
#bayes.random.effects.new <- as.data.frame(bayes.random.effects$strata) Keller example
bayes.random.effects_green_new <- as.data.frame(bayes.random.effects_green$strata)
bayes.random.effects_trees_new <- as.data.frame(bayes.random.effects_trees$strata)
bayes.random.effects_grass_new <- as.data.frame(bayes.random.effects_grass$strata)
bayes.random.effects_green_other_new <- as.data.frame(bayes.random.effects_green_other$strata)


#Round
#bayes.random.effects.new <- round(bayes.random.effects.new, 3)
bayes.random.effects_green_new <- round(bayes.random.effects_green_new, 3) 
bayes.random.effects_trees_new <- round(bayes.random.effects_trees_new, 3)
bayes.random.effects_grass_new <- round(bayes.random.effects_grass_new, 3) 
bayes.random.effects_green_other_new <- round(bayes.random.effects_green_other_new, 3)

# Add strata number
#bayes.random.effects.new$strata <- 1:36
bayes.random.effects_green_new$strata <- 1:36
bayes.random.effects_trees_new$strata <- 1:36
bayes.random.effects_grass_new$strata <- 1:36
bayes.random.effects_green_other_new$strata <- 1:36


# Change order of variables to put strata # first
#bayes.random.effects.new <- bayes.random.effects.new[,c(5,1,2,3,4)]
bayes.random.effects_green_new <- bayes.random.effects_green_new[,c(5,1,2,3,4)]
bayes.random.effects_trees_new <- bayes.random.effects_trees_new[,c(5,1,2,3,4)]
bayes.random.effects_grass_new <- bayes.random.effects_grass_new[,c(5,1,2,3,4)]
bayes.random.effects_green_other_new <- bayes.random.effects_green_other_new[,c(5,1,2,3,4)]


# Set working directory (e.g., "C:/Users/File")
setwd("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results")


# Export table
WriteXLS(bayes.random.effects_green_new, ExcelFileName = "bayes.random.effects_green.xls", SheetNames = NULL, perl = "perl",
         verbose = FALSE, Encoding = c("UTF-8", "latin1", "cp1252"),
         row.names = TRUE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = FALSE, BoldHeaderRow = FALSE,
         na = "",
         FreezeRow = 0, FreezeCol = 0,
         envir = parent.frame())


WriteXLS(bayes.random.effects_trees_new, ExcelFileName = "bayes.random.effects_trees.xls", SheetNames = NULL, perl = "perl",
         verbose = FALSE, Encoding = c("UTF-8", "latin1", "cp1252"),
         row.names = TRUE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = FALSE, BoldHeaderRow = FALSE,
         na = "",
         FreezeRow = 0, FreezeCol = 0,
         envir = parent.frame())

# Export table
WriteXLS(bayes.random.effects_grass_new, ExcelFileName = "bayes.random.effects_grass.xls", SheetNames = NULL, perl = "perl",
         verbose = FALSE, Encoding = c("UTF-8", "latin1", "cp1252"),
         row.names = TRUE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = FALSE, BoldHeaderRow = FALSE,
         na = "",
         FreezeRow = 0, FreezeCol = 0,
         envir = parent.frame())


WriteXLS(bayes.random.effects_green_other_new, ExcelFileName = "bayes.random.effects_gr_other.xls", SheetNames = "bayes.random.effects_other.xls", perl = "perl",
         verbose = FALSE, Encoding = c("UTF-8", "latin1", "cp1252"),
         row.names = TRUE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = FALSE, BoldHeaderRow = FALSE,
         na = "",
         FreezeRow = 0, FreezeCol = 0,
         envir = parent.frame())

# step 7: stratified analyses ---------
glimpse(race_edu_depr_strata) # 5,858
# already have pop density dichotomoized pop density var

## 7.0. split main dataset into high vs low pop density ---------
# create 2 subsets: popden_dichot == 0 and popden_dichot ==1
race_edu_depr_strata %>% 
  ungroup() %>% 
  count(popden_dichot)

race_edu_depr_strata_lowdens <-race_edu_depr_strata %>%
  ungroup() %>%
  dplyr::select(-strata) %>% 
  filter(popden_dichot==0)
dim(race_edu_depr_strata_lowdens) # 2724
glimpse(race_edu_depr_strata_lowdens)

race_edu_depr_strata_highdens <-race_edu_depr_strata %>% 
  ungroup() %>%
  dplyr::select(-strata) %>%
  filter(popden_dichot==1)
dim(race_edu_depr_strata_highdens) # 3134
glimpse(race_edu_depr_strata_highdens)
2724+3134 # 5858 - yep

## 7.1a. create intersectional strata for each dataset -------
# low pop dens dataset
race_edu_depr_low_new_strata <- race_edu_depr_strata_lowdens %>%
  dplyr::group_by(race1c,educ_3cat,ndepr_reord) %>%  # by race/eth, edu & NSES
  dplyr::mutate(strata=cur_group_id())               # 4 x 3 x 3 = 36 strata 
head(race_edu_depr_low_new_strata,50)
range(race_edu_depr_low_new_strata$strata) # 1-36 as expected for race/eth x edu strata

race_edu_depr_low_new_strata %>% 
  dplyr::select(strata, race1c, educ_3cat, ndepr_reord, F1_PC2) %>% 
  head(., 20)

strata_new_table_low <- table(race_edu_depr_low_new_strata$ndepr_reord,race_edu_depr_low_new_strata$strata) # check the recode
strata_new_table_low

# high pop dataset
race_edu_depr_high_new_strata <- race_edu_depr_strata_highdens %>%
  dplyr::group_by(race1c,educ_3cat,ndepr_reord) %>%  # by race/eth, edu & NSES
  dplyr::mutate(strata=cur_group_id())               # 4 x 3 x 3 = 36 strata 
head(race_edu_depr_high_new_strata,50)
range(race_edu_depr_high_new_strata$strata)

race_edu_depr_high_new_strata %>% 
  dplyr::select(strata, race1c, educ_3cat, ndepr_reord, F1_PC2) %>% 
  head(., 20)

strata_new_table_high <- table(race_edu_depr_high_new_strata$ndepr_reord,race_edu_depr_high_new_strata$strata) # check the recode
strata_new_table_high

## 7.1b. check counts of intersectional strata for each dataset -------
total.number.strata <- 36 # num strata we have for race x edu x f1
n.strata_low <- table(race_edu_depr_low_new_strata$strata) # store sample sizes of strata in an object
n.strata_low # lowest is 4
n.strata.df_low <- as.data.frame(t(n.strata_low)) # convert to df
n.strata.df_low
n.strata.30_low <- sum(n.strata.df_low$Freq>=30) #num strata with more than 30 individs
n.strata.30_low
n.strata.30_low/total.number.strata* 100 # 72% of strata have >30 individs

n.strata.20_low <- sum(n.strata.df_low$Freq>=20) #num strata with more than 30 individs
n.strata.20_low
n.strata.20_low/total.number.strata* 100 # 80.6% of strata have >20 individs

n.strata.10_low <- sum(n.strata.df_low$Freq>=10) #num strata with more than 10 individs
n.strata.10_low
n.strata.10_low/total.number.strata* 100 

n.strata.5_low <- sum(n.strata.df_low$Freq>=5) #num strata with more than 5 individs
n.strata.5_low
n.strata.5_low/total.number.strata* 100 # 97.2 (35 out of 36)

n.strata_high <- table(race_edu_depr_high_new_strata$strata) # store sample sizes of strata in an object
n.strata_high # lowest is 14
n.strata.df_high <- as.data.frame(t(n.strata_high)) # convert to df
n.strata.df_high
n.strata.30_high <- sum(n.strata.df_high$Freq>=30) #num strata with more than 30 individs
n.strata.30_high
n.strata.30_high/total.number.strata* 100 # 78% of strata have >30 individs

n.strata.20_high <- sum(n.strata.df_high$Freq>=20) #num strata with more than 30 individs
n.strata.20_high
n.strata.20_high/total.number.strata* 100 # 97.2% of strata have >20 individs

n.strata.10_high <- sum(n.strata.df_high$Freq>=10) #num strata with more than 10 individs
n.strata.10_high
n.strata.10_high/total.number.strata* 100 

n.strata.5_low <- sum(n.strata.df_low$Freq>=5) #num strata with more than 5 individs
n.strata.5_low
n.strata.5_low/total.number.strata* 100 # 100% 

#race_edu_depr_strata_lowdens <- race_edu_depr_strata_lowdens %>% 
#  mutate(strata = as.factor(strata))
#race_edu_depr_strata_highdens <- race_edu_depr_strata_highdens %>% 
#  mutate(strata = as.factor(strata))

## 7.2a.ii: % trees only, Simple intersectional  -----------------------------------------
glimpse(race_edu_depr_low_new_strata)
glimpse(race_edu_depr_high_new_strata)
glimpse(race_edu_depr_strata)


model1_race_edu_f1_trees_low <- brm(tree_total~1+age1c+gender1+income1+site1c+(1|strata),
                                data = race_edu_depr_low_new_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=3, seed=123)

model1_race_edu_f1_trees_low

model1_race_edu_f1_trees_high <- brm(tree_total~1+age1c+gender1+income1+site1c+(1|strata),
                                    data = race_edu_depr_high_new_strata,
                                    warmup = 5000,
                                    iter = 10000,
                                    chains=3, seed=123)

model1_race_edu_f1_trees_high



## 7.2a.iii:  % grass only, Simple intersectional  -----------------------------------------
model1_race_edu_f1_grass_low <- brm(grass~1+age1c+gender1+income1+site1c+(1|strata),
                                data = race_edu_depr_strata_lowdens,
                                warmup = 5000,
                                iter = 10000,
                                chains=3, seed=123)

model1_race_edu_f1_grass_low

model1_race_edu_f1_grass_high <- brm(grass~1+age1c+gender1+income1+site1c+(1|strata),
                                    data = race_edu_depr_strata_highdens,
                                    warmup = 5000,
                                    iter = 10000,
                                    chains=3, seed=123)

model1_race_edu_f1_grass_high


## 7.3 calc VPCs -----------------------------------------
# --> Calculate the average baseline greenspace outcome (and 95% CI) predicted by the simple intersectional model for each stratum
# trees - low
model1_race_edu_f1_trees_low
# Variance at race x edu x depr strata strata level model 1 (sd intercept estimate)^2
2.52 ^2 # 6.3504

# Variance at individual level model 1 (fam specific params sigma)^2
5.93^2 # 35.1649

# Calculate VPC model 1
round(6.3504/ (6.3504 + 35.1649)*100,2) # 15.3%

# trees - high
model1_race_edu_f1_trees_high
# Variance at race x edu x depr strata strata level model 1 (sd intercept estimate)^2
2.29 ^2 # 5.2441

# Variance at individual level model 1 (fam specific params sigma)^2
5.58^2 # 31.1364

# Calculate VPC model 1
round(5.2441/ (5.2441 + 31.1364)*100,2) # 14.41%

# grass - low
model1_race_edu_f1_grass_low
# Variance at race x edu x depr strata strata level model 1 (sd intercept estimate)^2
0.72 ^2 # 0.5184

# Variance at individual level model 1 (fam specific params sigma)^2
2.78^2 # 7.7284

# Calculate VPC model 1
round(0.5184/ (0.5184 + 7.7284)*100,2) # 6.29%

# grass - high
model1_race_edu_f1_grass_high
# Variance at race x edu x depr strata strata level model 1 (sd intercept estimate)^2
0.72^2 # 0.5184

# Variance at individual level model 1 (fam specific params sigma)^2
1.58^2 # 2.4964

# Calculate VPC model 1
round(0.5184/ (0.5184 + 2.4964)*100,2) # 17.2%


## 7.4 Predicted average greenspace per strata ------
### 7.4a. % trees only -------
# model 1 (simple/null)
model1_race_edu_f1_trees_low
#tidy_model1_trees_low <- tidy(model1_race_edu_f1_trees_low)
#tidy(model1_race_edu_f1_trees_low)
#tidy_model1_trees_low
#tidy_pred <- model1_race_edu_f1_trees_low %>% 
#  predicted_draws(newdata = race_edu_depr_strata_lowdens)
#tidy_pred

glimpse(race_edu_depr_low_new_strata)
pred.means.model1_race_edu_f1_trees_low <- model1_race_edu_f1_trees_low %>% 
  epred_draws(race_edu_depr_low_new_strata) %>% 
  group_by(strata) #%>% 
  #mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_trees_low)

# race_edu_depr_strata_lowdens <- race_edu_depr_strata_lowdens %>% 
#  ungroup()

race_edu_depr_strata_lowdens_df <- data.frame(race_edu_depr_strata_lowdens)
epred_draws(model1_race_edu_f1_trees_low, newdata = race_edu_depr_strata_lowdens, allow_new_levels = T) #Error

pred.means.model1_race_edu_f1_trees_low <- model1_race_edu_f1_trees_low %>% 
  epred_draws(race_edu_depr_strata_lowdens, re_formula = ~(1|strata), allow_new_levels = TRUE) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 

pred.means.model1_race_edu_f1_trees_low <- model1_race_edu_f1_trees_low %>% 
  tidybayes::epred_draws(race_edu_depr_strata_lowdens) %>% 
  dplyr::group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_trees_low)

x_low <- race_edu_depr_strata_lowdens$strata
x_low
y_low<-x_low[1:36] 
y_low
levels(y_low)

x_high <- race_edu_depr_strata_highdens$strata
x_high
y_high<-x_high[1:36] 
y_high
levels(y_high)


model1_race_edu_f1_trees_high
glimpse(race_edu_depr_strata_highdens)
race_edu_depr_strata_highdens <- race_edu_depr_strata_highdens %>% 
  ungroup()

pred.means.model1_race_edu_f1_trees_high <- model1_race_edu_f1_trees_high %>% 
  epred_draws(race_edu_depr_high_new_strata) #%>% 
  group_by(strata) #%>% 
  #mean_qi(.epred) # 
epred_draws(model1_race_edu_f1_trees_high, newdata = race_edu_depr_strata_highdens, re_formula = ~(1|strata), allow_new_levels = T) #Error


model1_race_edu_f1_trees_high
pred.means.model1_race_edu_f1_trees_high <- model1_race_edu_f1_trees_high %>% 
  epred_draws(race_edu_depr_strata_highdens, sample_new_levels = "old_levels") #%>% 
  #dplyr::group_by(strata) %>% 
  #mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_trees_high)

### 7.4a. % grass only -------
pred.means.model1_race_edu_f1_grass_low <- model1_race_edu_f1_grass_low %>% 
  epred_draws(race_edu_depr_strata_lowdens) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_grass_low)

pred.means.model1_race_edu_f1_grass_high <- model1_race_edu_f1_grass_high %>% 
  epred_draws(race_edu_depr_strata_highdens) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_grass_high)

