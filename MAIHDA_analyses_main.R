# GREEENS project MAIHDA models coding-------------------------
# Author: Tara Jenson
# Created: 11/27/2023
# Last Edited: 08/10/2024

library(tidyverse)
library(brm)
library(brms)
library(tidybayes)
library(WriteXLS)
library(rcompanion)
library(extraDistr)


##### Setup/run MAIHDA models to assess relationship and interaction
###### of intersecting social strata on measures of greenspace 

setwd("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data")

# step 0. data import and mgmt------------------------------------------------------------------

## pull and join 2007 + 2005-2006 GSV values-------
gsv_2005_2006_for_join <- readr::read_csv(paste0("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/gsv_2005_2006.csv"))
glimpse(gsv_2005_2006_for_join) # 15,072 --> 7536 * 2 yrs
### check missingness
gsv_2005_2006_for_join %>% 
  summarise_all(~ sum(is.na(.))) # no missingness....OH WAIT there totally is, it's just not going to 
    # show up until I turn all the GSV cols into numeric down below

gsv_long_2007_and_later <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/datagsv_long.csv")
glimpse(gsv_long_2007_and_later) # 102,830 # 7345 * 14 yrs
# a bit of mismatch (191 ppl) in new 2000-2006 data vs prior 2007 and later. 
# I assume some updates/fixes to the newer data but will keep to those from the orig analysis with 2007+ data 
gsv_long_2007_and_later %>% 
  summarise_all(~ sum(is.na(.))) # a lot of missingness
# I went back to the original 2007+ data and doesn't seem to be any coding issues

gsv_long_2007 <- gsv_long_2007_and_later %>% 
  filter(year==2007) 
glimpse(gsv_long_2007) # 7,345
gsv_long_2007 %>% 
  summarise_all(~ sum(is.na(.)))

#subset 2005-2006 GSVs to just those in the 2007 only those common to both 
gsv_long_2007_idsonly <- gsv_long_2007 %>% 
  dplyr::select(idno)

gsv_2005_2006_idsincommon_w2007 <- inner_join(gsv_long_2007_idsonly, gsv_2005_2006_for_join, by="idno")
glimpse(gsv_2005_2006_idsincommon_w2007) # 134 cols, 14,676 rows --> 7338 * 2 yrs --> 7 missing, so some mismatch between the 2000-2006 and 2007+ data
# let it go for now
gsv_2005_2006_idsincommon_w2007_idsonly <- gsv_2005_2006_idsincommon_w2007 %>% 
  dplyr::select(idno) 
glimpse(gsv_2005_2006_idsincommon_w2007_idsonly) 
gsv_2005_2006_idsincommon_w2007_idsonly <- gsv_2005_2006_idsincommon_w2007 %>% 
  group_by (idno) %>% 
  slice(c(1)) %>% 
  ungroup() %>% 
  dplyr::select(idno)
glimpse(gsv_2005_2006_idsincommon_w2007_idsonly) # 7,338

#subset 2007 GSVs to those common to 2005-06 & 2007
gsv_long_2007_idsincommon_w2005_06 <- inner_join(gsv_2005_2006_idsincommon_w2007_idsonly, gsv_long_2007, by="idno")
glimpse(gsv_long_2007_idsincommon_w2005_06) # 7,338
head(gsv_long_2007_idsincommon_w2005_06)
# select only cols from both datasets we want for analyses
# total greenness (all vegetation: trees_500, palm_500, grass_500, field_500, flowers_500, plant_500)
# trees: trees_500 + palm_500)
# grass: grass_500
# green_other: field_500 + flowers_500 + plant_500
gsv_2005_2006_sm <- gsv_2005_2006_idsincommon_w2007 %>% 
  dplyr::select(idno, year, trees_0, trees_500, trees_1000, palm_0, palm_500, palm_1000, 
                grass_0, grass_500, grass_1000, field_0, field_500, field_1000, 
                flowers_0, flowers_500, flowers_1000, plant_0, plant_500, plant_1000)
glimpse(gsv_2005_2006_sm) # 14,676, 20 cols
gsv_2005_2006_sm <- gsv_2005_2006_sm %>% # fix GSV data types
  mutate(trees_0=as.numeric(trees_0)) %>% 
  mutate(trees_500=as.numeric(trees_500)) %>%
  mutate(trees_1000=as.numeric(trees_1000)) %>%
  mutate(palm_0=as.numeric(palm_0)) %>% 
  mutate(palm_500=as.numeric(palm_500)) %>%
  mutate(palm_1000=as.numeric(palm_1000)) %>%
  mutate(grass_0=as.numeric(grass_0)) %>% 
  mutate(grass_500=as.numeric(grass_500)) %>%
  mutate(grass_1000=as.numeric(grass_1000)) %>%
  mutate(field_0=as.numeric(field_0)) %>% 
  mutate(field_500=as.numeric(field_500)) %>%
  mutate(field_1000=as.numeric(field_1000)) %>%
  mutate(flowers_0=as.numeric(flowers_0)) %>% 
  mutate(flowers_500=as.numeric(flowers_500)) %>%
  mutate(flowers_1000=as.numeric(flowers_1000)) %>% 
  mutate(plant_0=as.numeric(plant_0)) %>% 
  mutate(plant_500=as.numeric(plant_500)) %>%
  mutate(plant_1000=as.numeric(plant_1000))

gsv_2005_2006_sm %>% 
  summarise_all(~ sum(is.na(.))) # yep there's the missings for 2005-2006!

gsv_2007_sm <- gsv_long_2007_idsincommon_w2005_06 %>% 
  dplyr::select(idno, year, trees_0, trees_500, trees_1000, palm_0, palm_500, palm_1000, 
                grass_0, grass_500, grass_1000, field_0, field_500, field_1000, 
                flowers_0, flowers_500, flowers_1000, plant_0, plant_500, plant_1000)
glimpse(gsv_2007_sm) # 7,338, 20 cols

# row bind 2005-2006 + 2007, and arrange by id
gsv_2005_2007 <- bind_rows(gsv_2005_2006_sm, gsv_2007_sm)

gsv_2005_2007 <- gsv_2005_2007 %>% 
  arrange(idno)
glimpse(gsv_2005_2007) # 22,014 -> 7338 * 3
gsv_2005_2007 %>% 
  count(idno) # 7338

gsv_2005_2007 %>% 
  group_by(year) %>% 
  summarise_all(~ sum(is.na(.))) # ok missing seems consistent by year
  # year to year

# save gsv_2005_2007 
readr::write_csv(x = gsv_2005_2007,
                 file = paste0("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/gsv_2005_2007_analysisvarsonly.csv"),
                 num_threads = 3) # adding option param to change NA to.

## recode analysis GSV measures -------
# total greenness (trees_500 + palm_500 + grass_500 + field_500 + flowers_500 + plant_500)
# trees: trees_500 + palm_500)
# grass: grass_500
# green_other: field_500 + flowers_500 + plant_500
gsv_2005_2007_analysis_measures <- gsv_2005_2007 %>% 
  mutate(green_total=trees_500 + palm_500 + grass_500 + field_500 + flowers_500 + plant_500) %>% 
  mutate(tree_total=trees_500 + palm_500) %>% 
  mutate(green_other=field_500 + flowers_500 + plant_500)
glimpse(gsv_2005_2007_analysis_measures) 

## avg GSV measures 2005-2007 -------
glimpse(gsv_2005_2007_analysis_measures) 
head(gsv_2005_2007_analysis_measures)

gsv_2005_2007_analysis_measures_avgd <- gsv_2005_2007_analysis_measures %>% 
  group_by(idno) %>% 
  summarise(green_total=mean(green_total), tree_total=mean(tree_total), 
            grass=mean(grass_500), green_other=mean(green_other))
glimpse(gsv_2005_2007_analysis_measures_avgd)
head(gsv_2005_2007_analysis_measures_avgd)
gsv_2005_2007_analysis_measures_avgd <- gsv_2005_2007_analysis_measures_avgd %>% 
  rename(green_total_2005_2007=green_total) %>% 
  rename(tree_total_2005_2007=tree_total) %>% 
  rename(grass_2005_2007=grass) %>% 
  rename(green_other_2005_2007=green_other)

glimpse(gsv_2005_2007_analysis_measures_avgd)

##Read in combined GSV-Mesa data file -------
gsv_mesa <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/gsv_demo_census_2007.csv")
head(gsv_mesa,20)
dim(gsv_mesa) # 6814 | 35
glimpse(gsv_mesa) 

## drop prior coded GSV measures-----
gsv_mesa <- gsv_mesa %>% 
  dplyr::select(-grass_500, -IQR_grass_500, -plant_500, -IQR_plant_500, -field_500, 
                -IQR_field_500, -trees_500, -IQR_trees_500, -flowers_500, -IQR_flowers_500,
                -palm_500, -IQR_palm_500, -green_total, -tree_total, -green_other)

### THIS NO LONGER NEEDED SINCE GSV MEASURES ARE COMING IN REDONE/CORRECTED
###recode other green to include flowers_500
###field_500 + flowers_500 + plant_500
#summary(gsv_mesa$green_other)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.020   0.550   0.860   1.003   1.240  18.020     314 
#gsv_mesa %>% 
#  summarize(n=n(green_other))
#sum(is.na(gsv_mesa$green_other)) # 314 missing from orig coded other_green

#gsv_mesa <- gsv_mesa %>% 
#  dplyr::select(-green_other)
#dim(gsv_mesa) # 6814 | 34

#gsv_mesa <- gsv_mesa %>%
#  mutate(green_other = flowers_500 + field_500 + plant_500)

#dim(gsv_mesa) # 6814 | 35
#sum(is.na(gsv_mesa$green_other))
#summary(gsv_mesa$green_other)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.020   0.560   0.860   1.005   1.250  18.020     314 

##join in new GSV measures (2005-2007 avg'ed)------
gsv20052007_mesa <- inner_join(gsv_2005_2007_analysis_measures_avgd, gsv_mesa, by="idno")
glimpse(gsv20052007_mesa) # 6,594

out_dir <- "/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/"
readr::write_csv(x = gsv20052007_mesa, 
                 file = paste0(out_dir, "gsv20052007_mesa.csv"), 
                 num_threads = 3)

###recode nhood deprivation in reverse order so 1 --> most depr, 3--> least depr
###   for more logical ordering of strata in order of most to least nhood depr
summary(gsv20052007_mesa$F1_PC2)

gsv20052007_mesa <- gsv20052007_mesa %>% 
  mutate(
    n_depr = case_when(
      f1_pc2_3cat == 1 ~ 3,        # flipping the least-depr indicator to 3 (highest NSES)
      f1_pc2_3cat == 2 ~ 2,        # stays same      
      f1_pc2_3cat == 3 ~ 1,        # flipping most-depr indicator to 1 (lowest NSES)
      is.na(f1_pc2_3cat) ~ NA      # if missing, stays missing
    )) 

gsv20052007_mesa %>% 
  dplyr::select(F1_PC2, f1_pc2_3cat, n_depr) %>% 
  head(., 20) # A higher cont value F1_PC2 indicates worse SES
              # so highest tertile F1_PC2 --> n_depr = 1, denotes worse NSES
              # e.g. lowest/neg tertile F1_PC2 --> n_depr = 3, denotes higher NSES

depr_table <- table(gsv20052007_mesa$f1_pc2_3cat,gsv20052007_mesa$n_depr) # check the recode
depr_table # looks good


###set cat vars to factors 
gsv20052007_mesa$race1c <- factor(gsv20052007_mesa$race1c)
gsv20052007_mesa$gender1 <- factor(gsv20052007_mesa$gender1)
gsv20052007_mesa$educ_3cat <- factor(gsv20052007_mesa$educ_3cat)
gsv20052007_mesa$f1_pc2_3cat <- factor(gsv20052007_mesa$f1_pc2_3cat)
gsv20052007_mesa$n_depr <- factor(gsv20052007_mesa$n_depr)
gsv20052007_mesa$site4c <- factor(gsv20052007_mesa$site4c)
gsv20052007_mesa$income1 <- factor(gsv20052007_mesa$income1)

###subset for non-missing race x edu x f1_pc2 
gsv_mesa_noNArace_edu_depr <- gsv20052007_mesa %>% 
  filter(!is.na(race1c)) %>%
  filter(!is.na(educ_3cat)) %>% # subset to non-missing edu & f1_pc2 for race/eth x edu x f1_pc2 strata...
  filter(!is.na(n_depr)) %>% 
  filter(!is.na(green_other_2005_2007)) %>%  # ... non-missing outcomes
  filter(!is.na(income1)) # non-missing income
dim(gsv_mesa_noNArace_edu_depr) # 5246

gsv_mesa_noNArace_edu_depr_sm <- gsv_mesa_noNArace_edu_depr %>% 
  dplyr::select(idno, race1c, educ1, educ_3cat, F1_PC2, f1_pc2_3cat, n_depr,
        age1c, agecat1c, gender1, income1, income_3cat, year, site1c, site4c, 
        green_total_2005_2007, tree_total_2005_2007, grass_2005_2007, green_other_2005_2007)

## check covar missingness------
glimpse(gsv_mesa_noNArace_edu_depr_sm)
gsv_mesa_noNArace_edu_depr_sm %>% 
  summarise_all(~ sum(is.na(.))) # only complete-case


# step 1 intersectional strata & size checks #################
### 1a. create intersectional strata ######################################

## prior dataset name race_edu_f1_tert_strata

race_edu_depr_strata <- gsv_mesa_noNArace_edu_depr_sm %>%
  dplyr::group_by(race1c,educ_3cat,n_depr) %>%  # by race/eth, edu & nhood depr
  dplyr::mutate(strata=cur_group_id())               # 4 x 3 x 3 = 36 strata 
head(race_edu_depr_strata,50)
range(race_edu_depr_strata$strata) # 1-36 as expected for race/eth x edu strata

race_edu_depr_strata %>% 
  count(strata) %>% 
  print(n=36)

# compare the new strata with the flipped depr var to the old
race_edu_depr_strata %>% 
  dplyr::select(strata, race1c, educ_3cat, n_depr, F1_PC2, f1_pc2_3cat) %>% 
  head(., 20)

strata_new_table <- table(race_edu_depr_strata$n_depr,race_edu_depr_strata$strata) # check the recode
strata_new_table

strata_old_table <- table(race_edu_depr_strata$f1_pc2_3cat,race_edu_depr_strata$strata) # check the recode
strata_old_table # looks good

out_dir <- "/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/"
readr::write_csv(x = race_edu_depr_strata, 
                file = paste0(out_dir, "race_edu_depr_strata.csv"), 
                num_threads = 3,) 

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
n.strata.50/total.number.strata* 100 # 83% of strata have >50 individs

n.strata.20 <- sum(n.strata.df$Freq>=20) #num strata with more than 30 individs
n.strata.20
n.strata.20/total.number.strata* 100 # 100% of strata have >20 individs


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
glimpse(race_edu_depr_strata) # 5,246 ppl
model1_race_edu_f1_greentotal <- brm(green_total_2005_2007~1+age1c+gender1+income1+site4c+(1|strata),
                                     data = race_edu_depr_strata,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=3, seed=123)

model1_race_edu_f1_greentotal # 5246 --> 289 drop outs - combo of missing income and GSV measures 
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
model1_race_edu_f1_greentotal_ln <- brm(green_total_2005_2007~1+age1c+gender1+income1+site4c+(1|strata),
                                        data = race_edu_depr_strata,
                                       family = "lognormal",
                                        warmup = 5000,
                                        iter = 10000,
                                       chains=1, seed=123)

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
model1_race_edu_f1_trees <- brm(tree_total_2005_2007~1+age1c+gender1+income1+site4c+(1|strata),
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
model1_race_edu_f1_grass <- brm(grass_2005_2007~1+age1c+gender1+income1+site4c+(1|strata),
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
model1_race_edu_f1_green_other <- brm(green_other_2005_2007~1+age1c+gender1+income1+site4c+(1|strata),
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

model2_race_edu_f1_greentotal <- brm(green_total_2005_2007~1+race1c+educ_3cat+n_depr+age1c+gender1+income1+site4c+(1|strata),
                                     data = race_edu_depr_strata,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=3, seed=123)

model2_race_edu_f1_greentotal

### 2b.ii: % trees only, interactional model -----------------------------------------

model2_race_edu_f1_trees <- brm(tree_total_2005_2007~1+race1c+educ_3cat+n_depr+age1c+gender1+income1+site4c+(1|strata),
                                data = race_edu_depr_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=3, seed=123)

model2_race_edu_f1_trees

### 2b.iii: % grass only, interactional model -----------------------------------------

model2_race_edu_f1_grass <- brm(grass_2005_2007~1+race1c+educ_3cat+n_depr+age1c+gender1+income1+site4c+(1|strata),
                                     data = race_edu_depr_strata,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=3, seed=123)

model2_race_edu_f1_grass

### 2b.iv: % other greenness, interactional model -----------------------------------------

model2_race_edu_f1_green_other <- brm(green_other_2005_2007~1+race1c+educ_3cat+n_depr+age1c+gender1+income1+site4c+(1|strata),
                                data = race_edu_depr_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=3, seed=123)

model2_race_edu_f1_green_other

# step 3: RQ 1: How useful are intersectional strata for predicting baseline greenspace? -----------------------------------------
#   --> Calculate the VPC (in %)
### 3a. % total green space  -----------------------------------------
# Check results
model1_race_edu_f1_greentotal
# Variance at race x edu x depr strata strata level model 1 (sd intercept estimate)^2
2.81 ^2 # 7.8961

# Variance at individual level model 1 (fam specific params sigma)^2
8.17^2 # 66.7489

# Calculate VPC model 1
round(7.8961/ (7.8961 + 66.7489)*100,2) # 10.58%

### 3b. % trees only -----------------------------------------
model1_race_edu_f1_trees

# Variance at race x edu strata strata level model 1 (sd intercept estimate)^2
2.07 ^2 # 4.2849

# Variance at individual level model 1 (fam specific params sigma)^2
6.57^2 # 43.1649

# Calculate VPC model 1
round(4.2849/ (4.2849 + 43.1649)*100,2) # 9.03%

### 3c. % grass only  -----------------------------------------
# Check results
model1_race_edu_f1_grass
# Variance at race x edu strata strata level model 1 (sd intercept estimate)^2
0.81 ^2 # 0.6561

# Variance at individual level model 1 (fam specific params sigma)^2
2.68^2 # 7.1824

# Calculate VPC model 1
round(0.6561/ (0.6561 + 7.1824)*100,2) # 8.37%

### 3d. % other greenness  -----------------------------------------
model1_race_edu_f1_green_other

# Variance at race x edu strata strata level model 1 (sd intercept estimate)^2
0.22 ^2 # 0.0484

# Variance at individual level model 1 (fam specific params sigma)^2
0.75^2 # 0.5625

# Calculate VPC model 1
round(0.0484/ (0.0484 + 0.5625)*100,2) # 7.92%

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


#### 08/03/24: BELOW PORTIONS ARE NOT CURRENTLY INCLUDED IN EJ MANU SO NO UPDATES HAVE BEEN MADE TO THIS SECTION
#### --> INSTEAD PROCEED TO MAIHDA forest plots.R
# step 5: RQ 3: To what extent do interx of social categories contribute incrementally to explaining greenspace inequalities? ---------------
# --> Calculate the VPC (in %) in the interactional model (VPC adjusted)

## race x edu x neighborhood dep ####
### total greenspace ####
model2_race_edu_f1_greentotal
# Variance at race x edu strata strata level model 2 (sd intercept estimate)^2
1.93^2 # 3.7249

# Variance at individual level model 2 (fam specific params sigma)^2
8.42^2 # 70.8964

# Calculate adjusted VPC model 2
round(3.7249 / (3.7249 + 70.8964)*100,2) # 4.99%

# Proportional Change in Variance (PCV) = Assessment of the extent to which between-stratum 
# inequalities are explained by additive vs. interactive/residual effects
# i.e. percentage of between-strata variance that cannot be explained by main effects (in %):
# (Variance at individual level model 2 - Variance at strata strata level model 2)/Variance at individual level model 2 
round(((70.8964-3.7249)/70.8964)*100,2) # 94.75
100 - round(((70.8964-3.7249)/70.8964)*100,2) # 5.25

### trees ####
model2_race_edu_f1_trees
# Variance at race x edu strata strata level model 2 (sd intercept estimate)^2
1.09^2 # 1.1881

# Variance at individual level model 2 (fam specific params sigma)^2
6.78 ^2 # 45.9684

# Calculate adjusted VPC model 2
round(1.1881 / (1.1881 + 45.9684)*100,2) # 2.52%

# Proportional Change in Variance (PCV) = Assessment of the extent to which between-stratum 
# inequalities are explained by additive vs. interactive/residual effects
# i.e. percentage of between-strata variance that cannot be explained by main effects (in %):
round(((45.9684-1.1881)/45.9684)*100,2) # 97.46
100 - round(((45.9684-1.1881)/45.9684)*100,2) # 2.58

### grass ####
model2_race_edu_f1_grass
# Variance at race x edu strata strata level model 2 (sd intercept estimate)^2
0.76^2 # 0.5776

# Variance at individual level model 2 (fam specific params sigma)^2
2.79^2 # 7.7841

# Calculate adjusted VPC model 2
round(0.5776 / (0.5776 + 7.7841)*100,2) # 5.04%

# Proportional Change in Variance (PCV) = Assessment of the extent to which between-stratum 
# inequalities are explained by additive vs. interactive/residual effects
# i.e. percentage of between-strata variance that cannot be explained by main effects (in %):
# (Variance at individual level model 2 - Variance at strata strata level model 2)/Variance at individual level model 2 
round(((7.7841-0.5776)/7.7841)*100,2) # 92.58
100 - round(((7.7841-0.5776)/7.7841)*100,2) # 7.42

### other green ####
model2_race_edu_f1_green_other
# Variance at race x edu strata strata level model 2 (sd intercept estimate)^2
0.12^2 # 0.0144

# Variance at individual level model 2 (fam specific params sigma)^2
0.77 ^2 # 0.5929

# Calculate adjusted VPC model 2
round(0.0144 / (0.0144 + 0.5929)*100,2) # 2.37%

# Proportional Change in Variance (PCV) = Assessment of the extent to which between-stratum 
# inequalities are explained by additive vs. interactive/residual effects
# i.e. percentage of between-strata variance that cannot be explained by main effects (in %):
round(((0.5929-0.0144)/0.5929)*100,2) # 97.57
100 - round(((0.5929-0.0144)/0.5929)*100,2) # 2.43

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

# Change order of variables
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


