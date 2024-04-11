# GREEENS project MAIHDA models coding-------------------------
# Author: Tara Jenson
# Created: 11/27/2023
# Last Edited: 12/16/2023

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

###scope original long gsv values
gsv_long <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/datagsv_long.csv")
head(gsv_long[c("idno","year","grass_500", "trees_500")],150) %>% 
  filter(year >= 2007 & year<=2009)



###Read in combined GSV-Mesa data file 
gsv_mesa <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/gsv_demo_census_2007.csv")
head(gsv_mesa,20)
dim(gsv_mesa) # 6814 | 35
glimpse(gsv_mesa)


###recode other green to include flowers_500
###field_500 + flowers_500 + plant_500
summary(gsv_mesa$green_other)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.020   0.550   0.860   1.003   1.240  18.020     314 
#gsv_mesa %>% 
#  summarize(n=n(green_other))
sum(is.na(gsv_mesa$green_other)) # 314 missing from orig coded other_green

gsv_mesa <- gsv_mesa %>% 
  dplyr::select(-green_other)
dim(gsv_mesa) # 6814 | 34

gsv_mesa <- gsv_mesa %>%
  mutate(green_other = flowers_500 + field_500 + plant_500)

dim(gsv_mesa) # 6814 | 35
sum(is.na(gsv_mesa$green_other))
summary(gsv_mesa$green_other)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.020   0.560   0.860   1.005   1.250  18.020     314 

###recode nhood deprivation in reverse order so 1 --> most depr, 3--> least depr
###   for more logical ordering of strata in order of most to least nhood depr
summary(gsv_mesa$F1_PC2)

gsv_mesa <- gsv_mesa %>% 
  mutate(
    n_depr = case_when(
      f1_pc2_3cat == 1 ~ 3,        # flipping the least-depr indicator to 3 
      f1_pc2_3cat == 2 ~ 2,        # stays same      
      f1_pc2_3cat == 3 ~ 1,        # flipping most-depr indicator to 1
      is.na(f1_pc2_3cat) ~ NA      # if missing, stays missing
    )
  )

gsv_mesa %>% 
  dplyr::select(F1_PC2, f1_pc2_3cat, n_depr) %>% 
  head(., 20)

depr_table <- table(gsv_mesa$f1_pc2_3cat,gsv_mesa$n_depr) # check the recode
depr_table # looks good


###set cat vars to factors 
gsv_mesa$race1c <- factor(gsv_mesa$race1c)
gsv_mesa$gender1 <- factor(gsv_mesa$gender1)
gsv_mesa$educ_3cat <- factor(gsv_mesa$educ_3cat)
gsv_mesa$f1_pc2_3cat <- factor(gsv_mesa$f1_pc2_3cat)
gsv_mesa$n_depr <- factor(gsv_mesa$n_depr)
gsv_mesa$site4c <- factor(gsv_mesa$site4c)
gsv_mesa$income1 <- factor(gsv_mesa$income1)

###subset for non-missing race x edu x f1_pc2 
gsv_mesa_noNArace_edu_depr <- gsv_mesa %>% 
  filter(!is.na(race1c)) %>%
  filter(!is.na(educ_3cat)) %>% # subset to non-missing edu & f1_pc2 for race/eth x edu x f1_pc2 strata
  filter(!is.na(n_depr))
dim(gsv_mesa_noNArace_edu_depr) # 5536

gsv_mesa_noNArace_edu_depr_sm <- gsv_mesa_noNArace_edu_depr %>% 
  dplyr::select(idno, race1c, educ1, educ_3cat, F1_PC2, f1_pc2_3cat, n_depr,
        age1c, agecat1c, gender1, income1, income_3cat, year, site1c, site4c, 
        green_total, tree_total, green_other, grass_500)




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
n.strata.50/total.number.strata* 100 # 86% of strata have >50 individs

n.strata.20 <- sum(n.strata.df$Freq>=20) #num strata with more than 30 individs
n.strata.20
n.strata.20/total.number.strata* 100 # 86% of strata have >50 individs

# step 2. MLM by strata: race x edu x n'hood depriv-----------------------------------------
### Perform multilevel analysis to partition the variance between 
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
model1_race_edu_f1_greentotal <- brm(green_total~1+age1c+gender1+income1+site4c+(1|strata),
                                     data = race_edu_depr_strata,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=3, seed=123)

model1_race_edu_f1_greentotal

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
model1_race_edu_f1_greentotal_ln <- brm(green_total~1+age1c+gender1+income1+site4c+(1|strata),
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
model1_race_edu_f1_trees <- brm(tree_total~1+age1c+gender1+income1+site4c+(1|strata),
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
model1_race_edu_f1_grass <- brm(grass_500~1+age1c+gender1+income1+site4c+(1|strata),
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
model1_race_edu_f1_green_other <- brm(green_other~1+age1c+gender1+income1+site4c+(1|strata),
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

model2_race_edu_f1_greentotal <- brm(green_total~1+race1c+educ_3cat+n_depr+age1c+gender1+income1+site4c+(1|strata),
                                     data = race_edu_depr_strata,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=3, seed=123)

model2_race_edu_f1_greentotal

### 2b.ii: % trees only, interactional model -----------------------------------------

model2_race_edu_f1_trees <- brm(tree_total~1+race1c+educ_3cat+n_depr+age1c+gender1+income1+site4c+(1|strata),
                                data = race_edu_depr_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=3, seed=123)

model2_race_edu_f1_trees

### 2b.iii: % grass only, interactional model -----------------------------------------

model2_race_edu_f1_grass <- brm(grass_500~1+race1c+educ_3cat+n_depr+age1c+gender1+income1+site4c+(1|strata),
                                     data = race_edu_depr_strata,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=3, seed=123)

model2_race_edu_f1_grass

### 2b.iv: % other greenness, interactional model -----------------------------------------

model2_race_edu_f1_green_other <- brm(green_other~1+race1c+educ_3cat+n_depr+age1c+gender1+income1+site4c+(1|strata),
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
# Variance at race x edu strata strata level model 1 (sd intercept estimate)^2
2.80 ^2 # 7.84

# Variance at individual level model 1 (fam specific params sigma)^2
8.42^2 # 70.8964

# Calculate VPC model 1
round(7.84/ (7.84 + 70.8964)*100,2) # 9.96%

### 3b. % trees only -----------------------------------------
model1_race_edu_f1_trees

# Variance at race x edu strata strata level model 1 (sd intercept estimate)^2
2.05 ^2 # 4.2025

# Variance at individual level model 1 (fam specific params sigma)^2
6.78^2 # 45.9684

# Calculate VPC model 1
round(4.2025/ (4.2025 + 45.9684)*100,2) # 8.38%

### 3c. % grass only  -----------------------------------------
# Check results
model1_race_edu_f1_grass
# Variance at race x edu strata strata level model 1 (sd intercept estimate)^2
0.79 ^2 # 0.6241

# Variance at individual level model 1 (fam specific params sigma)^2
2.79^2 # 7.7841

# Calculate VPC model 1
round(0.6241/ (0.6241 + 7.7841)*100,2) # 7.42%

### 3d. % other greenness  -----------------------------------------
model1_race_edu_f1_green_other

# Variance at race x edu strata strata level model 1 (sd intercept estimate)^2
0.22 ^2 # 0.0484

# Variance at individual level model 1 (fam specific params sigma)^2
0.77^2 # 0.5929

# Calculate VPC model 1
round(0.0484/ (0.0484 + 0.5929)*100,2) # 7.55%

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

### 4d. % tother greenness -------
pred.means.model1_race_edu_f1_green_other <- model1_race_edu_f1_green_other %>% 
  epred_draws(race_edu_depr_strata) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_green_other)



#
#
#
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


