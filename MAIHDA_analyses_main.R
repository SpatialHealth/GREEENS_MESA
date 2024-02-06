# GREEENS project MAIHDA models proof-of-concept coding-------------------------
# Author: Tara Jenson
# Created: 11/27/2023
# Last Edited: 12/16/2023

library(dplyr)
library(tidyverse)
library(brm)
library(brms)
library(tidybayes)
library(WriteXLS)


##### Setup/run MAIHDA models to assess relationship and interaction
###### of intersecting social strata on measures of greenspace 

setwd("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data")

# step 0. data import and mgmt------------------------------------------------------------------

###Read in combined GSV-Mesa data file 
gsv_mesa <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/gsv_demo_census_2007.csv")
head(gsv_mesa,20)
dim(gsv_mesa) # 6814

###check for what needs converting to factors
is.factor(gsv_mesa$race1c)
is.factor(gsv_mesa$gender1)
is.factor(gsv_mesa$educ_3cat)
is.factor(gsv_mesa$f1_pc2_3cat)
is.factor(gsv_mesa$site4c)
is.factor(gsv_mesa$site1c)
is.factor(gsv_mesa$income1)
#gsv_mesa$race1c <- factor(gsv_mesa$race1c)
#gsv_mesa$gender1 <- factor(gsv_mesa$gender1)
#gsv_mesa$educ_3cat <- factor(gsv_mesa$educ_3cat)
#gsv_mesa$f1_pc2_3cat <- factor(gsv_mesa$f1_pc2_3cat)
#sv_mesa$site4c <- factor(gsv_mesa$site4c)
#sv_mesa$site1c <- factor(gsv_mesa$site1c)
#gsv_mesa$income1 <- factor(gsv_mesa$income1)
summary(gsv_mesa$race1c)
summary(gsv_mesa$educ_3cat)
summary(gsv_mesa$gender1)
summary(gsv_mesa$site4c)

gsv_mesa$race1c <- factor(gsv_mesa$race1c, levels=c(1,2,3,4),
                      labels=c("White", 
                               "Chinese American",
                               "Black",
                               "Hispanic"))

gsv_mesa$educ_3cat <- factor(gsv_mesa$educ_3cat, levels = c(1,2,3), 
                       labels = c("High School or less", 
                                  "Some college", 
                                  "Bachelor's Degree or higher"))

gsv_mesa$f1_pc2_3cat <- factor(gsv_mesa$f1_pc2_3cat, levels = c(1,2,3), 
                           labels=c("Least deprived neighborhood", 
                                    "Moderately deprived neighborhood",
                                    "Most deprived neighborhood"))


gsv_mesa$gender1 <- factor(gsv_mesa$gender1, levels = c(0,1), 
                       labels = c("Female", "Male"))

gsv_mesa$income1 <- factor(gsv_mesa$income1, levels = 1:13, 
                       labels = c("< $5,000", "$5,000-$7,999", "$8,000-$11,999", "$12,000-$15,999", "$16,000-$19,999", "$20,000-$24,999", "$25,000-$29,999", "$30,000-$34,999", "$35,000-$39,999", "$40,000-$49,000", "$50,000-$74,999", "$75,000-$99,999", "$100,000 +"))

gsv_mesa$site4c <- factor(gsv_mesa$site4c, levels = c(3,4,5,6,7,8), 
                       labels = c("WFU", "COL", "JHU", "UMN", "NWU", "UCLA"))




###subset for non-missing race x edu x f1_pc2 
gsv_mesa_noNAedu_f1pc2 <- gsv_mesa %>% 
  filter(!is.na(educ_3cat)) %>% # subset to non-missing edu & f1_pc2 for race/eth x edu x f1_pc2 strata
  filter(!is.na(f1_pc2_3cat))
dim(gsv_mesa_noNAedu_f1pc2) # 5536

gsv_mesa_noNAedu_f1pc2_sm <- gsv_mesa_noNAedu_f1pc2 %>% select(idno, race1c, educ_3cat, f1_pc2_3cat,
                                                               age1c, gender1, income1, year, green_total, 
                                                               tree_total, green_other, grass_500, site1c, site4c)

# step 1 intersectional strata & size checks #################
### 1a. create intersectional strata ######################################
race_edu_f1_tert_strata <- gsv_mesa_noNAedu_f1pc2_sm %>%
  dplyr::group_by(race1c,educ_3cat,f1_pc2_3cat) %>%  # by race/eth, edu & f1
  dplyr::mutate(strata=cur_group_id())               # 4 x 3 x 3 = 36 strata 
head(race_edu_f1_tert_strata,50)
range(race_edu_f1_tert_strata$strata) # 1-36 as expected for race/eth x edu strata

out_dir <- "/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/"

readr::write_csv(x = race_edu_f1_tert_strata, 
                 file = paste0(out_dir, "race_edu_f1_tert_strata.csv"), 
                 num_threads = 3, na=".") # adding option param to change NA to.

### 1b. Check that sample sizes are sufficiently large #########################
total.number.strata <- 36 # num strata we have for race x edu x f1
n.strata <- table(race_edu_f1_tert_strata$strata) # store sample sizes of strata in an object
n.strata
n.strata.df <- as.data.frame(t(n.strata)) # convert to df
n.strata.df
n.strata.30 <- sum(n.strata.df$Freq>=30) #num strata with more than 30 individs
n.strata.30
n.strata.30/total.number.strata* 100 # 97% of strata have >30 individs

# step 2. MLM by strata: race x edu x n'hood depriv-----------------------------------------
### Perform multilevel analysis to partition the variance between 
###     and within intersectional strata                       
###     --> include age, sex, income, site as covars

### 2a.i: % total greenness, Simple intersectional  -----------------------------------------
### Bayesian MLM for simple intersectional model 
model1_race_edu_f1_greentotal <- brm(green_total~1+age1c+gender1+income1+site4c+(1|strata),
                                     data = race_edu_f1_tert_strata,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=1, seed=123)

model1_race_edu_f1_greentotal


# Check plots
plot(model1_race_edu_f1_greentotal, variable = "^b", regex = TRUE)

# Check Gelman-Rubin convergence diagnostic (Rhat value should be lower than 1.1/1.05 for good convergence)
model1_race_edu_f1_greentotal.rhats <- round(as.numeric(model1_race_edu_f1_greentotal$rhats), 2)
model1_race_edu_f1_greentotal.rhats

any(model1_race_edu_f1_greentotal.rhats > 1.1) # convergence good
any(model1_race_edu_f1_greentotal.rhats > 1.05) # convergence good

### 2a.ii: % trees only, Simple intersectional  -----------------------------------------
model1_race_edu_f1_trees <- brm(tree_total~1+age1c+gender1+income1+site4c+(1|strata),
                                data = race_edu_f1_tert_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=1, seed=123)

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
                                data = race_edu_f1_tert_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=1, seed=123)

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
                                data = race_edu_f1_tert_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=1, seed=123)

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

model2_race_edu_f1_greentotal <- brm(green_total~1+race1c+educ_3cat+f1_pc2_3cat+age1c+gender1+income1+site4c+(1|strata),
                                     data = race_edu_f1_tert_strata,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=1, seed=123)

model2_race_edu_f1_greentotal

### 2b.ii: % trees only, interactional model -----------------------------------------

model2_race_edu_f1_trees <- brm(tree_total~1+race1c+educ_3cat+f1_pc2_3cat+age1c+gender1+income1+site4c+(1|strata),
                                data = race_edu_f1_tert_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=1, seed=123)

model2_race_edu_f1_trees

### 2b.iii: % grass only, interactional model -----------------------------------------

model2_race_edu_f1_grass <- brm(grass_500~1+race1c+educ_3cat+f1_pc2_3cat+age1c+gender1+income1+site4c+(1|strata),
                                     data = race_edu_f1_tert_strata,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=1, seed=123)

model2_race_edu_f1_grass

### 2b.iv: % other greenness, interactional model -----------------------------------------

model2_race_edu_f1_green_other <- brm(green_other~1+race1c+educ_3cat+f1_pc2_3cat+age1c+gender1+income1+site4c+(1|strata),
                                data = race_edu_f1_tert_strata,
                                warmup = 5000,
                                iter = 10000,
                                chains=1, seed=123)

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

### 4b. % trees only -------
pred.means.model1_race_edu_f1_trees <- model1_race_edu_f1_trees %>% 
  epred_draws(race_edu_f1_tert_strata) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_trees)

### 4c. % grass only -------
pred.means.model1_race_edu_f1_grass <- model1_race_edu_f1_grass %>% 
  epred_draws(race_edu_f1_tert_strata) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_grass)

### 4d. % tother greenness -------
pred.means.model1_race_edu_f1_green_other <- model1_race_edu_f1_green_other %>% 
  epred_draws(race_edu_f1_tert_strata) %>% 
  group_by(strata) %>% 
  mean_qi(.epred) # 
View(pred.means.model1_race_edu_f1_green_other)


