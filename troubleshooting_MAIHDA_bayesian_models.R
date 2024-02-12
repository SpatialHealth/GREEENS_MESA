# GREEENS project: MAIHDA analyses - troubleshooting Bayseian models -------------------------
# Author: Tara Jenson
# Created: 2/8/2024
# Last Edited: 

library(dplyr)
library(tidyverse)
library(brm)
library(brms)
library(tidybayes)

# Import datafile used for brm(s) model(s) --------
maindata <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/race_edu_f1_tert_strata.csv")
head(maindata,20)
dim(maindata) 


# Assess distrib of vars of interest --------
hist(maindata$green_total) # normalish, right skewed
hist(maindata$tree_total)  # normalish, right skewed
hist(maindata$grass_500) # not normal 
hist(maindata$green_other) 

ggplot(maindata, aes(x=green_total)) +
  geom_density(fill="darkgreen")
ggplot(maindata, aes(x=tree_total)) +
  geom_density(fill="forestgreen")
ggplot(maindata, aes(x=grass_500)) +
  geom_density(fill="green")
ggplot(maindata, aes(x=green_other)) +
  geom_density(fill="lightgreen")

qqnorm(maindata$green_total, main='Normal') 
qqline(maindata$green_total)

qqnorm(maindata$tree_total, main='Normal')
qqline(maindata$tree_total)

qqnorm(maindata$grass_500, main='Normal')
qqline(maindata$grass_500) # 

qqnorm(maindata$green_other, main='Normal')
qqline(maindata$green_other) # not normal

greentotal <-array(maindata$green_total)
greentotal_sample <- sample(greentotal, 5000, replace = TRUE, prob = NULL)
dim(greentotal_sample)
hist(greentotal_sample)
# shapiro-wilkes test of normality (p-value < 0.05 denotes non normal, p-value > 0.05 denotes normal)
shapiro.test(greentotal_sample) # not-normal

# ok let's do some transforms of the outcome & check distribs ----
head(maindata)
maindata <- maindata %>% 
  mutate(green_total_sqrd =green_total^2) %>% 
  mutate(green_total_sqrt = sqrt(green_total)) %>% 
  mutate(greent_total_log = log(green_total))

head(maindata,20)

hist(maindata$green_total) 
hist(maindata$green_total_sqrd) # poisson distrib
hist(maindata$green_total_sqrt) # more normalish but still humps
hist(maindata$greent_total_log) # more normalish with a left skew

summary(maindata$green_total_sqrt) # range from 1.17 to 8.33
maindata <- maindata %>% 
  mutate(green_total_sqrt_x10_int =round(green_total_sqrt*10,0)) 

# oooh normal scores transformation...
# https://rcompanion.org/handbook/I_13.html
library(rcompanion)
plotNormalHistogram(maindata$green_total)
qqnorm(maindata$green_total, main='Normal') 
qqline(maindata$green_total)

maindata$green_totalNST = blom(maindata$green_total)

plotNormalHistogram(maindata$green_totalNST) # hey wow!
qqnorm(maindata$green_totalNST, main='Normal') 
qqline(maindata$green_totalNST)

# hell yeah let's try this transform in the model below!

# do some plots to assess linearity of individual strata vars race1c, educ_3cat, age1c, F1_PC2, income1 with greenness
b <- ggplot(mtcars, aes(x = wt, y = mpg))
inc_green <- ggplot(maindata, aes(x = income1, y=green_total)) +
  geom_point() +
  geom_smooth()

depr_green <- ggplot(maindata, aes(x = F1_PC2, y=green_total)) +
  geom_point() +
  geom_smooth() # whoa that is not linear

depr_green_lm <- ggplot(maindata, aes(x = F1_PC2, y=green_total)) +
  geom_point() +
  geom_smooth(method=lm) 

# try brm using some non-normal distribs & using transformed outcomes -------
# https://discourse.mc-stan.org/t/gaussian-vs-skew-normal-model-selection/8947
## set as normal gaussian distrib
model1_maindata_greentotal <- brm(green_total~1+age1c+gender1+income1+site4c+(1|strata),
                                  data = maindata, # gaussian is default fam
                                  warmup = 5000,
                                  iter = 10000,
                                  chains=1, seed=123)

model1_maindata_greentotal

## using the normal scores transformed version of greentotal generated above
model1_maindata_greentotalNST <- brm(green_totalNST~1+age1c+gender1+income1+site4c+(1|strata),
                                  data = maindata, # gaussian is default fam
                                  warmup = 5000,
                                  iter = 10000,
                                  chains=1, seed=123)

model1_maindata_greentotal

# trying out skew-normal distrib 
model1_maindata_greentotal_sn <- brm(green_total~1+age1c+gender1+income1+site4c+(1|strata),
                                     data = maindata,
                                     family = "skew_normal",
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=1, seed=123)

model1_maindata_greentotal_sn # 

# try binomial distrib with sqrt outcome

model1_maindata_greentotal_sqrt <- brm(green_total_sqrt_x10_int~1+age1c+gender1+income1+site4c+(1|strata),
                                     data = maindata,
                                     family = "binomial",
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=1, seed=123)

model1_maindata_greentotal_sqrt

# try poisson distrib with squared outcome
summary(maindata$green_total_sqrd)
maindata <- maindata %>% 
  mutate(green_total_sqrd_int =round(green_total_sqrd,0)) # poisson requires integer outcome
model1_maindata_greentotal_sqrd <- brm(green_total_sqrd_int~1+age1c+gender1+income1+site4c+(1|strata),
                                     data = maindata,
                                     family = "poisson",
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=1, seed=123)

model1_maindata_greentotal_sqrd

# run the Posterior (or prior) predictive checks using pp_check 
pp_check(model1_maindata_greentotal)
pp_check(model1_maindata_greentotal_sn) # not really any diff

# List of params & classes priors can be specified for -------------------------
## To get a full list of parameters and parameter classes for which priors
##    can be specified (depending on the model) use function get_prior.
## Example from https://paul-buerkner.github.io/brms/reference/get_prior.html
(prior <- get_prior(count ~ zAge + zBase * Trt + (1|patient) + (1|obs),
                    data = epilepsy, family = poisson()))

# using my simple random effect total greenness model
maihda_priors <- get_prior(green_total~1+age1c+gender1+income1+site4c+(1|strata),
                           data = race_edu_f1_tert_strata ) #using default distrib just to see what's what

maihda_priors

model1_race_edu_f1_greentotal <- brm(green_total~1+age1c+gender1+income1+site4c+(1|strata),
                                     data = race_edu_f1_tert_strata,
                                     warmup = 5000,
                                     iter = 10000,
                                     chains=1, seed=123)


