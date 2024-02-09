library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(jtools)
library(forcats)
library(survminer)
library(forestplot)
library(ggh4x) # extends ggplot 

#0. Import results table(s) ------------------------------------------------------------------

predicted_greentotal_results <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedgreentotalforplots.csv")
head(predicted_greentotal_results)
dim(predicted_greentotal_results) 
view(predicted_greentotal_results) # some trailing rows

predicted_treesonly_results <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedtreesonlyforplots.csv")
head(predicted_treesonly_results)
dim(predicted_treesonly_results) 
view(predicted_treesonly_results) # some trailing columns

predicted_grassonly_results <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedgrassonlyforplots.csv")
head(predicted_grassonly_results)
dim(predicted_grassonly_results) # some trailing columns
view(predicted_grassonly_results)

predicted_greenother_results <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedgreenotherforplots.csv")
head(predicted_greenother_results)
dim(predicted_greenother_results) # some trailing columns
view(predicted_greenother_results)

#1. Clean up and set var as factors/labels as needed  ------------------------------------------------------------------

#get rid of NAs rows that ended up in there
predicted_greentotal_results <- predicted_greentotal_results %>%
  mutate %>% 
  filter(!is.na(race_num))
head(predicted_greentotal_results)
dim(predicted_greentotal_results) 

predicted_treesonly_results <- predicted_treesonly_results %>%
  mutate %>% 
  filter(!is.na(race_num)) %>% 
  select(-X, -X.1, -X.2)
head(predicted_treesonly_results)
dim(predicted_treesonly_results) 

predicted_grassonly_results <- predicted_grassonly_results %>%
  mutate %>% 
  filter(!is.na(race_num))  %>% 
  select(-X, -X.1, -X.2)
head(predicted_grassonly_results)
dim(predicted_grassonly_results) 

predicted_greenother_results <- predicted_greenother_results %>%
  mutate %>% 
  filter(!is.na(race_num)) %>% 
  select(-X, -X.1, -X.2)
head(predicted_greenother_results)
dim(predicted_greenother_results) 

###set cat vars to factors and assign labels for levels 
predicted_greentotal_results$race_num <- factor(predicted_greentotal_results$race_num, levels=c(1,2,3,4),
                                           labels=c("White", 
                                                    "Chinese American",
                                                    "Black",
                                                    "Hispanic"))

predicted_greentotal_results$edu_num <- factor(predicted_greentotal_results$edu_num, levels = c(1,2,3), 
                                              labels = c("High School \nor less", #reference
                                                         "Some \ncollege", 
                                                         "Bachelor's \nor higher"))

predicted_greentotal_results$depr_num <- factor(predicted_greentotal_results$depr_num, levels = c(1,2,3), 
                                                labels=c("Least", 
                                                         "Moderate",
                                                         "Most"))  
head(predicted_greentotal_results)

predicted_treesonly_results$race_num <- factor(predicted_treesonly_results$race_num, levels=c(1,2,3,4),
                                                labels=c("White", 
                                                         "Chinese American",
                                                         "Black",
                                                         "Hispanic"))

predicted_treesonly_results$edu_num <- factor(predicted_treesonly_results$edu_num, levels = c(1,2,3), 
                                               labels = c("High School \nor less", #reference
                                                          "Some \ncollege", 
                                                          "Bachelor's \nor higher"))

predicted_treesonly_results$depr_num <- factor(predicted_treesonly_results$depr_num, levels = c(1,2,3), 
                                                labels=c("Least", 
                                                         "Moderate",
                                                         "Most"))  
head(predicted_treesonly_results)

predicted_grassonly_results$race_num <- factor(predicted_grassonly_results$race_num, levels=c(1,2,3,4),
                                               labels=c("White", 
                                                        "Chinese American",
                                                        "Black",
                                                        "Hispanic"))

predicted_grassonly_results$edu_num <- factor(predicted_grassonly_results$edu_num, levels = c(1,2,3), 
                                              labels = c("High School \nor less", #reference
                                                         "Some \ncollege", 
                                                         "Bachelor's \nor higher"))

predicted_grassonly_results$depr_num <- factor(predicted_grassonly_results$depr_num, levels = c(1,2,3), 
                                               labels=c("Least", 
                                                        "Moderate",
                                                        "Most"))  
head(predicted_grassonly_results)

predicted_greenother_results$race_num <- factor(predicted_greenother_results$race_num, levels=c(1,2,3,4),
                                               labels=c("White", 
                                                        "Chinese American",
                                                        "Black",
                                                        "Hispanic"))

predicted_greenother_results$edu_num <- factor(predicted_greenother_results$edu_num, levels = c(1,2,3), 
                                              labels = c("High School \nor less", #reference
                                                         "Some \ncollege", 
                                                         "Bachelor's \nor higher"))

predicted_greenother_results$depr_num <- factor(predicted_greenother_results$depr_num, levels = c(1,2,3), 
                                               labels=c("Least", 
                                                        "Moderate",
                                                        "Most"))  
head(predicted_greenother_results)


#2.  nested facet forest plots------------------------------------------------------------------
##Outcome: total greenspace-------------------------------
#Strata: Race/ethnicity x education x neighborhood
###edu level nested in race/eth at top, depriv at bottom--------

#colnames (table_plot_carrier) <-c("Model", "Estimate", "LowerLevel", "UpperLevel", "result", "ApoE4", "Outcome", "Status")
colnames (predicted_greentotal_results) <-c("Strata", "RaceEthnicity", "EducationLevel", "Deprivation", "Estimate", "LowerLevel", "UpperLevel")
colnames (predicted_treesonly_results) <-c("Strata", "RaceEthnicity", "EducationLevel", "Deprivation", "Estimate", "LowerLevel", "UpperLevel")
colnames (predicted_grassonly_results) <-c("Strata", "RaceEthnicity", "EducationLevel", "Deprivation", "Estimate", "LowerLevel", "UpperLevel")
colnames (predicted_greenother_results) <-c("Strata", "RaceEthnicity", "EducationLevel", "Deprivation", "Estimate", "LowerLevel", "UpperLevel")

head(predicted_greentotal_results)

test_plot1 <- ggplot(data=predicted_greentotal_results, aes(x = Strata, y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel)) + geom_errorbar(aes (color = EducationLevel)) 
test_plot1 # works but not there yet

test_plot2 <- ggplot(data=predicted_greentotal_results, aes(x = Strata, y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel)) + geom_errorbar(aes (color = EducationLevel)) + 
  facet_nested(~ RaceEthnicity + EducationLevel)
test_plot2 # this gets me the nested facets, now need to sort what the x should be instead of strata

test_plot3 <- ggplot(data=predicted_greentotal_results, aes(x = Depr, y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel)) + geom_errorbar(aes (color = EducationLevel)) + 
  facet_nested(~ RaceEthnicity + EducationLevel)
test_plot3 # this gets me what I want, but need to change angles of x labels and drop the legend

test_plot4 <- ggplot(data=predicted_greentotal_results, aes(x = Deprivation, y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel)) + geom_errorbar(aes (color = EducationLevel)) + 
  facet_nested(~ RaceEthnicity + EducationLevel) + theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")
  test_plot4
  
test_plot5 <- ggplot(data=predicted_greentotal_results, aes(x = Deprivation, y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel), size=0.1) + geom_errorbar(aes (color = EducationLevel)) + 
  scale_color_manual(values=c("darkred", "darkseagreen4", "steelblue4"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust=1))+ # no legend, sets x-axis text size and angle
  theme(strip.background = element_rect(colour = "black", linewidth = 1)) + # creates rectangle around nested var labels
  labs(title = "Figure 3. Predicted mean percent total greenness with 95% credible intervals across intersectional strata \nin the simple intersectional model", 
       x = "Neighborhood Deprivation", y = "% Total Street View Greenness") 
  
test_plot5 # this is the current best plot

## Figure 3 a, b, c & d -------
greentotal <- ggplot(data=predicted_greentotal_results, aes((reorder(x = Deprivation, desc(Deprivation))), y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel), size=0.1) + geom_errorbar(aes (color = EducationLevel)) + 
  scale_color_manual(values=c("darkred", "darkseagreen4", "steelblue4"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust=1))+ # no legend, sets x-axis text size and angle
  theme(strip.background = element_rect(colour = "black", linewidth = 1)) + # creates rectangle around nested var labels
  labs(title = "Figure 3. Predicted mean percent street-view of measures of greenness with 95% credible intervals across
  intersectional strata in the simple intersectional model.", 
       x = "Neighborhood Deprivation", y = "% Total Street View Greenness") 

greentotal 

treesonly <- ggplot(data=predicted_treesonly_results, aes((reorder(x = Deprivation, desc(Deprivation))), y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel), size=0.1) + geom_errorbar(aes (color = EducationLevel)) + 
  scale_color_manual(values=c("darkred", "darkseagreen4", "steelblue4"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust=1))+ # no legend, sets x-axis text size and angle
  theme(strip.background = element_rect(colour = "black", linewidth = 1)) + # creates rectangle around nested var labels
  labs(title = "B.", 
       x = "Neighborhood Deprivation", y = "% Street View Trees") 

treesonly 

grassonly <- ggplot(data=predicted_grassonly_results, aes((reorder(x = Deprivation, desc(Deprivation))), y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel), size=0.1) + geom_errorbar(aes (color = EducationLevel)) + 
  scale_color_manual(values=c("darkred", "darkseagreen4", "steelblue4"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust=1))+ # no legend, sets x-axis text size and angle
  theme(strip.background = element_rect(colour = "black", linewidth = 1)) + # creates rectangle around nested var labels
  labs(title = "C.", 
       x = "Neighborhood Deprivation", y = "% Street View Grass") 

grassonly 

greenother <- ggplot(data=predicted_greenother_results, aes((reorder(x = Deprivation, desc(Deprivation))), y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel), size=0.1) + geom_errorbar(aes (color = EducationLevel)) + 
  scale_color_manual(values=c("darkred", "darkseagreen4", "steelblue4"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust=1))+ # no legend, sets x-axis text size and angle
  theme(strip.background = element_rect(colour = "black", linewidth = 1)) + # creates rectangle around nested var labels
  labs(title = "D.", 
       x = "Neighborhood Deprivation", y = "% Street View Other Greennes") 

greenother
