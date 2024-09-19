# GREEENS project MAIHDA forest plots coding-------------------------
# Author: Tara Jenson
# Created: 11/27/2023
# Last Edited: 08/10/2024

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(jtools)
library(forcats)
library(survminer)
library(forestplot)
library(ggh4x) # extends ggplot 
library(latex2exp)
library(ggtext)

#0. Import results table(s) ------------------------------------------------------------------

predicted_greentotal_results <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedgreentotalforplots_redo_exam1.csv")
head(predicted_greentotal_results)
dim(predicted_greentotal_results) 
view(predicted_greentotal_results) 

predicted_greentotal_results_ln <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedgreentotalforplots_nl_redo_exam1.csv")
head(predicted_greentotal_results_ln)
dim(predicted_greentotal_results_ln) 
view(predicted_greentotal_results_ln) 

predicted_treesonly_results <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedtreesonlyforplots_redo_exam1.csv")
head(predicted_treesonly_results)
dim(predicted_treesonly_results) 
view(predicted_treesonly_results) 

predicted_grassonly_results <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedgrassonlyforplots_redo_exam1.csv")
head(predicted_grassonly_results)
dim(predicted_grassonly_results) 
view(predicted_grassonly_results)

predicted_greenother_results <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedgreenotherforplots_redo_exam1.csv")
head(predicted_greenother_results)
dim(predicted_greenother_results) 
view(predicted_greenother_results)

# stratified results
predicted_trees_lowdens_results <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedtreesforplots_lowdensity_redo.csv")
head(predicted_trees_lowdens_results)
dim(predicted_trees_lowdens_results) 
view(predicted_trees_lowdens_results) 

predicted_trees_highdens_results <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedtreesforplots_highdensity_redo.csv")
head(predicted_trees_highdens_results)
dim(predicted_trees_highdens_results) 
view(predicted_trees_highdens_results) 

predicted_grass_lowdens_results <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedgrassforplots_lowdensity_redo.csv")
head(predicted_grass_lowdens_results)
dim(predicted_grass_lowdens_results) 
view(predicted_grass_lowdens_results) 

predicted_grass_highdens_results <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedgrassforplots_highdensity_redo.csv")
head(predicted_grass_highdens_results)
dim(predicted_grass_highdens_results) 
view(predicted_grass_highdens_results) 


#1. Set var as factors/labels as needed  ------------------------------------------------------------------



###set cat vars to factors and assign labels for levels 
predicted_greentotal_results$race_num <- factor(predicted_greentotal_results$race_num, levels=c(3,2,4,1),
                                           labels=c("Black", 
                                                    "Chinese American",
                                                    "Hispanic",
                                                    "White"))

predicted_greentotal_results$edu_num <- factor(predicted_greentotal_results$edu_num, levels = c(1,2,3), 
                                              labels = c(" High School \nor less", #reference
                                                         "Some \ncollege", 
                                                         "Bachelor's \nor higher"))

predicted_greentotal_results$depr_num <- factor(predicted_greentotal_results$depr_num, levels = c(3,2,1), 
                                                labels=c("High", "Mod", "Low"))  # changing to NSES verbiage: 3 = high NSES, 2 = mod NSES, 1 = low NSES
# A higher cont value F1_PC2 indicates worse SES
# so highest tertile F1_PC2 --> n_depr = 1, denotes worse NSES
# e.g. lowest/neg tertile F1_PC2 --> n_depr = 3, denotes higher NSES
predicted_greentotal_results # checking compared with .csv file numbers looks good
head(predicted_greentotal_results,40) 

predicted_greentotal_results_ln$race_num <- factor(predicted_greentotal_results_ln$race_num, levels=c(3,2,4,1),
                                                labels=c("Black", 
                                                         "Chinese American",
                                                         "Hispanic",
                                                         "White"))

predicted_greentotal_results_ln$edu_num <- factor(predicted_greentotal_results_ln$edu_num, levels = c(1,2,3), 
                                               labels = c(" High School \nor less", #reference
                                                          "Some \ncollege", 
                                                          "Bachelor's \nor higher"))

predicted_greentotal_results_ln$depr_num <- factor(predicted_greentotal_results_ln$depr_num, levels = c(3,2,1), 
                                                   labels=c("High", "Mod", "Low"))  # changing to NSES verbiage: least = high, mod = mod, most = low  
head(predicted_greentotal_results_ln)

predicted_treesonly_results$race_num <- factor(predicted_treesonly_results$race_num, levels=c(3,2,4,1),
                                               labels=c("Black", 
                                                        "Chinese American",
                                                        "Hispanic",
                                                        "White"))

predicted_treesonly_results$edu_num <- factor(predicted_treesonly_results$edu_num, levels = c(1,2,3), 
                                               labels = c(" High School \nor less", #reference
                                                          "Some \ncollege", 
                                                          "Bachelor's \nor higher"))

predicted_treesonly_results$depr_num <- factor(predicted_treesonly_results$depr_num, levels = c(3,2,1), 
                                               labels=c("High", "Mod", "Low"))  # changing to NSES verbiage: least = high, mod = mod, most = low 
head(predicted_treesonly_results)

predicted_grassonly_results$race_num <- factor(predicted_grassonly_results$race_num, levels=c(3,2,4,1),
                                               labels=c("Black", 
                                                        "Chinese American",
                                                        "Hispanic",
                                                        "White"))

predicted_grassonly_results$edu_num <- factor(predicted_grassonly_results$edu_num, levels = c(1,2,3), 
                                              labels = c(" High School \nor less", #reference
                                                         "Some \ncollege", 
                                                         "Bachelor's \nor higher"))

predicted_grassonly_results$depr_num <- factor(predicted_grassonly_results$depr_num, levels = c(3,2,1), 
                                               labels=c("High", "Mod", "Low"))  # changing to NSES verbiage: least = high, mod = mod, most = low  
head(predicted_grassonly_results)

predicted_greenother_results$race_num <- factor(predicted_greenother_results$race_num, levels=c(3,2,4,1),
                                                labels=c("Black", 
                                                         "Chinese American",
                                                         "Hispanic",
                                                         "White"))

predicted_greenother_results$edu_num <- factor(predicted_greenother_results$edu_num, levels = c(1,2,3), 
                                              labels = c(" High School \nor less", #reference
                                                         "Some \ncollege", 
                                                         "Bachelor's \nor higher"))

predicted_greenother_results$depr_num <- factor(predicted_greenother_results$depr_num, levels = c(3,2,1), 
                                                labels=c("High", "Mod", "Low"))  # changing to NSES verbiage: least = high, mod = mod, most = low  
head(predicted_greenother_results)

# to save space just swap out for low & high density for each outcome
predicted_trees_lowdens_results$race_num <- factor(predicted_trees_lowdens_results$race_num, levels=c(3,2,4,1),
                                              labels=c("Black", 
                                                       "Chinese American",
                                                       "Hispanic",
                                                       "White"))

predicted_trees_lowdens_results$edu_num <- factor(predicted_trees_lowdens_results$edu_num, levels = c(1,2,3), 
                                               labels = c(" High School \nor less", #reference
                                                          "Some \ncollege", 
                                                          "Bachelor's \nor higher"))

predicted_trees_lowdens_results$depr_num <- factor(predicted_trees_lowdens_results$depr_num, levels = c(3,2,1), 
                                                        labels=c("High", "Mod", "Low"))  # changing to NSES verbiage: least = high, mod = mod, most = low 
head(predicted_trees_lowdens_results)


predicted_grass_highdens_results$race_num <- factor(predicted_grass_highdens_results$race_num, levels=c(3,2,4,1),
                                                    labels=c("Black", 
                                                             "Chinese American",
                                                             "Hispanic",
                                                             "White"))

predicted_grass_highdens_results$edu_num <- factor(predicted_grass_highdens_results$edu_num, levels = c(1,2,3), 
                                                   labels = c(" High School \nor less", #reference
                                                              "Some \ncollege", 
                                                              "Bachelor's \nor higher"))

predicted_grass_highdens_results$depr_num <- factor(predicted_grass_highdens_results$depr_num, levels = c(3,2,1), 
                                                    labels=c("High", "Mod", "Low"))  # changing to NSES verbiage: least = high, mod = mod, most = low 
head(predicted_grass_highdens_results)


#2.  nested facet forest plots------------------------------------------------------------------
##Outcome: total greenspace-------------------------------
#Strata: Race/ethnicity x education x neighborhood
###edu level nested in race/eth at top, depriv at bottom--------

#colnames (table_plot_carrier) <-c("Model", "Estimate", "LowerLevel", "UpperLevel", "result", "ApoE4", "Outcome", "Status")
colnames (predicted_greentotal_results) <-c("Strata", "RaceEthnicity", "EducationLevel", "NSES", "Estimate", "LowerLevel", "UpperLevel")
colnames (predicted_greentotal_results_ln) <-c("Strata", "RaceEthnicity", "EducationLevel", "NSES", "Estimate", "LowerLevel", "UpperLevel")
colnames (predicted_treesonly_results) <-c("Strata", "RaceEthnicity", "EducationLevel", "NSES", "Estimate", "LowerLevel", "UpperLevel")
colnames (predicted_grassonly_results) <-c("Strata", "RaceEthnicity", "EducationLevel", "NSES", "Estimate", "LowerLevel", "UpperLevel")
colnames (predicted_greenother_results) <-c("Strata", "RaceEthnicity", "EducationLevel", "NSES", "Estimate", "LowerLevel", "UpperLevel")
colnames (predicted_trees_lowdens_results) <-c("Strata", "RaceEthnicity", "EducationLevel", "NSES", "Estimate", "LowerLevel", "UpperLevel")
colnames (predicted_trees_highdens_results) <-c("Strata", "RaceEthnicity", "EducationLevel", "NSES", "Estimate", "LowerLevel", "UpperLevel")
colnames (predicted_grass_lowdens_results) <-c("Strata", "RaceEthnicity", "EducationLevel", "NSES", "Estimate", "LowerLevel", "UpperLevel")
colnames (predicted_grass_highdens_results) <-c("Strata", "RaceEthnicity", "EducationLevel", "NSES", "Estimate", "LowerLevel", "UpperLevel")


head(predicted_grass_lowdens_results)

#test_plot1 <- ggplot(data=predicted_greentotal_results, aes(x = Strata, y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
#  geom_pointrange(aes (color = EducationLevel)) + geom_errorbar(aes (color = EducationLevel)) 
#test_plot1 # works but not there yet

#test_plot2 <- ggplot(data=predicted_greentotal_results, aes(x = Strata, y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
#  geom_pointrange(aes (color = EducationLevel)) + geom_errorbar(aes (color = EducationLevel)) + 
#  facet_nested(~ RaceEthnicity + EducationLevel)
#test_plot2 # this gets me the nested facets, now need to sort what the x should be instead of strata

#test_plot3 <- ggplot(data=predicted_greentotal_results, aes(x = Depr, y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
#  geom_pointrange(aes (color = EducationLevel)) + geom_errorbar(aes (color = EducationLevel)) + 
#  facet_nested(~ RaceEthnicity + EducationLevel)
#test_plot3 # this gets me what I want, but need to change angles of x labels and drop the legend

#test_plot4 <- ggplot(data=predicted_greentotal_results, aes(x = Deprivation, y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
#  geom_pointrange(aes (color = EducationLevel)) + geom_errorbar(aes (color = EducationLevel)) + 
#  facet_nested(~ RaceEthnicity + EducationLevel) + theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust=1))+
#  theme(legend.position = "none")
#  test_plot4
  
#test_plot5 <- ggplot(data=predicted_greentotal_results, aes(x = Deprivation, y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
#  geom_pointrange(aes (color = EducationLevel), size=0.1) + geom_errorbar(aes (color = EducationLevel)) + 
#  scale_color_manual(values=c("darkred", "darkseagreen4", "steelblue4"))+
#  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
#  theme_bw()+ # sets white background with gray grid marks
#  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust=1))+ # no legend, sets x-axis text size and angle
#  theme(strip.background = element_rect(colour = "black", linewidth = 1)) + # creates rectangle around nested var labels
 # labs(title = "Figure 3. Predicted mean percent total greenness with 95% credible intervals across intersectional strata \nin the simple intersectional model", 
 #      x = "Neighborhood Deprivation", y = "% Total Street View Greenness") 
  
#test_plot5 # this is the current best plot

## Figure 2 a, b, c & d -------
greentotal <- ggplot(data=predicted_greentotal_results, aes((reorder(x = NSES, desc(NSES))), y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel, shape=EducationLevel), size=1) + 
  geom_errorbar(aes (color = EducationLevel), width=0.5) + 
  geom_hline(yintercept=mean(predicted_greentotal_results$Estimate), linetype="dashed", color = "black") +
  ylim(0, 48) +
  #scale_color_viridis_d() +
  scale_color_manual(values=c("orchid", "skyblue4", "darkgoldenrod2"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, angle = 45, hjust = 1.1, vjust = 1.2),
        axis.text.y = element_text(face = "bold", size = 18),
        strip.text.x = element_text(face = "bold", size = 11),
        strip.background = element_rect(colour = "black", linewidth = 1) ,
        #plot.title = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold", angle = 0, vjust = 0.5, size = 18),
        axis.title.x = element_text(face = "bold", size = 18)) + 
  labs(x = "Neighborhood SES", y = "%\n Predicted\n Total\n Greenness") # leaving off title to do in SnagIt instead
# labs(title = "Figure 2. Predicted mean percent street-view of measures of greenness with 95% credible intervals across intersectional strata 
#in the simple intersectional model. MESA 2005-2007, N=5,246.
# A.", x = "Neighborhood SES", y = "% Predicted\n Total\n Greenness") 

greentotal 

treesonly <- ggplot(data=predicted_treesonly_results, aes((reorder(x = NSES, desc(NSES))), y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel, shape=EducationLevel), size=1) + 
  geom_errorbar(aes (color = EducationLevel), width=0.5) + 
  geom_hline(yintercept=mean(predicted_treesonly_results$Estimate), linetype="dashed", color = "black") +
  ylim(0, 35) +
  scale_color_manual(values=c("orchid", "skyblue4", "darkgoldenrod2"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, angle = 45, vjust = 1.1, hjust=1.2),
        axis.text.y = element_text(face = "bold", size = 18),
        strip.text.x = element_text(face = "bold", size = 11),
        strip.background = element_rect(colour = "black", linewidth = 1) ,
        #plot.title = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold", angle = 0, vjust = 0.5, size = 18),
        axis.title.x = element_text(face = "bold", size = 18)) + 
  labs(x = "Neighborhood SES", y = "%\n Predicted\n Trees") # leaving of "B" - doing that instead via SnagIt editor
  #theme(legend.position = "none", axis.text.x = element_text(size = 10, angle = 45, vjust = 1.1, hjust=1))+ # no legend, sets x-axis text size and angle
  #theme(strip.background = element_rect(colour = "black", linewidth = 1)) + # creates rectangle around nested var labels
  #theme(plot.title = element_text(face = "bold"), axis.title.y = element_text(face = "bold", angle = 0, vjust = 0.5)) +
  
  

treesonly 

grassonly <- ggplot(data=predicted_grassonly_results, aes((reorder(x = NSES, desc(NSES))), y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel,shape=EducationLevel), size=1) + 
  geom_errorbar(aes (color = EducationLevel), width=0.5) + 
  geom_hline(yintercept=mean(predicted_grassonly_results$Estimate), linetype="dashed", color = "black") +
  ylim(-2, 15) +
  scale_color_manual(values=c("orchid", "skyblue4", "darkgoldenrod2"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, angle = 45, vjust = 1.1, hjust=1.2),
        axis.text.y = element_text(face = "bold", size = 18),
        strip.text.x = element_text(face = "bold", size = 11),
        strip.background = element_rect(colour = "black", linewidth = 1) ,
        plot.title = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold", angle = 0, vjust = 0.5, size = 18),
        axis.title.x = element_text(face = "bold", size = 18)) + 
  labs(x = "Neighborhood SES", y = "%\n Predicted\n Grass") 

grassonly 

greenother <- ggplot(data=predicted_greenother_results, aes((reorder(x = NSES, desc(NSES))), y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel, shape=EducationLevel), size=0.7) + 
  geom_errorbar(aes (color = EducationLevel), width=0.5) + 
  geom_hline(yintercept=mean(predicted_greenother_results$Estimate), linetype="dashed", color = "black") +
  #ylim(0, 3) +
  scale_color_manual(values=c("orchid", "skyblue4", "darkgoldenrod2"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", axis.text.x = element_text(size = 10, angle = 45, vjust = 1.1, hjust=1),
        strip.background = element_rect(colour = "black", linewidth = 1) ,
        plot.title = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold", angle = 0, vjust = 0.5),
        axis.title.x = element_text(face = "bold")) + 
  labs(x = "Neighborhood SES", y = "%\n Predicted\n Other\n Greennes") 

greenother

## Figure 3 a & b trees stratified by low vs high popn density -------
trees_lowdens <- ggplot(data=predicted_trees_lowdens_results, aes((reorder(x = NSES, desc(NSES))), y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel, shape=EducationLevel), size=1) + 
  geom_errorbar(aes (color = EducationLevel), width=0.5) + 
  geom_hline(yintercept=mean(predicted_trees_lowdens_results$Estimate), linetype="dashed", color = "black") +
  scale_color_manual(values=c("orchid", "skyblue4", "darkgoldenrod2"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  ylim(0, 35) +
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, angle = 45, vjust = 1.1, hjust=1.2),
        axis.text.y = element_text(face = "bold", size = 18),
        strip.text.x = element_text(face = "bold", size = 11),
        strip.background = element_rect(colour = "black", linewidth = 1) , 
        axis.title.y = element_text(face = "bold", angle = 0, vjust = 0.5, size = 18),
        axis.title.x = element_text(face = "bold", size = 18)) + 
  labs(x = "Neighborhood SES", y = "%\n Predicted\n Trees") 
  #labs(title="A. Low Population Density (N=2,873)",
  #  x = "Neighborhood SES", y = "% Predicted Total Greenness") 
  #theme(plot.title = ggtext::element_markdown())
  #labs(title="Figure 3a. Predicted mean percent street-view of measures of greenness for those in low population density area
  #(<7,500 people per square mile) with 95% CIs across intersectional strata in the simple intersectional model.",
  #     x = "Neighborhood Deprivation", y = "% Total Street View Greenness")  
                   
trees_lowdens 

trees_highdens <- ggplot(data=predicted_trees_highdens_results, aes((reorder(x = NSES, desc(NSES))), y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel, shape=EducationLevel), size=1) + 
  geom_errorbar(aes (color = EducationLevel), width=0.5) + 
  geom_hline(yintercept=mean(predicted_trees_highdens_results$Estimate), linetype="dashed", color = "black") +
  scale_color_manual(values=c("orchid", "skyblue4", "darkgoldenrod2"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  ylim(0, 35) +
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, angle = 45, vjust = 1.1, hjust=1.2),
        axis.text.y = element_text(face = "bold", size = 18),
        strip.text.x = element_text(face = "bold", size = 11),
        strip.background = element_rect(colour = "black", linewidth = 1) , 
        axis.title.y = element_text(face = "bold", angle = 0, vjust = 0.5, size = 18),
        axis.title.x = element_text(face = "bold", size = 18)) + 
  labs(x = "Neighborhood SES", y = "%\n Predicted\n Trees") 
  #labs(title = "Figure 3b. Predicted mean percent street-view of measures of greenness for those in **high population** density area \n
  #(>7,500 people per square mile) with 95% CIs across intersectional strata in the simple intersectional model.", 
  #     x = "Neighborhood Deprivation", y = "% Total Street View Greenness") 

trees_highdens 

## Figure 4 a & b grass stratified by low vs high popn density -------
grass_lowdens <- ggplot(data=predicted_grass_lowdens_results, aes((reorder(x = NSES, desc(NSES))), y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel, shape=EducationLevel), size=1) + 
  geom_errorbar(aes (color = EducationLevel), width=0.5) + 
  geom_hline(yintercept=mean(predicted_grass_lowdens_results$Estimate), linetype="dashed", color = "black") +
  scale_color_manual(values=c("orchid", "skyblue4", "darkgoldenrod2"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  ylim(0, 15) +
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, angle = 45, vjust = 1.1, hjust=1.2),
        axis.text.y = element_text(face = "bold", size = 18),
        strip.text.x = element_text(face = "bold", size = 11),
        strip.background = element_rect(colour = "black", linewidth = 1) , 
        axis.title.y = element_text(face = "bold", angle = 0, vjust = 0.5, size = 18),
        axis.title.x = element_text(face = "bold", size = 18)) + 
  labs(x = "Neighborhood SES", y = "%\n Predicted\n Grass") 
#labs(title="A. Low Population Density (N=2,873)",
#  x = "Neighborhood SES", y = "% Predicted Total Greenness") 
#theme(plot.title = ggtext::element_markdown())
#labs(title="Figure 3a. Predicted mean percent street-view of measures of greenness for those in low population density area
#(<7,500 people per square mile) with 95% CIs across intersectional strata in the simple intersectional model.",
#     x = "Neighborhood Deprivation", y = "% Total Street View Greenness")  

grass_lowdens 

grass_highdens <- ggplot(data=predicted_grass_highdens_results, aes((reorder(x = NSES, desc(NSES))), y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel, shape=EducationLevel), size=1) + 
  geom_errorbar(aes (color = EducationLevel), width=0.5) + 
  geom_hline(yintercept=mean(predicted_grass_highdens_results$Estimate), linetype="dashed", color = "black") +
  scale_color_manual(values=c("orchid", "skyblue4", "darkgoldenrod2"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  ylim(0, 15) +
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 11, angle = 45, vjust = 1.1, hjust=1.2),
        axis.text.y = element_text(face = "bold", size = 18),
        strip.text.x = element_text(face = "bold", size = 11),
        strip.background = element_rect(colour = "black", linewidth = 1) , 
        axis.title.y = element_text(face = "bold", angle = 0, vjust = 0.5, size = 18),
        axis.title.x = element_text(face = "bold", size = 18)) + 
  labs(x = "Neighborhood SES", y = "%\n Predicted\n Grass") 
#labs(title = "Figure 3b. Predicted mean percent street-view of measures of greenness for those in **high population** density area \n
#(>7,500 people per square mile) with 95% CIs across intersectional strata in the simple intersectional model.", 
#     x = "Neighborhood Deprivation", y = "% Total Street View Greenness") 

grass_highdens 


## Supp fig for log normal model: total greens --------
greentotal_ln <- ggplot(data=predicted_greentotal_results_ln, aes((reorder(x = NSES, desc(NSES))), y = Estimate, ymin=LowerLevel, ymax=UpperLevel))+
  geom_pointrange(aes (color = EducationLevel, shape=EducationLevel), size=0.7) + 
  geom_errorbar(aes (color = EducationLevel, width=0.5)) + 
  geom_hline(yintercept=mean(predicted_greentotal_results_ln$Estimate), linetype="dashed", color = "black") +
  ylim(0, 60) +
  #scale_color_viridis_d() +
  scale_color_manual(values=c("orchid", "skyblue4", "darkgoldenrod2"))+
  facet_nested(~ RaceEthnicity + EducationLevel, nest_line = TRUE) + 
  theme_bw()+ # sets white background with gray grid marks
  theme(legend.position = "none", axis.text.x = element_text(size = 10, angle = 45, vjust = 1.1, hjust=1),
        strip.background = element_rect(colour = "black", linewidth = 1) , # creates rectangle around nested var labels
        plot.title = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold", angle = 0, vjust = 0.5),
        axis.title.x = element_text(face = "bold")) + # creates rectangle around nested var labels
  labs(x = "Neighborhood SES", y = "%\n Predicted\n Total\n Greenness") 
greentotal_ln 
