library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(jtools)
library(forcats)
library(survminer)
library(forestplot)
library(ggh4x) # extends ggplot 

# import results tables
predicted_greentotal_results <- read.csv("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/results/predictedgreentotalforplots_sm.csv")
head(predicted_greentotal_results)
dim(predicted_greentotal_results) 
view(predicted_greentotal_results)

#get rid of NAs rows that ended up in there
predicted_greentotal_results <- predicted_greentotal_results %>%
  mutate %>% 
  filter(!is.na(race_num))
view(predicted_greentotal_results)
dim(predicted_greentotal_results) 

###set cat vars to factors and assign labels for levels
predicted_greentotal_results$race_num <- factor(predicted_greentotal_results$race_num, levels=c(1,2,3,4),
                                           labels=c("White", 
                                                    "Chinese American",
                                                    "Black",
                                                    "Hispanic"))

predicted_greentotal_results$edu_num <- factor(predicted_greentotal_results$edu_num, levels = c(1,2,3), 
                                              labels = c("High School/GED or less", #reference
                                                         "Some college", 
                                                         "Bachelor's Degree or higher"))

predicted_greentotal_results$depr_num <- factor(predicted_greentotal_results$depr_num, levels = c(1,2,3), 
                                                labels=c("Least", 
                                                         "Moderate",
                                                         "Most"))  
head(predicted_greentotal_results)

########################
#Outcome: total greenspace
#Strata: Race/ethnicity x education x neighborhood


results_REN_totalgreen<-data.frame(strata_REN_totalgreen,ES_REN_totalgreen,LCI_REN_totalgreen,UCI_REN_totalgreen)

results_REN_totalgreen$strata_REN_totalgreen<-factor(results_REN_totalgreen$strata_REN_totalgreen)

#colnames (table_plot_carrier) <-c("Model", "Estimate", "LowerLevel", "UpperLevel", "result", "ApoE4", "Outcome", "Status")
colnames (predicted_greentotal_results) <-c("Strata", "RaceEthnicity", "EducationLevel", "Deprivation", "Estimate", "LowerLevel", "UpperLevel")
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
    geom_pointrange(aes (color = EducationLevel)) + geom_errorbar(aes (color = EducationLevel)) + 
    facet_nested(~ RaceEthnicity + EducationLevel) + theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust=1))+
    theme(legend.position = "none")
test_plot5


g1<-ggplot(data=table_plot_carrier, aes(x = Outcome, y = Estimate, col=ApoE4))+
  geom_line(aes(linetype = ApoE4), size=0.5, position = pd)+
  geom_point(aes(col = factor(ApoE4)), size=2.5, position = pd)+
  geom_errorbar(aes(ymin=LowerLevel, ymax=UpperLevel, linetype=ApoE4, col=ApoE4), size=0.8, position = pd)+
  xlab('')+
  ylim(-0.02, 0.06) +
  geom_hline(yintercept =0, linetype=1)+
  theme_bw() + 
  scale_colour_manual(values=cols) +
  theme(axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "right")
g1

fp_totalgreen <- ggplot(data=predicted_greentotal_results, aes(x = Strata, y = Estimate))+
  geom_hline(yintercept=0, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=10, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=20, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=30, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=40, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=50, size =1, colour="gray90") + # add lines at tick marks
  geom_pointrange() + # creates the whiskers 
  geom_point(size = 3, shape=23) +
  geom_errorbar(aes(ymin=LCI_REN_totalgreen, ymax=UCI_REN_totalgreen), width=0.2, cex=0.5)+ # Makes whiskers on the range (more aesthetically pleasing)
  #scale_colour_brewer(palette = "Dark2") +
  labs(title="Figure 1. Association of Intersecting Strata of Race/Ethnicity, Education & Neighborhood Deprivation \n with Baseline Google Street View Measures of Greenspace.\nMESA (N=5264)") +
  labs(color='Strata')+
  xlab("") + # Label on the Y axis (flipped specification due to coord_flip)
  ylab("Mean Estimates of % Total GSV Greenspace + 95% CI") + # Label on the X axis (flipped specification due to coord_flip)
  scale_y_continuous(limits = c(0,50), breaks = c(0,10,20,30,40,50))+ # limits and tic marks on X axis (flipped specification do to coord_flip)
  theme(strip.background = element_rect(fill="gray90"), 
        legend.position ="right", 
        axis.text.y=element_blank(),
        axis.line.x = element_line(colour = "black"), 
        axis.line.y = element_blank(), 
        panel.border= element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        text=element_text(size=10),
        plot.title = element_text(face = "bold"),
        axis.ticks = element_blank())
fp_REN_totalgreen


g1<-ggplot(data=table_plot_carrier, aes(x = Outcome, y = Estimate, col=ApoE4))+
  geom_line(aes(linetype = ApoE4), size=0.5, position = pd)+
  geom_point(aes(col = factor(ApoE4)), size=2.5, position = pd)+
  geom_errorbar(aes(ymin=LowerLevel, ymax=UpperLevel, linetype=ApoE4, col=ApoE4), size=0.8, position = pd)+
  xlab('')+
  ylim(-0.02, 0.06) +
  geom_hline(yintercept =0, linetype=1)+
  theme_bw() + 
  scale_colour_manual(values=cols) +
  theme(axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "right")
g1


