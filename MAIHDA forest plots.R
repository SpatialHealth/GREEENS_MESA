library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(jtools)
library(forcats)
library(survminer)
library(forestplot)

########################
#Outcome: total greenspace
#Strata: Race/ethnicity x education
strata<-c("White + High school/GED","White + Some college/AA","White + Bachelor's+", 
          "Chinese + High school/GED","Chinese + Some college/AA","Chinese + Bachelor's+",
          "Black + High school/GED","Black + Some college/AA","Black + Bachelor's+",
          "Hispanic + High school/GED","Hispanic + Some college/AA","Hispanic + Bachelor's+")
ES<-c(32.7, 32.0, 29.3, 22.3, 23.9, 27.9, 26.2, 27.6, 30.3, 21.7, 23.8, 24.8) # Mean estimates
LCI<-c(15.0, 15.5,15.2, 13.6, 17.5, 20.1, 10.4, 13.4, 14.2, 10.4, 11.6, 13.4) # Lower 95% confidence interval
UCI<-c(43.8, 43.6, 44.0, 41.2, 43.5, 48.1, 40.8, 43.5, 44.0, 39.1, 40.8, 43.5) # Upper 95% confidence interval

results<-data.frame(strata,ES,LCI,UCI)

results$strata<-factor(results$strata)

results_reord <- results %>% # to keep the strata in the order we want them
  mutate(strata = fct_relevel(strata, 
                              "White + High school/GED","White + Some college/AA","White + Bachelor's+", 
                              "Chinese + High school/GED","Chinese + Some college/AA","Chinese + Bachelor's+",
                              "Black + High school/GED","Black + Some college/AA","Black + Bachelor's+",
                              "Hispanic + High school/GED","Hispanic + Some college/AA","Hispanic + Bachelor's+"))

fp_race_edu_totalgreen <- ggplot(results_reord, aes(reorder(strata, desc(strata)), ES, ymin=LCI, ymax=UCI, colour = factor(strata)))+
  coord_flip() + # note in aes line above 'desc' reverses display order of the modelNames 
  geom_hline(yintercept=10, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=20, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=30, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=40, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=50, size =1, colour="gray90") + # add lines at tick marks
  geom_pointrange() + # creates the whiskers 
  geom_point(size = 3, shape=23) +
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=0.2, cex=0.5)+ # Makes whiskers on the range (more aesthetically pleasing)
  #scale_colour_brewer(palette = "Dark2") +
  labs(title="Figure 1. Association of Intersecting Strata of Race/Ethnicity and Education \nLevel with Baseline Google Street View Measures of Greenspace.\nMESA (N=5264)") +
  labs(color='Strata')+
  xlab("") + # Label on the Y axis (flipped specification due to coord_flip)
  ylab("Mean Estimates of % Total GSV Greenspace + 95% CI") + # Label on the X axis (flipped specification due to coord_flip)
  scale_y_continuous(limits = c(10,50), breaks = c(10,20,30,40,50))+ # limits and tic marks on X axis (flipped specification do to coord_flip)
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
fp_race_edu_totalgreen

########################
#Outcome: total trees
#Strata: Race/ethnicity x education
strata_race_edu_trees<-c("White + High school/GED","White + Some college/AA","White + Bachelor's+", 
          "Chinese + High school/GED","Chinese + Some college/AA","Chinese + Bachelor's+",
          "Black + High school/GED","Black + Some college/AA","Black + Bachelor's+",
          "Hispanic + High school/GED","Hispanic + Some college/AA","Hispanic + Bachelor's+")
ES_race_edu_trees<-c(22.9, 22.8, 22.1, 16.3, 18.0, 20.3, 18.6, 20.0, 21.6, 16.5, 17.8, 19.3) # Mean estimates
LCI_race_edu_trees<-c(12.7, 13.2, 14.2, 10.6, 13.5, 14.9, 10.1, 12.2, 12.8, 10.0, 10.4, 12.4) # Lower 95% confidence interval
UCI_race_edu_trees<-c(29.1, 29.4, 30.4, 27.5, 29.5, 31.9, 27.2, 29.3, 29.7, 26.2, 27.0, 29.3) # Upper 95% confidence interval

results_race_edu_trees<-data.frame(strata_race_edu_trees,ES_race_edu_trees,LCI_race_edu_trees,
                                   UCI_race_edu_trees)

results_race_edu_trees$strata_race_edu_trees<-factor(results_race_edu_trees$strata_race_edu_trees)

results_race_edu_trees_reord <- results_race_edu_trees %>% # to keep the strata in the order we want them
  mutate(strata_race_edu_trees = fct_relevel(strata_race_edu_trees, 
                              "White + High school/GED","White + Some college/AA","White + Bachelor's+", 
                              "Chinese + High school/GED","Chinese + Some college/AA","Chinese + Bachelor's+",
                              "Black + High school/GED","Black + Some college/AA","Black + Bachelor's+",
                              "Hispanic + High school/GED","Hispanic + Some college/AA","Hispanic + Bachelor's+"))

fp_race_edu_trees <- ggplot(results_race_edu_trees_reord, aes(reorder(strata_race_edu_trees, desc(strata_race_edu_trees)), 
                                                              ES_race_edu_trees, ymin=LCI_race_edu_trees, ymax=UCI_race_edu_trees, 
                                                              colour = factor(strata_race_edu_trees)))+
  coord_flip() + # note in aes line above 'desc' reverses display order of the modelNames 
  geom_hline(yintercept=10, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=20, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=30, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=40, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=50, size =1, colour="gray90") + # add lines at tick marks
  geom_pointrange() + # creates the whiskers 
  geom_point(size = 3, shape=23) +
  geom_errorbar(aes(ymin=LCI_race_edu_trees, ymax=UCI_race_edu_trees), width=0.2, cex=0.5)+ # Makes whiskers on the range (more aesthetically pleasing)
  #scale_colour_brewer(palette = "Dark2") +
  labs(title="Figure 2. Association of Intersecting Strata of Race/Ethnicity and Education \nLevel with Baseline Google Street View Measures of Greenspace.\nMESA (N=5264)") +
  labs(color='Strata')+
  xlab("") + # Label on the Y axis (flipped specification due to coord_flip)
  ylab("Mean Estimates of % Total GSV Trees + 95% CI") + # Label on the X axis (flipped specification due to coord_flip)
  scale_y_continuous(limits = c(10,50), breaks = c(10,20,30,40,50))+ # limits and tic marks on X axis (flipped specification do to coord_flip)
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
fp_race_edu_trees

########################
#Outcome: total greenspace
#Strata: Race/ethnicity x education x neighborhood
strata_REN_totalgreen<-c("White + HS/GED + least-deprived neighborhood","White + HS/GED + moderately-deprived neighborhood","White + HS/GED + most-deprived neighborhood",
          "White + Some college/AA + least-deprived neighborhood","White + Some college/AA + moderately-deprived neighborhood","White + Some college/AA + most-deprived neighborhood",
          "White + Bachelor's or greater + least-deprived neighborhood","White + Bachelor's or greater + moderately-deprived neighborhood","White + Bachelor's or greater + most-deprived neighborhood",
          "Chinese + HS/GED + least-deprived neighborhood","Chinese + HS/GED + moderately-deprived neighborhood","Chinese + HS/GED + most-deprived neighborhood",
          "Chinese + Some college/AA + least-deprived neighborhood","Chinese + Some college/AA + moderately-deprived neighborhood","Chinese + Some college/AA + most-deprived neighborhood",
          "Chinese + Bachelor's or greater + least-deprived neighborhood","Chinese + Bachelor's or greater + moderately-deprived neighborhood","Chinese + Bachelor's or greater + most-deprived neighborhood",
          "Black + HS/GED + least-deprived neighborhood","Black + HS/GED + moderately-deprived neighborhood","Black + HS/GED + most-deprived neighborhood",
          "Black + Some college/AA + least-deprived neighborhood","Black + Some college/AA + moderately-deprived neighborhood","Black + Some college/AA + most-deprived neighborhood",
          "Black + Bachelor's or greater + least-deprived neighborhood","Black + Bachelor's or greater + moderately-deprived neighborhood","Black + Bachelor's or greater + most-deprived neighborhood",
          "Hispanic + HS/GED + least-deprived neighborhood","Hispanic + HS/GED + moderately-deprived neighborhood","Hispanic + HS/GED + most-deprived neighborhood",
          "Hispanic + Some college/AA + least-deprived neighborhood","Hispanic + Some college/AA + moderately-deprived neighborhood","Hispanic + Some college/AA + most-deprived neighborhood",
          "Hispanic + Bachelor's or greater + least-deprived neighborhood","Hispanic + Bachelor's or greater + moderately-deprived neighborhood","Hispanic + Bachelor's or greater + most-deprived neighborhood")
ES_REN_totalgreen<-c(29.2, 33.0, 31.8, 26.5, 32.8, 32.4, 25.7, 33.1, 33.3, 22.9, 17.9, 13.4, 25.9, 20.2,
      15.1, 29.8, 21.7, 17.2, 19.0, 22.1, 25.6, 21.6, 25.6, 28.3, 27.7, 28.8, 29.5, 16.4,
      20.1, 17.7, 22.5, 23.2, 19.8, 24.3, 23.8, 22.8) # Mean estimates
LCI_REN_totalgreen<-c(14.5, 15.4, 16.5, 14.4, 16.0, 16.3, 14.8, 16.5, 16.8, 18.8, 13.8, 9.9, 21.2, 16.0,
       10.7, 24.3, 17.6, 13.0, 8.9, 12.0, 9.8, 12.0, 13.7, 13.5, 14.6, 14.9, 12.7, 10.8,
       10.8, 9.1, 13.4, 11.8, 9.6, 14.2, 14.5, 10.2) # Lower 95% confidence interval
UCI_REN_totalgreen<-c(44.8, 44.5, 42.5, 43.5, 44.2, 43.1, 43.8, 45.0, 43.7, 28.5, 22.9, 18.3, 30.9, 25.6,
       21.0, 33.6, 26.5, 23.1, 40.0, 42.6, 40.0, 42.0, 44.5, 43.2, 45.7, 44.6, 42.2, 32.3,
       33.9, 31.6, 37.6, 34.7, 32.7, 38.2, 38.6, 35.2) # Upper 95% confidence interval

results_REN_totalgreen<-data.frame(strata_REN_totalgreen,ES_REN_totalgreen,LCI_REN_totalgreen,UCI_REN_totalgreen)

results_REN_totalgreen$strata_REN_totalgreen<-factor(results_REN_totalgreen$strata_REN_totalgreen)

results_REN_totalgreen_reord <- results_REN_totalgreen %>% # to keep the strata in the order we want them
  mutate(strata_REN_totalgreen = fct_relevel(strata_REN_totalgreen, 
                                             "White + HS/GED + least-deprived neighborhood","White + HS/GED + moderately-deprived neighborhood","White + HS/GED + most-deprived neighborhood",
                                             "White + Some college/AA + least-deprived neighborhood","White + Some college/AA + moderately-deprived neighborhood","White + Some college/AA + most-deprived neighborhood",
                                             "White + Bachelor's or greater + least-deprived neighborhood","White + Bachelor's or greater + moderately-deprived neighborhood","White + Bachelor's or greater + most-deprived neighborhood",
                                             "Chinese + HS/GED + least-deprived neighborhood","Chinese + HS/GED + moderately-deprived neighborhood","Chinese + HS/GED + most-deprived neighborhood",
                                             "Chinese + Some college/AA + least-deprived neighborhood","Chinese + Some college/AA + moderately-deprived neighborhood","Chinese + Some college/AA + most-deprived neighborhood",
                                             "Chinese + Bachelor's or greater + least-deprived neighborhood","Chinese + Bachelor's or greater + moderately-deprived neighborhood","Chinese + Bachelor's or greater + most-deprived neighborhood",
                                             "Black + HS/GED + least-deprived neighborhood","Black + HS/GED + moderately-deprived neighborhood","Black + HS/GED + most-deprived neighborhood",
                                             "Black + Some college/AA + least-deprived neighborhood","Black + Some college/AA + moderately-deprived neighborhood","Black + Some college/AA + most-deprived neighborhood",
                                             "Black + Bachelor's or greater + least-deprived neighborhood","Black + Bachelor's or greater + moderately-deprived neighborhood","Black + Bachelor's or greater + most-deprived neighborhood",
                                             "Hispanic + HS/GED + least-deprived neighborhood","Hispanic + HS/GED + moderately-deprived neighborhood","Hispanic + HS/GED + most-deprived neighborhood",
                                             "Hispanic + Some college/AA + least-deprived neighborhood","Hispanic + Some college/AA + moderately-deprived neighborhood","Hispanic + Some college/AA + most-deprived neighborhood",
                                             "Hispanic + Bachelor's or greater + least-deprived neighborhood","Hispanic + Bachelor's or greater + moderately-deprived neighborhood","Hispanic + Bachelor's or greater + most-deprived neighborhood"))

fp_REN_totalgreen <- ggplot(results_REN_totalgreen_reord, aes(reorder(strata_REN_totalgreen, desc(strata_REN_totalgreen)), ES_REN_totalgreen, ymin=LCI_REN_totalgreen, ymax=UCI_REN_totalgreen, colour = factor(strata_REN_totalgreen)))+
  coord_flip() + # note in aes line above 'desc' reverses display order of the modelNames 
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

########################
#Outcome: total trees
#Strata: Race/ethnicity x education x neighborhood deprivation
strata_REN_trees<-c("White + HS/GED + least-deprived neighborhood","White + HS/GED + moderately-deprived neighborhood","White + HS/GED + most-deprived neighborhood",
          "White + Some college/AA + least-deprived neighborhood","White + Some college/AA + moderately-deprived neighborhood","White + Some college/AA + most-deprived neighborhood",
          "White + Bachelor's or greater + least-deprived neighborhood","White + Bachelor's or greater + moderately-deprived neighborhood","White + Bachelor's or greater + most-deprived neighborhood",
          "Chinese + HS/GED + least-deprived neighborhood","Chinese + HS/GED + moderately-deprived neighborhood","Chinese + HS/GED + most-deprived neighborhood",
          "Chinese + Some college/AA + least-deprived neighborhood","Chinese + Some college/AA + moderately-deprived neighborhood","Chinese + Some college/AA + most-deprived neighborhood",
          "Chinese + Bachelor's or greater + least-deprived neighborhood","Chinese + Bachelor's or greater + moderately-deprived neighborhood","Chinese + Bachelor's or greater + most-deprived neighborhood",
          "Black + HS/GED + least-deprived neighborhood","Black + HS/GED + moderately-deprived neighborhood","Black + HS/GED + most-deprived neighborhood",
          "Black + Some college/AA + least-deprived neighborhood","Black + Some college/AA + moderately-deprived neighborhood","Black + Some college/AA + most-deprived neighborhood",
          "Black + Bachelor's or greater + least-deprived neighborhood","Black + Bachelor's or greater + moderately-deprived neighborhood","Black + Bachelor's or greater + most-deprived neighborhood",
          "Hispanic + HS/GED + least-deprived neighborhood","Hispanic + HS/GED + moderately-deprived neighborhood","Hispanic + HS/GED + most-deprived neighborhood",
          "Hispanic + Some college/AA + least-deprived neighborhood","Hispanic + Some college/AA + moderately-deprived neighborhood","Hispanic + Some college/AA + most-deprived neighborhood",
          "Hispanic + Bachelor's or greater + least-deprived neighborhood","Hispanic + Bachelor's or greater + moderately-deprived neighborhood","Hispanic + Bachelor's or greater + most-deprived neighborhood")
ES_REN_trees<-c(21.5, 23.4, 21.8, 20.5, 23.4, 22.5, 20.3, 24.0, 24.0, 17.1, 14.1, 10.5, 19.6, 15.7, 12.1, 22.1,
                16.3, 13.4, 15.4, 16.5, 18.1, 17.3, 19.0, 20.1, 21.1, 20.3, 20.6, 13.9, 16.0, 14.0, 17.9, 17.4,
                15.6, 19.9, 18.2, 18.0) # Mean estimates
LCI_REN_trees<-c(13.3, 13.2, 12.0, 13.8, 13.3, 12.5, 14.4, 14.2, 13.5, 14.0, 10.9, 7.9, 16.0, 12.5, 8.6,
                 17.9, 13.2, 10.2, 9.6, 11.0, 9.5, 11.9, 12.6, 12.1, 14.0, 12.9, 11.5, 10.4, 10.3, 9.0,
                 12.2, 10.1, 9.2, 13.5, 12.6, 10.0) # Lower 95% confidence interval
UCI_REN_trees<-c(30.6, 29.6, 27.6, 30.3, 29.4, 28.2, 30.6, 30.5, 29.5, 21.3, 17.7, 14.3, 23.3, 19.8,
                 16.7, 24.8, 19.8, 17.7, 27.4, 28.3, 26.7, 29.0, 29.9, 29.1, 31.5, 29.7, 28.3, 24.8,
                 25.3, 23.7, 28.1, 25.3, 24.4, 29.2, 28.5, 26.5) # Upper 95% confidence interval

results_REN_trees<-data.frame(strata_REN_trees,ES_REN_trees,LCI_REN_trees,
                                   UCI_REN_trees)

results_REN_trees$strata_REN_trees<-factor(results_REN_trees$strata_REN_trees)

results_REN_trees_reord <- results_REN_trees %>% # to keep the strata in the order we want them
  mutate(strata_REN_trees = fct_relevel(strata_REN_trees, 
                                        "White + HS/GED + least-deprived neighborhood","White + HS/GED + moderately-deprived neighborhood","White + HS/GED + most-deprived neighborhood",
                                        "White + Some college/AA + least-deprived neighborhood","White + Some college/AA + moderately-deprived neighborhood","White + Some college/AA + most-deprived neighborhood",
                                        "White + Bachelor's or greater + least-deprived neighborhood","White + Bachelor's or greater + moderately-deprived neighborhood","White + Bachelor's or greater + most-deprived neighborhood",
                                        "Chinese + HS/GED + least-deprived neighborhood","Chinese + HS/GED + moderately-deprived neighborhood","Chinese + HS/GED + most-deprived neighborhood",
                                        "Chinese + Some college/AA + least-deprived neighborhood","Chinese + Some college/AA + moderately-deprived neighborhood","Chinese + Some college/AA + most-deprived neighborhood",
                                        "Chinese + Bachelor's or greater + least-deprived neighborhood","Chinese + Bachelor's or greater + moderately-deprived neighborhood","Chinese + Bachelor's or greater + most-deprived neighborhood",
                                        "Black + HS/GED + least-deprived neighborhood","Black + HS/GED + moderately-deprived neighborhood","Black + HS/GED + most-deprived neighborhood",
                                        "Black + Some college/AA + least-deprived neighborhood","Black + Some college/AA + moderately-deprived neighborhood","Black + Some college/AA + most-deprived neighborhood",
                                        "Black + Bachelor's or greater + least-deprived neighborhood","Black + Bachelor's or greater + moderately-deprived neighborhood","Black + Bachelor's or greater + most-deprived neighborhood",
                                        "Hispanic + HS/GED + least-deprived neighborhood","Hispanic + HS/GED + moderately-deprived neighborhood","Hispanic + HS/GED + most-deprived neighborhood",
                                        "Hispanic + Some college/AA + least-deprived neighborhood","Hispanic + Some college/AA + moderately-deprived neighborhood","Hispanic + Some college/AA + most-deprived neighborhood",
                                        "Hispanic + Bachelor's or greater + least-deprived neighborhood","Hispanic + Bachelor's or greater + moderately-deprived neighborhood","Hispanic + Bachelor's or greater + most-deprived neighborhood"))

fp_REN_trees <- ggplot(results_REN_trees_reord, aes(reorder(strata_REN_trees, desc(strata_REN_trees)), 
                                                              ES_REN_trees, ymin=LCI_REN_trees, ymax=UCI_REN_trees, 
                                                              colour = factor(strata_REN_trees)))+
  coord_flip() + # note in aes line above 'desc' reverses display order of the modelNames 
  geom_hline(yintercept=0, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=10, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=20, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=30, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=40, size =1, colour="gray90") + # add lines at tick marks
  geom_hline(yintercept=50, size =1, colour="gray90") + # add lines at tick marks
  geom_pointrange() + # creates the whiskers 
  geom_point(size = 3, shape=23) +
  geom_errorbar(aes(ymin=LCI_REN_trees, ymax=UCI_REN_trees), width=0.2, cex=0.5)+ # Makes whiskers on the range (more aesthetically pleasing)
  #scale_colour_brewer(palette = "Dark2") +
  labs(title="Figure 2. Association of Intersecting Strata of Race/Ethnicity and Education \nLevel with Baseline Google Street View Measures of Greenspace.\nMESA (N=5264)") +
  labs(color='Strata')+
  xlab("") + # Label on the Y axis (flipped specification due to coord_flip)
  ylab("Mean Estimates of % Total GSV Trees + 95% CI") + # Label on the X axis (flipped specification due to coord_flip)
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
fp_REN_trees



