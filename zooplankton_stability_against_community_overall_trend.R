#-------------------------------------------------------------------------------------------#

## Stability ~ community structure variables (Zooplankton) overall trend ####

#-------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------#

## 0. About ####

#-------------------------------------------------------------------------------------------#
# load packages
library(tidyverse)
library(scales)
library(gridExtra)
library(ggplot2)
library(ggpubr)


# stability measures:
# AUC (total_impact)
# resistance (initial_stab) for pulse
# recovery (final_stab) for pulse & press
# resilience (rate_change_ort, rate_change_oti) for pulse & press

# community structure variables:
# Initial Biomass (Zooplankton)
# Evenness (Zooplankton)
# Richness (Zooplankton)
# Effective number of species (Zooplankton)
# Body size (zooplankton)
#-------------------------------------------------------------------------------------------#

## 1. load and transform datasets ####

#-------------------------------------------------------------------------------------------#
# load stability dataset
stability1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Stability/stab_stand_yes_c.csv", sep=",")

# deleting first column
stability <- stability1[-1] 

# changing Lake names 
levels(stability$Lake)
#  "Bolmen"       "Erken"        "Erssj\xf6n"   "Feresj\xf6n"  "Stortj\xe4rn"

stability$Lake<- as.character(stability$Lake)

stability$Lake[stability$Lake == "Erssj\xf6n"] <- "Erssjoen"

stability$Lake[stability$Lake == "Feresj\xf6n"] <- "Feresjoen"

stability$Lake[stability$Lake == "Stortj\xe4rn"] <- "Stortjaern"

stability$Lake <- as.factor(stability$Lake)

levels(stability$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# filtering variable for zooplankton only 
zoop_stability <- stability[which(stability$variable=='zoop_function'), ]

# only filtering the parameters we need ()
zoop_stability  <- select(zoop_stability , Lake, Experiment, Enclosure, 
                          Treatment, final_stab, initial_stab, 
                          rate_change_ort, rate_change_oti, total_impact)
#-------------------------------------------------------------------------------------------#
# load biomass dataset
zooplankton_biomass1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Zooplankton_biomass.csv", sep =";")

# deleting Replicate = Lake
zooplankton_biomass <- zooplankton_biomass1[!(zooplankton_biomass1$Replicate %in% c("Lake", "lake")),]

# deleting Treatment = Lake & C 
zooplankton_biomass <- zooplankton_biomass[!(zooplankton_biomass$Treatment %in% c("C", "Lake")),]


# hanging names of column "Lake"
zooplankton_biomass$Lake<- as.character(zooplankton_biomass$Lake)

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Asa"] <- "Feresjoen"

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Skogaryd"] <- "Erssjoen"

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Svartberget"] <- "Stortjaern"

zooplankton_biomass$Lake <- as.factor(zooplankton_biomass$Lake)

levels(zooplankton_biomass$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# change column Experiment
zooplankton_biomass$Experiment<- as.factor(zooplankton_biomass$Experiment)

levels(zooplankton_biomass$Experiment)

zooplankton_biomass$Experiment<- as.character(zooplankton_biomass$Experiment)

zooplankton_biomass$Experiment[zooplankton_biomass$Experiment == "1"] <- "Spring"

zooplankton_biomass$Experiment[zooplankton_biomass$Experiment == "2"] <- "Summer"

zooplankton_biomass$Experiment <- as.factor(zooplankton_biomass$Experiment)

levels(zooplankton_biomass$Experiment)

# calculate mean values for biomass 
zooplankton_biomass_mean <- zooplankton_biomass %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_Biomass=mean(Clean_Biomass, na.rm=T))

#-------------------------------------------------------------------------------------------#
# merging zooplankton biomass (mean) and stability datasets

zoop_stability_biomass <- merge(x=zooplankton_biomass_mean, y=zoop_stability, 
                                by =c("Lake", "Experiment", "Enclosure", "Treatment"), 
                                all.x=TRUE)
#-------------------------------------------------------------------------------------------#
# load diversity dataset
# loading diversity dataset
zooplankton_diversity1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Diversity/diversity_metrics.csv")

# deleting first column
zooplankton_diversity <- zooplankton_diversity1[-1] 

# renaming lakes
levels(zooplankton_diversity$Lake)
# "Bolmen"       "Erken"        "Erssj\xf6n"   "Feresj\xf6n"  "Stortj\xe4rn"

zooplankton_diversity$Lake<- as.character(zooplankton_diversity$Lake)

zooplankton_diversity$Lake[zooplankton_diversity$Lake == "Feresj\xf6n"] <- "Feresjoen"

zooplankton_diversity$Lake[zooplankton_diversity$Lake == "Erssj\xf6n"] <- "Erssjoen"

zooplankton_diversity$Lake[zooplankton_diversity$Lake == "Stortj\xe4rn"] <- "Stortjaern"

zooplankton_diversity$Lake <- as.factor(zooplankton_diversity$Lake)

levels(zooplankton_diversity$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# deleting all Replicate = Lake

zooplankton_diversity$Replicate <- as.factor(zooplankton_diversity$Replicate)
levels(zooplankton_diversity$Replicate)

zooplankton_diversity <- zooplankton_diversity[!(zooplankton_diversity$Replicate == "Lake"),]

# deleting all Treatment = Lake + C 
zooplankton_diversity$Treatment <- as.factor(zooplankton_diversity$Treatment)
levels(zooplankton_diversity$Treatment)

zooplankton_diversity <- zooplankton_diversity[!(zooplankton_diversity$Treatment %in% c("Lake", "C")),]

# filtering only zooplankton for column "variable" 

zooplankton_diversity <- zooplankton_diversity[which(zooplankton_diversity$variable=='zooplankton'), ]

# mean values
zooplankton_diversity_mean <- zooplankton_diversity %>%
  group_by(Lake, Experiment,Enclosure, Treatment) %>%
  summarize(mean_J = mean(J, na.rm=T), mean_S = mean(S, na.rm=T), mean_ENS_D = mean(ENS_D, na.rm=T))

#-------------------------------------------------------------------------------------------#
# merging zooplankton diversity and stability datasets
zoop_stability_diversity <- merge(x=zoop_stability, y=zooplankton_diversity_mean,
                                  by =c("Lake", "Experiment", "Enclosure", "Treatment"), 
                                  all.x=TRUE)

#-------------------------------------------------------------------------------------------#

## 2. AUC ~ Biomass  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_AUC_biomass <- ggplot(data=zoop_stability_biomass,aes(x=mean_Biomass, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Biomass") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = F
zoop_stability_biomass_F <- zoop_stability_biomass[which(zoop_stability_biomass$Treatment=='F'), ]
# plotting all lakes, all experiments, treatment "F"
plot_zoop_AUC_biomass_F <- ggplot(data=zoop_stability_biomass_F,aes(x=mean_Biomass, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Biomass") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
zoop_stability_biomass_S <- zoop_stability_biomass[which(zoop_stability_biomass$Treatment=='S'), ]
# plotting all lakes, all experiments, treatment "S"
plot_zoop_AUC_biomass_S <- ggplot(data=zoop_stability_biomass_S,aes(x=mean_Biomass, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Biomass") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
zoop_stability_biomass_FS <- zoop_stability_biomass[which(zoop_stability_biomass$Treatment=='FS'), ]
# plotting all lakes, all experiments, treatment "FS"
plot_zoop_AUC_biomass_FS <- ggplot(data=zoop_stability_biomass_FS,aes(x=mean_Biomass, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Biomass") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press & pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_AUC_biomass, plot_zoop_AUC_biomass_F, plot_zoop_AUC_biomass_S, 
             plot_zoop_AUC_biomass_FS, ncol=2)
#-------------------------------------------------------------------------------------------#

## 3. AUC ~ Evenness  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_AUC_evenness <- ggplot(data=zoop_stability_diversity,aes(x=mean_J, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Evenness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = F
zoop_stability_diversity_F <- zoop_stability_diversity[which(zoop_stability_diversity$Treatment=='F'), ]
# plotting all lakes, all experiments, treatment "F"
plot_zoop_AUC_evenness_F <- ggplot(data=zoop_stability_diversity_F,aes(x=mean_J, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Evenness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
zoop_stability_diversity_S <- zoop_stability_diversity[which(zoop_stability_diversity$Treatment=='S'), ]
# plotting all lakes, all experiments, treatment "S"
plot_zoop_AUC_evenness_S <- ggplot(data=zoop_stability_diversity_S,aes(x=mean_J, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Evenness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
zoop_stability_diversity_FS <- zoop_stability_diversity[which(zoop_stability_diversity$Treatment=='FS'), ]
# plotting all lakes, all experiments, treatment "FS"
plot_zoop_AUC_evenness_FS <- ggplot(data=zoop_stability_diversity_FS,aes(x=mean_J, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Evenness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press & pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


grid.arrange(plot_zoop_AUC_evenness, plot_zoop_AUC_evenness_F, plot_zoop_AUC_evenness_S, 
             plot_zoop_AUC_evenness_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 4. AUC ~ Richness  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_AUC_richness <- ggplot(data=zoop_stability_diversity,aes(x=mean_S, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Richness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_AUC_richness_F <- ggplot(data=zoop_stability_diversity_F,aes(x=mean_S, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Richness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_AUC_richness_S <- ggplot(data=zoop_stability_diversity_S,aes(x=mean_S, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Richness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_AUC_richness_FS <- ggplot(data=zoop_stability_diversity_FS,aes(x=mean_S, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Richness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press & pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_AUC_richness, plot_zoop_AUC_richness_F, plot_zoop_AUC_richness_S, 
             plot_zoop_AUC_richness_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 5. AUC ~ Effective number of species  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_AUC_ENS <- ggplot(data=zoop_stability_diversity,aes(x=mean_ENS_D, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Effective number of species (Simpson)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_AUC_ENS_F <- ggplot(data=zoop_stability_diversity_F,aes(x=mean_ENS_D, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Effective number of species (Simpson)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_AUC_ENS_S <- ggplot(data=zoop_stability_diversity_S,aes(x=mean_ENS_D, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Effective number of species (Simpson)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_AUC_ENS_FS <- ggplot(data=zoop_stability_diversity_FS,aes(x=mean_ENS_D, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Effective number of species (Simpson)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press & pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


grid.arrange(plot_zoop_AUC_ENS, plot_zoop_AUC_ENS_F, plot_zoop_AUC_ENS_S, 
             plot_zoop_AUC_ENS_FS, ncol=2)
#-------------------------------------------------------------------------------------------#

## 6. AUC ~ Initial body size (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
zoopl_size <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/cwm_zooplankton_body_size.csv")

# delete columns 
zoopl_size <- zoopl_size[-1] 

# deleting controls 
levels(zoopl_size$Treatment)
zoopl_size <- zoopl_size[ ! (zoopl_size$Treatment %in% "C"), ]

# merge
zoopl_size_stability <- merge(x=zoop_stability, y=zoopl_size, 
                              by =c("Lake", "Experiment", "Enclosure", "Treatment"), 
                              all.x=TRUE)
# delete columns 
zoopl_size_stability <- zoopl_size_stability[-c(10,11)] 


# plotting all lakes, all treatments, all experiments
plot_zoop_AUC_size <- ggplot(data=zoopl_size_stability, aes(x=cwm, 
                                                            y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Initial body size") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Initial body size (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = F
zoopl_size_stability_F <- zoopl_size_stability[which(zoopl_size_stability$Treatment=='F'), ]

# plotting all lakes, all experiments, treatment "F"
plot_zoop_AUC_size_F<- ggplot(data=zoopl_size_stability_F, aes(x=cwm, 
                                                               y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Initial body size") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Initial body size (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
zoopl_size_stability_S <- zoopl_size_stability[which(zoopl_size_stability$Treatment=='S'), ]

# plotting all lakes, all experiments, treatment "S"
plot_zoop_AUC_size_S<- ggplot(data=zoopl_size_stability_S, aes(x=cwm, 
                                                               y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Initial body size") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Initial body size (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
zoopl_size_stability_FS <- zoopl_size_stability[which(zoopl_size_stability$Treatment=='FS'), ]

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_AUC_size_FS<- ggplot(data=zoopl_size_stability_FS, aes(x=cwm, 
                                                                 y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Initial body size") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Initial body size (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_AUC_size, plot_zoop_AUC_size_F, 
             plot_zoop_AUC_size_S, plot_zoop_AUC_size_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 7. Recovery ~ Biomass ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_recovery_biomass <- ggplot(data=zoop_stability_biomass,aes(x=mean_Biomass, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Biomass") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_recovery_biomass_F <- ggplot(data=zoop_stability_biomass_F,aes(x=mean_Biomass, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Biomass") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_recovery_biomass_S <- ggplot(data=zoop_stability_biomass_S,aes(x=mean_Biomass, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Biomass") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_recovery_biomass_FS <- ggplot(data=zoop_stability_biomass_FS,aes(x=mean_Biomass, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Biomass") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press & pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_recovery_biomass, plot_zoop_recovery_biomass_F, plot_zoop_recovery_biomass_S, 
             plot_zoop_recovery_biomass_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 8. Recovery ~ Evenness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_recovery_evenness <- ggplot(data=zoop_stability_diversity,aes(x=mean_J, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Evenness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_recovery_evenness_F <- ggplot(data=zoop_stability_diversity_F,aes(x=mean_J, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Evenness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_recovery_evenness_S <- ggplot(data=zoop_stability_diversity_S,aes(x=mean_J, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Evenness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


# plotting all lakes, all experiments, treatment "FS"
plot_zoop_recovery_evenness_FS <- ggplot(data=zoop_stability_diversity_FS,aes(x=mean_J, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Evenness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press & pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


grid.arrange(plot_zoop_recovery_evenness, plot_zoop_recovery_evenness_F, 
             plot_zoop_recovery_evenness_S, plot_zoop_recovery_evenness_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 9. Recovery ~ Richness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_recovery_richness <- ggplot(data=zoop_stability_diversity,aes(x=mean_S, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Richness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_recovery_richness_F <- ggplot(data=zoop_stability_diversity_F,aes(x=mean_S, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Richness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_recovery_richness_S <- ggplot(data=zoop_stability_diversity_S,aes(x=mean_S, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Richness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_recovery_richness_FS <- ggplot(data=zoop_stability_diversity_FS,aes(x=mean_S, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Richness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press & pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


grid.arrange(plot_zoop_recovery_richness, plot_zoop_recovery_richness_F, 
             plot_zoop_recovery_richness_S, plot_zoop_recovery_richness_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 10. Recovery ~ Effective number of species ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_recovery_ENS <- ggplot(data=zoop_stability_diversity,aes(x=mean_ENS_D, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Effective number of species (Simpson)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_recovery_ENS_F <- ggplot(data=zoop_stability_diversity_F,aes(x=mean_ENS_D, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Effective number of species (Simpson)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


# plotting all lakes, all experiments, treatment "S"
plot_zoop_recovery_ENS_S <- ggplot(data=zoop_stability_diversity_S,aes(x=mean_ENS_D, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Effective number of species (Simpson)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_recovery_ENS_FS <- ggplot(data=zoop_stability_diversity_FS,aes(x=mean_ENS_D, y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Effective number of species (Simpson)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press & pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_recovery_ENS, plot_zoop_recovery_ENS_F, plot_zoop_recovery_ENS_S, 
             plot_zoop_recovery_ENS_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 11. Recovery ~ Initial body size (Zooplankton) ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments
plot_zoop_recovery_size <- ggplot(data=zoopl_size_stability, aes(x=cwm, 
                                                                 y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Initial body size") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Initial body size (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_recovery_size_F<- ggplot(data=zoopl_size_stability_F, aes(x=cwm, 
                                                                    y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Initial body size") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Initial body size (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_recovery_size_S<- ggplot(data=zoopl_size_stability_S, aes(x=cwm, 
                                                                    y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Initial body size") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Initial body size (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_recovery_size_FS<- ggplot(data=zoopl_size_stability_FS, aes(x=cwm, 
                                                                      y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Initial body size") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Initial body size (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_recovery_size, plot_zoop_recovery_size_F, 
             plot_zoop_recovery_size_S, plot_zoop_recovery_size_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 12. Resistance ~ Biomass ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resistance_biomass_F <- ggplot(data=zoop_stability_biomass_F,aes(x=mean_Biomass, y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Biomass") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


# plotting all lakes, all experiments, treatment "FS"
plot_zoop_resistance_biomass_FS <- ggplot(data=zoop_stability_biomass_FS,aes(x=mean_Biomass, y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Biomass") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press & pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 13. Resistance ~ Evenness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resistance_evenness_F <- ggplot(data=zoop_stability_diversity_F,aes(x=mean_J, y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Evenness") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_resistance_evenness_FS <- ggplot(data=zoop_stability_diversity_FS,aes(x=mean_J, y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Evenness") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_resistance_biomass_F, plot_zoop_resistance_biomass_FS, 
             plot_zoop_resistance_evenness_F, plot_zoop_resistance_evenness_FS,
             ncol=2)

#-------------------------------------------------------------------------------------------#

## 14. Resistance ~ Richness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resistance_richness_F <- ggplot(data=zoop_stability_diversity_F,aes(x=mean_S, y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Richness") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_resistance_richness_FS <- ggplot(data=zoop_stability_diversity_FS,aes(x=mean_S, y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Richness") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press & pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 15. Resistance ~ Effective number of species ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resistance_ENS_F <- ggplot(data=zoop_stability_diversity_F,aes(x=mean_ENS_D, y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Effective number of species") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_resistance_ENS_FS <- ggplot(data=zoop_stability_diversity_FS,aes(x=mean_ENS_D, y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Effective number of species") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_resistance_richness_F, plot_zoop_resistance_richness_FS, 
             plot_zoop_resistance_ENS_F, plot_zoop_resistance_ENS_FS, ncol=2)


#-------------------------------------------------------------------------------------------#

## 16. Resistance ~ Initial body size (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resistance_size_F<- ggplot(data=zoopl_size_stability_F, aes(x=cwm, 
                                                                      y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Initial body size") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Initial body size (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_resistance_size_FS<- ggplot(data=zoopl_size_stability_FS, aes(x=cwm, 
                                                                        y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Initial body size") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Initial body size (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_resistance_size_F, plot_zoop_resistance_size_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 17. Resilience ~ Biovolume ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resilience_biomass_F <- ggplot(data=zoop_stability_biomass_F,aes(x=mean_Biomass, y=rate_change_ort)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Biomass") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_resilience_biomass_S <- ggplot(data=zoop_stability_biomass_S,aes(x=mean_Biomass, y=rate_change_oti)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Biomass") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 18. Resilience ~ Evenness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resilience_evenness_F <- ggplot(data=zoop_stability_diversity_F,aes(x=mean_J, y=rate_change_ort)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Evenness") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_resilience_evenness_S <- ggplot(data=zoop_stability_diversity_S,aes(x=mean_J, y=rate_change_oti)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Evenness") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_resilience_biomass_F, plot_zoop_resilience_biomass_S, 
             plot_zoop_resilience_evenness_F,plot_zoop_resilience_evenness_S,
             ncol=2)

#-------------------------------------------------------------------------------------------#

## 19. Resilience ~ Richness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resilience_richness_F <- ggplot(data=zoop_stability_diversity_F,aes(x=mean_S, y=rate_change_ort)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Richness") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_resilience_richness_S <- ggplot(data=zoop_stability_diversity_S,aes(x=mean_S, y=rate_change_oti)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Richness") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ diversity (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 20. Resilience ~ ENS ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resilience_ENS_F <- ggplot(data=zoop_stability_diversity_F,aes(x=mean_ENS_D, y=rate_change_ort)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Effective number of species") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_resilience_ENS_S <- ggplot(data=zoop_stability_diversity_S,aes(x=mean_ENS_D, y=rate_change_oti)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Effective number of species") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_resilience_richness_F, plot_zoop_resilience_richness_S,
             plot_zoop_resilience_ENS_F, plot_zoop_resilience_ENS_S, 
             nrow=2)

#-------------------------------------------------------------------------------------------#

## 21. Resilience ~ Initial body size (Zooplankton) ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all experiments, treatment "F"
plot_zoop_resilience_size_F<- ggplot(data=zoopl_size_stability_F, aes(x=cwm, 
                                                                      y=rate_change_ort)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Initial body size") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Initial body size (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_resilience_size_S<- ggplot(data=zoopl_size_stability_S, aes(x=cwm, 
                                                                      y=rate_change_oti)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Initial body size") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Initial body size (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_resilience_size_F, 
             plot_zoop_resilience_size_S, ncol=1)




