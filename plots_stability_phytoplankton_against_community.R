#-------------------------------------------------------------------------------------------#

## Stability ~ community structure variables (Phyotplankton) ####

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
library(gdata)


# stability measures:
# AUC (total_impact)
# recovery (final_stab) for pulse & press
# resistance (initial_stab) for pulse
# resilience (rate_change_ort, rate_change_oti) for pulse & press

# community structure variables:
# Biovolume (Phytoplankton)
# Evenness (Phytoplankton)
# Richness (Phytoplankton)
# Effective number of species (Phytoplankton)
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


# filtering variable for phytoplankton only 
phyto_stability <- stability[which(stability$variable=='phyto_function'), ]

# only filtering the parameters we need ()
phyto_stability  <- select(phyto_stability , Lake, Experiment, Enclosure, 
                         Treatment, final_stab, initial_stab, 
                         rate_change_ort, rate_change_oti, total_impact)

#-------------------------------------------------------------------------------------------#
# load biovolume dataset
phytoplankton_biovolume1 <- read.xls("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/phyto_biovolume.xlsx")

# a) deleting Replicate = Lake
levels(phytoplankton_biovolume1$Replicate)
phytoplankton_biovolume <- phytoplankton_biovolume1[!(phytoplankton_biovolume1$Replicate == "Lake"),]

# b) deleting Treatment = Lake & C 
levels(phytoplankton_biovolume$Treatment)
phytoplankton_biovolume <- phytoplankton_biovolume[!(phytoplankton_biovolume$Treatment %in% c("C", "Lake")),]

# c) change column Lake
phytoplankton_biovolume$Lake<- as.character(phytoplankton_biovolume$Lake)

phytoplankton_biovolume$Lake[phytoplankton_biovolume$Lake == "Asa"] <- "Feresjoen"

phytoplankton_biovolume$Lake[phytoplankton_biovolume$Lake == "Skogaryd"] <- "Erssjoen"

phytoplankton_biovolume$Lake[phytoplankton_biovolume$Lake == "Svartberget"] <- "Stortjaern"

phytoplankton_biovolume$Lake <- as.factor(phytoplankton_biovolume$Lake)

# change column Experiment
phytoplankton_biovolume$Experiment<- as.factor(phytoplankton_biovolume$Experiment)

levels(phytoplankton_biovolume$Experiment)

phytoplankton_biovolume$Experiment<- as.character(phytoplankton_biovolume$Experiment)

phytoplankton_biovolume$Experiment[phytoplankton_biovolume$Experiment == "1"] <- "Spring"

phytoplankton_biovolume$Experiment[phytoplankton_biovolume$Experiment == "2"] <- "Summer"

phytoplankton_biovolume$Experiment <- as.factor(phytoplankton_biovolume$Experiment)

levels(phytoplankton_biovolume$Experiment)

# d) calculate mean values for biovolume 
phytoplankton_biovolume_mean <- phytoplankton_biovolume %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_Biovol=mean(Biovol, na.rm=T))

#-------------------------------------------------------------------------------------------#
# merging both datasets

phyto_stability_biovolume <- merge(x=phytoplankton_biovolume_mean, y=phyto_stability, 
                                   by =c("Lake", "Experiment", "Enclosure", "Treatment"), 
                                   all.x=TRUE)
#-------------------------------------------------------------------------------------------#
# loading diversity dataset
phytoplankton_diversity1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Diversity/diversity_metrics.csv")

# deleting first column
phytoplankton_diversity <- phytoplankton_diversity1[-1] 

# renaming lakes
levels(phytoplankton_diversity$Lake)
# "Bolmen"       "Erken"        "Erssj\xf6n"   "Feresj\xf6n"  "Stortj\xe4rn"

phytoplankton_diversity$Lake<- as.character(phytoplankton_diversity$Lake)

phytoplankton_diversity$Lake[phytoplankton_diversity$Lake == "Feresj\xf6n"] <- "Feresjoen"

phytoplankton_diversity$Lake[phytoplankton_diversity$Lake == "Erssj\xf6n"] <- "Erssjoen"

phytoplankton_diversity$Lake[phytoplankton_diversity$Lake == "Stortj\xe4rn"] <- "Stortjaern"

phytoplankton_diversity$Lake <- as.factor(phytoplankton_diversity$Lake)

levels(phytoplankton_diversity$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# delete all replicate / treatment "lake"
levels(phytoplankton_diversity$Replicate)
phytoplankton_diversity <- phytoplankton_diversity[!(phytoplankton_diversity$Replicate %in% c("lake", "Lake")), ]

levels(phytoplankton_diversity$Treatment)
phytoplankton_diversity <- phytoplankton_diversity[ ! (phytoplankton_diversity$Treatment %in% c("Lake", "C")), ]

# filtering only zooplankton for column "variable" 
phytoplankton_diversity<- phytoplankton_diversity[which(phytoplankton_diversity$variable%in%c('phytoplankton1', 'phytoplankton2')), ]

# calculate mean values for species richness, evenness, effective number of species in extra columns 
phytoplankton_diversity_mean <- phytoplankton_diversity %>%
  group_by(Lake, Experiment,Enclosure, Treatment) %>%
  summarize(mean_J = mean(J, na.rm=T), mean_S = mean(S, na.rm=T), mean_ENS_D = mean(ENS_D, na.rm=T))

# combining datasets
phyto_stability_diversity <- merge(x=phyto_stability, y=phytoplankton_diversity_mean, 
                                   by =c("Lake", "Experiment", "Enclosure", "Treatment"), 
                                   all.x=TRUE)

#-------------------------------------------------------------------------------------------#

## 2. AUC ~ Biovolume ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_phyto_AUC_biovol <- ggplot(data=phyto_stability_biovolume,aes(x=mean_Biovol, y=total_impact, 
                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
 scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Biovolume (log-transformed)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (all treatments)")+
  guides(shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = F
phyto_stability_biovolume_F <- phyto_stability_biovolume[which(phyto_stability_biovolume$Treatment=='F'), ]
# plotting all lakes, all experiments, treatment "F"
plot_phyto_AUC_biovol_F <- ggplot(data=phyto_stability_biovolume_F,aes(x=mean_Biovol, y=total_impact, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Biovolume (log-transformed)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (pulse)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
phyto_stability_biovolume_S <- phyto_stability_biovolume[which(phyto_stability_biovolume$Treatment=='S'), ]
# plotting all lakes, all experiments, treatment "S"
plot_phyto_AUC_biovol_S <- ggplot(data=phyto_stability_biovolume_S,aes(x=mean_Biovol, y=total_impact, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Biovolume (log-transformed)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press)")+
  guides(shape=FALSE,color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
phyto_stability_biovolume_FS <- phyto_stability_biovolume[which(phyto_stability_biovolume$Treatment=='FS'), ]
# plotting all lakes, all experiments, treatment "FS"
plot_phyto_AUC_biovol_FS <- ggplot(data=phyto_stability_biovolume_FS,aes(x=mean_Biovol, y=total_impact, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Biovolume (log-transformed)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press & pulse)")+
  guides(shape=FALSE, color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_AUC_biovol, plot_phyto_AUC_biovol_F, plot_phyto_AUC_biovol_S, 
             plot_phyto_AUC_biovol_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 3. AUC ~ Evenness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_phyto_AUC_evenness <- ggplot(data=phyto_stability_diversity,aes(x=mean_J, y=total_impact, 
                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (all treatments)")+
  guides(shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = F
phyto_stability_diversity_F <- phyto_stability_diversity[which(phyto_stability_diversity$Treatment=='F'), ]
# plotting all lakes, all experiments, treatment "F"
plot_phyto_AUC_evenness_F <- ggplot(data=phyto_stability_diversity_F,aes(x=mean_J, y=total_impact, 
                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (pulse)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
phyto_stability_diversity_S <- phyto_stability_diversity[which(phyto_stability_diversity$Treatment=='S'), ]
# plotting all lakes, all experiments, treatment "S"
plot_phyto_AUC_evenness_S <- ggplot(data=phyto_stability_diversity_S,aes(x=mean_J, y=total_impact, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press)")+
  guides(shape=FALSE,color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
phyto_stability_diversity_FS <- phyto_stability_diversity[which(phyto_stability_diversity$Treatment=='FS'), ]
# plotting all lakes, all experiments, treatment "FS"
plot_phyto_AUC_evenness_FS <- ggplot(data=phyto_stability_diversity_FS,aes(x=mean_J, y=total_impact, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press & pulse)")+
  guides(shape=FALSE, color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_AUC_evenness, plot_phyto_AUC_evenness_F, plot_phyto_AUC_evenness_S, 
             plot_phyto_AUC_evenness_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 4. AUC ~ Richness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_phyto_AUC_richness <- ggplot(data=phyto_stability_diversity,aes(x=mean_S, y=total_impact, 
                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (all treatments)")+
  guides(shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_phyto_AUC_richness_F <- ggplot(data=phyto_stability_diversity_F,aes(x=mean_S, y=total_impact, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (pulse)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_phyto_AUC_richness_S <- ggplot(data=phyto_stability_diversity_S,aes(x=mean_S, y=total_impact, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press)")+
  guides(shape=FALSE,color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_phyto_AUC_richness_FS <- ggplot(data=phyto_stability_diversity_FS,aes(x=mean_S, y=total_impact, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press & pulse)")+
  guides(shape=FALSE, color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_AUC_richness, plot_phyto_AUC_richness_F, plot_phyto_AUC_richness_S, 
             plot_phyto_AUC_richness_FS, ncol=2)
#-------------------------------------------------------------------------------------------#

## 5. AUC ~ ENS ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_phyto_AUC_ENS <- ggplot(data=phyto_stability_diversity,aes(x=mean_ENS_D, y=total_impact, 
                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (all treatments)")+
  guides(shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_phyto_AUC_ENS_F <- ggplot(data=phyto_stability_diversity_F,aes(x=mean_ENS_D, y=total_impact, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (pulse)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_phyto_AUC_ENS_S <- ggplot(data=phyto_stability_diversity_S,aes(x=mean_ENS_D, y=total_impact, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press)")+
  guides(shape=FALSE,color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_phyto_AUC_ENS_FS <- ggplot(data=phyto_stability_diversity_FS,aes(x=mean_ENS_D, y=total_impact, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press & pulse)")+
  guides(shape=FALSE, color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_AUC_ENS, plot_phyto_AUC_ENS_F, plot_phyto_AUC_ENS_S, 
             plot_phyto_AUC_ENS_FS, ncol=2)
#-------------------------------------------------------------------------------------------#

## 6. Recovery ~ Biovolume ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_phyto_recovery_biovol <- ggplot(data=phyto_stability_biovolume,aes(x=mean_Biovol, y=final_stab, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
 scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Biovolume (log-transformed)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (all treatments)")+
  guides(shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_phyto_recovery_biovol_F <- ggplot(data=phyto_stability_biovolume_F,aes(x=mean_Biovol, y=final_stab, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Biovolume (log-transformed)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (pulse)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_phyto_recovery_biovol_S <- ggplot(data=phyto_stability_biovolume_S,aes(x=mean_Biovol, y=final_stab, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
 scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Biovolume (log-transformed)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press)")+
  guides(shape=FALSE,color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_phyto_recovery_biovol_FS <- ggplot(data=phyto_stability_biovolume_FS,aes(x=mean_Biovol, y=final_stab, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Biovolume (log-transformed)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press & pulse)")+
  guides(shape=FALSE, color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_recovery_biovol, plot_phyto_recovery_biovol_F, plot_phyto_recovery_biovol_S, 
             plot_phyto_recovery_biovol_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 7. Recovery ~ Evenness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_phyto_recovery_evenness <- ggplot(data=phyto_stability_diversity,aes(x=mean_J, y=final_stab, 
                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (all treatments)")+
  guides(shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_phyto_recovery_evenness_F <- ggplot(data=phyto_stability_diversity_F,aes(x=mean_J, y=final_stab, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (pulse)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_phyto_recovery_evenness_S <- ggplot(data=phyto_stability_diversity_S,aes(x=mean_J, y=final_stab, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press)")+
  guides(shape=FALSE,color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_phyto_recovery_evenness_FS <- ggplot(data=phyto_stability_diversity_FS,aes(x=mean_J, y=final_stab, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press & pulse)")+
  guides(shape=FALSE, color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_recovery_evenness, plot_phyto_recovery_evenness_F, plot_phyto_recovery_evenness_S, 
             plot_phyto_recovery_evenness_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 8. Recovery ~ Richness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_phyto_recovery_richness <- ggplot(data=phyto_stability_diversity,aes(x=mean_S, y=final_stab, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (all treatments)")+
  guides(shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_phyto_recovery_richness_F <- ggplot(data=phyto_stability_diversity_F,aes(x=mean_S, y=final_stab, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (pulse)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_phyto_recovery_richness_S <- ggplot(data=phyto_stability_diversity_S,aes(x=mean_S, y=final_stab, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press)")+
  guides(shape=FALSE,color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_phyto_recovery_richness_FS <- ggplot(data=phyto_stability_diversity_FS,aes(x=mean_S, y=final_stab, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press & pulse)")+
  guides(shape=FALSE, color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_recovery_richness, plot_phyto_recovery_richness_F, plot_phyto_recovery_richness_S, 
             plot_phyto_recovery_richness_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 9. Recovery ~ Effective number of species ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_phyto_recovery_ENS <- ggplot(data=phyto_stability_diversity,aes(x=mean_ENS_D, y=final_stab, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (all treatments)")+
  guides(shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_phyto_recovery_ENS_F <- ggplot(data=phyto_stability_diversity_F,aes(x=mean_ENS_D, y=final_stab, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (pulse)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_phyto_recovery_ENS_S <- ggplot(data=phyto_stability_diversity_S,aes(x=mean_ENS_D, y=final_stab, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press)")+
  guides(shape=FALSE,color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_phyto_recovery_ENS_FS <- ggplot(data=phyto_stability_diversity_FS,aes(x=mean_ENS_D, y=final_stab, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press & pulse)")+
  guides(shape=FALSE, color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_recovery_ENS, plot_phyto_recovery_ENS_F, plot_phyto_recovery_ENS_S, 
             plot_phyto_recovery_ENS_FS, ncol=2)


#-------------------------------------------------------------------------------------------#

## 10. Resistance ~ Biovolume ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_phyto_resistance_biovol_F <- ggplot(data=phyto_stability_biovolume_F,aes(x=mean_Biovol, y=initial_stab, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Biovolume (log-transformed)") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (pulse)")+
  guides(shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_phyto_resistance_biovol_FS <- ggplot(data=phyto_stability_biovolume_FS,aes(x=mean_Biovol, y=initial_stab, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Biovolume (log-transformed)") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press & pulse)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 11. Resistance ~ Evenness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_phyto_resistance_evenness_F <- ggplot(data=phyto_stability_diversity_F,aes(x=mean_J, y=initial_stab, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color=FALSE, shape = FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_phyto_resistance_evenness_FS <- ggplot(data=phyto_stability_diversity_FS,aes(x=mean_J, y=initial_stab, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(shape=FALSE, color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_resistance_biovol_F, plot_phyto_resistance_biovol_FS, 
             plot_phyto_resistance_evenness_F, plot_phyto_resistance_evenness_FS,
             ncol=2)

#-------------------------------------------------------------------------------------------#

## 12. Resistance ~ Richness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_phyto_resistance_richness_F <- ggplot(data=phyto_stability_diversity_F,aes(x=mean_S, y=initial_stab, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (pulse)")+
  guides(shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_phyto_resistance_richness_FS <- ggplot(data=phyto_stability_diversity_FS,aes(x=mean_S, y=initial_stab, 
                                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press & pulse)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 13. Resistance ~ Effective number of species ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_phyto_resistance_ENS_F <- ggplot(data=phyto_stability_diversity_F,aes(x=mean_ENS_D, y=initial_stab, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color=FALSE, shape = FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_phyto_resistance_ENS_FS <- ggplot(data=phyto_stability_diversity_FS,aes(x=mean_ENS_D, y=initial_stab, 
                                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(shape=FALSE, color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


grid.arrange(plot_phyto_resistance_richness_F, plot_phyto_resistance_richness_FS, 
             plot_phyto_resistance_ENS_FS, plot_phyto_resistance_ENS_F, 
             ncol=2)


#-------------------------------------------------------------------------------------------#

## 14. Resilience ~ Biovolume ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_phyto_resilience_biovol_F <- ggplot(data=phyto_stability_biovolume_F,aes(x=mean_Biovol, y=rate_change_ort, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Biovolume (log-transformed)") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (pulse)")+
  guides(shape = FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_phyto_resilience_biovol_S <- ggplot(data=phyto_stability_biovolume_S,aes(x=mean_Biovol, y=rate_change_oti, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Biovolume (log-transformed)") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 15. Resilience ~ Evenness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_phyto_resilience_evenness_F <- ggplot(data=phyto_stability_diversity_F,aes(x=mean_J, y=rate_change_ort, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(shape = FALSE, color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_phyto_resilience_evenness_S <- ggplot(data=phyto_stability_diversity_S,aes(x=mean_J, y=rate_change_oti, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color=FALSE, shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_resilience_biovol_F, plot_phyto_resilience_biovol_S, 
             plot_phyto_resilience_evenness_F,plot_phyto_resilience_evenness_S,
             ncol=2)


#-------------------------------------------------------------------------------------------#

## 16. Resilience ~ Richness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_phyto_resilience_richness_F <- ggplot(data=phyto_stability_diversity_F,aes(x=mean_S, y=rate_change_ort, 
                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (pulse)")+
  guides(shape = FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_phyto_resilience_richness_S <- ggplot(data=phyto_stability_diversity_S,aes(x=mean_S, y=rate_change_oti, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton stability ~ diversity (press)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 17. Resilience ~ ENS ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all experiments, treatment "F"
plot_phyto_resilience_ENS_F <- ggplot(data=phyto_stability_diversity_F,aes(x=mean_ENS_D, y=rate_change_ort, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(shape = FALSE, color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_phyto_resilience_ENS_S <- ggplot(data=phyto_stability_diversity_S,aes(x=mean_ENS_D, y=rate_change_oti, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color=FALSE, shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_resilience_richness_F, plot_phyto_resilience_richness_S,
             plot_phyto_resilience_ENS_F, plot_phyto_resilience_ENS_S, 
             nrow=2)
