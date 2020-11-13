#-------------------------------------------------------------------------------------------#

## Stability ~ community structure variables (Bacteria) ####

#-------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------#

## 0. About ####

#-------------------------------------------------------------------------------------------#
# stability measures:
# AUC (total_impact)
# resistance (initial_stab) for pulse
# recovery (final_stab) for pulse & press
# resilience (rate_change_ort, rate_change_oti) for pulse & press

# community structure variables:
# Evenness (Bacteria)
# Richness (Bacteria)
# Effective number of species (Bacteria)
#-------------------------------------------------------------------------------------------#

## 1. Load packages ####

#-------------------------------------------------------------------------------------------#
library(tidyverse)
library(scales)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(gdata)
#-------------------------------------------------------------------------------------------#

## 2. Load and transform stability dataset ####

#-------------------------------------------------------------------------------------------#
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

# filtering variable for bacteria only 
bact_stability <- stability[which(stability$variable=='bact_function'), ]

# only filtering the parameters we need ()
bact_stability <- select(bact_stability, Lake, Experiment, Enclosure, 
                                Treatment, final_stab, initial_stab, 
                         rate_change_ort, rate_change_oti, total_impact)

#-------------------------------------------------------------------------------------------#

## 3. Load and transform diversity dataset ####

#-------------------------------------------------------------------------------------------#
# loading bacteria diversity dataset
bact_diversity1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Diversity/diversity_metrics.csv")

# deleting first column
bact_diversity <- bact_diversity1[-1] 

# renaming lakes
levels(bact_diversity$Lake)

# "Bolmen"       "Erken"        "Erssj\xf6n"   "Feresj\xf6n"  "Stortj\xe4rn"

bact_diversity$Lake<- as.character(bact_diversity$Lake)

bact_diversity$Lake[bact_diversity$Lake == "Feresj\xf6n"] <- "Feresjoen"

bact_diversity$Lake[bact_diversity$Lake == "Erssj\xf6n"] <- "Erssjoen"

bact_diversity$Lake[bact_diversity$Lake == "Stortj\xe4rn"] <- "Stortjaern"

bact_diversity$Lake <- as.factor(bact_diversity$Lake)

levels(bact_diversity$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# delete all replicate / treatment "lake"
levels(bact_diversity$Replicate)
bact_diversity <- bact_diversity[!(bact_diversity$Replicate %in% c("lake", "Lake")), ]

levels(bact_diversity$Treatment)
bact_diversity <- bact_diversity[ ! (bact_diversity$Treatment %in% c("C", "Lake")), ]


# filtering only zooplankton for column "variable" 
bact_diversity<- bact_diversity[which(bact_diversity$variable=="bacteria"), ]

# calculate mean values for species richness, evenness, effective number of species in extra columns 
bact_diversity_mean <- bact_diversity %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_J = mean(J, na.rm=T), mean_S = mean(S, na.rm=T), mean_ENS_D = mean(ENS_D, na.rm=T))

# combining datasets
bact_stability_diversity <- merge(x=bact_stability, y=bact_diversity_mean, by =c("Lake", "Experiment", 
                                          "Enclosure", "Treatment"), all.x=TRUE)

#-------------------------------------------------------------------------------------------#

## 4. AUC ~ evenness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_bact_AUC_evenness <- ggplot(data=bact_stability_diversity,aes(x=mean_J, y=total_impact, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = F
bact_stability_diversity_F <- bact_stability_diversity[which(bact_stability_diversity$Treatment=='F'), ]

# plotting all lakes, all experiments, treatment "F"
plot_bact_AUC_evenness_F <- ggplot(data=bact_stability_diversity_F, aes(x=mean_J, y=total_impact, 
                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (pulse disturbance)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
bact_stability_diversity_S <- bact_stability_diversity[which(bact_stability_diversity$Treatment=='S'), ]

# plotting all lakes, all experiments, treatment "S"
plot_bact_AUC_evenness_S <- ggplot(data=bact_stability_diversity_S, aes(x=mean_J, y=total_impact, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (press disturbance)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
bact_stability_diversity_FS <- bact_stability_diversity[which(bact_stability_diversity$Treatment=='FS'), ]

# plotting all lakes, all experiments, treatment "FS"
plot_bact_AUC_evenness_FS <- ggplot(data=bact_stability_diversity_FS, aes(x=mean_J, y=total_impact, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (press & pulse disturbance)")+
  theme(legend.title = element_blank()) +
  guides(shape = FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bact_AUC_evenness, plot_bact_AUC_evenness_F, plot_bact_AUC_evenness_S, 
             plot_bact_AUC_evenness_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 5. AUC ~ richness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, all treatments
plot_bact_AUC_richness <- ggplot(data=bact_stability_diversity,aes(x=mean_S, y=total_impact, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_bact_AUC_richness_F <- ggplot(data=bact_stability_diversity_F, aes(x=mean_S, y=total_impact, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (pulse disturbance)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_bact_AUC_richness_S <- ggplot(data=bact_stability_diversity_S, aes(x=mean_S, y=total_impact, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (press disturbance)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_bact_AUC_richness_FS <- ggplot(data=bact_stability_diversity_FS, aes(x=mean_S, y=total_impact, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (press & pulse disturbance)")+
  theme(legend.title = element_blank()) +
  guides(shape = FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bact_AUC_richness, plot_bact_AUC_richness_F, plot_bact_AUC_richness_S, 
             plot_bact_AUC_richness_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 6. AUC ~ ENS ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, all treatments
plot_bact_AUC_ENS <- ggplot(data=bact_stability_diversity,aes(x=mean_ENS_D, y=total_impact, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_bact_AUC_ENS_F <- ggplot(data=bact_stability_diversity_F, aes(x=mean_ENS_D, y=total_impact, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (pulse disturbance)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_bact_AUC_ENS_S <- ggplot(data=bact_stability_diversity_S, aes(x=mean_ENS_D, y=total_impact, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (press disturbance)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_bact_AUC_ENS_FS <- ggplot(data=bact_stability_diversity_FS, aes(x=mean_ENS_D, y=total_impact, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (press & pulse disturbance)")+
  theme(legend.title = element_blank()) +
  guides(shape = FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bact_AUC_ENS, plot_bact_AUC_ENS_F, plot_bact_AUC_ENS_S, 
             plot_bact_AUC_ENS_FS, ncol=2)
#-------------------------------------------------------------------------------------------#

## 7. Recovery ~ Evenness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_bact_recovery_evenness <- ggplot(data=bact_stability_diversity,aes(x=mean_J, y=final_stab, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_bact_recovery_evenness_F <- ggplot(data=bact_stability_diversity_F, aes(x=mean_J, y=final_stab, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (pulse disturbance)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_bact_recovery_evenness_S <- ggplot(data=bact_stability_diversity_S, aes(x=mean_J, y=final_stab, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (press disturbance)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_bact_recovery_evenness_FS <- ggplot(data=bact_stability_diversity_FS, aes(x=mean_J, y=final_stab, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (press & pulse disturbance)")+
  theme(legend.title = element_blank()) +
  guides(shape = FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bact_recovery_evenness, plot_bact_recovery_evenness_F, plot_bact_recovery_evenness_S, 
             plot_bact_recovery_evenness_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 8. Recovery ~ Richness ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, all treatments
plot_bact_recovery_richness <- ggplot(data=bact_stability_diversity,aes(x=mean_S, y=final_stab, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_bact_recovery_richness_F <- ggplot(data=bact_stability_diversity_F, aes(x=mean_S, y=final_stab, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (pulse disturbance)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_bact_recovery_richness_S <- ggplot(data=bact_stability_diversity_S, aes(x=mean_S, y=final_stab, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (press disturbance)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_bact_recovery_richness_FS <- ggplot(data=bact_stability_diversity_FS, aes(x=mean_S, y=final_stab, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (press & pulse disturbance)")+
  theme(legend.title = element_blank()) +
  guides(shape = FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bact_recovery_richness, plot_bact_recovery_richness_F, plot_bact_recovery_richness_S, 
             plot_bact_recovery_richness_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 9. Recovery ~ ENS ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, all treatments
plot_bact_recovery_ENS <- ggplot(data=bact_stability_diversity,aes(x=mean_ENS_D, y=final_stab, 
                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (all treatments)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_bact_recovery_ENS_F <- ggplot(data=bact_stability_diversity_F, aes(x=mean_ENS_D, y=final_stab, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (pulse disturbance)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_bact_recovery_ENS_S <- ggplot(data=bact_stability_diversity_S, aes(x=mean_ENS_D, y=final_stab, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (press disturbance)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_bact_recovery_ENS_FS <- ggplot(data=bact_stability_diversity_FS, aes(x=mean_ENS_D, y=final_stab, 
                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=3) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (press & pulse disturbance)")+
  theme(legend.title = element_blank()) +
  guides(shape = FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bact_recovery_ENS, plot_bact_recovery_ENS_F, plot_bact_recovery_ENS_S, 
             plot_bact_recovery_ENS_FS, ncol=2)
#-------------------------------------------------------------------------------------------#

## 10. Resistance (for pulse disturbance) ~ Evenness / Richness / ENS ####

#-------------------------------------------------------------------------------------------#
# Resistance ~ Evenness for bacteria

# deleting press disturbance
bact_stability_diversity_resistance <- bact_stability_diversity[!(bact_stability_diversity$Treatment=="S"),]

# plotting resistance ~ evenness for all lakes, all experiments, treatment F

# filtering only Treatment = F
bact_stability_diversity_resistance_F <- bact_stability_diversity_resistance[which(bact_stability_diversity_resistance$Treatment=='F'), ]

plot_bact_resistance_evenness_F <- ggplot(data=bact_stability_diversity_resistance_F,aes(x=mean_J, y=initial_stab, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (pulse)")+
  guides(shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


# filtering only Treatment = FS
bact_stability_diversity_resistance_FS <- bact_stability_diversity_resistance[which(bact_stability_diversity_resistance$Treatment=='FS'), ]

plot_bact_resistance_evenness_FS <- ggplot(data=bact_stability_diversity_resistance_FS,aes(x=mean_J, y=initial_stab, 
                                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (pulse & press)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


# Resistance ~ Richness for bacteria

# plotting resistance ~ evenness for all lakes, all experiments, treatment F
plot_bact_resistance_richness_F <- ggplot(data=bact_stability_diversity_resistance_F,aes(x=mean_S, y=initial_stab, 
                                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color= FALSE, shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bact_resistance_richness_FS <- ggplot(data=bact_stability_diversity_resistance_FS,aes(x=mean_S, y=initial_stab, 
                                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color=FALSE, shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# Resistance ~ ENS for bacteria

plot_bact_resistance_ENS_F <- ggplot(data=bact_stability_diversity_resistance_F,aes(x=mean_ENS_D, y=initial_stab, 
                                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color=FALSE, shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bact_resistance_ENS_FS <- ggplot(data=bact_stability_diversity_resistance_FS,aes(x=mean_ENS_D, y=initial_stab, 
                                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_x_continuous(trans = "log", labels = comma) +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color=FALSE, shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bact_resistance_evenness_F, plot_bact_resistance_evenness_FS, plot_bact_resistance_richness_F, 
             plot_bact_resistance_richness_FS, plot_bact_resistance_ENS_F, plot_bact_resistance_ENS_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 11. Resilience (for pulse & press disturbance) ~ Evenness / Richness / ENS ####

#-------------------------------------------------------------------------------------------#
# Resilience ~ Evenness for bacteria (pulse disturbance)

plot_bact_resilience_evenness_F <- ggplot(data=bact_stability_diversity_F,aes(x=mean_J, 
                       y=rate_change_ort, color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (pulse)")+
  guides(shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bact_resilience_evenness_S <- ggplot(data=bact_stability_diversity_S,aes(x=mean_J, 
                                             y=rate_change_oti, color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Evenness") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria stability ~ diversity (press)")+
  guides(color=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# Resilience ~ Richness for bacteria

plot_bact_resilience_richness_F <- ggplot(data=bact_stability_diversity_F,aes(x=mean_S, 
                                                                              y=rate_change_ort, color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color=FALSE, shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bact_resilience_richness_S <- ggplot(data=bact_stability_diversity_S,aes(x=mean_S, 
                                                                              y=rate_change_oti, color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Richness") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color=FALSE, shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# Resilience ~ ENS for bacteria

plot_bact_resilience_ENS_F <- ggplot(data=bact_stability_diversity_F,aes(x=mean_ENS_D, 
                                                                              y=rate_change_ort, color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color=FALSE, shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bact_resilience_ENS_S <- ggplot(data=bact_stability_diversity_S,aes(x=mean_ENS_D, 
                                              y=rate_change_oti, color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Effective number of species (Simpson)") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color=FALSE,shape=FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bact_resilience_evenness_F, plot_bact_resilience_evenness_S, 
             plot_bact_resilience_richness_F , plot_bact_resilience_richness_S, 
             plot_bact_resilience_ENS_F, plot_bact_resilience_ENS_S, ncol=2)




