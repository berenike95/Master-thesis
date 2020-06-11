#-------------------------------------------------------------------------------------------#

## Environmental parameters ~ stability (resistance) correlation plots ####

#-------------------------------------------------------------------------------------------#



#-------------------------------------------------------------------------------------------#

## 0. About ####

#-------------------------------------------------------------------------------------------#
# environmental parameters:
# TP
# TN 
# light (PAR I)
# DOC 
# temperature 

# stability measures:
# resistance (pulse)
# recovery (pulse)
# resilience (pulse & press)

#-------------------------------------------------------------------------------------------#

## 1. load packages and datasets ####

#-------------------------------------------------------------------------------------------#
# load packages
library(tidyverse)
library(scales)
library(gridExtra)
library(ggplot2)

show(zooplankton_biomass$Sampling_day)

# load stability measures:
stability1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Stability/stab_stand_yes_c.csv", sep=",")

# load environmental parameters:
env_parameters1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Chemistry_Fish_SITES_AquaNet_2017 Kopie.csv", sep=";")



#-------------------------------------------------------------------------------------------#

## 2. deleting values in env_parameters ####

#-------------------------------------------------------------------------------------------#


# a) Deleting all Sampling_day = 21


env_parameters1 <- env_parameters1[!(env_parameters1$Sampling_day ==21),]

#-------------------------------------------------------------------------------------------#

# b) deleting all rows with Enclosures = 1, 7, 10, 16, 21 (controls)


env_parameters1 <- env_parameters1[ ! (env_parameters1$Enclosure %in% c(1, 7, 10, 16,21)), ]

#-------------------------------------------------------------------------------------------#


## 3. filtering datasets ####

#-------------------------------------------------------------------------------------------#

# only filtering the parameters we need (TP, TN, DOC, Temperature, Chla_flu, PAR_I)
env_parameters <- select(env_parameters1, Lake, Experiment,Enclosure, 
                         Treatment, Replicate, TP, TN, DOC, Temperature,Chla_flu, PAR_I)

# only filtering the data points that we need 
stability <- select(stability1, Lake, Experiment, Enclosure, Treatment, Replicate, variable, 
                  initial_stab, final_stab, rate_change_ort, rate_change_oti)
#-------------------------------------------------------------------------------------------#

## 4. changing names in column "Experiment" ####

#-------------------------------------------------------------------------------------------#


# changing Spring = 1 and Summer = 2 in dataset "stability" in column "Experiment"

stability$Experiment <- as.character(stability$Experiment)

stability$Experiment[stability$Experiment == "Spring"] <- 1

stability$Experiment[stability$Experiment == "Summer"] <- 2

stability$Experiment <- as.integer(stability$Experiment)

#-------------------------------------------------------------------------------------------#

## 5. changing names in column "Lakes" ####

#-------------------------------------------------------------------------------------------#

# stability dataset

levels(stability$Lake)
#  "Bolmen"       "Erken"        "Erssj\xf6n"   "Feresj\xf6n"  "Stortj\xe4rn"

stability$Lake<- as.character(stability$Lake)

stability$Lake[stability$Lake == "Erssj\xf6n"] <- "Erssjoen"

stability$Lake[stability$Lake == "Feresj\xf6n"] <- "Feresjoen"

stability$Lake[stability$Lake == "Stortj\xe4rn"] <- "Stortjaern"

stability$Lake <- as.factor(stability$Lake)

levels(stability$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

#-------------------------------------------------------------------------------------------#

# environmental parameters dataset 

levels(env_parameters$Lake)
# "Asa"         "Bolmen"      "Erken"       "Skogaryd"    "Svartberget"

env_parameters$Lake<- as.character(env_parameters$Lake)

env_parameters$Lake[env_parameters$Lake == "Asa"] <- "Feresjoen"

env_parameters$Lake[env_parameters$Lake == "Skogaryd"] <- "Erssjoen"

env_parameters$Lake[env_parameters$Lake == "Svartberget"] <- "Stortjaern"

env_parameters$Lake <- as.factor(env_parameters$Lake)

levels(env_parameters$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"



#-------------------------------------------------------------------------------------------#

## 6. merging both datasets ####

#-------------------------------------------------------------------------------------------#


env_stability <- merge(x=stability, y=env_parameters, by =c("Lake", "Experiment", 
                                                            "Enclosure", "Treatment", "Replicate"), all.x=TRUE)

#-------------------------------------------------------------------------------------------#

## 7. TP ~ stability(resistance) for pulse ####

#-------------------------------------------------------------------------------------------#

# TP for phytoplankton
# filtering only phytoplankton_function for column "variable" 
function_env_stability_phyto <- env_stability[which(env_stability$variable=='phyto_function'), ]

plot_TP_phyto_resistance <- ggplot(data=function_env_stability_phyto, aes(x=initial_stab, 
                                                               y=TP, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  ylab("Total phosphorus") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# TP for zooplankton
function_env_stability_zoop <- env_stability[which(env_stability$variable=='zoop_function'), ]

plot_TP_zoop_resistance <- ggplot(data=function_env_stability_zoop, aes(x=initial_stab, 
                                                             y=TP, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


# TP for Baceteria
function_env_stability_bact <- env_stability[which(env_stability$variable=='bact_function'), ]

plot_TP_bact_resistance <- ggplot(data=function_env_stability_bact, aes(x=initial_stab, 
                                                                        y=TP, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_TP_phyto_resistance, plot_TP_zoop_resistance, plot_TP_bact_resistance, ncol = 2)

#-------------------------------------------------------------------------------------------#

## 8. TN ~ stability(resistance) for pulse ####

#-------------------------------------------------------------------------------------------#

# TN for Phyotplankton 
# setting TN as numeric variable
function_env_stability_phyto$TN <- as.numeric(function_env_stability_phyto$TN)

plot_TN_phyto_resistance <- ggplot(data=function_env_stability_phyto, aes(x=initial_stab, 
                                                                          y=TN, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  ylab("Total nitrogen") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


# TN for Zooplannkton
# setting TN as numeric variable
function_env_stability_zoop$TN <- as.numeric(function_env_stability_zoop$TN)
plot_TN_zoop_resistance <- ggplot(data=function_env_stability_zoop, aes(x=initial_stab, 
                                                                        y=TN, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


# TN for Bacteria
# setting TN as numeric variable
function_env_stability_bact$TN <- as.numeric(function_env_stability_bact$TN)

plot_TN_bact_resistance <- ggplot(data=function_env_stability_bact, aes(x=initial_stab, 
                                                                        y=TN, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method= lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_TN_phyto_resistance, plot_TN_zoop_resistance, plot_TN_bact_resistance, ncol = 2)

#-------------------------------------------------------------------------------------------#

## 9. DOC ~ stability(resistance) for pulse ####

#-------------------------------------------------------------------------------------------#

# DOC for phyto

plot_DOC_phyto_resistance <- ggplot(data=function_env_stability_phyto, aes(x=initial_stab, 
                                                                          y=DOC, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  ylab("DOC") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# DOC for zooplankton

plot_DOC_zoop_resistance <- ggplot(data=function_env_stability_zoop, aes(x=initial_stab, 
                                                                        y=DOC, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# DOC for Bacteria 

plot_DOC_bact_resistance <- ggplot(data=function_env_stability_bact, aes(x=initial_stab, 
                                                                        y=DOC, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_DOC_phyto_resistance, plot_DOC_zoop_resistance, plot_DOC_bact_resistance, ncol = 2)

#-------------------------------------------------------------------------------------------#

## 10. Temperature ~ stability(resistance) for pulse ####

#-------------------------------------------------------------------------------------------#

# Temperature for Phytoplankton

plot_Temp_phyto_resistance <- ggplot(data=function_env_stability_phyto, aes(x=initial_stab, 
                                                                           y=Temperature, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  ylab("Temperature") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# Temperature for Zooplankton

plot_Temp_zoop_resistance <- ggplot(data=function_env_stability_zoop, aes(x=initial_stab, 
                                                                         y=Temperature, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# Temperature for Bacteria

plot_Temp_bact_resistance <- ggplot(data=function_env_stability_bact, aes(x=initial_stab, 
                                                                         y=Temperature, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_Temp_phyto_resistance, plot_Temp_zoop_resistance, plot_Temp_bact_resistance, ncol = 2)

#-------------------------------------------------------------------------------------------#

## 11. PAR ~ stability(resistance) for pulse ####

#-------------------------------------------------------------------------------------------#

# PAR for Phytoplankton

plot_PAR_phyto_resistance <- ggplot(data=function_env_stability_phyto, aes(x=initial_stab, 
                                                                            y=PAR_I, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  ylab("PAR") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# PAR for Zooplankton

plot_PAR_zoop_resistance <- ggplot(data=function_env_stability_zoop, aes(x=initial_stab, 
                                                                          y=PAR_I, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# PAR for Bacteria 

plot_PAR_bact_resistance <- ggplot(data=function_env_stability_bact, aes(x=initial_stab, 
                                      y=PAR_I, color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = Treatment)) +
  xlab("Stability (resistance)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria functional stability (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_PAR_phyto_resistance, plot_PAR_zoop_resistance, plot_PAR_bact_resistance, ncol = 2)

