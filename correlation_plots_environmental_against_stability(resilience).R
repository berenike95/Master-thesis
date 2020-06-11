#-------------------------------------------------------------------------------------------#

## Environmental parameters ~ stability (resilience) correlation plots ####

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

## 7. TP, TN, DOC ~ stability(resilience) for pulse ####

#-------------------------------------------------------------------------------------------#

# TP for all communities and all lakes (pulse disturbance)
function_env_stability_pulse <- env_stability[which(env_stability$variable==c('phyto_function', 
                            'zoop_function', 'bact_function') &  env_stability$Treatment=='F' ), ]

plot_TP_pulse_resilience <- ggplot(data=function_env_stability_pulse, aes(x=rate_change_ort, 
                                          y=TP, color = variable, shape = Lake)) +
  labs(color = "Community") +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = variable)) +
  xlab("Stability (resilience)") +
  ylab("Total phosphorus") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Resilience for all lakes and all communities (pulse)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# TN for all communities and all lakes (pulse disturbance)

function_env_stability_pulse$TN <- as.numeric(function_env_stability_pulse$TN)

plot_TN_pulse_resilience <- ggplot(data=function_env_stability_pulse, aes(x=rate_change_ort, 
                               y=TN, color = variable, shape = Lake)) +
  labs(color = "Community") +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = variable)) +
  xlab("Stability (resilience)") +
  ylab("Total nitrogen") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("   ")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# DOC for all communities and all lakes (pulse disturbance)

plot_DOC_pulse_resilience <- ggplot(data=function_env_stability_pulse, aes(x=rate_change_ort, 
                                          y=DOC, color = variable, shape = Lake)) +
  labs(color = "Community") +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = variable)) +
  xlab("Stability (resilience)") +
  ylab("DOC") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_TP_pulse_resilience, plot_TN_pulse_resilience, plot_DOC_pulse_resilience, ncol = 2)

#-------------------------------------------------------------------------------------------#

## 8. Temp, Par ~ stability(resilience) for pulse ####

#-------------------------------------------------------------------------------------------#

# Temp for all communities and all lakes (pulse disturbance)

plot_Temp_pulse_resilience <- ggplot(data=function_env_stability_pulse, aes(x=rate_change_ort, 
                                      y=Temperature, color = variable, shape = Lake)) +
  labs(color = "Community") +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = variable)) +
  xlab("Stability (resilience)") +
  ylab("Temperature") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Resilience for all lakes and all communities (pulse)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# PAR for all communities and all lakes (pulse disturbance)

plot_PAR_pulse_resilience <- ggplot(data=function_env_stability_pulse, aes(x=rate_change_ort, 
                                y=PAR_I, color = variable, shape = Lake)) +
  labs(color = "Community") +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = variable)) +
  xlab("Stability (resilience)") +
  ylab("PAR") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("   ")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_Temp_pulse_resilience, plot_PAR_pulse_resilience, ncol = 2)

#-------------------------------------------------------------------------------------------#

## 9. TP, TN, DOC ~ stability(resilience) for press ####

#-------------------------------------------------------------------------------------------#

# TP for all communities and all lakes (press disturbance)
function_env_stability_press <- env_stability[which(env_stability$variable==c('phyto_function', 
                                     'zoop_function', 'bact_function') &  env_stability$Treatment=='S' ), ]

plot_TP_press_resilience <- ggplot(data=function_env_stability_press, aes(x=rate_change_oti, 
                                                y=TP, color = variable, shape = Lake)) +
  labs(color = "Community") +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = variable)) +
  xlab("Stability (resilience)") +
  ylab("Total phosphorus") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Resilience for all lakes and all communities (press)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# TN for all communities and all lakes (press disturbance)

function_env_stability_press$TN <- as.numeric(function_env_stability_press$TN)

plot_TN_press_resilience <- ggplot(data=function_env_stability_press, aes(x=rate_change_oti, 
                                  y=TN, color = variable, shape = Lake)) +
  labs(color = "Community") +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = variable)) +
  xlab("Stability (resilience)") +
  ylab("Total nitrogen") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("   ")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# DOC for all communities and all lakes (press disturbance)

plot_DOC_press_resilience <- ggplot(data=function_env_stability_press, aes(x=rate_change_oti, 
                                           y=DOC, color = variable, shape = Lake)) +
  labs(color = "Community") +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = variable)) +
  xlab("Stability (resilience)") +
  ylab("DOC") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_TP_press_resilience, plot_TN_press_resilience, plot_DOC_press_resilience, ncol = 2)

#-------------------------------------------------------------------------------------------#

## 10. Temp, Par ~ stability(resilience) for press ####

#-------------------------------------------------------------------------------------------#

# Temp for all communities and all lakes (press disturbance)

plot_Temp_press_resilience <- ggplot(data=function_env_stability_press, aes(x=rate_change_oti, 
                                             y=Temperature, color = variable, shape = Lake)) +
  labs(color = "Community") +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = variable)) +
  xlab("Stability (resilience)") +
  ylab("Temperature") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Resilience for all lakes and all communities (press)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# PAR for all communities and all lakes (press disturbance)

plot_PAR_press_resilience <- ggplot(data=function_env_stability_press, aes(x=rate_change_oti, 
                                                  y=PAR_I, color = variable, shape = Lake)) +
  labs(color = "Community") +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = variable)) +
  xlab("Stability (resilience)") +
  ylab("PAR") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("   ")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_Temp_press_resilience, plot_PAR_press_resilience, ncol = 2)


