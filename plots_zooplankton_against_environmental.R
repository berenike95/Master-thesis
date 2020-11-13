#-------------------------------------------------------------------------------------------#

## Community structure variables correlation plots for Zooplankton ~ environmental parameters####

#-------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------#

## 0. About ####

#-------------------------------------------------------------------------------------------#
# community structure variables:
# Initial biomass (Zooplankton)
# Evenness (Zooplankton)
# Species richness (Zooplankton)
# Effective number of species (Zooplankton)
# Initial body size (Zooplankton)


# environmental parameters:
# TP
# TN 
# light (PAR I)
# DOC 
# temperature 
# TN/TP ratio
# DN/TP ratio
#-------------------------------------------------------------------------------------------#

## 1. load packages ####

#-------------------------------------------------------------------------------------------#
# load packages
library(tidyverse)
library(scales)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(dplyr)
library("readxl")
#-------------------------------------------------------------------------------------------#

## 2. load and transform zooplankton biomass dataset ####

#-------------------------------------------------------------------------------------------#
# load dataset
zooplankton_biomass1 <- read_excel("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Zooplankton_SITES_AquaNet_2017.xlsx")

# delete columns 
zooplankton_biomass <- zooplankton_biomass1[-c(10:15)] 

# changing names of column "Lake"
zooplankton_biomass$Lake<- as.character(zooplankton_biomass$Lake)

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Asa"] <- "Feresjoen"

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Skogaryd"] <- "Erssjoen"

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Svartberget"] <- "Stortjaern"

zooplankton_biomass$Lake <- as.factor(zooplankton_biomass$Lake)

levels(zooplankton_biomass$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# changing names for column "Experiment"
zooplankton_biomass$Experiment <- as.integer(zooplankton_biomass$Experiment)

zooplankton_biomass$Experiment[zooplankton_biomass$Experiment == 1] <- "Spring"

zooplankton_biomass$Experiment[zooplankton_biomass$Experiment == 2] <- "Summer"

zooplankton_biomass$Experiment <- as.character(zooplankton_biomass$Experiment)

# delete all replicate / treatment "lake"
levels(zooplankton_biomass$Replicate)
zooplankton_biomass <- zooplankton_biomass[ ! (zooplankton_biomass$Replicate %in% c("lake", "Lake")), ]

levels(zooplankton_biomass$Treatment)
zooplankton_biomass <- zooplankton_biomass[ ! (zooplankton_biomass$Treatment %in% c("Lake")), ]

zooplankton_biomass$Clean_Biomass<- as.numeric(zooplankton_biomass$Clean_Biomass)

# calculate mean values for Clean_biomass in extra column 
zooplankton_biomass_mean <- zooplankton_biomass %>%
group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_biomass = mean(Clean_Biomass, na.rm=T))

#-------------------------------------------------------------------------------------------#

## 3. load and transform env_parameters ####

#-------------------------------------------------------------------------------------------#
env_parameters1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/physico_chemistry_fish.csv", sep=";")

#filtering 
env_parameters <- select(env_parameters1, Lake, Experiment, Sampling, Exp_day, Sampling_day, 
                         Sampling_month, Enclosure, Treatment, Replicate, TP, TN, DN, DOC, PAR_I, Temperature)

# changing names for column "Experiment"
env_parameters$Experiment <- as.integer(env_parameters$Experiment)

env_parameters$Experiment[env_parameters$Experiment == 1] <- "Spring"

env_parameters$Experiment[env_parameters$Experiment == 2] <- "Summer"

env_parameters$Experiment <- as.character(env_parameters$Experiment)


# changing names of column "Lake"
levels(env_parameters$Lake)
# "Asa"         "Bolmen"      "Erken"       "Skogaryd"    "Svartberget"

env_parameters$Lake<- as.character(env_parameters$Lake)

env_parameters$Lake[env_parameters$Lake == "Asa"] <- "Feresjoen"

env_parameters$Lake[env_parameters$Lake == "Skogaryd"] <- "Erssjoen"

env_parameters$Lake[env_parameters$Lake == "Svartberget"] <- "Stortjaern"

env_parameters$Lake <- as.factor(env_parameters$Lake)

levels(env_parameters$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# set TN as numeric
env_parameters$TN <- as.character(env_parameters$TN)

env_parameters$TN <- as.numeric(env_parameters$TN)

# delete all replicate / treatment "lake"
levels(env_parameters$Replicate)
env_parameters <- env_parameters[ ! (env_parameters$Replicate %in% c("lake", "Lake")), ]

levels(env_parameters$Treatment)
env_parameters <- env_parameters[ ! (env_parameters$Treatment %in% c("Lake")), ]

# new column with TN/TP ratio
env_parameters <- mutate(env_parameters, "TN_TP_ratio" = TN / TP)

# new column with DN/TP ratio
env_parameters <- mutate(env_parameters, "DN_TP_ratio" = DN / TP)

# creating mean values for all environmental parameters
env_parameters_mean <- env_parameters %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_TP = mean(TP, na.rm=T), mean_TN= mean(TN, na.rm=T), mean_DOC= mean(DOC, na.rm=T), 
            mean_PAR= mean(PAR_I, na.rm=T), mean_Temp = mean(Temperature, na.rm=T), 
            mean_TN_TP_ratio = mean(TN_TP_ratio, na.rm=T), mean_DN_TP_ratio=mean(DN_TP_ratio, na.rm=T))
#-------------------------------------------------------------------------------------------#

## 4. merge datasets ####

#-------------------------------------------------------------------------------------------#
mean_zoopl_biomass_environ <- merge(x=env_parameters_mean, y=zooplankton_biomass_mean, 
                                    by =c("Lake", "Experiment", "Enclosure", "Treatment"), 
                                    all.x=TRUE)
#set Experiment as factor
mean_zoopl_biomass_environ$Experiment <- as.factor(mean_zoopl_biomass_environ$Experiment)

#-------------------------------------------------------------------------------------------#

## 5. plotting zooplankton biomass ~ TP ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ total phosphorus
plot_zoopl_biomass_TP <- ggplot(data=mean_zoopl_biomass_environ, aes(x=mean_TP, y=mean_biomass, 
                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
 geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ total phosphorus (Treatment F)

# filtering only Treatment = F
mean_zoopl_biomass_environ_F <- mean_zoopl_biomass_environ[which(mean_zoopl_biomass_environ$Treatment=='F'), ]

plot_zoopl_biomass_TP_F <- ggplot(data=mean_zoopl_biomass_environ_F, aes(x=mean_TP, y=mean_biomass, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Biomass (mean)") +
  ylim(0,100) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
mean_zoopl_biomass_environ_S <- mean_zoopl_biomass_environ[which(mean_zoopl_biomass_environ$Treatment=='S'), ]

plot_zoopl_biomass_TP_S <- ggplot(data=mean_zoopl_biomass_environ_S, aes(x=mean_TP, y=mean_biomass, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
 guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
mean_zoopl_biomass_environ_FS <- mean_zoopl_biomass_environ[which(mean_zoopl_biomass_environ$Treatment=='FS'), ]

plot_zoopl_biomass_TP_FS <- ggplot(data=mean_zoopl_biomass_environ_FS, aes(x=mean_TP, y=mean_biomass, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Biomass (mean)") +
  ylim(-10,80) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
mean_zoopl_biomass_environ_C <- mean_zoopl_biomass_environ[which(mean_zoopl_biomass_environ$Treatment=='C'), ]

plot_zoopl_biomass_TP_C <- ggplot(data=mean_zoopl_biomass_environ_C, aes(x=mean_TP, y=mean_biomass, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Biomass (mean)") +
  ylim(0,150)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_biomass_TP, plot_zoopl_biomass_TP_F, plot_zoopl_biomass_TP_S, 
             plot_zoopl_biomass_TP_FS, plot_zoopl_biomass_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 6. plotting zooplankton biomass ~ TN ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ total nitrogen
plot_zoopl_biomass_TN <- ggplot(data=mean_zoopl_biomass_environ, aes(x=mean_TN, y=mean_biomass, 
                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ total nitrogen (Treatment F)
plot_zoopl_biomass_TN_F <- ggplot(data=mean_zoopl_biomass_environ_F, aes(x=mean_TN, y=mean_biomass, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_TN_S <- ggplot(data=mean_zoopl_biomass_environ_S, aes(x=mean_TN, y=mean_biomass, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_TN_FS <- ggplot(data=mean_zoopl_biomass_environ_FS, aes(x=mean_TN, y=mean_biomass, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_TN_C <- ggplot(data=mean_zoopl_biomass_environ_C, aes(x=mean_TN, y=mean_biomass, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_biomass_TN, plot_zoopl_biomass_TN_F, plot_zoopl_biomass_TN_S, 
             plot_zoopl_biomass_TN_FS, plot_zoopl_biomass_TN_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 7. plotting zooplankton biomass ~ DOC ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ DOC
plot_zoopl_biomass_DOC <- ggplot(data=mean_zoopl_biomass_environ, aes(x=mean_DOC, y=mean_biomass, 
                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ DOC (Treatment F)
plot_zoopl_biomass_DOC_F <- ggplot(data=mean_zoopl_biomass_environ_F, aes(x=mean_DOC, y=mean_biomass, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_DOC_S <- ggplot(data=mean_zoopl_biomass_environ_S, aes(x=mean_DOC, y=mean_biomass, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_DOC_FS <- ggplot(data=mean_zoopl_biomass_environ_FS, aes(x=mean_DOC, y=mean_biomass, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_DOC_C <- ggplot(data=mean_zoopl_biomass_environ_C, aes(x=mean_DOC, y=mean_biomass, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_biomass_DOC, plot_zoopl_biomass_DOC_F, plot_zoopl_biomass_DOC_S, 
             plot_zoopl_biomass_DOC_FS, plot_zoopl_biomass_DOC_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 8. plotting zooplankton biomass ~ Temperature ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ Temp
plot_zoopl_biomass_Temp <- ggplot(data=mean_zoopl_biomass_environ, aes(x=mean_Temp, y=mean_biomass, 
                                                                      color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Biomass (mean)") +
  xlim(11,20)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ Temp (Treatment F)
plot_zoopl_biomass_Temp_F <- ggplot(data=mean_zoopl_biomass_environ_F, aes(x=mean_Temp, y=mean_biomass, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Biomass (mean)") +
  xlim(11.5,20)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_Temp_S <- ggplot(data=mean_zoopl_biomass_environ_S, aes(x=mean_Temp, y=mean_biomass, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_Temp_FS <- ggplot(data=mean_zoopl_biomass_environ_FS, aes(x=mean_Temp, y=mean_biomass, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_Temp_C <- ggplot(data=mean_zoopl_biomass_environ_C, aes(x=mean_Temp, y=mean_biomass, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_biomass_Temp, plot_zoopl_biomass_Temp_F, plot_zoopl_biomass_Temp_S, 
             plot_zoopl_biomass_Temp_FS, plot_zoopl_biomass_Temp_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 9. plotting zooplankton biomass ~ PAR ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ PAR
plot_zoopl_biomass_PAR <- ggplot(data=mean_zoopl_biomass_environ, aes(x=mean_PAR, y=mean_biomass, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ PAR (Treatment F)
plot_zoopl_biomass_PAR_F <- ggplot(data=mean_zoopl_biomass_environ_F, aes(x=mean_PAR, y=mean_biomass, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_PAR_S <- ggplot(data=mean_zoopl_biomass_environ_S, aes(x=mean_PAR, y=mean_biomass, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_PAR_FS <- ggplot(data=mean_zoopl_biomass_environ_FS, aes(x=mean_PAR, y=mean_biomass, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_PAR_C <- ggplot(data=mean_zoopl_biomass_environ_C, aes(x=mean_PAR, y=mean_biomass, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_biomass_PAR, plot_zoopl_biomass_PAR_F, plot_zoopl_biomass_PAR_S, 
             plot_zoopl_biomass_PAR_FS, plot_zoopl_biomass_PAR_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 10. plotting zooplankton biomass ~ TN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ TN/TP ratio
plot_zoopl_biomass_TN_TP <- ggplot(data=mean_zoopl_biomass_environ, aes(x=mean_TN_TP_ratio, y=mean_biomass, 
                                                                      color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ TN/TP ratio (Treatment F)
plot_zoopl_biomass_TN_TP_F <- ggplot(data=mean_zoopl_biomass_environ_F, aes(x=mean_TN_TP_ratio, y=mean_biomass, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_TN_TP_S <- ggplot(data=mean_zoopl_biomass_environ_S, aes(x=mean_TN_TP_ratio, y=mean_biomass, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  ylim(0,150)+
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_TN_TP_FS <- ggplot(data=mean_zoopl_biomass_environ_FS, aes(x=mean_TN_TP_ratio, y=mean_biomass, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  ylim(0,100)+
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_TN_TP_C <- ggplot(data=mean_zoopl_biomass_environ_C, aes(x=mean_TN_TP_ratio, y=mean_biomass, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  ylim(0,150)+
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_biomass_TN_TP, plot_zoopl_biomass_TN_TP_F, plot_zoopl_biomass_TN_TP_S, 
             plot_zoopl_biomass_TN_TP_FS, plot_zoopl_biomass_TN_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 11. plotting zooplankton biomass ~ DN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ DN/TP ratio
plot_zoopl_biomass_DN_TP <- ggplot(data=mean_zoopl_biomass_environ, aes(x=mean_DN_TP_ratio, y=mean_biomass, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ DN/TP (Treatment F)
plot_zoopl_biomass_DN_TP_F <- ggplot(data=mean_zoopl_biomass_environ_F, aes(x=mean_DN_TP_ratio, y=mean_biomass, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_DN_TP_S <- ggplot(data=mean_zoopl_biomass_environ_S, aes(x=mean_DN_TP_ratio, y=mean_biomass, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_DN_TP_FS <- ggplot(data=mean_zoopl_biomass_environ_FS, aes(x=mean_DN_TP_ratio, y=mean_biomass, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_biomass_DN_TP_C <- ggplot(data=mean_zoopl_biomass_environ_C, aes(x=mean_DN_TP_ratio, y=mean_biomass, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_biomass_DN_TP, plot_zoopl_biomass_DN_TP_F, plot_zoopl_biomass_DN_TP_S, 
             plot_zoopl_biomass_DN_TP_FS, plot_zoopl_biomass_DN_TP_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 12. plotting zooplankton evenness ~ TP ####

#-------------------------------------------------------------------------------------------#

# loading dataset zooplankton diversity 
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

# delete all replicate / treatment "lake"
levels(zooplankton_diversity$Replicate)
zooplankton_diversity <- zooplankton_diversity[!(zooplankton_diversity$Replicate %in% c("lake", "Lake")), ]

levels(zooplankton_diversity$Treatment)
zooplankton_diversity <- zooplankton_diversity[ ! (zooplankton_diversity$Treatment %in% c("Lake")), ]

# filtering only zooplankton for column "variable" 
zooplankton_diversity <- zooplankton_diversity[which(zooplankton_diversity$variable=='zooplankton'), ]

# calculate mean values for species richness, evenness, effective number of species in extra columns 
zooplankton_diversity_mean <- zooplankton_diversity %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_J = mean(J, na.rm=T), mean_s = mean(S, na.rm=T), mean_ENS_D = mean(ENS_D, na.rm=T))

zooplankton_diversity_mean = zooplankton_diversity_mean %>%
  ungroup(Lake, Experiment, Treatment, Enclosure)


# combining datasets
mean_zoopl_diversity_environ <- merge(x=env_parameters_mean, y=zooplankton_diversity_mean, 
                                      by =c("Lake", "Experiment", "Enclosure", "Treatment"), 
                                      all.x=TRUE)

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton evenness ~ total phosphorus

plot_zoopl_evenness_TP <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_TP, y=mean_J, 
                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ TP (Treatment F)

# filtering only Treatment = F
mean_zoopl_diversity_environ_F <- mean_zoopl_diversity_environ[which(mean_zoopl_diversity_environ$Treatment=='F'), ]

plot_zoopl_evenness_TP_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_TP, y=mean_J, 
                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
mean_zoopl_diversity_environ_S <- mean_zoopl_diversity_environ[which(mean_zoopl_diversity_environ$Treatment=='S'), ]

plot_zoopl_evenness_TP_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_TP, y=mean_J, 
                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
mean_zoopl_diversity_environ_FS <- mean_zoopl_diversity_environ[which(mean_zoopl_diversity_environ$Treatment=='FS'), ]

plot_zoopl_evenness_TP_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_TP, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
mean_zoopl_diversity_environ_C <- mean_zoopl_diversity_environ[which(mean_zoopl_diversity_environ$Treatment=='C'), ]

plot_zoopl_evenness_TP_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_TP, y=mean_J, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_evenness_TP, plot_zoopl_evenness_TP_F, plot_zoopl_evenness_TP_S, 
             plot_zoopl_evenness_TP_FS, plot_zoopl_evenness_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 13. plotting zooplankton evenness ~ TN ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton evenness ~ total nitrogen

plot_zoopl_evenness_TN <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_TN, y=mean_J, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ TN (Treatment F)

# filtering only Treatment = F
mean_zoopl_diversity_environ_F <- mean_zoopl_diversity_environ[which(mean_zoopl_diversity_environ$Treatment=='F'), ]

plot_zoopl_evenness_TN_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_TN, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
mean_zoopl_diversity_environ_S <- mean_zoopl_diversity_environ[which(mean_zoopl_diversity_environ$Treatment=='S'), ]

plot_zoopl_evenness_TN_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_TN, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
mean_zoopl_diversity_environ_FS <- mean_zoopl_diversity_environ[which(mean_zoopl_diversity_environ$Treatment=='FS'), ]

plot_zoopl_evenness_TN_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_TN, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
mean_zoopl_diversity_environ_C <- mean_zoopl_diversity_environ[which(mean_zoopl_diversity_environ$Treatment=='C'), ]

plot_zoopl_evenness_TN_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_TN, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_evenness_TN, plot_zoopl_evenness_TN_F, plot_zoopl_evenness_TN_S, 
             plot_zoopl_evenness_TN_FS, plot_zoopl_evenness_TN_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 14. plotting zooplankton evenness ~ DOC ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton evenness ~ DOC

plot_zoopl_evenness_DOC <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_DOC, y=mean_J, 
                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ DOC (Treatment F)

plot_zoopl_evenness_DOC_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_DOC, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_DOC_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_DOC, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_DOC_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_DOC, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_DOC_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_DOC, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_evenness_DOC, plot_zoopl_evenness_DOC_F, plot_zoopl_evenness_DOC_S, 
             plot_zoopl_evenness_DOC_FS, plot_zoopl_evenness_DOC_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 15. plotting zooplankton evenness ~ PAR ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton evenness ~ PAR

plot_zoopl_evenness_PAR <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_PAR, y=mean_J, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ PAR (Treatment F)
plot_zoopl_evenness_PAR_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_PAR, y=mean_J, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_PAR_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_PAR, y=mean_J, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_PAR_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_PAR, y=mean_J, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_PAR_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_PAR, y=mean_J, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_evenness_PAR, plot_zoopl_evenness_PAR_F, plot_zoopl_evenness_PAR_S, 
             plot_zoopl_evenness_PAR_FS, plot_zoopl_evenness_PAR_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 16. plotting zooplankton evenness ~ Temperature ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton evenness ~ Temp

plot_zoopl_evenness_Temp <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_Temp, y=mean_J, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  xlim(11,20)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ PAR (Treatment F)
plot_zoopl_evenness_Temp_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_Temp, y=mean_J, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  xlim(11,20)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_Temp_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_Temp, y=mean_J, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_Temp_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_Temp, y=mean_J, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_Temp_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_Temp, y=mean_J, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_evenness_Temp, plot_zoopl_evenness_Temp_F, plot_zoopl_evenness_Temp_S, 
             plot_zoopl_evenness_Temp_FS, plot_zoopl_evenness_Temp_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 17. plotting zooplankton evenness ~ TN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton evenness ~ TN/TP ratio 
plot_zoopl_evenness_TN_TP <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ TN/TP ratio (Treatment F)
plot_zoopl_evenness_TN_TP_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_TN_TP_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_TN_TP_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_TN_TP_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_evenness_TN_TP, plot_zoopl_evenness_TN_TP_F, plot_zoopl_evenness_TN_TP_S, 
             plot_zoopl_evenness_TN_TP_FS, plot_zoopl_evenness_TN_TP_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 18. plotting zooplankton evenness ~ DN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton evenness ~ DN/TP ratio 
plot_zoopl_evenness_DN_TP <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ DN/TP ratio (Treatment F)
plot_zoopl_evenness_DN_TP_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_DN_TP_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_DN_TP_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_evenness_DN_TP_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_evenness_DN_TP, plot_zoopl_evenness_DN_TP_F, plot_zoopl_evenness_DN_TP_S, 
             plot_zoopl_evenness_DN_TP_FS, plot_zoopl_evenness_DN_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 19. plotting zooplankton richness ~ TP ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton richness ~ TP

plot_zoopl_richness_TP <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_TP, y=mean_s, 
                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton richness ~ TP (Treatment F)
plot_zoopl_richness_TP_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_TP, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_TP_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_TP, y=mean_s, 
                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_TP_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_TP, y=mean_s, 
                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_TP_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_TP, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_richness_TP, plot_zoopl_richness_TP_F, plot_zoopl_richness_TP_S, 
             plot_zoopl_richness_TP_FS, plot_zoopl_richness_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 20. plotting zooplankton richness ~ TN ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for zooplankton richness ~ TN

plot_zoopl_richness_TN <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_TN, y=mean_s, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton richness ~ TN (Treatment F)
plot_zoopl_richness_TN_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_TN, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_TN_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_TN, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_TN_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_TN, y=mean_s, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_TN_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_TN, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_richness_TN, plot_zoopl_richness_TN_F, plot_zoopl_richness_TN_S, 
             plot_zoopl_richness_TN_FS, plot_zoopl_richness_TN_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 21. plotting zooplankton richness ~ DOC ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for zooplankton richness ~ DOC

plot_zoopl_richness_DOC <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_DOC, y=mean_s, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton richness ~ TN (Treatment F)
plot_zoopl_richness_DOC_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_DOC, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_DOC_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_DOC, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_DOC_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_DOC, y=mean_s, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_DOC_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_DOC, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_richness_DOC, plot_zoopl_richness_DOC_F, plot_zoopl_richness_DOC_S, 
             plot_zoopl_richness_DOC_FS, plot_zoopl_richness_DOC_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 22. plotting zooplankton richness ~ Temperature ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton richness ~ Temp

plot_zoopl_richness_Temp <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_Temp, y=mean_s, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  xlim(11,20)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton richness ~ TN (Treatment F)
plot_zoopl_richness_Temp_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_Temp, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  xlim(11,20)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_Temp_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_Temp, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_Temp_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_Temp, y=mean_s, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_Temp_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_Temp, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_richness_Temp, plot_zoopl_richness_Temp_F, plot_zoopl_richness_Temp_S, 
             plot_zoopl_richness_Temp_FS, plot_zoopl_richness_Temp_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 23. plotting zooplankton richness ~ TN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton richness ~ TN/TP ratio

plot_zoopl_richness_TN_TP <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton richness ~ TN/TP ratio (Treatment F)
plot_zoopl_richness_TN_TP_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_TN_TP_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_TN_TP_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_TN_TP_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_richness_TN_TP, plot_zoopl_richness_TN_TP_F, plot_zoopl_richness_TN_TP_S, 
             plot_zoopl_richness_TN_TP_FS, plot_zoopl_richness_TN_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 24. plotting zooplankton richness ~ DN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton richness ~ DN/TP ratio
plot_zoopl_richness_DN_TP <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton richness ~ DN/TP ratio (Treatment F)
plot_zoopl_richness_DN_TP_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_DN_TP_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_DN_TP_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_DN_TP_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_richness_DN_TP, plot_zoopl_richness_DN_TP_F, plot_zoopl_richness_DN_TP_S, 
             plot_zoopl_richness_DN_TP_FS, plot_zoopl_richness_DN_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 25. plotting zooplankton richness ~ PAR ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton richness ~ PAR

plot_zoopl_richness_PAR <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_PAR, y=mean_s, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton richness ~ PAR (Treatment F)
plot_zoopl_richness_PAR_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_PAR, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_PAR_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_PAR, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_PAR_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_PAR, y=mean_s, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_richness_PAR_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_PAR, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_richness_PAR, plot_zoopl_richness_PAR_F, plot_zoopl_richness_PAR_S, 
             plot_zoopl_richness_PAR_FS, plot_zoopl_richness_PAR_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 26. plotting zooplankton effective number of species ~ TP ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton ENS ~ TP

plot_zoopl_ENS_TP <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_TP, y=mean_ENS_D, 
                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton ENS ~ TP (Treatment F)
plot_zoopl_ENS_TP_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_TP, y=mean_ENS_D, 
                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_TP_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_TP, y=mean_ENS_D, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_TP_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_TP, y=mean_ENS_D, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_TP_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_TP, y=mean_ENS_D, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_ENS_TP, plot_zoopl_ENS_TP_F, plot_zoopl_ENS_TP_S, 
             plot_zoopl_ENS_TP_FS, plot_zoopl_ENS_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 27. plotting zooplankton effective number of species ~ TN ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton ENS ~ TN

plot_zoopl_ENS_TN <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_TN, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton richness ~ TN (Treatment F)
plot_zoopl_ENS_TN_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_TN, y=mean_ENS_D, 
                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_TN_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_TN, y=mean_ENS_D, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_TN_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_TN, y=mean_ENS_D, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_TN_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_TN, y=mean_ENS_D, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_ENS_TN, plot_zoopl_ENS_TN_F, plot_zoopl_ENS_TN_S, 
             plot_zoopl_ENS_TN_FS, plot_zoopl_ENS_TN_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 28. plotting zooplankton effective number of species ~ DOC ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for zooplankton ENS ~ DOC

plot_zoopl_ENS_DOC <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_DOC, y=mean_ENS_D, 
                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton richness ~ TN (Treatment F)
plot_zoopl_ENS_DOC_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_DOC_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_DOC_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_DOC_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_ENS_DOC, plot_zoopl_ENS_DOC_F, plot_zoopl_ENS_DOC_S, 
             plot_zoopl_ENS_DOC_FS, plot_zoopl_ENS_DOC_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 29. plotting zooplankton effective number of species ~ Temperature ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for zooplankton ENS ~ Temp
plot_zoopl_ENS_Temp <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  xlim(11,20)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton richness ~ Temp (Treatment F)
plot_zoopl_ENS_Temp_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  xlim(11.5,19.5)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_Temp_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_Temp_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_Temp_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_ENS_Temp, plot_zoopl_ENS_Temp_F, plot_zoopl_ENS_Temp_S, 
             plot_zoopl_ENS_Temp_FS, plot_zoopl_ENS_Temp_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 30. plotting zooplankton effective number of species ~ PAR ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton ENS ~ PAR

plot_zoopl_ENS_PAR <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton richness ~ PAR (Treatment F)
plot_zoopl_ENS_PAR_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_PAR_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_PAR_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_PAR_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_ENS_PAR, plot_zoopl_ENS_PAR_F, plot_zoopl_ENS_PAR_S, 
             plot_zoopl_ENS_PAR_FS, plot_zoopl_ENS_PAR_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 31. plotting zooplankton effective number of species ~ TN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton ENS ~ TN/TP ratio

plot_zoopl_ENS_TN_TP <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton richness ~ TN/TP ratio (Treatment F)
plot_zoopl_ENS_TN_TP_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_TN_TP_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_TN_TP_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_TN_TP_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_ENS_TN_TP, plot_zoopl_ENS_TN_TP_F, plot_zoopl_ENS_TN_TP_S, 
             plot_zoopl_ENS_TN_TP_FS, plot_zoopl_ENS_TN_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 32. plotting zooplankton effective number of species ~ DN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton ENS ~ TN/TP ratio

plot_zoopl_ENS_DN_TP <- ggplot(data=mean_zoopl_diversity_environ, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                      color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton richness ~ DN/TP ratio (Treatment F)
plot_zoopl_ENS_DN_TP_F <- ggplot(data=mean_zoopl_diversity_environ_F, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_DN_TP_S <- ggplot(data=mean_zoopl_diversity_environ_S, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_DN_TP_FS <- ggplot(data=mean_zoopl_diversity_environ_FS, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_ENS_DN_TP_C <- ggplot(data=mean_zoopl_diversity_environ_C, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of spcies (Simpson) (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_ENS_DN_TP, plot_zoopl_ENS_DN_TP_F, plot_zoopl_ENS_DN_TP_S, 
             plot_zoopl_ENS_DN_TP_FS, plot_zoopl_ENS_DN_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 33. Initial body size  ~ TP####

#-------------------------------------------------------------------------------------------#

zoopl_size <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/cwm_zooplankton_body_size.csv")

# delete columns 
zoopl_size <- zoopl_size[-1] 

# merge
zoopl_size_environ <- merge(x=env_parameters_mean, y=zoopl_size, 
                                    by =c("Lake", "Experiment", "Enclosure", "Treatment"), 
                                    all.x=TRUE)
# delete columns 
zoopl_size_environ <- zoopl_size_environ[-c(12,13)] 

# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ total phosphorus
plot_zoopl_size_TP <- ggplot(data=zoopl_size_environ, aes(x=mean_TP, y=cwm, 
                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ total phosphorus (Treatment F)

# filtering only Treatment = F
zoopl_size_environ_F <- zoopl_size_environ[which(zoopl_size_environ$Treatment=='F'), ]

plot_zoopl_size_TP_F <- ggplot(data=zoopl_size_environ_F, aes(x=mean_TP, y=cwm, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
zoopl_size_environ_S <- zoopl_size_environ[which(zoopl_size_environ$Treatment=='S'), ]

plot_zoopl_size_TP_S <- ggplot(data=zoopl_size_environ_S, aes(x=mean_TP, y=cwm, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
zoopl_size_environ_FS <- zoopl_size_environ[which(zoopl_size_environ$Treatment=='FS'), ]

plot_zoopl_size_TP_FS <- ggplot(data=zoopl_size_environ_FS, aes(x=mean_TP, y=cwm, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
zoopl_size_environ_C <- zoopl_size_environ[which(zoopl_size_environ$Treatment=='C'), ]

plot_zoopl_size_TP_C <- ggplot(data=zoopl_size_environ_C, aes(x=mean_TP, y=cwm, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_size_TP, plot_zoopl_size_TP_F, plot_zoopl_size_TP_S, 
             plot_zoopl_size_TP_FS, plot_zoopl_size_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 34. Initial body size  ~ TN####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ total nitrogen
plot_zoopl_size_TN <- ggplot(data=zoopl_size_environ, aes(x=mean_TN, y=cwm, 
                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ total nitrogen (Treatment F)
plot_zoopl_size_TN_F <- ggplot(data=zoopl_size_environ_F, aes(x=mean_TN, y=cwm, 
                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


plot_zoopl_size_TN_S <- ggplot(data=zoopl_size_environ_S, aes(x=mean_TN, y=cwm, 
                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


plot_zoopl_size_TN_FS <- ggplot(data=zoopl_size_environ_FS, aes(x=mean_TN, y=cwm, 
                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_TN_C <- ggplot(data=zoopl_size_environ_C, aes(x=mean_TN, y=cwm, 
                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_size_TN, plot_zoopl_size_TN_F, plot_zoopl_size_TN_S, 
             plot_zoopl_size_TN_FS, plot_zoopl_size_TN_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 35. Initial body size  ~ DOC ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ total nitrogen
plot_zoopl_size_DOC <- ggplot(data=zoopl_size_environ, aes(x=mean_DOC, y=cwm, 
                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ total nitrogen (Treatment F)
plot_zoopl_size_DOC_F <- ggplot(data=zoopl_size_environ_F, aes(x=mean_DOC, y=cwm, 
                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


plot_zoopl_size_DOC_S <- ggplot(data=zoopl_size_environ_S, aes(x=mean_DOC, y=cwm, 
                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_DOC_FS <- ggplot(data=zoopl_size_environ_FS, aes(x=mean_DOC, y=cwm, 
                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_DOC_C <- ggplot(data=zoopl_size_environ_C, aes(x=mean_DOC, y=cwm, 
                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_size_DOC, plot_zoopl_size_DOC_F, plot_zoopl_size_DOC_S, 
             plot_zoopl_size_DOC_FS, plot_zoopl_size_DOC_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 36. Initial body size  ~ PAR ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ total nitrogen
plot_zoopl_size_PAR <- ggplot(data=zoopl_size_environ, aes(x=mean_PAR, y=cwm, 
                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ total nitrogen (Treatment F)
plot_zoopl_size_PAR_F <- ggplot(data=zoopl_size_environ_F, aes(x=mean_PAR, y=cwm, 
                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


plot_zoopl_size_PAR_S <- ggplot(data=zoopl_size_environ_S, aes(x=mean_PAR, y=cwm, 
                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_PAR_FS <- ggplot(data=zoopl_size_environ_FS, aes(x=mean_PAR, y=cwm, 
                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_PAR_C <- ggplot(data=zoopl_size_environ_C, aes(x=mean_PAR, y=cwm, 
                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_size_PAR, plot_zoopl_size_PAR_F, plot_zoopl_size_PAR_S, 
             plot_zoopl_size_PAR_FS, plot_zoopl_size_PAR_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 37. Initial body size  ~ Temp ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ total nitrogen
plot_zoopl_size_Temp <- ggplot(data=zoopl_size_environ, aes(x=mean_Temp, y=cwm, 
                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  xlim(11,20)+
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ total nitrogen (Treatment F)
plot_zoopl_size_Temp_F <- ggplot(data=zoopl_size_environ_F, aes(x=mean_Temp, y=cwm, 
                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  xlim(11,20)+
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_Temp_S <- ggplot(data=zoopl_size_environ_S, aes(x=mean_Temp, y=cwm, 
                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_Temp_FS <- ggplot(data=zoopl_size_environ_FS, aes(x=mean_Temp, y=cwm, 
                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_Temp_C <- ggplot(data=zoopl_size_environ_C, aes(x=mean_Temp, y=cwm, 
                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_size_Temp, plot_zoopl_size_Temp_F, plot_zoopl_size_Temp_S, 
             plot_zoopl_size_Temp_FS, plot_zoopl_size_Temp_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 38. Initial body size  ~ TN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ total nitrogen
plot_zoopl_size_TN_TP <- ggplot(data=zoopl_size_environ, aes(x=mean_TN_TP_ratio, y=cwm, 
                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ total nitrogen (Treatment F)
plot_zoopl_size_TN_TP_F <- ggplot(data=zoopl_size_environ_F, aes(x=mean_TN_TP_ratio, y=cwm, 
                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_TN_TP_S <- ggplot(data=zoopl_size_environ_S, aes(x=mean_TN_TP_ratio, y=cwm, 
                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_TN_TP_FS <- ggplot(data=zoopl_size_environ_FS, aes(x=mean_TN_TP_ratio, y=cwm, 
                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_TN_TP_C <- ggplot(data=zoopl_size_environ_C, aes(x=mean_TN_TP_ratio, y=cwm, 
                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_size_TN_TP, plot_zoopl_size_TN_TP_F, plot_zoopl_size_TN_TP_S, 
             plot_zoopl_size_TN_TP_FS, plot_zoopl_size_TN_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 39. Initial body size  ~ DN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for zooplankton biomass ~ total nitrogen
plot_zoopl_size_DN_TP <- ggplot(data=zoopl_size_environ, aes(x=mean_DN_TP_ratio, y=cwm, 
                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ total nitrogen (Treatment F)
plot_zoopl_size_DN_TP_F <- ggplot(data=zoopl_size_environ_F, aes(x=mean_DN_TP_ratio, y=cwm, 
                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Initial body size") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_DN_TP_S <- ggplot(data=zoopl_size_environ_S, aes(x=mean_DN_TP_ratio, y=cwm, 
                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_DN_TP_FS <- ggplot(data=zoopl_size_environ_FS, aes(x=mean_DN_TP_ratio, y=cwm, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_zoopl_size_DN_TP_C <- ggplot(data=zoopl_size_environ_C, aes(x=mean_DN_TP_ratio, y=cwm, 
                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Initial body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoopl_size_DN_TP, plot_zoopl_size_DN_TP_F, plot_zoopl_size_DN_TP_S, 
             plot_zoopl_size_DN_TP_FS, plot_zoopl_size_DN_TP_C, ncol=3)




