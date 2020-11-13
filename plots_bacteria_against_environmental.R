#-------------------------------------------------------------------------------------------#

## community structure variables correlation plots for Bacteria ~  Environmental parameters####

#-------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------#

## 0. About ####

#-------------------------------------------------------------------------------------------#
# environmental parameters:
# TP
# TN 
# DOC 
# light (PAR I)
# temperature 


# community structure variables:
# Evenness (Bacteria)
# Species Richness (Bacteria)
# Effective number of species, Simpson (Bacteria)
#-------------------------------------------------------------------------------------------#

## 1. load packages ####

#-------------------------------------------------------------------------------------------#
# load packages
library(tidyverse)
library(scales)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(gdata)
#-------------------------------------------------------------------------------------------#

## 2. load and transform bacteria diversity dataset ####

#-------------------------------------------------------------------------------------------#
# loading dataset phytoplankton diversity 
bacteria_diversity1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Diversity/diversity_metrics.csv")

# deleting first column
bacteria_diversity <- bacteria_diversity1[-1] 

# renaming lakes
levels(bacteria_diversity$Lake)
# "Bolmen"       "Erken"        "Erssj\xf6n"   "Feresj\xf6n"  "Stortj\xe4rn"

bacteria_diversity$Lake<- as.character(bacteria_diversity$Lake)

bacteria_diversity$Lake[bacteria_diversity$Lake == "Feresj\xf6n"] <- "Feresjoen"

bacteria_diversity$Lake[bacteria_diversity$Lake == "Erssj\xf6n"] <- "Erssjoen"

bacteria_diversity$Lake[bacteria_diversity$Lake == "Stortj\xe4rn"] <- "Stortjaern"

bacteria_diversity$Lake <- as.factor(bacteria_diversity$Lake)

levels(bacteria_diversity$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# delete all replicate / treatment "lake"
levels(bacteria_diversity$Replicate)
bacteria_diversity <- bacteria_diversity[!(bacteria_diversity$Replicate %in% c("lake", "Lake")), ]

# filtering only bacteria for column "variable" 
bacteria_diversity <- bacteria_diversity[which(bacteria_diversity$variable='bacteria'), ]

# calculate mean values for species richness, evenness, effective number of species in extra columns 
bacteria_diversity_mean <- bacteria_diversity %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_J = mean(J, na.rm=T), mean_s = mean(S, na.rm=T), mean_ENS_D = mean(ENS_D, na.rm=T))
#-------------------------------------------------------------------------------------------#

## 3. load and transform env_parameters dataset ####

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

## 4. merge both datasets ####

#-------------------------------------------------------------------------------------------#
# combining datasets
mean_bacteria_diversity_environ <- merge(x=env_parameters_mean, y=bacteria_diversity_mean, 
                                      by =c("Lake", "Experiment", "Enclosure", "Treatment"), 
                                      all.x=TRUE)

#-------------------------------------------------------------------------------------------#

## 5. plotting bacteria evenness ~ TP  ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for bacteria evenness ~ total phosphorus
plot_bacteria_evenness_TP <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_TP, y=mean_J, 
                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria evenness ~ TP (Treatment F)

# filtering only Treatment = F
mean_bacteria_diversity_environ_F <- mean_bacteria_diversity_environ[which(mean_bacteria_diversity_environ$Treatment=='F'), ]

plot_bacteria_evenness_TP_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_TP, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
mean_bacteria_diversity_environ_S <- mean_bacteria_diversity_environ[which(mean_bacteria_diversity_environ$Treatment=='S'), ]

plot_bacteria_evenness_TP_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_TP, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
mean_bacteria_diversity_environ_FS <- mean_bacteria_diversity_environ[which(mean_bacteria_diversity_environ$Treatment=='FS'), ]

plot_bacteria_evenness_TP_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_TP, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
mean_bacteria_diversity_environ_C <- mean_bacteria_diversity_environ[which(mean_bacteria_diversity_environ$Treatment=='C'), ]

plot_bacteria_evenness_TP_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_TP, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_evenness_TP, plot_bacteria_evenness_TP_F, plot_bacteria_evenness_TP_S, 
             plot_bacteria_evenness_TP_FS, plot_bacteria_evenness_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 6. plotting bacteria evenness ~ TN  ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for bacteria evenness ~ total nitrogen
plot_bacteria_evenness_TN <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_TN, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria evenness ~ TN (Treatment F)

# filtering only Treatment = F
plot_bacteria_evenness_TN_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_TN, y=mean_J, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_evenness_TN_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_TN, y=mean_J, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_evenness_TN_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_TN, y=mean_J, 
                                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_evenness_TN_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_TN, y=mean_J, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_evenness_TN, plot_bacteria_evenness_TN_F, plot_bacteria_evenness_TN_S, 
             plot_bacteria_evenness_TN_FS, plot_bacteria_evenness_TN_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 7. plotting bacteria evenness ~ DOC  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria evenness ~ DOC
plot_bacteria_evenness_DOC <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_DOC, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria evenness ~ DOC (Treatment F)

# filtering only Treatment = F
plot_bacteria_evenness_DOC_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_DOC, y=mean_J, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_evenness_DOC_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_DOC, y=mean_J, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_evenness_DOC_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_DOC, y=mean_J, 
                                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_evenness_DOC_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_DOC, y=mean_J, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_evenness_DOC, plot_bacteria_evenness_DOC_F, plot_bacteria_evenness_DOC_S, 
             plot_bacteria_evenness_DOC_FS, plot_bacteria_evenness_DOC_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 8. plotting bacteria evenness ~ PAR  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria evenness ~ PAR
plot_bacteria_evenness_PAR <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_PAR, y=mean_J, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria evenness ~ PAR (Treatment F)

# filtering only Treatment = F
plot_bacteria_evenness_PAR_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_PAR, y=mean_J, 
                                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_evenness_PAR_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_PAR, y=mean_J, 
                                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_evenness_PAR_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_PAR, y=mean_J, 
                                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_evenness_PAR_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_PAR, y=mean_J, 
                                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_evenness_PAR, plot_bacteria_evenness_PAR_F, plot_bacteria_evenness_PAR_S, 
             plot_bacteria_evenness_PAR_FS, plot_bacteria_evenness_PAR_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 9. plotting bacteria evenness ~ Temperature  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria evenness ~ Temperature

plot_bacteria_evenness_Temp <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_Temp, y=mean_J, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  xlim(11,19.5)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria evenness ~ Temperature (Treatment F)

# filtering only Treatment = F
plot_bacteria_evenness_Temp_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_Temp, y=mean_J, 
                                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  xlim(11,19.5)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_evenness_Temp_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_Temp, y=mean_J, 
                                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_evenness_Temp_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_Temp, y=mean_J, 
                                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_evenness_Temp_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_Temp, y=mean_J, 
                                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_evenness_Temp, plot_bacteria_evenness_Temp_F, plot_bacteria_evenness_Temp_S, 
             plot_bacteria_evenness_Temp_FS, plot_bacteria_evenness_Temp_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 10. plotting bacteria evenness ~ TN/TP ratio  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria evenness ~ TN/TP ratio
plot_bacteria_evenness_TN_TP <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria evenness ~ TN/TP ratio (Treatment F)
plot_bacteria_evenness_TN_TP_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_evenness_TN_TP_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_evenness_TN_TP_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                                      color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_evenness_TN_TP_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_evenness_TN_TP, plot_bacteria_evenness_TN_TP_F, plot_bacteria_evenness_TN_TP_S, 
             plot_bacteria_evenness_TN_TP_FS, plot_bacteria_evenness_TN_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 11. plotting bacteria evenness ~ DN/TP ratio  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria evenness ~ DN/TP ratio
plot_bacteria_evenness_DN_TP <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria evenness ~ DN/TP ratio (Treatment F)
plot_bacteria_evenness_DN_TP_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_evenness_DN_TP_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_evenness_DN_TP_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_evenness_DN_TP_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_evenness_DN_TP, plot_bacteria_evenness_DN_TP_F, plot_bacteria_evenness_DN_TP_S, 
             plot_bacteria_evenness_DN_TP_FS, plot_bacteria_evenness_DN_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 12. plotting bacteria species richness ~ TP  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria species richness ~ total phosphorus
plot_bacteria_richness_TP <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_TP, y=mean_s, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria species richness ~ TP (Treatment F)

# filtering only Treatment = F
plot_bacteria_richness_TP_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_TP, y=mean_s, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_richness_TP_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_TP, y=mean_s, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_richness_TP_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_TP, y=mean_s, 
                                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_richness_TP_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_TP, y=mean_s, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_richness_TP, plot_bacteria_richness_TP_F, plot_bacteria_richness_TP_S, 
             plot_bacteria_richness_TP_FS, plot_bacteria_richness_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 13. plotting bacteria species richness ~ TN  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria species richness ~ total nitrogen
plot_bacteria_richness_TN <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_TN, y=mean_s, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria species richness ~ TN (Treatment F)

# filtering only Treatment = F
plot_bacteria_richness_TN_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_TN, y=mean_s, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_richness_TN_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_TN, y=mean_s, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_richness_TN_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_TN, y=mean_s, 
                                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_richness_TN_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_TN, y=mean_s, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_richness_TN, plot_bacteria_richness_TN_F, plot_bacteria_richness_TN_S, 
             plot_bacteria_richness_TN_FS, plot_bacteria_richness_TN_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 14. plotting bacteria species richness ~ DOC  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria species richness ~ DOC
plot_bacteria_richness_DOC <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_DOC, y=mean_s, 
                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria species richness ~ DOC (Treatment F)

# filtering only Treatment = F
plot_bacteria_richness_DOC_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_DOC, y=mean_s, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_richness_DOC_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_DOC, y=mean_s, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_richness_DOC_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_DOC, y=mean_s, 
                                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_richness_DOC_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_DOC, y=mean_s, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_richness_DOC, plot_bacteria_richness_DOC_F, plot_bacteria_richness_DOC_S, 
             plot_bacteria_richness_DOC_FS, plot_bacteria_richness_DOC_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 15. plotting bacteria species richness ~ PAR  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria species richness ~ PAR
plot_bacteria_richness_PAR <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_PAR, y=mean_s, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria species richness ~ PAR (Treatment F)

# filtering only Treatment = F
plot_bacteria_richness_PAR_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_PAR, y=mean_s, 
                                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_richness_PAR_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_PAR, y=mean_s, 
                                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_richness_PAR_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_PAR, y=mean_s, 
                                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_richness_PAR_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_PAR, y=mean_s, 
                                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_richness_PAR, plot_bacteria_richness_PAR_F, plot_bacteria_richness_PAR_S, 
             plot_bacteria_richness_PAR_FS, plot_bacteria_richness_PAR_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 16. plotting bacteria species richness ~ Temperature  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria species richness ~ Temperatur
plot_bacteria_richness_Temp <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_Temp, y=mean_s, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  xlim(11,19.5)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria species richness ~ PAR (Treatment F)

# filtering only Treatment = F
plot_bacteria_richness_Temp_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_Temp, y=mean_s, 
                                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  xlim(11,19.5)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_richness_Temp_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_Temp, y=mean_s, 
                                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_richness_Temp_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_Temp, y=mean_s, 
                                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_richness_Temp_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_Temp, y=mean_s, 
                                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_richness_Temp, plot_bacteria_richness_Temp_F, plot_bacteria_richness_Temp_S, 
             plot_bacteria_richness_Temp_FS, plot_bacteria_richness_Temp_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 17. plotting bacteria species richness ~ TN/TP ratio  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria species richness ~ TN/TP ratio
plot_bacteria_richness_TN_TP <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria species richness ~ TN/TP ratio (Treatment F)
plot_bacteria_richness_TN_TP_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_richness_TN_TP_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_richness_TN_TP_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                                      color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_richness_TN_TP_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_richness_TN_TP, plot_bacteria_richness_TN_TP_F, plot_bacteria_richness_TN_TP_S, 
             plot_bacteria_richness_TN_TP_FS, plot_bacteria_richness_TN_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 18. plotting bacteria species richness ~ DN/TP ratio  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria species richness ~ DN/TP ratio
plot_bacteria_richness_DN_TP <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria species richness ~ DN/TP ratio (Treatment F)
plot_bacteria_richness_DN_TP_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_richness_DN_TP_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_richness_DN_TP_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_richness_DN_TP_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_richness_DN_TP, plot_bacteria_richness_DN_TP_F, plot_bacteria_richness_DN_TP_S, 
             plot_bacteria_richness_DN_TP_FS, plot_bacteria_richness_DN_TP_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 19. plotting bacteria effective number of species ~ TP  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria effective number of species ~ total phosphorus
plot_bacteria_ENS_TP <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_TP, y=mean_ENS_D, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria ENS ~ TP (Treatment F)

# filtering only Treatment = F
plot_bacteria_ENS_TP_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_TP, y=mean_ENS_D, 
                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_ENS_TP_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_TP, y=mean_ENS_D, 
                                      color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_ENS_TP_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_TP, y=mean_ENS_D, 
                                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_ENS_TP_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_TP, y=mean_ENS_D, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_ENS_TP, plot_bacteria_ENS_TP_F, plot_bacteria_ENS_TP_S, 
             plot_bacteria_ENS_TP_FS, plot_bacteria_ENS_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 20. plotting bacteria effective number of species ~ TN  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria effective number of species ~ total nitrogen
plot_bacteria_ENS_TN <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_TN, y=mean_ENS_D, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria ENS ~ TN (Treatment F)

# filtering only Treatment = F
plot_bacteria_ENS_TN_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_TN, y=mean_ENS_D, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_ENS_TN_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_TN, y=mean_ENS_D, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_ENS_TN_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_TN, y=mean_ENS_D, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_ENS_TN_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_TN, y=mean_ENS_D, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_ENS_TN, plot_bacteria_ENS_TN_F, plot_bacteria_ENS_TN_S, 
             plot_bacteria_ENS_TN_FS, plot_bacteria_ENS_TN_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 21. plotting bacteria effective number of species ~ DOC  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria effective number of species ~ DOC
plot_bacteria_ENS_DOC <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria ENS ~ DOC (Treatment F)

# filtering only Treatment = F
plot_bacteria_ENS_DOC_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_ENS_DOC_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_ENS_DOC_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_ENS_DOC_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_ENS_DOC, plot_bacteria_ENS_DOC_F, plot_bacteria_ENS_DOC_S, 
             plot_bacteria_ENS_DOC_FS, plot_bacteria_ENS_DOC_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 22. plotting bacteria effective number of species ~ PAR  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria effective number of species ~ PAR
plot_bacteria_ENS_PAR <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria ENS ~ TN (Treatment F)

# filtering only Treatment = F
plot_bacteria_ENS_PAR_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_ENS_PAR_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_ENS_PAR_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_ENS_PAR_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_ENS_PAR, plot_bacteria_ENS_PAR_F, plot_bacteria_ENS_PAR_S, 
             plot_bacteria_ENS_PAR_FS, plot_bacteria_ENS_PAR_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 23. plotting bacteria effective number of species ~ Temp  ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for bacteria effective number of species ~ Temperature
plot_bacteria_ENS_Temp <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  xlim(11,19.5)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria ENS ~ TN (Treatment F)

# filtering only Treatment = F
plot_bacteria_ENS_Temp_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  xlim(11,19.5)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_bacteria_ENS_Temp_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_bacteria_ENS_Temp_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_ENS_Temp_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_ENS_Temp, plot_bacteria_ENS_Temp_F, plot_bacteria_ENS_Temp_S, 
             plot_bacteria_ENS_Temp_FS, plot_bacteria_ENS_Temp_C, ncol=3)


#-------------------------------------------------------------------------------------------#

## 24. plotting bacteria effective number of species ~ TN/TP ratio  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria effective number of species ~ TN/TP ratio
plot_bacteria_ENS_TN_TP <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria ENS ~ TN/TP ratio (Treatment F)
plot_bacteria_ENS_TN_TP_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_ENS_TN_TP_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_ENS_TN_TP_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_ENS_TN_TP_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_ENS_TN_TP, plot_bacteria_ENS_TN_TP_F, plot_bacteria_ENS_TN_TP_S, 
             plot_bacteria_ENS_TN_TP_FS, plot_bacteria_ENS_TN_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 25. plotting bacteria effective number of species ~ DN/TP ratio  ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for bacteria effective number of species ~ DN/TP ratio
plot_bacteria_ENS_DN_TP <- ggplot(data=mean_bacteria_diversity_environ, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for bacteria ENS ~ DN/TP ratio (Treatment F)
plot_bacteria_ENS_DN_TP_F <- ggplot(data=mean_bacteria_diversity_environ_F, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_ENS_DN_TP_S <- ggplot(data=mean_bacteria_diversity_environ_S, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_bacteria_ENS_DN_TP_FS <- ggplot(data=mean_bacteria_diversity_environ_FS, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_bacteria_ENS_DN_TP_C <- ggplot(data=mean_bacteria_diversity_environ_C, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Bacteria for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_bacteria_ENS_DN_TP, plot_bacteria_ENS_DN_TP_F, plot_bacteria_ENS_DN_TP_S, 
             plot_bacteria_ENS_DN_TP_FS, plot_bacteria_ENS_DN_TP_C, ncol=3)

