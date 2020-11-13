#-------------------------------------------------------------------------------------------#

## Community structure variables ~ Environmental parameters plots for Phytoplankton ####

#-------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------#

## 0. About ####

#-------------------------------------------------------------------------------------------#
# Community structure variables:
# Biovolume
# Chl a (spectrophometer) 
# Evenness 
# Richness
# ENS 

# environmental parameters:
# TP
# TN 
# DOC 
# light (PAR I)
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
library(gdata)
#-------------------------------------------------------------------------------------------#

## 2. load and transform zooplankton biovolume dataset ####

#-------------------------------------------------------------------------------------------#
# load data 
phyotplankton_biovolume1 <- read.xls("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/phyto_biovolume.xlsx")

# delete columns 
phyotplankton_biovolume <- phyotplankton_biovolume1[-c(8:14, 16:17)] 

# changing names of column "Lake"
phyotplankton_biovolume$Lake<- as.character(phyotplankton_biovolume$Lake)

phyotplankton_biovolume$Lake[phyotplankton_biovolume$Lake == "Asa"] <- "Feresjoen"

phyotplankton_biovolume$Lake[phyotplankton_biovolume$Lake == "Skogaryd"] <- "Erssjoen"

phyotplankton_biovolume$Lake[phyotplankton_biovolume$Lake == "Svartberget"] <- "Stortjaern"

phyotplankton_biovolume$Lake <- as.factor(phyotplankton_biovolume$Lake)

# changing names for column "Experiment"
phyotplankton_biovolume$Experiment <- as.integer(phyotplankton_biovolume$Experiment)

phyotplankton_biovolume$Experiment[phyotplankton_biovolume$Experiment == 1] <- "Spring"

phyotplankton_biovolume$Experiment[phyotplankton_biovolume$Experiment == 2] <- "Summer"

phyotplankton_biovolume$Experiment <- as.character(phyotplankton_biovolume$Experiment)

# delete all replicate / treatment "lake"
levels(phyotplankton_biovolume$Replicate)
phyotplankton_biovolume <-phyotplankton_biovolume[ ! (phyotplankton_biovolume$Replicate %in% c("lake", "Lake")), ]

levels(phyotplankton_biovolume$Treatment)
phyotplankton_biovolume <- phyotplankton_biovolume[ ! (phyotplankton_biovolume$Treatment %in% c("Lake")), ]

# calculate mean values for Clean_biomass in extra column 
phyotplankton_biovolume_mean <- phyotplankton_biovolume %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_biovolume = mean(Biovol, na.rm=T))

phyotplankton_biovolume = phyotplankton_biovolume %>%
  ungroup(Lake, Experiment, Treatment, Enclosure)

#-------------------------------------------------------------------------------------------#

## 3. load and transform env_parameters dataset ####

#-------------------------------------------------------------------------------------------#
env_parameters2 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/physico_chemistry_fish.csv", sep=";")

# library("readxl")
env_parameters1<- read_excel("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Chemistry_Fish_SITES_AquaNet_2017.xlsx")


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

env_parameters$TP <- as.numeric(env_parameters$TP)

env_parameters$DN <- as.numeric(env_parameters$DN)

env_parameters$DOC <- as.numeric(env_parameters$DOC)

env_parameters$PAR_I <- as.numeric(env_parameters$PAR_I)

env_parameters$Temperature <- as.numeric(env_parameters$Temperature)

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

# write.csv(env_parameters_mean,"/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/mean_values_environmental_parameters.csv")


#-------------------------------------------------------------------------------------------#

## 4. merge datasets ####

#-------------------------------------------------------------------------------------------#
mean_phyto_biovolume_environ <- merge(x=env_parameters_mean, y=phyotplankton_biovolume_mean, 
                                    by =c("Lake", "Experiment", "Enclosure", "Treatment"), 
                                    all.x=TRUE)
#set Experiment as factor
mean_phyto_biovolume_environ$Experiment <- as.factor(mean_phyto_biovolume_environ$Experiment)

#-------------------------------------------------------------------------------------------#

## 5. plotting phytplankton biovolume  ~  TP####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton biovolume ~ total phosphorus
plot_phyto_biovolume_TP <- ggplot(data=mean_phyto_biovolume_environ, aes(x=mean_TP, y=mean_biovolume, 
                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ total phosphorus (Treatment F)

# filtering only Treatment = F
mean_phyto_biovolume_environ_F <- mean_phyto_biovolume_environ[which(mean_phyto_biovolume_environ$Treatment=='F'), ]

plot_phyto_biovolume_TP_F <- ggplot(data=mean_phyto_biovolume_environ_F, aes(x=mean_TP, y=mean_biovolume, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
mean_phyto_biovolume_environ_S <- mean_phyto_biovolume_environ[which(mean_phyto_biovolume_environ$Treatment=='S'), ]

plot_phyto_biovolume_TP_S <- ggplot(data=mean_phyto_biovolume_environ_S, aes(x=mean_TP, y=mean_biovolume, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
mean_phyto_biovolume_environ_FS <- mean_phyto_biovolume_environ[which(mean_phyto_biovolume_environ$Treatment=='FS'), ]

plot_phyto_biovolume_TP_FS <- ggplot(data=mean_phyto_biovolume_environ_FS, aes(x=mean_TP, y=mean_biovolume, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
 scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
mean_phyto_biovolume_environ_C <- mean_phyto_biovolume_environ[which(mean_phyto_biovolume_environ$Treatment=='C'), ]

plot_phyto_biovolume_TP_C <- ggplot(data=mean_phyto_biovolume_environ_C, aes(x=mean_TP, y=mean_biovolume, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Biovolume (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_biovolume_TP, plot_phyto_biovolume_TP_F, plot_phyto_biovolume_TP_S, 
             plot_phyto_biovolume_TP_FS, plot_phyto_biovolume_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 6. plotting phytplankton biovolume  ~  TN ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for phytoplankton biovolume ~ total nitrogen
plot_phyto_biovolume_TN <- ggplot(data=mean_phyto_biovolume_environ, aes(x=mean_TN, y=mean_biovolume, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton biovolume ~ total nitrogen (Treatment F)

# filtering only Treatment = F
plot_phyto_biovolume_TN_F <- ggplot(data=mean_phyto_biovolume_environ_F, aes(x=mean_TN, y=mean_biovolume, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_biovolume_TN_S <- ggplot(data=mean_phyto_biovolume_environ_S, aes(x=mean_TN, y=mean_biovolume, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_biovolume_TN_FS <- ggplot(data=mean_phyto_biovolume_environ_FS, aes(x=mean_TN, y=mean_biovolume, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_biovolume_TN_C <- ggplot(data=mean_phyto_biovolume_environ_C, aes(x=mean_TN, y=mean_biovolume, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Biovolume (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_biovolume_TN, plot_phyto_biovolume_TN_F, plot_phyto_biovolume_TN_S, 
             plot_phyto_biovolume_TN_FS, plot_phyto_biovolume_TN_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 7. plotting phytplankton biovolume  ~  DOC ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for phytoplankton biovolume ~ DOC
plot_phyto_biovolume_DOC <- ggplot(data=mean_phyto_biovolume_environ, aes(x=mean_DOC, y=mean_biovolume, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton biovolume ~ total nitrogen (Treatment F)

# filtering only Treatment = F
plot_phyto_biovolume_DOC_F <- ggplot(data=mean_phyto_biovolume_environ_F, aes(x=mean_DOC, y=mean_biovolume, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_biovolume_DOC_S <- ggplot(data=mean_phyto_biovolume_environ_S, aes(x=mean_DOC, y=mean_biovolume, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_biovolume_DOC_FS <- ggplot(data=mean_phyto_biovolume_environ_FS, aes(x=mean_DOC, y=mean_biovolume, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC(mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_biovolume_DOC_C <- ggplot(data=mean_phyto_biovolume_environ_C, aes(x=mean_DOC, y=mean_biovolume, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Biovolume (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_biovolume_DOC, plot_phyto_biovolume_DOC_F, plot_phyto_biovolume_DOC_S, 
             plot_phyto_biovolume_DOC_FS, plot_phyto_biovolume_DOC_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 8. plotting phytplankton biovolume  ~  PAR ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for phytoplankton biovolume ~ PAR
plot_phyto_biovolume_PAR <- ggplot(data=mean_phyto_biovolume_environ, aes(x=mean_PAR, y=mean_biovolume, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton biovolume ~ PAR (Treatment F)

# filtering only Treatment = F
plot_phyto_biovolume_PAR_F <- ggplot(data=mean_phyto_biovolume_environ_F, aes(x=mean_PAR, y=mean_biovolume, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_biovolume_PAR_S <- ggplot(data=mean_phyto_biovolume_environ_S, aes(x=mean_PAR, y=mean_biovolume, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_biovolume_PAR_FS <- ggplot(data=mean_phyto_biovolume_environ_FS, aes(x=mean_PAR, y=mean_biovolume, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_biovolume_PAR_C <- ggplot(data=mean_phyto_biovolume_environ_C, aes(x=mean_PAR, y=mean_biovolume, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Biovolume (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_biovolume_PAR, plot_phyto_biovolume_PAR_F, plot_phyto_biovolume_PAR_S, 
             plot_phyto_biovolume_PAR_FS, plot_phyto_biovolume_PAR_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 9. plotting phytplankton biovolume  ~  Temperature ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for phytoplankton biovolume ~ Temp
plot_phyto_biovolume_Temp <- ggplot(data=mean_phyto_biovolume_environ, aes(x=mean_Temp, y=mean_biovolume, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  xlim(11,20) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton biovolume ~ Temp (Treatment F)

# filtering only Treatment = F
plot_phyto_biovolume_Temp_F <- ggplot(data=mean_phyto_biovolume_environ_F, aes(x=mean_Temp, y=mean_biovolume, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  xlim(12,19) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_biovolume_Temp_S <- ggplot(data=mean_phyto_biovolume_environ_S, aes(x=mean_Temp, y=mean_biovolume, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_biovolume_Temp_FS <- ggplot(data=mean_phyto_biovolume_environ_FS, aes(x=mean_Temp, y=mean_biovolume, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_biovolume_Temp_C <- ggplot(data=mean_phyto_biovolume_environ_C, aes(x=mean_Temp, y=mean_biovolume, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Biovolume (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_biovolume_Temp, plot_phyto_biovolume_Temp_F, plot_phyto_biovolume_Temp_S, 
             plot_phyto_biovolume_Temp_FS, plot_phyto_biovolume_Temp_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 10. plotting phytplankton biovolume  ~  TN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton biovolume ~ TN/TP ratio
plot_phyto_biovolume_TN_TP <- ggplot(data=mean_phyto_biovolume_environ, aes(x=mean_TN_TP_ratio, y=mean_biovolume, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton biovolume ~ TN/TP ratio (Treatment F)
plot_phyto_biovolume_TN_TP_F <- ggplot(data=mean_phyto_biovolume_environ_F, aes(x=mean_TN_TP_ratio, y=mean_biovolume, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_phyto_biovolume_TN_TP_S <- ggplot(data=mean_phyto_biovolume_environ_S, aes(x=mean_TN_TP_ratio, y=mean_biovolume, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_phyto_biovolume_TN_TP_FS <- ggplot(data=mean_phyto_biovolume_environ_FS, aes(x=mean_TN_TP_ratio, y=mean_biovolume, 
                                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_phyto_biovolume_TN_TP_C <- ggplot(data=mean_phyto_biovolume_environ_C, aes(x=mean_TN_TP_ratio, y=mean_biovolume, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Biovolume (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_biovolume_TN_TP, plot_phyto_biovolume_TN_TP_F, plot_phyto_biovolume_TN_TP_S, 
             plot_phyto_biovolume_TN_TP_FS, plot_phyto_biovolume_TN_TP_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 11. plotting phytplankton biovolume  ~  DN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton biovolume ~ DN/TP ratio
plot_phyto_biovolume_DN_TP <- ggplot(data=mean_phyto_biovolume_environ, aes(x=mean_DN_TP_ratio, y=mean_biovolume, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton biovolume ~ DN/TP ratio (Treatment F)
plot_phyto_biovolume_DN_TP_F <- ggplot(data=mean_phyto_biovolume_environ_F, aes(x=mean_DN_TP_ratio, y=mean_biovolume, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_phyto_biovolume_DN_TP_S <- ggplot(data=mean_phyto_biovolume_environ_S, aes(x=mean_DN_TP_ratio, y=mean_biovolume, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_phyto_biovolume_DN_TP_FS <- ggplot(data=mean_phyto_biovolume_environ_FS, aes(x=mean_DN_TP_ratio, y=mean_biovolume, 
                                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Biovolume (mean & log10-transformed)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_phyto_biovolume_DN_TP_C <- ggplot(data=mean_phyto_biovolume_environ_C, aes(x=mean_DN_TP_ratio, y=mean_biovolume, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Biovolume (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_biovolume_DN_TP, plot_phyto_biovolume_DN_TP_F, plot_phyto_biovolume_DN_TP_S, 
             plot_phyto_biovolume_DN_TP_FS, plot_phyto_biovolume_DN_TP_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 12. plotting chl a  ~  TP ####

#-------------------------------------------------------------------------------------------#
phyto_chlorophyll1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/physico_chemistry_fish.csv", sep=";")

#filtering 
phyto_chlorophyll <- select(phyto_chlorophyll1, Lake, Experiment, Sampling, Exp_day, Sampling_day, 
                         Sampling_month, Enclosure, Treatment, Replicate, Chla, TP, TN, DN, DOC, PAR_I, Temperature)

# changing names for column "Experiment"
phyto_chlorophyll$Experiment <- as.integer(phyto_chlorophyll$Experiment)

phyto_chlorophyll$Experiment[phyto_chlorophyll$Experiment == 1] <- "Spring"

phyto_chlorophyll$Experiment[phyto_chlorophyll$Experiment == 2] <- "Summer"

phyto_chlorophyll$Experiment <- as.character(phyto_chlorophyll$Experiment)

# changing names of column "Lake"
levels(phyto_chlorophyll$Lake)
# "Asa"         "Bolmen"      "Erken"       "Skogaryd"    "Svartberget"

phyto_chlorophyll$Lake<- as.character(phyto_chlorophyll$Lake)

phyto_chlorophyll$Lake[phyto_chlorophyll$Lake == "Asa"] <- "Feresjoen"

phyto_chlorophyll$Lake[phyto_chlorophyll$Lake == "Skogaryd"] <- "Erssjoen"

phyto_chlorophyll$Lake[phyto_chlorophyll$Lake == "Svartberget"] <- "Stortjaern"

phyto_chlorophyll$Lake <- as.factor(phyto_chlorophyll$Lake)

levels(phyto_chlorophyll$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# set TN as numeric
phyto_chlorophyll$TN <- as.character(phyto_chlorophyll$TN)
phyto_chlorophyll$TN <- as.numeric(phyto_chlorophyll$TN)
# delete all replicate / treatment "lake"
levels(phyto_chlorophyll$Replicate)
phyto_chlorophyll<- phyto_chlorophyll[ ! (phyto_chlorophyll$Replicate %in% c("lake", "Lake")), ]

levels(env_parameters$Treatment)
phyto_chlorophyll <- phyto_chlorophyll[ ! (phyto_chlorophyll$Treatment %in% c("Lake")), ]

# new column with TN/TP ratio
phyto_chlorophyll <- mutate(phyto_chlorophyll, "TN_TP_ratio" = TN / TP)

# new column with DN/TP ratio
phyto_chlorophyll <- mutate(phyto_chlorophyll, "DN_TP_ratio" = DN / TP)

# creating mean values for all environmental parameters
phyto_chlorophyll_mean <- phyto_chlorophyll %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_TP = mean(TP, na.rm=T), mean_TN= mean(TN, na.rm=T), mean_DOC= mean(DOC, na.rm=T), 
            mean_PAR= mean(PAR_I, na.rm=T), mean_Temp = mean(Temperature, na.rm=T), 
            mean_Chla = mean(Chla, nar.rm=T), mean_TN_TP_ratio = mean(TN_TP_ratio, na.rm=T), 
            mean_DN_TP_ratio=mean(DN_TP_ratio, na.rm=T))

# plotting all lakes, all treatments, all experiments for Chla ~ TP
plot_phyto_chla_TP <- ggplot(data=phyto_chlorophyll_mean, aes(x=mean_TP, y=mean_Chla, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Chla (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ total phosphorus (Treatment F)

# filtering only Treatment = F
phyto_chlorophyll_mean_F <- phyto_chlorophyll_mean[which(phyto_chlorophyll_mean$Treatment=='F'), ]

plot_phyto_chla_TP_F <- ggplot(data=phyto_chlorophyll_mean_F, aes(x=mean_TP, y=mean_Chla, 
                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
phyto_chlorophyll_mean_S <- phyto_chlorophyll_mean[which(phyto_chlorophyll_mean$Treatment=='S'), ]

plot_phyto_chla_TP_S <- ggplot(data=phyto_chlorophyll_mean_S, aes(x=mean_TP, y=mean_Chla, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
phyto_chlorophyll_mean_FS <- phyto_chlorophyll_mean[which(phyto_chlorophyll_mean$Treatment=='FS'), ]

plot_phyto_chla_TP_FS <- ggplot(data=phyto_chlorophyll_mean_FS, aes(x=mean_TP, y=mean_Chla, 
                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorphyll a for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
phyto_chlorophyll_mean_C <- phyto_chlorophyll_mean[which(phyto_chlorophyll_mean$Treatment=='C'), ]

plot_phyto_chla_TP_C <- ggplot(data=phyto_chlorophyll_mean_C, aes(x=mean_TP, y=mean_Chla, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_chla_TP, plot_phyto_chla_TP_F, plot_phyto_chla_TP_S, 
             plot_phyto_chla_TP_FS, plot_phyto_chla_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 13. plotting chl a  ~  TN ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for Chla ~ TN
plot_phyto_chla_TN <- ggplot(data=phyto_chlorophyll_mean, aes(x=mean_TN, y=mean_Chla, 
                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Chla (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ total nitrogen (Treatment F)

# filtering only Treatment = F
plot_phyto_chla_TN_F <- ggplot(data=phyto_chlorophyll_mean_F, aes(x=mean_TN, y=mean_Chla, 
                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_chla_TN_S <- ggplot(data=phyto_chlorophyll_mean_S, aes(x=mean_TN, y=mean_Chla, 
                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_chla_TN_FS <- ggplot(data=phyto_chlorophyll_mean_FS, aes(x=mean_TN, y=mean_Chla, 
                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorphyll a for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_chla_TN_C <- ggplot(data=phyto_chlorophyll_mean_C, aes(x=mean_TN, y=mean_Chla, 
                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_chla_TN, plot_phyto_chla_TN_F, plot_phyto_chla_TN_S, 
             plot_phyto_chla_TN_FS, plot_phyto_chla_TN_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 14. plotting chl a  ~  DOC ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for Chla ~ DOC
plot_phyto_chla_DOC <- ggplot(data=phyto_chlorophyll_mean, aes(x=mean_DOC, y=mean_Chla, 
                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Chla (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ DOC (Treatment F)

# filtering only Treatment = F
plot_phyto_chla_DOC_F <- ggplot(data=phyto_chlorophyll_mean_F, aes(x=mean_DOC, y=mean_Chla, 
                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_chla_DOC_S <- ggplot(data=phyto_chlorophyll_mean_S, aes(x=mean_DOC, y=mean_Chla, 
                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_chla_DOC_FS <- ggplot(data=phyto_chlorophyll_mean_FS, aes(x=mean_DOC, y=mean_Chla, 
                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorphyll a for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_chla_DOC_C <- ggplot(data=phyto_chlorophyll_mean_C, aes(x=mean_DOC, y=mean_Chla, 
                                                                  color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_chla_DOC, plot_phyto_chla_DOC_F, plot_phyto_chla_DOC_S, 
             plot_phyto_chla_DOC_FS, plot_phyto_chla_DOC_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 15. plotting chl a  ~  PAR ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for Chla ~ PAR
plot_phyto_chla_PAR <- ggplot(data=phyto_chlorophyll_mean, aes(x=mean_PAR, y=mean_Chla, 
                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Chla (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for zooplankton biomass ~ PAR (Treatment F)

# filtering only Treatment = F
plot_phyto_chla_PAR_F <- ggplot(data=phyto_chlorophyll_mean_F, aes(x=mean_PAR, y=mean_Chla, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_chla_PAR_S <- ggplot(data=phyto_chlorophyll_mean_S, aes(x=mean_PAR, y=mean_Chla, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_chla_PAR_FS <- ggplot(data=phyto_chlorophyll_mean_FS, aes(x=mean_PAR, y=mean_Chla, 
                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorphyll a for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_chla_PAR_C <- ggplot(data=phyto_chlorophyll_mean_C, aes(x=mean_PAR, y=mean_Chla, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_chla_PAR, plot_phyto_chla_PAR_F, plot_phyto_chla_PAR_S, 
             plot_phyto_chla_PAR_FS, plot_phyto_chla_PAR_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 16. plotting chl a  ~  Temp ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for Chla ~ Temp
plot_phyto_chla_Temp <- ggplot(data=phyto_chlorophyll_mean, aes(x=mean_Temp, y=mean_Chla, 
                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Chla (mean)") +
  xlim(11,20) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for Chl a ~ Temp (Treatment F)

# filtering only Treatment = F
plot_phyto_chla_Temp_F <- ggplot(data=phyto_chlorophyll_mean_F, aes(x=mean_Temp, y=mean_Chla, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Chl a (mean)") +
  xlim(11.5,20) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_chla_Temp_S <- ggplot(data=phyto_chlorophyll_mean_S, aes(x=mean_Temp, y=mean_Chla, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_chla_Temp_FS <- ggplot(data=phyto_chlorophyll_mean_FS, aes(x=mean_Temp, y=mean_Chla, 
                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorphyll a for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_chla_Temp_C <- ggplot(data=phyto_chlorophyll_mean_C, aes(x=mean_Temp, y=mean_Chla, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Chl a (mean)") +
  xlim(15,19)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_chla_Temp, plot_phyto_chla_Temp_F, plot_phyto_chla_Temp_S, 
             plot_phyto_chla_Temp_FS, plot_phyto_chla_Temp_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 17. plotting chl a  ~  TN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for Chla ~ TN/TP ratio
plot_phyto_chla_TN_TP <- ggplot(data=phyto_chlorophyll_mean, aes(x=mean_TN_TP_ratio, y=mean_Chla, 
                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Chla (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for Chl a ~ TN/TP ratio (Treatment F)

# filtering only Treatment = F
plot_phyto_chla_TN_TP_F <- ggplot(data=phyto_chlorophyll_mean_F, aes(x=mean_TN_TP_ratio, y=mean_Chla, 
                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_chla_TN_TP_S <- ggplot(data=phyto_chlorophyll_mean_S, aes(x=mean_TN_TP_ratio, y=mean_Chla, 
                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_chla_TN_TP_FS <- ggplot(data=phyto_chlorophyll_mean_FS, aes(x=mean_TN_TP_ratio, y=mean_Chla, 
                                                                      color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorphyll a for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_chla_TN_TP_C <- ggplot(data=phyto_chlorophyll_mean_C, aes(x=mean_TN_TP_ratio, y=mean_Chla, 
                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_chla_TN_TP, plot_phyto_chla_TN_TP_F, plot_phyto_chla_TN_TP_S, 
             plot_phyto_chla_TN_TP_FS, plot_phyto_chla_TN_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 18. plotting chl a  ~  DN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for Chla ~ DN/TP ratio
plot_phyto_chla_DN_TP <- ggplot(data=phyto_chlorophyll_mean, aes(x=mean_DN_TP_ratio, y=mean_Chla, 
                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Chla (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for Chl a ~ DN/TP ratio (Treatment F)

# filtering only Treatment = F
plot_phyto_chla_DN_TP_F <- ggplot(data=phyto_chlorophyll_mean_F, aes(x=mean_DN_TP_ratio, y=mean_Chla, 
                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_chla_DN_TP_S <- ggplot(data=phyto_chlorophyll_mean_S, aes(x=mean_DN_TP_ratio, y=mean_Chla, 
                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_chla_DN_TP_FS <- ggplot(data=phyto_chlorophyll_mean_FS, aes(x=mean_DN_TP_ratio, y=mean_Chla, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorphyll a for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_chla_DN_TP_C <- ggplot(data=phyto_chlorophyll_mean_C, aes(x=mean_DN_TP_ratio, y=mean_Chla, 
                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  scale_y_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Chl a (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_chla_DN_TP, plot_phyto_chla_DN_TP_F, plot_phyto_chla_DN_TP_S, 
             plot_phyto_chla_DN_TP_FS, plot_phyto_chla_DN_TP_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 19. plotting phytoplankton evenness  ~  TP ####

#-------------------------------------------------------------------------------------------#
# loading dataset phytoplankton diversity 
phytoplankton_diversity1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Diversity/diversity_metrics.csv")

# deleting first column
phytoplankton_diversity <- phytoplankton_diversity1[-1] 

# renaming lakes
levels(zooplankton_diversity$Lake)
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
phytoplankton_diversity <- phytoplankton_diversity[ ! (phytoplankton_diversity$Treatment %in% c("Lake")), ]

# filtering only phytoplankton for column "variable" 
phytoplankton_diversity <- phytoplankton_diversity[which(phytoplankton_diversity$variable %in% c ('phytoplankton1', 'phytoplankton2')), ]

# calculate mean values for species richness, evenness, effective number of species in extra columns 
phytoplankton_diversity_mean <- phytoplankton_diversity %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_J = mean(J, na.rm=T), mean_s = mean(S, na.rm=T), mean_ENS_D = mean(ENS_D, na.rm=T))

phytoplankton_diversity_mean = phytoplankton_diversity_mean %>%
  ungroup(Lake, Experiment, Treatment, Enclosure)


# combining datasets
mean_phyto_diversity_environ <- merge(x=env_parameters_mean, y=phytoplankton_diversity_mean, 
                                      by =c("Lake", "Experiment", "Enclosure", "Treatment"), 
                                      all.x=TRUE)

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton evenness ~ total phosphorus
plot_phyto_evenness_TP <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_TP, y=mean_J, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton biomass ~ TP (Treatment F)

# filtering only Treatment = F
mean_phyto_diversity_environ_F <- mean_phyto_diversity_environ[which(mean_phyto_diversity_environ$Treatment=='F'), ]

plot_phyto_evenness_TP_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_TP, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
mean_phyto_diversity_environ_S <- mean_phyto_diversity_environ[which(mean_phyto_diversity_environ$Treatment=='S'), ]

plot_phyto_evenness_TP_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_TP, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
mean_phyto_diversity_environ_FS <- mean_phyto_diversity_environ[which(mean_phyto_diversity_environ$Treatment=='FS'), ]

plot_phyto_evenness_TP_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_TP, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
mean_phyto_diversity_environ_C <- mean_phyto_diversity_environ[which(mean_phyto_diversity_environ$Treatment=='C'), ]

plot_phyto_evenness_TP_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_TP, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_evenness_TP, plot_phyto_evenness_TP_F, plot_phyto_evenness_TP_S, 
             plot_phyto_evenness_TP_FS, plot_phyto_evenness_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 20. plotting phytoplankton evenness  ~  TN ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton evenness ~ total nitrogen
plot_phyto_evenness_TN <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_TN, y=mean_J, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton biomass ~ TN (Treatment F)

# filtering only Treatment = F
plot_phyto_evenness_TN_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_TN, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_evenness_TN_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_TN, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_evenness_TN_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_TN, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_evenness_TN_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_TN, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_evenness_TN, plot_phyto_evenness_TN_F, plot_phyto_evenness_TN_S, 
             plot_phyto_evenness_TN_FS, plot_phyto_evenness_TN_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 21. plotting phytoplankton evenness  ~  DOC ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton evenness ~ DOC
plot_phyto_evenness_DOC <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_DOC, y=mean_J, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton biomass ~ DOC (Treatment F)

# filtering only Treatment = F
plot_phyto_evenness_DOC_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_DOC, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_evenness_DOC_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_DOC, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_evenness_DOC_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_DOC, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_evenness_DOC_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_DOC, y=mean_J, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_evenness_DOC, plot_phyto_evenness_DOC_F, plot_phyto_evenness_DOC_S, 
             plot_phyto_evenness_DOC_FS, plot_phyto_evenness_DOC_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 22. plotting phytoplankton evenness  ~  PAR ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton evenness ~ PAR
plot_phyto_evenness_PAR <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_PAR, y=mean_J, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton biomass ~ PAR (Treatment F)

# filtering only Treatment = F
plot_phyto_evenness_PAR_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_PAR, y=mean_J, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_evenness_PAR_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_PAR, y=mean_J, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_evenness_PAR_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_PAR, y=mean_J, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_evenness_PAR_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_PAR, y=mean_J, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_evenness_PAR, plot_phyto_evenness_PAR_F, plot_phyto_evenness_PAR_S, 
             plot_phyto_evenness_PAR_FS, plot_phyto_evenness_PAR_C, ncol=3)


#-------------------------------------------------------------------------------------------#

## 23. plotting phytoplankton evenness  ~  Temperature ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton evenness ~ Temperature
plot_phyto_evenness_Temp <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_Temp, y=mean_J, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  xlim(11,19.5)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton evenness ~ Temp (Treatment F)

# filtering only Treatment = F
plot_phyto_evenness_Temp_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_Temp, y=mean_J, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  xlim(11.5,19.5)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_evenness_Temp_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_Temp, y=mean_J, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_evenness_Temp_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_Temp, y=mean_J, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_evenness_Temp_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_Temp, y=mean_J, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_evenness_Temp, plot_phyto_evenness_Temp_F, plot_phyto_evenness_Temp_S, 
             plot_phyto_evenness_Temp_FS, plot_phyto_evenness_Temp_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 24. plotting phytoplankton evenness  ~  TN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton evenness ~ TN/TP ratio
plot_phyto_evenness_TN_TP <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton evenness ~ TN/TP ratio (Treatment F)
plot_phyto_evenness_TN_TP_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_evenness_TN_TP_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_evenness_TN_TP_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_evenness_TN_TP_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_TN_TP_ratio, y=mean_J, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_evenness_TN_TP, plot_phyto_evenness_TN_TP_F, plot_phyto_evenness_TN_TP_S, 
             plot_phyto_evenness_TN_TP_FS, plot_phyto_evenness_TN_TP_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 25. plotting phytoplankton evenness  ~  DN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton evenness ~ DN/TP ratio
plot_phyto_evenness_DN_TP <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton evenness ~ DN/TP ratio (Treatment F)
plot_phyto_evenness_DN_TP_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_evenness_DN_TP_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_evenness_DN_TP_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_evenness_DN_TP_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_DN_TP_ratio, y=mean_J, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Evenness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_evenness_DN_TP, plot_phyto_evenness_DN_TP_F, plot_phyto_evenness_DN_TP_S, 
             plot_phyto_evenness_DN_TP_FS, plot_phyto_evenness_DN_TP_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 26. plotting phytoplankton species richness  ~  TP ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton richness ~ total phosphorus
plot_phyto_richness_TP <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_TP, y=mean_s, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton richness ~ TP (Treatment F)

# filtering only Treatment = F
plot_phyto_richness_TP_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_TP, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_richness_TP_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_TP, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_richness_TP_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_TP, y=mean_s, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_richness_TP_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_TP, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_richness_TP, plot_phyto_richness_TP_F, plot_phyto_richness_TP_S, 
             plot_phyto_richness_TP_FS, plot_phyto_richness_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 27. plotting phytoplankton species richness  ~  TN ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton richness ~ total nitrogen
plot_phyto_richness_TN <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_TN, y=mean_s, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton richness ~ TN (Treatment F)

# filtering only Treatment = F
plot_phyto_richness_TN_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_TN, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_richness_TN_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_TN, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_richness_TN_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_TN, y=mean_s, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_richness_TN_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_TN, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_richness_TN, plot_phyto_richness_TN_F, plot_phyto_richness_TN_S, 
             plot_phyto_richness_TN_FS, plot_phyto_richness_TN_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 28. plotting phytoplankton species richness  ~  DOC ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton richness ~ DOC
plot_phyto_richness_DOC <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_DOC, y=mean_s, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton richness ~ DOC (Treatment F)

# filtering only Treatment = F
plot_phyto_richness_DOC_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_DOC, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_richness_DOC_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_DOC, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_richness_DOC_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_DOC, y=mean_s, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_richness_DOC_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_DOC, y=mean_s, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_richness_DOC, plot_phyto_richness_DOC_F, plot_phyto_richness_DOC_S, 
             plot_phyto_richness_DOC_FS, plot_phyto_richness_DOC_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 29. plotting phytoplankton species richness  ~  PAR ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton richness ~ PAR
plot_phyto_richness_PAR <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_PAR, y=mean_s, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton richness ~ PAR (Treatment F)

# filtering only Treatment = F
plot_phyto_richness_PAR_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_PAR, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_richness_PAR_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_PAR, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_richness_PAR_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_PAR, y=mean_s, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_richness_PAR_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_PAR, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_richness_PAR, plot_phyto_richness_PAR_F, plot_phyto_richness_PAR_S, 
             plot_phyto_richness_PAR_FS, plot_phyto_richness_PAR_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 30. plotting phytoplankton species richness  ~  Temperature ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton richness ~ Temp
plot_phyto_richness_Temp <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_Temp, y=mean_s, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  xlim(11,20)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton richness ~ Temp (Treatment F)

# filtering only Treatment = F
plot_phyto_richness_Temp_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_Temp, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  xlim(11.5,20)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_richness_Temp_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_Temp, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_richness_Temp_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_Temp, y=mean_s, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_richness_Temp_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_Temp, y=mean_s, 
                                                                             color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_richness_Temp, plot_phyto_richness_Temp_F, plot_phyto_richness_Temp_S, 
             plot_phyto_richness_Temp_FS, plot_phyto_richness_Temp_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 31. plotting phytoplankton species richness  ~  TN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton richness ~ TN/TP ratio
plot_phyto_richness_TN_TP <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton richness ~ TN/TP ratio (Treatment F)
plot_phyto_richness_TN_TP_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_richness_TN_TP_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_richness_TN_TP_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                                color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_richness_TN_TP_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_TN_TP_ratio, y=mean_s, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_richness_TN_TP, plot_phyto_richness_TN_TP_F, plot_phyto_richness_TN_TP_S, 
             plot_phyto_richness_TN_TP_FS, plot_phyto_richness_TN_TP_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 32. plotting phytoplankton species richness  ~  DN/TP ratio ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton richness ~ DN/TP ratio
plot_phyto_richness_DN_TP <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton richness ~ DN/TP ratio (Treatment F)
plot_phyto_richness_DN_TP_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_richness_DN_TP_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_richness_DN_TP_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                                 color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_richness_DN_TP_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_DN_TP_ratio, y=mean_s, 
                                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Species richness (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_richness_DN_TP, plot_phyto_richness_DN_TP_F, plot_phyto_richness_DN_TP_S, 
             plot_phyto_richness_DN_TP_FS, plot_phyto_richness_DN_TP_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 33. plotting phytoplankton effective number of species  ~  TP ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton ENS  ~ total phosphorus
plot_phyto_ENS_TP <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_TP, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton richness ~ TP (Treatment F)

# filtering only Treatment = F
plot_phyto_ENS_TP_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_TP, y=mean_ENS_D, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_ENS_TP_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_TP, y=mean_ENS_D, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_ENS_TP_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_TP, y=mean_ENS_D, 
                                                                              color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_ENS_TP_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_TP, y=mean_ENS_D, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total phosphorus (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_ENS_TP, plot_phyto_ENS_TP_F, plot_phyto_ENS_TP_S, 
             plot_phyto_ENS_TP_FS, plot_phyto_ENS_TP_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 34. plotting phytoplankton effective number of species  ~  TN ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton ENS  ~ total nitrogen
plot_phyto_ENS_TN <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_TN, y=mean_ENS_D, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton richness ~ TP (Treatment F)

# filtering only Treatment = F
plot_phyto_ENS_TN_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_TN, y=mean_ENS_D, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_ENS_TN_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_TN, y=mean_ENS_D, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_ENS_TN_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_TN, y=mean_ENS_D, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_ENS_TN_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_TN, y=mean_ENS_D, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Total nitrogen (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_ENS_TN, plot_phyto_ENS_TN_F, plot_phyto_ENS_TN_S, 
             plot_phyto_ENS_TN_FS, plot_phyto_ENS_TN_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 35. plotting phytoplankton effective number of species  ~  DOC ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton ENS  ~ DOC
plot_phyto_ENS_DOC <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton richness ~ DOC (Treatment F)

# filtering only Treatment = F
plot_phyto_ENS_DOC_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_ENS_DOC_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_ENS_DOC_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_ENS_DOC_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_DOC, y=mean_ENS_D, 
                                                                       color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DOC (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_ENS_DOC, plot_phyto_ENS_DOC_F, plot_phyto_ENS_DOC_S, 
             plot_phyto_ENS_DOC_FS, plot_phyto_ENS_DOC_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 36. plotting phytoplankton effective number of species  ~  PAR ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments for phytoplankton ENS  ~ PAR
plot_phyto_ENS_PAR <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton ENS ~ PAR (Treatment F)

# filtering only Treatment = F
plot_phyto_ENS_PAR_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_ENS_PAR_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_ENS_PAR_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_ENS_PAR_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_PAR, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("PAR (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_ENS_PAR, plot_phyto_ENS_PAR_F, plot_phyto_ENS_PAR_S, 
             plot_phyto_ENS_PAR_FS, plot_phyto_ENS_PAR_C, ncol=3)
#-------------------------------------------------------------------------------------------#

## 37. plotting phytoplankton effective number of species  ~  Temperature ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for phytoplankton ENS  ~ Temp
plot_phyto_ENS_Temp <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                    color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  xlim(11,20)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton ENS ~ Temp (Treatment F)

# filtering only Treatment = F
plot_phyto_ENS_Temp_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  xlim(12,20)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
plot_phyto_ENS_Temp_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
plot_phyto_ENS_Temp_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = C
plot_phyto_ENS_Temp_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_Temp, y=mean_ENS_D, 
                                                                        color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("Temperature (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_ENS_Temp, plot_phyto_ENS_Temp_F, plot_phyto_ENS_Temp_S, 
             plot_phyto_ENS_Temp_FS, plot_phyto_ENS_Temp_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 38. plotting phytoplankton effective number of species  ~  TN/TP ratio ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for phytoplankton ENS  ~ TN/TP ratio
plot_phyto_ENS_TN_TP <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton ENS ~ TN/TP ratio (Treatment F)
plot_phyto_ENS_TN_TP_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_phyto_ENS_TN_TP_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_phyto_ENS_TN_TP_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                           color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_phyto_ENS_TN_TP_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_TN_TP_ratio, y=mean_ENS_D, 
                                                                         color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("TN/TP ratio (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_ENS_TN_TP, plot_phyto_ENS_TN_TP_F, plot_phyto_ENS_TN_TP_S, 
             plot_phyto_ENS_TN_TP_FS, plot_phyto_ENS_TN_TP_C, ncol=3)

#-------------------------------------------------------------------------------------------#

## 39. plotting phytoplankton effective number of species  ~  DN/TP ratio ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments for phytoplankton ENS  ~ DN/TP ratio
plot_phyto_ENS_DN_TP <- ggplot(data=mean_phyto_diversity_environ, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                      color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for all treatments")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments for phytoplankton ENS ~ DN/TP ratio (Treatment F)
plot_phyto_ENS_DN_TP_F <- ggplot(data=mean_phyto_diversity_environ_F, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_phyto_ENS_DN_TP_S <- ggplot(data=mean_phyto_diversity_environ_S, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press disturbance")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_phyto_ENS_DN_TP_FS <- ggplot(data=mean_phyto_diversity_environ_FS, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for press & pulse disturbance")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

plot_phyto_ENS_DN_TP_C <- ggplot(data=mean_phyto_diversity_environ_C, aes(x=mean_DN_TP_ratio, y=mean_ENS_D, 
                                                                          color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  xlab("DN/TP ratio (mean)") +
  ylab("Effective number of Species, Simpson (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton for control")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_phyto_ENS_DN_TP, plot_phyto_ENS_DN_TP_F, plot_phyto_ENS_DN_TP_S, 
             plot_phyto_ENS_DN_TP_FS, plot_phyto_ENS_DN_TP_C, ncol=3)


