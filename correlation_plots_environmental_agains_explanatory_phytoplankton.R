#-------------------------------------------------------------------------------------------#

## Environmental parameters ~ explanatory variables correlation plots for Phytoplankton ####

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

# explanatory variables:
# Biovolumne, Chl a (fluorometer), Chl a (spectrophometer) (Phytoplankton)
# Evenness (Phytoplankton)
# Alpha Diversity (Phytoplankton)
# Growth 

#-------------------------------------------------------------------------------------------#

## 1. load packages and datasets ####

#-------------------------------------------------------------------------------------------#
# load packages
library(tidyverse)
library(scales)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(gdata)


# load environmental parameters:
env_parameters1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Chemistry_Fish_SITES_AquaNet_2017 Kopie.csv", sep=";")

phyotplankton_biomass1 <- read.xls("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/phyto_biovolume.xlsx")

#-------------------------------------------------------------------------------------------#

## 2. editing dataset: env_parameters ####

#-------------------------------------------------------------------------------------------#

show(env_parameters1$Sampling)

# a) only keeping sampling = 6
env_parameters1 <- env_parameters1[!(env_parameters1$Sampling %in% c(0,1,2,3,4,5)),]

# b) deleting Replicate = Lake
env_parameters1 <- env_parameters1[!(env_parameters1$Replicate == "Lake"),]

# c) deleting Treatment = Lake & C 
env_parameters1 <- env_parameters1[!(env_parameters1$Treatment %in% c("C", "Lake")),]


# d) deleting all rows with Enclosures = 1, 7, 10, 16, 21 (controls)
env_parameters1 <- env_parameters1[ ! (env_parameters1$Enclosure %in% c(1, 7, 10, 16,21)), ]


# e) only filtering the parameters we need (TP, TN, DOC, Temperature, PAR_I)
env_parameters <- select(env_parameters1, Lake, Experiment,Enclosure, 
                         Treatment, Replicate, TP, TN, DOC, Temperature, PAR_I)

# d) changing names of column "Lake"

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

## 3. editing dataset: phytoplankton_biomass ####

#-------------------------------------------------------------------------------------------#

# a) deleting Replicate = Lake
phyotplankton_biomass1 <- phyotplankton_biomass1[!(phyotplankton_biomass1$Replicate == "Lake"),]

# b) deleting Treatment = Lake & C 
phyotplankton_biomass1 <- phyotplankton_biomass1[!(phyotplankton_biomass1$Treatment %in% c("C", "Lake")),]

# c) deleting all rows with Enclosures = 1, 7, 10, 16, 21 (controls)
phyotplankton_biomass1 <- phyotplankton_biomass1[ ! (phyotplankton_biomass1$Enclosure %in% c(1, 7, 10, 16,21)), ]

# d) only filtering the parameters we need (TP, TN, DOC, Temperature, PAR_I)
phyotplankton_biomass <- select(phyotplankton_biomass1, Lake, Experiment,Enclosure, 
                         Treatment, Replicate, Biovol)
#-------------------------------------------------------------------------------------------#

## 4. merging both datasets ####

#-------------------------------------------------------------------------------------------#

env_phyto_biomass <- merge(x=phyotplankton_biomass, y=env_parameters, by =c("Lake", "Experiment", 
                      "Enclosure", "Treatment", "Replicate"), all.x=TRUE)
#-------------------------------------------------------------------------------------------#

## 5. Environmental parameters ~ phytplankton biovolume ####

#-------------------------------------------------------------------------------------------#

# TP for phytoplankton
plot_TP_phyto_biomass <- ggplot(data=env_phyto_biomass,aes(x=Biovol, y=TP, 
                                                           color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Biovolume") +
  ylab("Total phosphorus") +
  xlim(0,2000000000)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton biovolume for Experiment day 28 (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# TN for phytoplankton
env_phyto_biomass$TN <- as.numeric(env_phyto_biomass$TN)
plot_TN_phyto_biomass <- ggplot(data=env_phyto_biomass,aes(x=Biovol, y=TN, 
                                                           color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Biovolume") +
  ylab("Total nitrogen") +
  xlim(0,2000000000)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("  ")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# DOC for phytoplankton
plot_DOC_phyto_biomass <- ggplot(data=env_phyto_biomass,aes(x=Biovol, y=DOC, 
                                                           color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Biovolume") +
  ylab("DOC") +
  xlim(0,2000000000)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape = FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_TP_phyto_biomass, plot_TN_phyto_biomass, plot_DOC_phyto_biomass, ncol= 2)

# Temp for phytoplankton
plot_Temp_phyto_biomass <- ggplot(data=env_phyto_biomass,aes(x=Biovol, y=Temperature, 
                                                            color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Biovolume") +
  ylab("Temperature") +
  xlim(0,2000000000)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton biovolume for Experiment day 28 (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# PAR for phytoplankton
plot_PAR_phyto_biomass <- ggplot(data=env_phyto_biomass,aes(x=Biovol, y=PAR_I, 
                                                             color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Biovolume") +
  ylab("PAR") +
  xlim(0,2000000000)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  guides(shape = FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_Temp_phyto_biomass, plot_PAR_phyto_biomass, ncol= 2)

#-------------------------------------------------------------------------------------------#

## 6. Environmental parameters ~ Chl a (in situ) ####

#-------------------------------------------------------------------------------------------#
phyto_chlorophyll <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Chemistry_Fish_SITES_AquaNet_2017 Kopie.csv", sep=";")

# b) deleting Replicate = Lake
phyto_chlorophyll <- phyto_chlorophyll[!(phyto_chlorophyll$Replicate == "Lake"),]

# c) deleting Treatment = Lake & C 
phyto_chlorophyll <- phyto_chlorophyll[!(phyto_chlorophyll$Treatment %in% c("C", "Lake")),]

# e) only filtering the parameters we need (TP, TN, DOC, Temperature, PAR_I)
phyto_chlorophyll <- select(phyto_chlorophyll, Lake, Experiment,Enclosure, 
                         Treatment, Replicate, TP, TN, DOC, Temperature, PAR_I, Chla, Chla_flu)

# d) changing names of column "Lake"

levels(phyto_chlorophyll$Lake)
# "Asa"         "Bolmen"      "Erken"       "Skogaryd"    "Svartberget"

phyto_chlorophyll$Lake<- as.character(phyto_chlorophyll$Lake)

phyto_chlorophyll$Lake[phyto_chlorophyll$Lake == "Asa"] <- "Feresjoen"

phyto_chlorophyll$Lake[phyto_chlorophyll$Lake == "Skogaryd"] <- "Erssjoen"

phyto_chlorophyll$Lake[phyto_chlorophyll$Lake == "Svartberget"] <- "Stortjaern"

phyto_chlorophyll$Lake <- as.factor(phyto_chlorophyll$Lake)

levels(phyto_chlorophyll$Lake)

# TP for Chl a 
plot_TP_phyto_chla <- ggplot(data=phyto_chlorophyll,aes(x=Chla, y=TP, 
                                                           color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Chlorophyll a (spectrophotometer)") +
  ylab("Total phosphorus") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a (all lakes)")+
  guides(color = FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# TN for Chl a 
phyto_chlorophyll$TN <- as.numeric(phyto_chlorophyll$TN)
plot_TN_phyto_chla <- ggplot(data=phyto_chlorophyll,aes(x=Chla, y=TN, 
                                                        color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Chlorophyll a (spectrophotometer)") +
  ylab("Total nitrogen") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(shape = FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# DOC for Chl a 
plot_DOC_phyto_chla <- ggplot(data=phyto_chlorophyll,aes(x=Chla, y=DOC, 
                                                        color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Chlorophyll a (spectrophotometer)") +
  ylab("DOC") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  guides(shape = FALSE, color = FALSE) +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


grid.arrange(plot_TP_phyto_chla, plot_TN_phyto_chla, plot_DOC_phyto_chla, ncol= 2)

# Temp for Chl a 
plot_Temp_phyto_chla <- ggplot(data=phyto_chlorophyll,aes(x=Chla, y=Temperature, 
                                                         color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Chlorophyll a (spectrophotometer)") +
  ylab("Temperature") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a (all lakes)")+
  guides(color = FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# PAR for Chl a 
plot_PAR_phyto_chla <- ggplot(data=phyto_chlorophyll,aes(x=Chla, y=PAR_I, 
                                  color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Chlorophyll a (spectrophotometer)") +
  ylab("PAR") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(shape = FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_Temp_phyto_chla, plot_PAR_phyto_chla, ncol= 2)


#-------------------------------------------------------------------------------------------#

## 7. Environmental parameters ~ Chl a (fluorometer) ####

#-------------------------------------------------------------------------------------------#

# TP for Chl a (fluorometer)
plot_TP_phyto_chla_flu <- ggplot(data=phyto_chlorophyll,aes(x=Chla_flu, y=TP, 
                                                        color = Treatment)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Chlorophyll a (fluorometer)") +
  ylab("Total phosphorus") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a (Erken)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# TN for Chl a (fluorometer)
phyto_chlorophyll$TN <- as.numeric(phyto_chlorophyll$TN)
plot_TN_phyto_chla_flu <- ggplot(data=phyto_chlorophyll,aes(x=Chla_flu, y=TN, 
                                                        color = Treatment)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Chlorophyll a (fluorometer)") +
  ylab("Total nitrogen") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color = FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# DOC for Chl a (fluorometer)
plot_DOC_phyto_chla_flu <- ggplot(data=phyto_chlorophyll,aes(x=Chla_flu, y=DOC, 
                                                         color = Treatment)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Chlorophyll a (fluorometer)") +
  ylab("DOC") +
  xlim(0,4) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  guides(color = FALSE) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


grid.arrange(plot_TP_phyto_chla_flu, plot_TN_phyto_chla_flu, plot_DOC_phyto_chla_flu, ncol= 2)

# Temp for Chl a 
plot_Temp_phyto_chla_flu <- ggplot(data=phyto_chlorophyll,aes(x=Chla_flu, y=Temperature, 
                                                          color = Treatment)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Chlorophyll a (fluorometer)") +
  ylab("Temperature") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chlorophyll a (Erken)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# PAR for Chl a 
plot_PAR_phyto_chla_flu <- ggplot(data=phyto_chlorophyll,aes(x=Chla_flu, y=PAR_I, 
                                                         color = Treatment)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Chlorophyll a (fluorometer)") +
  ylab("PAR") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  guides(color = FALSE) +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_Temp_phyto_chla_flu, plot_PAR_phyto_chla_flu, ncol= 2)

#-------------------------------------------------------------------------------------------#

## 8. Environmental parameters ~ phytoplankton evenness ####

#-------------------------------------------------------------------------------------------#

# loading dataset phytoplankton diversity 
phytoplankton_diversity <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Diversity/diversity_metrics.csv")

# deleting first column
phytoplankton_diversity <- phytoplankton_diversity[-1] 

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

# changing column "Experiment"

phytoplankton_diversity$Experiment <- as.character(phytoplankton_diversity$Experiment)

phytoplankton_diversity$Experiment[phytoplankton_diversity$Experiment == "Spring"] <- 1

phytoplankton_diversity$Experiment[phytoplankton_diversity$Experiment == "Summer"] <- 2

phytoplankton_diversity$Experiment <- as.integer(phytoplankton_diversity$Experiment)

# b) deleting Replicate = Lake
phytoplankton_diversity <- phytoplankton_diversity[!(phytoplankton_diversity$Replicate == "Lake"),]

# c) deleting Treatment = Lake & C 
phytoplankton_diversity <- phytoplankton_diversity[!(phytoplankton_diversity$Treatment %in% c("C", "Lake")),]

# d) filtering only zooplankton for column "variable" 
phytoplankton_diversity <- phytoplankton_diversity[which(phytoplankton_diversity$variable %in% c("phytoplankton1","phytoplankton2")), ]

# e) only filtering the parameters we need 
phytoplankton_diversity <- select(phytoplankton_diversity, Lake, Experiment,Enclosure, 
                            Treatment, Replicate, H, J, D, S)

#-------------------------------------------------------------------------------------------#


# loading environmental parameters

env_parameters1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Chemistry_Fish_SITES_AquaNet_2017 Kopie.csv", sep=";")

# a) Deleting all Sampling_day = 20 & 21


env_parameters1 <- env_parameters1[!(env_parameters1$Sampling_day %in% c(20,21)),]


# b) deleting all rows with Enclosures = 1, 7, 10, 16, 21 (controls)


env_parameters1 <- env_parameters1[ ! (env_parameters1$Enclosure %in% c(1, 7, 10, 16,21)), ]



# c) only filtering the parameters we need (TP, TN, DOC, Temperature, PAR_I)
env_parameters <- select(env_parameters1, Lake, Experiment,Enclosure, 
                         Treatment, Replicate, TP, TN, DOC, Temperature, PAR_I)

# d) changing names of column "Lake"

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

# merging both datasets

env_phyto_diversity <- merge(x=phytoplankton_diversity, y=env_parameters, by =c("Lake", "Experiment", 
                       "Enclosure", "Treatment", "Replicate"), all.x=TRUE)

#-------------------------------------------------------------------------------------------#

# TP for phytoplankton evenness
plot_TP_phyto_evenness <- ggplot(data=env_phyto_diversity,aes(x=J, y=TP, 
                                                             color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Evenness") +
  ylab("Total phosphorus") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton evenness (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# TN for phytoplankton evenness 
env_phyto_diversity$TN <- as.numeric(env_phyto_diversity$TN)

plot_TN_phyto_evenness <- ggplot(data=env_phyto_diversity,aes(x=J, y=TN, 
color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Evenness") +
  ylab("Total nitrogen") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# DOC for phytoplankton evenness 
plot_DOC_phyto_evenness <- ggplot(data=env_phyto_diversity,aes(x=J, y=DOC, 
                                                              color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Evenness") +
  ylab("DOC") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color = FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_TP_phyto_evenness, plot_TN_phyto_evenness, plot_DOC_phyto_evenness, ncol= 2)

# Temp for phytoplankton evenness
plot_Temp_phyto_evenness <- ggplot(data=env_phyto_diversity,aes(x=J, y=Temperature, 
                                                               color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Evenness") +
  ylab("Temperature") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Phytoplankton evenness (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# PAR for phytoplankton evenness
plot_PAR_phyto_evenness <- ggplot(data=env_phyto_diversity,aes(x=J, y=PAR_I, 
                                                                color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Evenness") +
  ylab("PAR") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle(" ")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_Temp_phyto_evenness, plot_PAR_phyto_evenness, ncol= 2)




