#-------------------------------------------------------------------------------------------#

## Environmental parameters ~ explanatory variables correlation plots for Zooplankton ####

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
# Biomass (Zooplankton)
# Evenness (Zooplankton)
# Alpha Diversity (Zooplankton)
# Body size (Zooplankton)
# Initial Zooplankton biomass

#-------------------------------------------------------------------------------------------#

## 1. load packages and datasets ####

#-------------------------------------------------------------------------------------------#
# load packages
library(tidyverse)
library(scales)
library(gridExtra)
library(ggplot2)
library("ggpubr")


# load environmental parameters:
env_parameters1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Chemistry_Fish_SITES_AquaNet_2017 Kopie.csv", sep=";")

zooplankton_biomass1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Zooplankton_biomass.csv", sep =";")

#-------------------------------------------------------------------------------------------#

## 2. editing dataset: env_parameters ####

#-------------------------------------------------------------------------------------------#


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

## 3. editing dataset: zooplankton_biomass ####

#-------------------------------------------------------------------------------------------#

# a) Deleting all Sampling_day = 21


zooplankton_biomass1 <- zooplankton_biomass1[!(zooplankton_biomass1$Sampling_day ==21),]

# b) deleting all Exp_day = 21


zooplankton_biomass1 <- zooplankton_biomass1[!(zooplankton_biomass1$Exp_day ==21),]


# c) deleting all rows with Enclosures = 1, 7, 10, 16, 21 (controls)


zooplankton_biomass1 <- zooplankton_biomass1[ ! (zooplankton_biomass1$Enclosure %in% c(1, 7, 10, 16,21)), ]


# d) only filtering the parameters we need (Zooplankton Biomass & Abundance)
zooplankton_biomass <- select(zooplankton_biomass1, Lake, Experiment,Enclosure, 
                         Treatment, Replicate, Abundance, Clean_Biomass, Mean_lenght)

# e) changing names of column "Lake"


# "Asa"         "Bolmen"      "Erken"       "Skogaryd"    "Svartberget"

zooplankton_biomass$Lake<- as.character(zooplankton_biomass$Lake)

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Asa"] <- "Feresjoen"

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Skogaryd"] <- "Erssjoen"

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Svartberget"] <- "Stortjaern"

zooplankton_biomass$Lake <- as.factor(zooplankton_biomass$Lake)

levels(zooplankton_biomass$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

#-------------------------------------------------------------------------------------------#

## 4. merging both datasets ####

#-------------------------------------------------------------------------------------------#


env_zoopl_biomass <- merge(x=zooplankton_biomass, y=env_parameters, by =c("Lake", "Experiment", 
                                  "Enclosure", "Treatment", "Replicate"), all.x=TRUE)

#-------------------------------------------------------------------------------------------#

## 5. Environmental parameters ~ zooplankton biomass ####

#-------------------------------------------------------------------------------------------#

levels(env_parameters$Lake)

levels(zooplankton_biomass$Lake)

# TP for zooplankton

plot_TP_zoopl_biomass <- ggplot(data=env_zoopl_biomass,aes(x=Clean_Biomass, y=TP, 
                         color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Biomass") +
  ylab("Total phosphorus") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton biomass (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# TN for zooplankton

env_zoopl_biomass$TN <- as.numeric(env_zoopl_biomass$TN)

plot_TN_zoopl_biomass <- ggplot(data=env_zoopl_biomass,aes(x=Clean_Biomass, y=TN, 
                                                           color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Biomass") +
  ylab("Total nitrogen") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton biomass (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# DOC for zooplankton

plot_DOC_zoopl_biomass <- ggplot(data=env_zoopl_biomass,aes(x=Clean_Biomass, y=DOC, 
                                                           color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Biomass") +
  ylab("DOC") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton biomass (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE, color = FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_TP_zoopl_biomass, plot_TN_zoopl_biomass, plot_DOC_zoopl_biomass, ncol= 2)

# Temp

par(mfrow=c(1,2))

plot_Temp_zoopl_biomass <- ggplot(data=env_zoopl_biomass,aes(x=Clean_Biomass, y=Temperature, 
                                                            color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Biomass") +
  ylab("Temperature") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton biomass (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# PAR 

plot_PAR_zoopl_biomass <- ggplot(data=env_zoopl_biomass,aes(x=Clean_Biomass, y=PAR_I, 
                                                             color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Biomass") +
  ylab("PAR") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton biomass (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color = FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


ggarrange(plot_Temp_zoopl_biomass, plot_PAR_zoopl_biomass, ncol = 2, nrow = 1)

grid.arrange(plot_Temp_zoopl_biomass, plot_PAR_zoopl_biomass,ncol= 2)

#-------------------------------------------------------------------------------------------#

## 6. Environmental parameters ~ Evenness ####

#-------------------------------------------------------------------------------------------#


# loading dataset zooplankton diversity 
zooplankton_diversity <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Diversity/diversity_metrics.csv")

# deleting first column
zooplankton_diversity <- zooplankton_diversity[-1] 

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

# changing column "Experiment"

zooplankton_diversity$Experiment <- as.character(zooplankton_diversity$Experiment)

zooplankton_diversity$Experiment[zooplankton_diversity$Experiment == "Spring"] <- 1

zooplankton_diversity$Experiment[zooplankton_diversity$Experiment == "Summer"] <- 2

zooplankton_diversity$Experiment <- as.integer(zooplankton_diversity$Experiment)

# deleting all rows with Enclosures = 1, 7, 10, 16, 21 (controls)
zooplankton_diversity <- zooplankton_diversity[ ! (zooplankton_diversity$Enclosure %in% c(1, 7, 10, 16,21)), ]

# deleting all Exp_day = 21
zooplankton_diversity <- zooplankton_diversity[!(zooplankton_diversity$Exp_day ==21),]


# filtering only zooplankton for column "variable" 

zooplankton_diversity <- zooplankton_diversity[which(zooplankton_diversity$variable=='zooplankton'), ]

# only filtering the parameters we need
zooplankton_diversity <- select(zooplankton_diversity, Lake, Experiment,Enclosure,Treatment, 
                                Replicate, H, J, D)

# combining datasets
env_zoopl_evenness <- merge(x=env_parameters, y=zooplankton_diversity, by =c("Lake", "Experiment", 
                                  "Enclosure", "Treatment", "Replicate"), all.x=TRUE)

# TP for Zooplankton Evenness
plot_TP_zoopl_evenness <- ggplot(data=env_zoopl_evenness,aes(x=J, y=TP, 
                          color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Evenness") +
  ylab("Total phosphorus") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton evenness (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# TN for Zooplankton Evenness
env_zoopl_evenness$TN <- as.numeric(env_zoopl_evenness$TN)
plot_TN_zoopl_evenness <- ggplot(data=env_zoopl_evenness,aes(x=J, y=TN, 
                                                             color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Evenness") +
  ylab("Total nitrogen") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton evenness (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# DOC for Zooplankton 
plot_DOC_zoopl_evenness <- ggplot(data=env_zoopl_evenness,aes(x=J, y=DOC, 
                                                             color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Evenness") +
  ylab("DOC") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton evenness (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape = FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_TP_zoopl_evenness, plot_TN_zoopl_evenness,plot_DOC_zoopl_evenness, ncol=2)

# Temperature for Zooplankton
plot_Temp_zoopl_evenness <- ggplot(data=env_zoopl_evenness,aes(x=J, y=Temperature, 
                                                              color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Evenness") +
  ylab("Temperature") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton evenness (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape = FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# PAR for Zooplankton
plot_PAR_zoopl_evenness <- ggplot(data=env_zoopl_evenness,aes(x=J, y=PAR_I, 
                                                               color = Treatment, shape = Lake)) +
  labs(color = "Treatment") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Treatment)) +
  xlab("Evenness") +
  ylab("PAR") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton evenness (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE, shape = FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_Temp_zoopl_evenness, plot_PAR_zoopl_evenness,ncol=2)


#-------------------------------------------------------------------------------------------#

## 7. Environmental parameters ~ Alpha Diversity  ####

#-------------------------------------------------------------------------------------------#




#-------------------------------------------------------------------------------------------#

## 8. Environmental parameters ~ Body size  ####

#-------------------------------------------------------------------------------------------#



#-------------------------------------------------------------------------------------------#

## 9. Environmental parameters ~ Initial Zooplankton biomass  ####

#-------------------------------------------------------------------------------------------#


