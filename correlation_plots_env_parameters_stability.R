#-------------------------------------------------------------------------------------------#

## Stability ~ environmental parameters correlation matrix ####

#-------------------------------------------------------------------------------------------#

# environmental parameters:
# TP
# TN 
# light (PAR I)
# DOC 
# temperature 

# load packages
library(tidyverse)
library(scales)
library(gridExtra)

#-------------------------------------------------------------------------------------------#

## 1. load datasets ####

#-------------------------------------------------------------------------------------------#

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

# b) deleting all rows with Enclosures = 1, 7, 10, 16, 21 


env_parameters1 <- env_parameters1[ ! (env_parameters1$Enclosure %in% c(1, 7, 10, 16,21)), ]


#-------------------------------------------------------------------------------------------#


## 3. filtering datasets ####

#-------------------------------------------------------------------------------------------#

# only filtering the parameters we need (TP, TN, DOC, Temperature, Chla_flu, PAR_I)
env_parameters <- select(env_parameters1, Lake, Experiment,Enclosure, 
                      Treatment, Replicate, TP, TN, DOC, Temperature,Chla_flu, PAR_I)

# only filtering the data points that we need 
stability <- select(stability1, Lake, Experiment, Enclosure, Treatment, Replicate, variable, 
                    total_impact)

#-------------------------------------------------------------------------------------------#

## 4. changing names for column "Experiment" ####

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

# > scored_policies<-merge(x=policies,y=limits,by="State",all.x=TRUE)

env_stability <- merge(x=stability, y=env_parameters, by =c("Lake", "Experiment", 
                                  "Enclosure", "Treatment", "Replicate"), all.x=TRUE)

#-------------------------------------------------------------------------------------------#

## 7. plotting environmental parameters against stability measures for all lakes ####

#-------------------------------------------------------------------------------------------#

function_env_stability <- env_stability[which(env_stability$variable==c('bact_function', 
                                  'phyto_function', 'zoop_function')), ]

# TP 


plot_TP <- ggplot(data=function_env_stability, aes(x=total_impact, 
                                      y=TP, color = variable, shape = Lake)) +
 labs(color = "Plankton variable") +
  geom_point() +
 geom_smooth(aes(group=variable)) +
  ylab("Total phosphorus") +
  scale_x_continuous(trans = log2_trans(),
                    breaks = trans_breaks("log2", function(x) 2^x),
                   labels = trans_format("log2", math_format(2^.x))) +
  theme_minimal(base_size = 15) +
 theme(legend.position = "none") +
  ggtitle("Total phosphorus (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


# TN

function_env_stability$TN <- as.numeric(function_env_stability$TN)

plot_TN <- ggplot(data=function_env_stability, aes(x=total_impact, y=TN, color = variable, shape = Lake)) +
  labs(color = "Plankton variable") +
  geom_point() +
  geom_smooth(aes(group=variable)) +
  ylab("Total nitrogen") +
  scale_x_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +
  theme_minimal(base_size = 15) +
  ggtitle("Total nitrogen (all lakes)")+
  theme(legend.title = element_blank()) +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# combining TP & TN plots
grid.arrange(plot_TP, plot_TN,  ncol=2)

# DOC 

plot_DOC <- ggplot(data=function_env_stability, aes(x=total_impact, 
                                                   y=DOC, color = variable, shape = Lake)) +
  labs(color = "Plankton variable") +
  geom_point() +
  geom_smooth(aes(group = variable)) +
  ylab("DOC") +
 scale_x_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +
  theme_minimal(base_size = 15) +
  theme(legend.title = element_blank()) +
  ggtitle("DOC (all lakes)")+
  theme(legend.position = "top", plot.title = element_text(hjust=0.5, face="bold")) +
  guides(shape=FALSE) +
  scale_color_brewer(palette="Dark2")

# Temperature

plot_Temp <- ggplot(data=function_env_stability, aes(x=total_impact, 
                                                    y=Temperature, color = variable, shape = Lake)) +
  labs(color = "Plankton variable") +
  geom_point() +
  geom_smooth(aes(group = variable)) +
  ylab("Temperature") +
 scale_x_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +
  theme_minimal(base_size = 15) +
  ggtitle("Temperature (all lakes)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top", plot.title = element_text(hjust=0.5, face="bold")) +
  guides(color=FALSE) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_DOC, plot_Temp, ncol = 2)

# PAR_I

ggplot(data=function_env_stability, aes(x=total_impact, 
                                                     y=PAR_I, color = variable, shape = Lake)) +
  labs(color = "Plankton variable") +
  geom_point() +
  geom_smooth(aes(group = variable)) +
  ylab("PAR") +
  scale_x_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +
  theme_minimal(base_size = 15) +
  ggtitle("PAR (all lakes)")+
  theme(plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

