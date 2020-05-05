# Stability ~ environmental parameters correlation matrix

# environmental paramters:
# TP
# TN 
# light (PAR I)
# DOC 
# Chl a (fluorometer)
# temperature 

# load packages
library(tidyverse)

# load datasets

# load stability measures:
stability1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Stability/stab_stand_yes_c.csv", sep=",")

# load environmental parameters:
env_parameters1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Chemistry_Fish_SITES_AquaNet_2017 Kopie.csv", sep=";")

levels(stability1$Lake)
#  "Bolmen"       "Erken"        "Erssj\xf6n"   "Feresj\xf6n"  "Stortj\xe4rn"

levels(env_parameters1$Lake)
# "Asa"         "Bolmen"      "Erken"       "Skogaryd"    "Svartberget" 

head(env_parameters)

# only filtering the parameters we need (TP, TN, DOC, Temperature, Chla_flu, PAR_I)
env_parameters <- select(env_parameters1, Lake, Experiment,Enclosure, 
                      Treatment, Replicate, TP, TN, DOC, Temperature,Chla_flu, PAR_I)

# only filtering the data points that we need 
stability <- select(stability1, Lake, Experiment, Enclosure, Treatment, Replicate, variable, total_impact)

# changing Spring = 1 and Summer = 2 in dataset "stability" in column "Experiment"

stability$Experiment <- as.character(stability$Experiment)

stability$Experiment[stability$Experiment == "Spring"] <- 1

stability$Experiment[stability$Experiment == "Summer"] <- 2

stability$Experiment <- as.integer(stability$Experiment)

            
                    
                    
                    