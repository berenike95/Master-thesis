#-------------------------------------------------------------------------------------------#

## Stability ~ environmental parameters correlation matrix ####

#-------------------------------------------------------------------------------------------#

# environmental parameters:
# TP
# TN 
# light (PAR I)
# DOC 
# Chl a (fluorometer)
# temperature 

# load packages
library(tidyverse)

#-------------------------------------------------------------------------------------------#

## 1. load datasets ####

#-------------------------------------------------------------------------------------------#

# load stability measures:
stability1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Stability/stab_stand_yes_c.csv", sep=",")

# load environmental parameters:
env_parameters1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Chemistry_Fish_SITES_AquaNet_2017 Kopie.csv", sep=";")


head(env_parameters)
#-------------------------------------------------------------------------------------------#

## 2. filtering datasets ####

#-------------------------------------------------------------------------------------------#

# only filtering the parameters we need (TP, TN, DOC, Temperature, Chla_flu, PAR_I)
env_parameters <- select(env_parameters1, Lake, Experiment,Enclosure, 
                      Treatment, Replicate, TP, TN, DOC, Temperature,Chla_flu, PAR_I)

# only filtering the data points that we need 
stability <- select(stability1, Lake, Experiment, Enclosure, Treatment, Replicate, variable, 
                    total_impact)

#-------------------------------------------------------------------------------------------#

## 3. changing names for column "Experiment" ####

#-------------------------------------------------------------------------------------------#


# changing Spring = 1 and Summer = 2 in dataset "stability" in column "Experiment"

stability$Experiment <- as.character(stability$Experiment)

stability$Experiment[stability$Experiment == "Spring"] <- 1

stability$Experiment[stability$Experiment == "Summer"] <- 2

stability$Experiment <- as.integer(stability$Experiment)

#-------------------------------------------------------------------------------------------#

## 4. changing names in column "Lakes" ####

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

## 5. deleting rows in env_parameters with Enclosures = 16 & Enclosures = 21 ####

#-------------------------------------------------------------------------------------------#

env_parameters <- env_parameters[ ! env_parameters$Enclosure %in% c(16,21), ]

nrow(stability[stability$Lake == "Bolmen",])
# 144
nrow(env_parameters[env_parameters$Lake == "Bolmen",])
# 180 

nrow(stability[stability$Lake == "Erken",])
# 144
nrow(env_parameters[env_parameters$Lake == "Bolmen",])
# 180

#-------------------------------------------------------------------------------------------#

## 6. getting mean values ####

#-------------------------------------------------------------------------------------------#

stability$Replicate <- as.factor(stability$Replicate)

levels(stability$Replicate)



levels(env_parameters$Replicate)

