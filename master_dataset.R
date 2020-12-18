#---------------------------------------------------------------------------------------------------------#
#### Master dataset ####
#---------------------------------------------------------------------------------------------------------#

# columns:

# lake
# experiment
# treatment
# enclosure
# community
# ID (unique identifier)

# evenness
# richness
# ens

# final recovery
# resistance
# resilience 
# AUC

# zooplankton body size
# zooplankto biomass


# TP
# TN
# DOC
# PAR
# Temp
# Chl a 
# TN/TP
# DN/TP



# ????????????????????????????????????
# resistance_all (only for FS & F)
# resistance_1_7
# resistance_8-28
# resilience_all


#---------------------------------------------------------------------------------------------------------#
#### packages ####
#---------------------------------------------------------------------------------------------------------#
library(tidyverse)
library(readxl)

#---------------------------------------------------------------------------------------------------------#
#### 1. phytoplankton ####
#---------------------------------------------------------------------------------------------------------#

phytoplankton <- read_csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/new_phytoplankton_dataset.csv")

phytoplankton["community"] <- c("phyto")

# unique identifier
phytoplankton <- phytoplankton %>% 
  mutate(ID = paste(Lake, Experiment, Treatment, Enclosure, community, sep = "_"))

head(phytoplankton)

names(phytoplankton)[names(phytoplankton) == "mean_S"] <- "mean_s"




#---------------------------------------------------------------------------------------------------------#
#### 2. zooplankton ####
#---------------------------------------------------------------------------------------------------------#

zooplankton <- read_csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/new_zooplankton_dataset.csv")

zooplankton["community"] <- c("zoopl")

# unique identifier
zooplankton <- zooplankton %>% 
  mutate(ID = paste(Lake, Experiment, Treatment, Enclosure, community, sep = "_"))

#---------------------------------------------------------------------------------------------------------#
#### 3. envrionmental paramters ####
#---------------------------------------------------------------------------------------------------------#
environment <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/mean_values_environmental_parameters.csv")


#---------------------------------------------------------------------------------------------------------#
#### 4. merging everything ####
#---------------------------------------------------------------------------------------------------------#

# phytoplankton
# zooplankton
# environment


dataset_final_ <- merge(x=phytoplankton, y=zooplankton, 
                       by=c("Lake", "Experiment", "Treatment", "Enclosure", "community", "ID",
                            "mean_J", "mean_s", "mean_ENS_D", "final_stab", "initial_stab",
                            "rate_change_ort", "rate_change_oti"),all = TRUE)

dataset_final <- merge(x=dataset_final_, y=environment, 
                        by=c("Lake", "Experiment", "Treatment", "Enclosure"),all = TRUE)


# reanming columns 
names(dataset_final)[names(dataset_final) == "mean_biomass"] <- "mean_zoop_biomass"



# deleting all control values
dataset_final <- dataset_final[ ! (dataset_final$Treatment == 'C'), ]



write_csv(dataset_final, "/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/new_master_dataset.csv")
