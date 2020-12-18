# Phytoplankton data set


# load packages

library(tidyverse)
library(readxl)
library(dplyr)


#---------------------------------------------------------------------------------------------#  
#--------------------------------------diversity----------------------------------------------# 
#---------------------------------------------------------------------------------------------#  
# evenness
# richness
# ENS

# loading diversity dataset
phytoplankton_diversity1 <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/Pablos_Data/diversity_metrics.csv")

# deleting first column
phytoplankton_diversity <- phytoplankton_diversity1[-1] 

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

# delete all replicate / treatment "lake"
levels(phytoplankton_diversity$Replicate)
phytoplankton_diversity <- phytoplankton_diversity[!(phytoplankton_diversity$Replicate %in% c("lake", "Lake")), ]

levels(phytoplankton_diversity$Treatment)
phytoplankton_diversity <- phytoplankton_diversity[ ! (phytoplankton_diversity$Treatment %in% c("Lake")), ]

# filtering only zooplankton for column "variable" 
phytoplankton_diversity<- phytoplankton_diversity[which(phytoplankton_diversity$variable=='phytoplankton1'), ]

# calculate mean values for species richness, evenness, effective number of species in extra columns 
phytoplankton_diversity_mean <- phytoplankton_diversity %>%
  group_by(Lake, Experiment, Enclosure,Treatment) %>%
  summarize(mean_J = mean(J, na.rm=T), mean_S = mean(S, na.rm=T), mean_ENS_D = mean(ENS_D, na.rm=T))


#-----------------------------------------------------------------------------------------------------#  
#---------------------------------stability-----------------------------------------------------------#  
#-----------------------------------------------------------------------------------------------------#  
# recovery
# resistance
# resilience

# adding recovery and resistance non standardized 
stability1 <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/Pablos_Data/stab_stand_no_c.csv", sep=",")

# deleting first column
stability <- stability1[-1] 

# changing Lake names 
levels(stability$Lake)
#  "Bolmen"       "Erken"        "Erssj\xf6n"   "Feresj\xf6n"  "Stortj\xe4rn"

stability$Lake<- as.character(stability$Lake)

stability$Lake[stability$Lake == "Erssj\xf6n"] <- "Erssjoen"

stability$Lake[stability$Lake == "Feresj\xf6n"] <- "Feresjoen"

stability$Lake[stability$Lake == "Stortj\xe4rn"] <- "Stortjaern"

stability$Lake <- as.factor(stability$Lake)

levels(stability$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"


# filtering variable for phytoplankton only 
phyto_stability <- stability[which(stability$variable=='phyto_function'), ]


phyto_stability <- phyto_stability %>% spread(aspect, value)

# only filtering the parameters we need ()
phyto_stability  <- select(phyto_stability, Lake, Experiment, Enclosure, 
                           Treatment, final_stab, initial_stab)





# adding resilience with removed overcompensation
stability2 <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/Pablos_Data/stab_stand_yes_c2.csv", sep=",")

# deleting first column
stability2 <- stability2[-1] 

# changing Lake names 

stability2$Lake<- as.character(stability2$Lake)

levels(stability2$Lake)
#  "Bolmen"       "Erken"        "Erssj\xf6n"   "Feresj\xf6n"  "Stortj\xe4rn"

stability2$Lake[stability2$Lake == "Erssj\xf6n"] <- "Erssjoen"

stability2$Lake[stability2$Lake == "Feresj\xf6n"] <- "Feresjoen"

stability2$Lake[stability2$Lake == "Stortj\xe4rn"] <- "Stortjaern"

stability2$Lake <- as.factor(stability2$Lake)

levels(stability2$Lake)

# filtering variable for phytoplankton only 
phyto_stability2 <- stability2[which(stability2$variable=='phyto_function'), ]


# only filtering the parameters we need ()
phyto_stability2  <- select(phyto_stability2, Lake, Experiment, Enclosure, 
                            Treatment,rate_change_ort, rate_change_oti)

# merge both stability datasets 

phyto_stability_final <- merge(x=phyto_stability, y=phyto_stability2, 
                by = c("Lake", "Experiment", "Treatment", "Enclosure"), all = TRUE)


#----------------------------------------merge-------------------------------------------------#  

phyto_dataset_final <- merge(x=phytoplankton_diversity_mean, y=phyto_stability_final, by = c("Lake", "Experiment", "Treatment", "Enclosure"), all = TRUE)

write_csv(phyto_dataset_final, "/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/new_phytoplankton_dataset.csv")



