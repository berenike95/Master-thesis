# Zooplankton data set

# load packages

library(tidyverse)
library(readxl)
library(dplyr)

#--------------------------------------------biomass--------------------------------------------------# 


zooplankton_biomass1 <- read_excel("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/Pablos_Data/Zooplankton_SITES_AquaNet_2017.xlsx")

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

 

#-----------------------------------------body size-------------------------------------------# 


zoopl_size1 <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/cwm_zooplankton_body_size.csv")

# delete columns 
zoopl_size <- select(zoopl_size1, Lake, Experiment, Treatment, Enclosure, initial_zoop_body_size)


#---------------------------------------------------------------------------------------------#  
#--------------------------------------diversity----------------------------------------------# 
#---------------------------------------------------------------------------------------------#  

# loading dataset zooplankton diversity 
zooplankton_diversity1 <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/Pablos_Data/diversity_metrics.csv")

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



#-----------------------------------------------------------------------------------------------------#  
#---------------------------------stability-----------------------------------------------------------#  
#-----------------------------------------------------------------------------------------------------#  

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

# filtering variable for zooplankton only 
zoop_stability <- stability[which(stability$variable=='zoop_function'), ]

zoop_stability1 <- zoop_stability %>% spread(aspect, value)

# only filtering the parameters we need ()
zoop_stability  <- select(zoop_stability1, Lake, Experiment, Enclosure, 
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
zoop_stability2 <- stability2[which(stability2$variable=='zoop_function'), ]


# only filtering the parameters we need ()
zoop_stability2  <- select(zoop_stability2, Lake, Experiment, Enclosure, 
                            Treatment,rate_change_ort, rate_change_oti)

# merge both stability datasets 

zoop_stability_final <- merge(x=zoop_stability, y=zoop_stability2, 
                               by = c("Lake", "Experiment", "Treatment", "Enclosure"), all = TRUE)


#-----------------------------------------------------------------------------------------------------#  

# merge
zoop_dataset_ <- merge(x=zooplankton_biomass_mean, y=zoopl_size, by = c("Lake", "Experiment", "Treatment", "Enclosure"), all = TRUE)

zoop_dataset_final <- merge(x=zoop_dataset_ , y=zooplankton_diversity_mean, by = c("Lake", "Experiment", "Treatment", "Enclosure"), all = TRUE)

zoop_dataset <- merge(x=zoop_dataset_final , y=zoop_stability_final, by = c("Lake", "Experiment", "Treatment", "Enclosure"), all = TRUE)


# write csv file

write_csv(zoop_dataset, "/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/new_zooplankton_dataset.csv")




