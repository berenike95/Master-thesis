# Phytoplankton data set


#-----------------------------------------------------------------------------------------------------#  
#--------------------------------------community structure--------------------------------------------# 
#-----------------------------------------------------------------------------------------------------#  


#-------------------------------------biovolume-------------------------------------------------------#  

phyotplankton_biovolume1 <- read.xls("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/phyto_biovolume.xlsx", na = "")



# selecting columns 
phyotplankton_biovolume <- select(phyotplankton_biovolume1, Lake, Experiment, Sampling, 
                                  Exp_day, Enclosure, Replicate, Treatment, Biovol)


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

#-------------------------------------Chla-------------------------------------------------------#  

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

#---------------------------------------------------------------------------------------------#  
#--------------------------------------diversity----------------------------------------------# 
#---------------------------------------------------------------------------------------------#  
# evenness
# richness
# ENS

# loading diversity dataset
phytoplankton_diversity1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Diversity/diversity_metrics.csv")

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
phytoplankton_diversity<- phytoplankton_diversity[which(phytoplankton_diversity$variable%in%c('phytoplankton1', 'phytoplankton2')), ]

# calculate mean values for species richness, evenness, effective number of species in extra columns 
phytoplankton_diversity_mean <- phytoplankton_diversity %>%
  group_by(Lake, Experiment,Enclosure, Treatment) %>%
  summarize(mean_J = mean(J, na.rm=T), mean_S = mean(S, na.rm=T), mean_ENS_D = mean(ENS_D, na.rm=T))


#-----------------------------------------------------------------------------------------------------#  
#---------------------------------stability-----------------------------------------------------------#  
#-----------------------------------------------------------------------------------------------------#  
# recovery
# resistance
# resilience
# AUC

stability1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Stability/stab_stand_yes_c.csv", sep=",")

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

# only filtering the parameters we need ()
phyto_stability  <- select(phyto_stability , Lake, Experiment, Enclosure, 
                           Treatment, final_stab, initial_stab, 
                           rate_change_ort, rate_change_oti, total_impact)


#----------------------------------------merge-------------------------------------------------#  
phyto_dataset <- merge(x=phytoplankton_diversity_mean, y=phyto_chlorophyll_mean, by = c("Lake", "Experiment", "Treatment", "Enclosure"), na ="")

# phyotplankton_biovolume_mean

phyto_dataset_ <- merge(x=phyto_dataset, y=phyto_stability, by = c("Lake", "Experiment", "Treatment", "Enclosure"), all = TRUE)

phyto_datset_final <- merge(x=phyto_dataset_, y=phyotplankton_biovolume_mean, by = c("Lake", "Experiment", "Treatment", "Enclosure"), all = TRUE)

write_csv(phyto_datset_final, "/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/phytoplankton_dataset.csv")



