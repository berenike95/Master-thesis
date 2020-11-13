# Bacteria data set


#---------------------------------------------------------------------------------------------#  
#--------------------------------------diversity----------------------------------------------# 
#---------------------------------------------------------------------------------------------#  

bacteria_diversity1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Diversity/diversity_metrics.csv")

# deleting first column
bacteria_diversity <- bacteria_diversity1[-1] 

# renaming lakes
levels(bacteria_diversity$Lake)
# "Bolmen"       "Erken"        "Erssj\xf6n"   "Feresj\xf6n"  "Stortj\xe4rn"

bacteria_diversity$Lake<- as.character(bacteria_diversity$Lake)

bacteria_diversity$Lake[bacteria_diversity$Lake == "Feresj\xf6n"] <- "Feresjoen"

bacteria_diversity$Lake[bacteria_diversity$Lake == "Erssj\xf6n"] <- "Erssjoen"

bacteria_diversity$Lake[bacteria_diversity$Lake == "Stortj\xe4rn"] <- "Stortjaern"

bacteria_diversity$Lake <- as.factor(bacteria_diversity$Lake)

levels(bacteria_diversity$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# delete all replicate / treatment "lake"
levels(bacteria_diversity$Replicate)
bacteria_diversity <- bacteria_diversity[!(bacteria_diversity$Replicate %in% c("lake", "Lake")), ]

# filtering only bacteria for column "variable" 
bacteria_diversity <- bacteria_diversity[which(bacteria_diversity$variable='bacteria'), ]

# calculate mean values for species richness, evenness, effective number of species in extra columns 
bacteria_diversity_mean <- bacteria_diversity %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_J = mean(J, na.rm=T), mean_s = mean(S, na.rm=T), mean_ENS_D = mean(ENS_D, na.rm=T))



#-----------------------------------------------------------------------------------------------------#  
#---------------------------------stability-----------------------------------------------------------#  
#-----------------------------------------------------------------------------------------------------#  

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

# filtering variable for bacteria only 
bact_stability <- stability[which(stability$variable=='bact_function'), ]

# only filtering the parameters we need ()
bact_stability <- select(bact_stability, Lake, Experiment, Enclosure, 
                         Treatment, final_stab, initial_stab, 
                         rate_change_ort, rate_change_oti, total_impact)




# merge
bacteria_dataset <- merge(x=bact_stability, y=bacteria_diversity_mean, by = c("Lake", "Experiment", "Treatment", "Enclosure"), all = TRUE)


write_csv(bacteria_dataset, "/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/bacteria_dataset.csv")

