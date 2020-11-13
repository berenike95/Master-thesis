#---------------------------------------------------------------------------------------------------------#
#### Master dataset ####
#---------------------------------------------------------------------------------------------------------#

# columns:

# lake
# experiment
# treatment
# enclosure
# community
# unique identifier

# evenness_all
# richness_all
# ens_all
# biomass_all
# body_size_all


# recovery_all
# resistance_all (only for FS & F)
# resistance_1_7
# resistance_8-28
# resilience_all
# AUC_all

# Chla
# TP
# TN
# DOC
# PAR
# Temp
# TN/TP
# DN/TP

#---------------------------------------------------------------------------------------------------------#
#### packages ####
#---------------------------------------------------------------------------------------------------------#
library(tidyverse)
library(readxl)

#---------------------------------------------------------------------------------------------------------#
#### 1. phytoplankton ####
#---------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------#
#### 1.1 biovolume ####
#---------------------------------------------------------------------------------------------------------#
phyotplankton_biovolume1 <- read_excel("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/phyto_biovolume.xlsx", na = "")


# selecting columns 
phyotplankton_biovolume <- select(phyotplankton_biovolume1, Lake, Experiment, Treatment, Enclosure, Sampling, 
                                  Exp_day,Replicate, Biovol)


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

phyotplankton_biovolume$Biovol <- as.numeric(phyotplankton_biovolume$Biovol)

# calculate mean values for Clean_biomass in extra column 
phyotplankton_biovolume_mean <- phyotplankton_biovolume %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_biomass = mean(Biovol, na.rm=T))



#---------------------------------------------------------------------------------------------------------#
#### 1.2 diversity ####
#---------------------------------------------------------------------------------------------------------#

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

#---------------------------------------------------------------------------------------------------------#
#### 1.3 stability ####
#---------------------------------------------------------------------------------------------------------#

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

#---------------------------------------------------------------------------------------------------------#
#### 1.4 merging ####
#---------------------------------------------------------------------------------------------------------#

# phyotplankton_biovolume_mean
# phytoplankton_diversity_mean
# phyto_stability

phyto_dataset_final_ <- merge(x=phyotplankton_biovolume_mean, y= phytoplankton_diversity_mean,
                               by=c("Lake", "Experiment", "Treatment", "Enclosure"), 
                               all = TRUE)

phyto_dataset_final <- merge(x=phyto_dataset_final_, y= phyto_stability,
                              by=c("Lake", "Experiment", "Treatment", "Enclosure"), 
                             all = TRUE)

phyto_dataset_final["community"] <- c("phyto")

# unique identifier
phyto_dataset_final <- phyto_dataset_final %>% 
  mutate(ID = paste(Lake, Experiment, Treatment, Enclosure, community, sep = "_"))

names(phyto_dataset_final)[names(phyto_dataset_final) == 'mean_S'] <- 'mean_s'
head(phyto_dataset_final)
#---------------------------------------------------------------------------------------------------------#
#### 2. zooplankton ####
#---------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------#
#### 2.1 biomass ####
#---------------------------------------------------------------------------------------------------------#
zooplankton_biomass1 <- read_excel("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Zooplankton_SITES_AquaNet_2017.xlsx")

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

# initial biomass 
zooplankton_initial_biomass <- zooplankton_biomass[which(zooplankton_biomass$Exp_day==1), ]


# calculate mean values for initial biomass in extra column 
zooplankton_initial_biomass_mean <- zooplankton_initial_biomass %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_initial_zoop_biomass = mean(Clean_Biomass, na.rm=T))



#---------------------------------------------------------------------------------------------------------#
#### 2.2 body size ####
#---------------------------------------------------------------------------------------------------------#

zoopl_size1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Master-thesis/cwm_zooplankton_body_size.csv")

# delete columns 
zoopl_size <- select(zoopl_size1, Lake, Experiment, Treatment, Enclosure, initial_zoop_body_size)

#---------------------------------------------------------------------------------------------------------#
#### 2.3 diversity ####
#---------------------------------------------------------------------------------------------------------#
# loading dataset zooplankton diversity 
zooplankton_diversity1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Diversity/diversity_metrics.csv")

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


#---------------------------------------------------------------------------------------------------------#
#### 2.4 stability ####
#---------------------------------------------------------------------------------------------------------#

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

# filtering variable for zooplankton only 
zoop_stability <- stability[which(stability$variable=='zoop_function'), ]

# only filtering the parameters we need ()
zoop_stability  <- select(zoop_stability , Lake, Experiment, Enclosure, 
                          Treatment, final_stab, initial_stab, 
                          rate_change_ort, rate_change_oti, total_impact)

#---------------------------------------------------------------------------------------------------------#
#### 2.5 merge ####
#---------------------------------------------------------------------------------------------------------#

# zooplankton_biomass_mean
# zoopl_size
# zooplankton_diversity_mean
# zoop_stability

zoop_dataset_final___ <- merge(x=zooplankton_biomass_mean, y=zoopl_size, 
                              by=c("Lake", "Experiment", "Treatment", "Enclosure"),all = TRUE)

zoop_dataset_final__ <- merge(x=zoop_dataset_final___, y=zooplankton_diversity_mean, 
                             by=c("Lake", "Experiment", "Treatment", "Enclosure"),all = TRUE)

zoop_dataset_final_<- merge(x=zoop_dataset_final__, y=zoop_stability, 
                           by=c("Lake", "Experiment", "Treatment", "Enclosure"),all = TRUE)

zoop_dataset_final<- merge(x=zoop_dataset_final_, y=zooplankton_initial_biomass_mean, 
                           by=c("Lake", "Experiment", "Treatment", "Enclosure"),all = TRUE)

zoop_dataset_final["community"] <- c("zoopl")

# unique identifier
zoop_dataset_final <- zoop_dataset_final %>% 
  mutate(ID = paste(Lake, Experiment, Treatment, Enclosure, community, sep = "_"))

#---------------------------------------------------------------------------------------------------------#
#### 3. bacteria ####
#---------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------#
#### 3.1 diversity ####
#---------------------------------------------------------------------------------------------------------#
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

#---------------------------------------------------------------------------------------------------------#
#### 3.2 stability ####
#---------------------------------------------------------------------------------------------------------#
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

#---------------------------------------------------------------------------------------------------------#
#### 3.3 merging ####
#---------------------------------------------------------------------------------------------------------#
bact_dataset_final<- merge(x=bact_stability, y=bacteria_diversity_mean, 
                           by=c("Lake", "Experiment", "Treatment", "Enclosure"),all = TRUE)

bact_dataset_final["community"] <- c("bact")

# unique identifier
bact_dataset_final <- bact_dataset_final %>% 
  mutate(ID = paste(Lake, Experiment, Treatment, Enclosure, community, sep = "_"))


#---------------------------------------------------------------------------------------------------------#
#### 4. envrionmental paramters ####
#---------------------------------------------------------------------------------------------------------#
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
environ_mean <- phyto_chlorophyll %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_TP = mean(TP, na.rm=T), mean_TN= mean(TN, na.rm=T), mean_DOC= mean(DOC, na.rm=T), 
            mean_PAR= mean(PAR_I, na.rm=T), mean_Temp = mean(Temperature, na.rm=T), 
            mean_Chla = mean(Chla, nar.rm=T), mean_TN_TP_ratio = mean(TN_TP_ratio, na.rm=T), 
            mean_DN_TP_ratio=mean(DN_TP_ratio, na.rm=T))

zooplankton_biomass1 <- read_excel("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Zooplankton_SITES_AquaNet_2017.xlsx")

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
  summarize(mean_biomass_zoopl = mean(Clean_Biomass, na.rm=T))

environ_mean<- merge(x=environ_mean, y=zooplankton_biomass_mean, 
                         by=c("Lake", "Experiment", "Treatment", "Enclosure"),all = TRUE)

#---------------------------------------------------------------------------------------------------------#
#### 5. merging everything ####
#---------------------------------------------------------------------------------------------------------#

# phyto_dataset_final
# bact_dataset_final
# zoop_dataset_final
# environ_mean


dataset_final__ <- merge(x=phyto_dataset_final, y=zoop_dataset_final, 
                       by=c("Lake", "Experiment", "Treatment", "Enclosure", "community", "ID", "mean_biomass",
                            "mean_J", "mean_s", "mean_ENS_D", "final_stab", "initial_stab",
                            "rate_change_ort", "rate_change_oti", "total_impact"),all = TRUE)

dataset_final_ <- merge(x=dataset_final__, y=bact_dataset_final, 
                       by=c("Lake", "Experiment", "Treatment", "Enclosure", "community", "ID",
                            "mean_J", "mean_s", "mean_ENS_D", "final_stab", "initial_stab",
                            "rate_change_ort", "rate_change_oti", "total_impact"),all = TRUE)

dataset_final <- merge(x=dataset_final_, y=environ_mean, 
                        by=c("Lake", "Experiment", "Treatment", "Enclosure"),all = TRUE)


write_csv(dataset_final, "/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/master_dataset2.csv")
