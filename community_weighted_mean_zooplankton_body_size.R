#----------------------community weighted mean (CWM) for zooplankton body size--------------------------#

# load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(plyr)

#-------------------------------------------!!!!!!!ALT!!!!!!----------------------------------------------------------#  

# load zooplankton biomass dataset
zooplankton_biomass1 <- read_excel("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Zooplankton_SITES_AquaNet_2017.xlsx")

# delete columns 
zooplankton_biomass <- zooplankton_biomass1[-c(10, 12:15)] 

# delete all replicate / treatment "lake"
levels(zooplankton_biomass$Replicate)
zooplankton_biomass <- zooplankton_biomass[ ! (zooplankton_biomass$Replicate %in% c("lake", "Lake")), ]

levels(zooplankton_biomass$Treatment)

zooplankton_biomass <- zooplankton_biomass[ ! (zooplankton_biomass$Treatment %in% c("Lake")), ]

# changing names for column "Experiment"
zooplankton_biomass$Experiment <- as.integer(zooplankton_biomass$Experiment)

zooplankton_biomass$Experiment[zooplankton_biomass$Experiment == 1] <- "Spring"

zooplankton_biomass$Experiment[zooplankton_biomass$Experiment == 2] <- "Summer"

zooplankton_biomass$Experiment <- as.character(zooplankton_biomass$Experiment)


# changing names of column "Lake"
zooplankton_biomass$Lake<- as.character(zooplankton_biomass$Lake)

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Asa"] <- "Feresjoen"

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Skogaryd"] <- "Erssjoen"

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Svartberget"] <- "Stortjaern"

zooplankton_biomass$Lake <- as.factor(zooplankton_biomass$Lake)

levels(zooplankton_biomass$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

zooplankton_biomass$Clean_Biomass <- as.numeric(zooplankton_biomass$Clean_Biomass)

zooplankton_initial_biomass <- zooplankton_biomass[which(zooplankton_biomass$Exp_day==1), ]


# calculate mean values for initial biomass in extra column 
zooplankton_initial_biomass_mean <- zooplankton_initial_biomass %>%
  group_by(Lake, Experiment, Treatment, Enclosure, Replicate, Taxa) %>%
  summarize(mean_initial_biomass = mean(Clean_Biomass, na.rm=T), 
            mean_initial_lenght = mean(Mean_lenght, nar.rm=T))

# calculate mean values for biomass 
zooplankton_biomass_mean <- zooplankton_biomass %>%
  group_by(Lake, Experiment, Treatment, Enclosure, Replicate, Taxa) %>%
  summarize(mean_Biomass=mean(Clean_Biomass, na.rm=T), Mean_lenght = mean(Mean_lenght, na.rm=T))


# community weighted mean in R function
?weighted.mean()
# formula for function weighted.mean()
# weighted.mean(x,w):
# sum(x * w) / sum(w) 

# for our values:
# weighted.mean(length, biomass)
# sum (length x biomass) / sum (biomass) 

# change zooplankton length from mm to µm
zooplankton_biomass_mean$Mean_lenght <- (zooplankton_biomass_mean$Mean_lenght * 1000)

# create new dataset with community weighted mean for body size 
zoop_cwm_body_size <- 
  zooplankton_biomass_mean %>%
  group_by(Lake, Experiment, Treatment, Enclosure, Replicate, Taxa) %>%
  summarize(body_size_cwm = weighted.mean(Mean_lenght, mean_Biomass)) # creating new column for community weighted mean for zooplankton body size

# change zooplankton community weighted mean from µm to mm
zoop_cwm_body_size$body_size_cwm <- (zoop_cwm_body_size$body_size_cwm/1000)

#-------------------------------------------!!!!!!!ALT!!!!!!----------------------------------------------------------#  


#-----------------------------------------------!!!!NEU!!!!!------------------------------------------------------#  

# load excel
# library("readxl")
zooplankton_biomass1 <- read_excel("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Zooplankton_SITES_AquaNet_2017.xlsx")


# delete all replicate / treatment "lake"
levels(zooplankton_biomass1$Replicate)
zooplankton_biomass <- zooplankton_biomass1[ ! (zooplankton_biomass1$Replicate %in% c("lake", "Lake")), ]

levels(zooplankton_biomass$Treatment)
zooplankton_biomass <- zooplankton_biomass[ ! (zooplankton_biomass$Treatment %in% c("Lake")), ]

# changing names for column "Experiment"
zooplankton_biomass$Experiment <- as.integer(zooplankton_biomass$Experiment)

zooplankton_biomass$Experiment[zooplankton_biomass$Experiment == 1] <- "Spring"

zooplankton_biomass$Experiment[zooplankton_biomass$Experiment == 2] <- "Summer"

zooplankton_biomass$Experiment <- as.character(zooplankton_biomass$Experiment)

# changing names of column "Lake"
zooplankton_biomass$Lake<- as.character(zooplankton_biomass$Lake)

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Asa"] <- "Feresjoen"

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Skogaryd"] <- "Erssjoen"

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Svartberget"] <- "Stortjaern"

zooplankton_biomass$Lake <- as.factor(zooplankton_biomass$Lake)

levels(zooplankton_biomass$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# delete Exp_Day = 21
zooplankton_biomass$Exp_day<-as.numeric(zooplankton_biomass$Exp_day)
zooplankton_biomass <- zooplankton_biomass[ ! (zooplankton_biomass$Exp_day==21), ]

test1 <- zooplankton_biomass[zooplankton_biomass$Lake == "Bolmen" & 
                               zooplankton_biomass$Experiment == "Spring" &
                               zooplankton_biomass$Exp_day == 1 &
                               zooplankton_biomass$Enclosure == 1 &
                               zooplankton_biomass$Treatment == "C" &
                               zooplankton_biomass$Replicate == 1
                             ,]


# change zooplankton length from mm to µm
zooplankton_biomass$Mean_lenght <- (zooplankton_biomass$Mean_lenght * 1000)

# only selecting columns we need
zooplankton_biomass <- select(zooplankton_biomass, Lake, Experiment, Exp_day, Enclosure, Treatment,
                              Replicate, Taxa, Clean_Biomass, Mean_lenght)

# calculating proportion 
# load package 
# library(plyr)

zooplankton_biomass$Clean_Biomass <- as.numeric(zooplankton_biomass$Clean_Biomass)


# calculating sum of biomass for each sample
zoop_biomass2 <- ddply(zooplankton_biomass,.(Lake, Experiment, Exp_day, Enclosure, 
                      Treatment, Replicate), colwise(sum, . (Clean_Biomass)),
                      na.rm=T)

# renaming column 
names(zoop_biomass2)[names(zoop_biomass2) == "Clean_Biomass"] <- "sum_biomass"

# merge both datasets
zoop_prop <- merge(x=zoop_biomass2, y=zooplankton_biomass, by =c("Lake", "Experiment", 
                                  "Exp_day", "Enclosure", "Treatment", "Replicate"),all.x=TRUE)

# new column with the proportion values 
zoop_prop$proportion <- zoop_prop$Clean_Biomass/zoop_prop$sum_biomass

# checking if the proportion measures are correct  
str(zoop_prop)
zoop_prop2 <- zoop_prop[zoop_prop$Lake == "Bolmen" & 
                                     zoop_prop$Experiment == "Spring" &
                                     zoop_prop$Exp_day == 1 &
                                     zoop_prop$Enclosure == 1 &
                                     zoop_prop$Treatment == "C" &
                                     zoop_prop$Replicate == 1
                                     ,]
sum(zoop_prop2$proportion)
# sum is 1 

# proportion * size 
zoop_prop$pre_cwm <- zoop_prop$proportion*zoop_prop$Mean_lenght

# sum of proportion * size
zoop_prop_final <- ddply(zoop_prop,.(Lake, Experiment, Exp_day, Enclosure, 
         Treatment, Replicate), colwise(sum, . (pre_cwm)), na.rm=T)

# renaming column 
names(zoop_prop_final)[names(zoop_prop_final) == "pre_cwm"] <- "initial_zoop_body_size"

# converting unit from µm to mm
zoop_prop_final$initial_zoop_body_size <- zoop_prop_final$initial_zoop_body_size/1000

# only filtering for Exp_day = 1, because we want to have the initial body size
zoop_prop_final <- zoop_prop_final[(zoop_prop_final$Exp_day==1), ]

#-----------------------------------------------------------------------------------------------------#  

write.csv(zoop_prop_final,"/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Master-thesis/cwm_zooplankton_body_size.csv")

#-----------------------------------------------------------------------------------------------------#  

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# checking by one example if the calculation is right 
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

test2 <- zooplankton_biomass[zooplankton_biomass$Lake == "Bolmen" & 
                                         zooplankton_biomass$Experiment == "Spring" &
                                         zooplankton_biomass$Exp_day == 1 &
                                         zooplankton_biomass$Enclosure == 1 &
                                         zooplankton_biomass$Treatment == "C" &
                                         zooplankton_biomass$Replicate == 1
                                       ,]

sum(test2$Clean_Biomass)

sum((0.509256*375),(0.3386427*537),(0.08572073*280),(0.06476035*550),(0.000685899*70),(0.0003615539*150),(0.0005727013*90))



