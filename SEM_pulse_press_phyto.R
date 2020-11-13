#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse& press disturbance for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/master_dataset.csv")

# filtering dataset for zooplankton data only 
dataset_phyto <- dataset_final[which(dataset_final$community=='phyto'), ]

# filtering data for pulse disturbance only 
phyto_pulse_press <- dataset_phyto[which(dataset_phyto$Treatment=='FS'), ]

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for press disturbance only 
dataset_zoopl_pulse <- dataset_zoopl[which(dataset_zoopl$Treatment=='FS'), ]

# filtering for body size and biomass 
dataset_zoopl_pulse <- select(dataset_zoopl_pulse, Lake, Experiment, Treatment, Enclosure, 
                              initial_zoop_body_size, mean_biomass)

# renaming columns
names(dataset_zoopl_pulse)[names(dataset_zoopl_pulse)=="mean_biomass"] <- "mean_zoopl_biomass"
names(dataset_zoopl_pulse)[names(dataset_zoopl_pulse)=="initial_zoop_body_size"] <- "zoopl_body_size"

# merging both datasets 
phyto_pulse_press <- merge(x=phyto_pulse_press, y=dataset_zoopl_pulse, by= c("Lake", "Experiment", 
                                                                                 "Treatment", "Enclosure"))

phyto_pulse_press = subset(phyto_pulse_press, select = -c(16:17))

#---------------------------------------------------------------------------------------------------------#

# load packages

library('lme4')

library('lmerTest')

library('MuMIn')

library(tidyverse)

library(dplyr)

#---------------------------------------------------------------------------------------------------------#
#### 0. normal distribution and scaling of variables ####
#---------------------------------------------------------------------------------------------------------#
# Variables:

# TN, TP, DOC, PAR, Temp, TN/TP ratio, DN/TP ratio


# checking normal distribution of TN
hist(phyto_pulse_press$mean_TN)
shapiro.test(phyto_pulse_press$mean_TN)
# p-value = 2.56e-05
phyto_pulse_press$mean_TN_100 <- phyto_pulse_press$mean_TN*100


# checking normal distribution of TP
hist(phyto_pulse_press$mean_TP)
shapiro.test(phyto_pulse_press$mean_TP)
# p-value = 0.03724
phyto_pulse_press$mean_TP_log <- log(phyto_pulse_press$mean_TP)
hist(phyto_pulse_press$mean_TP_log)
shapiro.test(phyto_pulse_press$mean_TP_log)
# p-value = 0.3883


# checking normal distribution of DOC
hist(phyto_pulse_press$mean_DOC)
shapiro.test(phyto_pulse_press$mean_DOC)
#  p-value = 0.001361
phyto_pulse_press$mean_DOC_sqrt <- sqrt(phyto_pulse_press$mean_DOC)
hist(phyto_pulse_press$mean_DOC_sqrt)
shapiro.test(phyto_pulse_press$mean_DOC_sqrt)
#  p-value = 0.001937


# checking normal distribution of PAR
hist(phyto_pulse_press$mean_PAR)
shapiro.test(phyto_pulse_press$mean_PAR)
# p-value = 1.844e-06
phyto_pulse_press$mean_PAR_log <- log(phyto_pulse_press$mean_PAR)
hist(phyto_pulse_press$mean_PAR_log)
shapiro.test(phyto_pulse_press$mean_PAR_log)
# p-value = 0.01372


# checking normal distribution of Temp
hist(phyto_pulse_press$mean_Temp)
shapiro.test(phyto_pulse_press$mean_Temp)
# p-value = 3.616e-06
phyto_pulse_press$mean_Temp_10 <- phyto_pulse_press$mean_Temp/10


# checking normal distribution of TN/TP ratio 
hist(phyto_pulse_press$mean_TN_TP_ratio)
shapiro.test(phyto_pulse_press$mean_TN_TP_ratio)
# p-value = 0.002027
phyto_pulse_press$mean_TN_TP_ratio_log <- log(phyto_pulse_press$mean_TN_TP_ratio * 100)
hist(phyto_pulse_press$mean_TN_TP_ratio_log)
shapiro.test(phyto_pulse_press$mean_TN_TP_ratio_log)
# p-value = 0.003649


# checking normal distribution of DN/TP ratio 
hist(phyto_pulse_press$mean_DN_TP_ratio)
shapiro.test(phyto_pulse_press$mean_DN_TP_ratio)
# p-value = 0.0003171
phyto_pulse_press$mean_DN_TP_ratio_log <- log(phyto_pulse_press$mean_DN_TP_ratio * 100)
hist(phyto_pulse_press$mean_DN_TP_ratio_log)
shapiro.test(phyto_pulse_press$mean_DN_TP_ratio_log)
#  p-value = 0.01289


# checking normal distribution of Chl a  
hist(phyto_pulse_press$mean_Chla)
shapiro.test(phyto_pulse_press$mean_Chla)
# p-value = 1.133e-08
phyto_pulse_press$mean_Chla_log <- log(phyto_pulse_press$mean_Chla)
hist(phyto_pulse_press$mean_Chla_log)
shapiro.test(phyto_pulse_press$mean_Chla_log)
# p-value = 0.07454

#---------------------------------------------------------------------------------------------------------#
# biovolume, Evenness, ENS, Richness, zooplankton biomass, zooplankton body size 

# checking normal distribution of biovolume
hist(phyto_pulse_press$mean_biomass)
shapiro.test(phyto_pulse_press$mean_biomass)
# p-value = 1.432e-06
phyto_pulse_press$mean_biomass_log <- log(phyto_pulse_press$mean_biomass)
hist(phyto_pulse_press$mean_biomass_log)
shapiro.test(phyto_pulse_press$mean_biomass_log)
#  p-value = 0.1644


# checking normal distribution of evenness
hist(phyto_pulse_press$mean_J)
shapiro.test(phyto_pulse_press$mean_J)
# p-value = 0.175


# checking normal distribution of ENS
hist(phyto_pulse_press$mean_ENS_D)
shapiro.test(phyto_pulse_press$mean_ENS_D)
# p-value = 0.0049
phyto_pulse_press$mean_ENS_D_log <- log(phyto_pulse_press$mean_ENS_D)
hist(phyto_pulse_press$mean_ENS_D_log)
shapiro.test(phyto_pulse_press$mean_ENS_D_log)
# p-value = 0.01516


# checking normal distribution of richness
hist(phyto_pulse_press$mean_s)
shapiro.test(phyto_pulse_press$mean_s)
# p-value = 0.002444


# checking normal distribution of richness
hist(phyto_pulse_press$mean_s)
shapiro.test(phyto_pulse_press$mean_s)


# checking normal distribution of zooplankton biomass
hist(phyto_pulse_press$mean_zoopl_biomass)
shapiro.test(phyto_pulse_press$mean_zoopl_biomass)
# p-value = 3.378e-06
phyto_pulse_press$mean_zoopl_biomass_sqrt <- sqrt(phyto_pulse_press$mean_zoopl_biomass)
hist(phyto_pulse_press$mean_zoopl_biomass_sqrt)
shapiro.test(phyto_pulse_press$mean_zoopl_biomass_sqrt)
# p-value = 0.0282


# checking normal distribution of zooplankton body size 
hist(phyto_pulse_press$zoopl_body_size)
shapiro.test(phyto_pulse_press$zoopl_body_size)
# p-value = 0.0009254

#---------------------------------------------------------------------------------------------------------#
# Resistance, final reovery, resilience, area under the curve


# checking normal distribution of zooplankton resistance 
hist(phyto_pulse_press$initial_stab)
shapiro.test(phyto_pulse_press$initial_stab)
# p-value = 0.3974


# checking normal distribution of zooplankton resistance 
hist(phyto_pulse_press$final_stab)
shapiro.test(phyto_pulse_press$final_stab)
# p-value = 0.001492


# checking normal distribution of resilience 
hist(phyto_pulse_press$rate_change_ort)
shapiro.test(phyto_pulse_press$rate_change_ort)
# p-value = 0.1132

# checking normal distribution of AUC 
hist(phyto_pulse_press$total_impact)
shapiro.test(phyto_pulse_press$total_impact)
# p-value = 0.04629


#---------------------------------------------------------------------------------------------------------#
#### 1. resistance for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resistance 
# explanatory = Chl a, TP, TN, TN/TP, DN/TP, DOC, biovolume, evenness, ENS, zooplankton biomass, zooplankton body size 

resistance <- lmer(initial_stab ~ mean_TP_log + mean_TN_100 + mean_DN_TP_ratio_log + mean_DOC_sqrt + mean_Chla_log +
                     mean_ENS_D_log + zoopl_body_size + (1|Lake), data=phyto_pulse_press)

summary(resistance)
# TP
# TN
# DN/TP ratio 
# chl a 
# zoopl body size

r.squaredGLMM(resistance)
# R2m       R2c
# [1,] 0.1097894 0.7930914

#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = recovery 
# explanatory = Chl a, TP, TN, TN/TP, DN/TP, DOC, biovolume, evenness, ENS, zooplankton biomass, zooplankton body size 

recovery <- lmer(final_stab ~ mean_TN_100 + mean_DN_TP_ratio_log + mean_DOC_sqrt  + mean_J + 
                   mean_biomass_log + zoopl_body_size + (1|Lake), data=phyto_pulse_press)

summary(recovery)
# TN
# evenness 
# DOC
# biovolume
# zoopl body size 

r.squaredGLMM(recovery)
# R2m       R2c
# [1,] 0.3585448 0.4222014


#---------------------------------------------------------------------------------------------------------#
#### 3. AUC for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC 
# explanatory =  Chl a, TP, TN, TN/TP, DN/TP, DOC, biovolume, evenness, ENS, zooplankton biomass, 
# zooplankton body size 

AUC <- lmer(total_impact ~ mean_TN_100 + mean_DN_TP_ratio_log + mean_DOC_sqrt + mean_biomass_log + 
              zoopl_body_size + (1|Lake), data=phyto_pulse_press)

summary(AUC)
# TN
# DN/TP ratio 
# DOC
# biovolume 

r.squaredGLMM(AUC)
#  R2m       R2c
# [1,] 0.4980357 0.4980357

#---------------------------------------------------------------------------------------------------------#
#### 4. resilience for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience 
# explanatory = Chl a, TP, TN, TN/TP, DN/TP, DOC, biovolume, evenness, ENS, zooplankton biomass, 
# zooplankton body size


resilience <- lmer(rate_change_ort ~ mean_Chla_log + mean_ENS_D + mean_DN_TP_ratio_log + mean_DOC_sqrt + 
                     mean_biomass_log + mean_J + mean_ENS_D_log + mean_zoopl_biomass_sqrt + 
                     zoopl_body_size + (1|Lake), data=phyto_pulse_press)

summary(resilience)
# Chl a 
# ENS
# DN/TP ratio
# DOC
# biovolume
# evenness
# ENS
# zooplankton biomass
# zooplankton body size 

r.squaredGLMM(resilience)
# R2m       R2c
# [1,] 0.3944727 0.4241223


#---------------------------------------------------------------------------------------------------------#
#### 5. Biovolume for phytoplankton  ####
#---------------------------------------------------------------------------------------------------------#
# response = mean biovolume 
# explanatory = Chl a, TP, TN, TN/TP, DN/TP, DOC, zooplankton biomass, zooplankton body size

biomass <- lmer(mean_biomass_log ~ mean_TN_100 + mean_DN_TP_ratio_log + mean_DOC_sqrt + 
                  zoopl_body_size + (1|Lake), data=phyto_pulse_press)

summary(biomass)
# TN
# DN/TP ratio
# DOC

r.squaredGLMM(biomass)
#  R2m       R2c
# [1,] 0.5489954 0.7503183

#---------------------------------------------------------------------------------------------------------#
#### 6. Evenness for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = Chl a, TP, TN, TN/TP, DN/TP, DOC, zooplankton biomass, zooplankton body size

evenness <- lmer(mean_J ~ mean_Chla_log + mean_TP_log + mean_TN_TP_ratio_log + 
                   mean_DN_TP_ratio_log + mean_zoopl_biomass_sqrt +
                   zoopl_body_size + (1|Lake), data=phyto_pulse_press)

summary(evenness)
# Chl a 
# TP
# TN/TP ratio
# DN/TP ratio
# zoopl biomass
# zoopl body size 

r.squaredGLMM(evenness)
# R2m       R2c
# [1,] 0.5654664 0.6125189

#---------------------------------------------------------------------------------------------------------#
#### 7. ENS for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = Chl a, TP, TN, TN/TP, DN/TP, DOC, zooplankton biomass, zooplankton body size 

ENS <- lmer(mean_ENS_D ~ mean_Chla_log + mean_TP_log + mean_TN_TP_ratio_log +
              mean_DN_TP_ratio_log + mean_zoopl_biomass_sqrt + (1|Lake), 
            data=phyto_pulse_press)


summary(ENS)
# Chl a 
# DN/TP ratio
# zoopl biomass


r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.5436285 0.8446834

#---------------------------------------------------------------------------------------------------------#


