#---------------------------------------------------------------------------------------------------------#
#### SEM for press disturbance & zooplankton ####
#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/master_dataset.csv")

# filtering dataset for phytoplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for press disturbance only 
dataset_zoopl_press <- dataset_zoopl[which(dataset_zoopl$Treatment=='S'), ]

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for phytoplankton data only 
dataset_phyto <- dataset_final[which(dataset_final$community=='phyto'), ]

# filtering data for pulse disturbance only 
dataset_phyto_press <- dataset_phyto[which(dataset_phyto$Treatment=='S'), ]

# renaming columns for phyto biovolume and chl a
names(dataset_phyto_press)[names(dataset_phyto_press)=="mean_biomass"] <- "phyto_biovolume"

dataset_phyto_press <- select(dataset_phyto_press, Lake, Experiment, Treatment, Enclosure, phyto_biovolume)

dataset_zoopl_press <- merge(x=dataset_phyto_press, y=dataset_zoopl_press, by= c("Lake", "Experiment", 
                                                                                       "Treatment", "Enclosure"))

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

# TN, TP, DOC, PAR, Temp, TN/TP ratio, DN/TP ratio, Chl a 

# checking normal distribution of TP
hist(dataset_zoopl_press$mean_TP)
shapiro.test(dataset_zoopl_press$mean_TP)
#  p-value = 0.005709

# checking normal distribution of TN  
hist(dataset_zoopl_press$mean_TN)
shapiro.test(dataset_zoopl_press$mean_TN)
dataset_zoopl_press$mean_TN_log <- log(dataset_zoopl_press$mean_TN*100)
hist(dataset_zoopl_press$mean_TN_log)
shapiro.test(dataset_zoopl_press$mean_TN_log)
# p-value = 0.0001328


# checking normal distribution of DOC
hist(dataset_zoopl_press$mean_DOC)
shapiro.test(dataset_zoopl_press$mean_DOC)
# p-value = 0.0008227
dataset_zoopl_press$mean_DOC_sqrt <- sqrt(dataset_zoopl_press$mean_DOC)
hist(dataset_zoopl_press$mean_DOC_sqrt)
shapiro.test(dataset_zoopl_press$mean_DOC_sqrt)
# p-value = 0.001198


# checking normal distribution of PAR
hist(dataset_zoopl_press$mean_PAR)
shapiro.test(dataset_zoopl_press$mean_PAR)
# p-value = 1.545e-06
dataset_zoopl_press$mean_PAR_log <- log(dataset_zoopl_press$mean_PAR)
hist(dataset_zoopl_press$mean_PAR_log)
shapiro.test(dataset_zoopl_press$mean_PAR_log)
# p-value = 0.00313

# checking normal distribution of Temp
hist(dataset_zoopl_press$mean_Temp)
shapiro.test(dataset_zoopl_press$mean_Temp)
# p-value = 3.777e-06
dataset_zoopl_press$mean_Temp_log <- log(dataset_zoopl_press$mean_Temp)
hist(dataset_zoopl_press$mean_Temp_log)
shapiro.test(dataset_zoopl_press$mean_Temp_log)


# checking normal distribution of Chl a 
hist(dataset_zoopl_press$mean_Chla)
shapiro.test(dataset_zoopl_press$mean_Chla)
# p-value = 1.67e-08
dataset_zoopl_press$mean_Chla_log <- log(dataset_zoopl_press$mean_Chla)
hist(dataset_zoopl_press$mean_Chla_log)
shapiro.test(dataset_zoopl_press$mean_Chla_log)
# p-value = 0.3784

# checking normal distribution of TN/TP ratio
hist(dataset_zoopl_press$mean_TN_TP_ratio)
shapiro.test(dataset_zoopl_press$mean_TN_TP_ratio)
#  p-value = 0.004568
dataset_zoopl_press$mean_TN_TP_ratio_100 <- dataset_zoopl_press$mean_TN_TP_ratio*100
hist(dataset_zoopl_press$mean_TN_TP_ratio_100)
shapiro.test(dataset_zoopl_press$mean_TN_TP_ratio_100)


# checking normal distribution of DN/TP ratio 
hist(dataset_zoopl_press$mean_DN_TP_ratio)
shapiro.test(dataset_zoopl_press$mean_DN_TP_ratio)
# p-value = 0.0007845
dataset_zoopl_press$mean_DN_TP_ratio_log <- log(dataset_zoopl_press$mean_DN_TP_ratio*100)
hist(dataset_zoopl_press$mean_DN_TP_ratio_log)
shapiro.test(dataset_zoopl_press$mean_DN_TP_ratio_log)
# p-value = 0.02709


#---------------------------------------------------------------------------------------------------------#
# Biomass, Evenness, ENS, Richness, Phytoplankton biovolume

# checking normal distribution of biomass
hist(dataset_zoopl_press$mean_biomass)
shapiro.test(dataset_zoopl_press$mean_biomass)
# p-value = 0.0007597
dataset_zoopl_press$mean_biomass_log <- log(dataset_zoopl_press$mean_biomass)
hist(dataset_zoopl_press$mean_biomass_log)
shapiro.test(dataset_zoopl_press$mean_biomass_log)
# p-value = 0.04232

# checking normal distribution of evenness
hist(dataset_zoopl_press$mean_J)
shapiro.test(dataset_zoopl_press$mean_J)
# p-value = 0.2807


# checking normal distribution of ENS
hist(dataset_zoopl_press$mean_ENS_D)
shapiro.test(dataset_zoopl_press$mean_ENS_D)
# p-value = 0.1171


# checking normal distribution of richness
hist(dataset_zoopl_press$mean_s)
shapiro.test(dataset_zoopl_press$mean_s)
# p-value = 0.07563


# checking normal distribution of phytoplankton biovolume
hist(dataset_zoopl_press$phyto_biovolume)
shapiro.test(dataset_zoopl_press$phyto_biovolume)
# p-value = 1.776e-12
dataset_zoopl_press$phyto_biovolume_log <- log(dataset_zoopl_press$phyto_biovolume)
hist(dataset_zoopl_press$phyto_biovolume_log)
shapiro.test(dataset_zoopl_press$phyto_biovolume_log)
# p-value = 0.1872


#---------------------------------------------------------------------------------------------------------#
# final reovery, resilience, area under the curve


# checking normal distribution of final recovery
hist(dataset_zoopl_press$final_stab)
shapiro.test(dataset_zoopl_press$final_stab)
# p-value = 0.0008583
dataset_zoopl_press$final_stab_log <- log(dataset_zoopl_press$final_stab*-1)
hist(dataset_zoopl_press$final_stab_log)
shapiro.test(dataset_zoopl_press$final_stab_log)
# p-value = 0.00458


# checking normal distribution of resilience
hist(dataset_zoopl_press$rate_change_oti)
shapiro.test(dataset_zoopl_press$rate_change_oti)
# p-value = 0.000104


# checking normal distribution of AUC
hist(dataset_zoopl_press$total_impact)
shapiro.test(dataset_zoopl_press$total_impact)
# p-value = 0.0004573
dataset_zoopl_press$total_impact_log <- log(dataset_zoopl_press$total_impact)
hist(dataset_zoopl_press$total_impact_log)
shapiro.test(dataset_zoopl_press$total_impact_log)
# p-value = 0.1646

#---------------------------------------------------------------------------------------------------------#
#### 1. final recovery for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = recovery 
# explanatory = Temp, biomass, evenness, ens, phyto biovolume, chl a, phyto biovolume, TP

recovery <- lmer(final_stab_log ~ mean_Chla_log + mean_J + mean_ENS_D + mean_DOC_sqrt + mean_TP + (1|Lake), 
                 data=dataset_zoopl_press)

summary(recovery)
# Chl a 
# evenness
# ENS 
# DOC
# TP 

r.squaredGLMM(recovery)
#  R2m       R2c
# [1,] 0.1607737 0.1607737


#---------------------------------------------------------------------------------------------------------#
#### 2. resilience for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience 
# explanatory = Temp, biomass, evenness, ens, phyto biovolume, chl a, phyto biovolume, TP

resilience <- lmer(rate_change_oti ~ mean_biomass_log + mean_ENS_D + mean_J + mean_TP + (1|Lake), 
                   data=dataset_zoopl_press)

summary(resilience)
# biomass
# TP

r.squaredGLMM(resilience)
# R2m       R2c
# [1,] 0.1926186 0.1926186

#---------------------------------------------------------------------------------------------------------#
#### 3. AUC for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC 
# explanatory = Temp, biomass, evenness, ens, phyto biovolume, chl a, phyto biovolume, TP

AUC <- lmer(total_impact_log ~ mean_J + mean_ENS_D + mean_Chla_log +  mean_DOC_sqrt + mean_biomass_log + 
              mean_TP + (1|Lake), data=dataset_zoopl_press)

summary(AUC)
# evenness
# ENS
# CHl a 
# biomass

r.squaredGLMM(AUC)
#  R2m       R2c
# [1,] 0.2512569 0.2512569

#---------------------------------------------------------------------------------------------------------#
#### 4. Biomass for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = mean zoopl biomass
# explanatory = Temp, phyto biovolume, chl a, TP, DOC

biomass <- lmer(mean_biomass_log ~ mean_Chla_log + phyto_biovolume_log + mean_Temp_log + mean_DOC_sqrt + 
                  mean_TP + (1|Lake), data=dataset_zoopl_press)

summary(biomass)
# Chl a 
# phyto biovolume
# Temperature

r.squaredGLMM(biomass)
# R2m       R2c
# [1,] 0.500953 0.8692953

#---------------------------------------------------------------------------------------------------------#
#### 5. Evenness for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = Temp, phyto biovolume, chl a, TP, DOC

evenness <- lmer(mean_J ~ mean_Chla_log + mean_Temp_log + mean_TP + (1|Lake), 
                 data=dataset_zoopl_press)

summary(evenness)
# Chl a 
# TP 

r.squaredGLMM(evenness)
# R2m       R2c
# [1,] 0.4685087 0.6434607

#---------------------------------------------------------------------------------------------------------#
#### 6. ENS for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = Temp, phyto biovolume, chl a, TP, DOC

ENS <- lmer(mean_ENS_D ~ mean_Chla_log + phyto_biovolume_log + mean_Temp_log + mean_TP + (1|Lake), 
            data=dataset_zoopl_press)


summary(ENS)
# Chl a 
# Temp
# TP

r.squaredGLMM(ENS)
#R2m       R2c
#[1,] 0.4135887 0.6008042

#---------------------------------------------------------------------------------------------------------#



