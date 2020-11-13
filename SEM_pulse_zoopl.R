#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse disturbance & zooplankton ####
#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/master_dataset.csv")

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for pulse disturbance only 
dataset_zoopl_pulse <- dataset_zoopl[which(dataset_zoopl$Treatment=='F'), ]

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for phytoplankton data only 
dataset_phyto <- dataset_final[which(dataset_final$community=='phyto'), ]

# filtering data for pulse disturbance only 
dataset_phyto_pulse <- dataset_phyto[which(dataset_phyto$Treatment=='F'), ]

# renaming columns for phyto biovolume and chl a
names(dataset_phyto_pulse)[names(dataset_phyto_pulse)=="mean_biomass"] <- "phyto_biovolume"

dataset_phyto_pulse <- select(dataset_phyto_pulse, Lake, Experiment, Treatment, Enclosure, phyto_biovolume)

dataset_zooplankton_pulse <- merge(x=dataset_phyto_pulse, y=dataset_zoopl_pulse, by= c("Lake", "Experiment", 
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

# TN, TP, DOC, PAR, Temp, TN/TP ratio, DN/TP ratio


# checking normal distribution of TN
hist(dataset_zooplankton_pulse$mean_TN)
shapiro.test(dataset_zooplankton_pulse$mean_TN)
#  p-value = 1.406e-05
dataset_zooplankton_pulse$mean_TN_100 <- dataset_zooplankton_pulse$mean_TN
hist(dataset_zooplankton_pulse$mean_TN_100)
shapiro.test(dataset_zooplankton_pulse$mean_TN_100)

# checking normal distribution of TP
hist(dataset_zooplankton_pulse$mean_TP)
shapiro.test(dataset_zooplankton_pulse$mean_TP)
# p-value = 0.003156
dataset_zooplankton_pulse$mean_TP_log <- log(dataset_zooplankton_pulse$mean_TP)
hist(dataset_zooplankton_pulse$mean_TP_log)
shapiro.test(dataset_zooplankton_pulse$mean_TP_log)
# p-value = 0.1248


# checking normal distribution of DOC
hist(dataset_zooplankton_pulse$mean_DOC)
shapiro.test(dataset_zooplankton_pulse$mean_DOC)
#  p-value = 0.002989
dataset_zooplankton_pulse$mean_DOC_sqrt <- sqrt(dataset_zooplankton_pulse$mean_DOC)
hist(dataset_zooplankton_pulse$mean_DOC_sqrt)
shapiro.test(dataset_zooplankton_pulse$mean_DOC_sqrt)
#  p-value = 0.004453


# checking normal distribution of PAR
hist(dataset_zooplankton_pulse$mean_PAR)
shapiro.test(dataset_zooplankton_pulse$mean_PAR)
# p-value = 5.475e-07
dataset_zooplankton_pulse$mean_PAR_log <- log(dataset_zooplankton_pulse$mean_PAR)
hist(dataset_zooplankton_pulse$mean_PAR_log)
shapiro.test(dataset_zooplankton_pulse$mean_PAR_log)
# p-value = 0.1951


# checking normal distribution of Temp
hist(dataset_zooplankton_pulse$mean_Temp)
shapiro.test(dataset_zooplankton_pulse$mean_Temp)
# p-value = 1.809e-12
dataset_zooplankton_pulse$mean_Temp_log <- log(dataset_zooplankton_pulse$mean_Temp)
hist(dataset_zooplankton_pulse$mean_Temp_log)
shapiro.test(dataset_zooplankton_pulse$mean_Temp_log)


# checking normal distribution of TN/TP ratio 
hist(dataset_zooplankton_pulse$mean_TN_TP_ratio)
shapiro.test(dataset_zooplankton_pulse$mean_TN_TP_ratio)
# p-value = 0.003622
dataset_zooplankton_pulse$mean_TN_TP_ratio_log <- log(dataset_zooplankton_pulse$mean_TN_TP_ratio * 100)
hist(dataset_zooplankton_pulse$mean_TN_TP_ratio_log)
shapiro.test(dataset_zooplankton_pulse$mean_TN_TP_ratio_log)
# p-value = 0.01473


# checking normal distribution of DN/TP ratio 
hist(dataset_zooplankton_pulse$mean_DN_TP_ratio)
shapiro.test(dataset_zooplankton_pulse$mean_DN_TP_ratio)
# p-value = 0.0005185
dataset_zooplankton_pulse$mean_DN_TP_ratio_log <- log(dataset_zooplankton_pulse$mean_DN_TP_ratio * 100)
hist(dataset_zooplankton_pulse$mean_DN_TP_ratio_log)
shapiro.test(dataset_zooplankton_pulse$mean_DN_TP_ratio_log)
#  p-value = 0.05559

#---------------------------------------------------------------------------------------------------------#
# Biomass, Evenness, ENS, Richness, Phytoplankton biovolume, Chl a 


# checking normal distribution of biomass
hist(dataset_zooplankton_pulse$mean_biomass)
shapiro.test(dataset_zooplankton_pulse$mean_biomass)
# p-value = 7.328e-06
dataset_zooplankton_pulse$mean_biomass_log <- log(dataset_zooplankton_pulse$mean_biomass)
hist(dataset_zooplankton_pulse$mean_biomass_log)
shapiro.test(dataset_zooplankton_pulse$mean_biomass_log)
# p-value = 0.2326

# checking normal distribution of evenness
hist(dataset_zooplankton_pulse$mean_J)
shapiro.test(dataset_zooplankton_pulse$mean_J)
# p-value = 0.001136


# checking normal distribution of ENS
hist(dataset_zooplankton_pulse$mean_ENS_D)
shapiro.test(dataset_zooplankton_pulse$mean_ENS_D)
# p-value = 0.956


# checking normal distribution of richness
hist(dataset_zooplankton_pulse$mean_s)
shapiro.test(dataset_zooplankton_pulse$mean_s)
# p-value = 0.04756


# checking normal distribution of phytoplankton biovolume
hist(dataset_zooplankton_pulse$phyto_biovolume)
shapiro.test(dataset_zooplankton_pulse$phyto_biovolume)
# p-value = 7.695e-12
dataset_zooplankton_pulse$phyto_biovolume_log <- log(dataset_zooplankton_pulse$phyto_biovolume)
hist(dataset_zooplankton_pulse$phyto_biovolume_log)
shapiro.test(dataset_zooplankton_pulse$phyto_biovolume_log)
# p-value = 0.1764


# checking normal distribution of Chl a 
hist(dataset_zooplankton_pulse$mean_Chla)
shapiro.test(dataset_zooplankton_pulse$mean_Chla)
# p-value = 8.299e-09
dataset_zooplankton_pulse$mean_Chla_log <- log(dataset_zooplankton_pulse$mean_Chla)
hist(dataset_zooplankton_pulse$mean_Chla_log)
shapiro.test(dataset_zooplankton_pulse$mean_Chla_log)
# p-value = 0.2532

#---------------------------------------------------------------------------------------------------------#
# Resistance, final reovery, resilience, area under the curve


# checking normal distribution of resistance
hist(dataset_zooplankton_pulse$initial_stab)
shapiro.test(dataset_zooplankton_pulse$initial_stab)
# p-value = 4.078e-05
dataset_zooplankton_pulse$initial_stab_log <- log(dataset_zooplankton_pulse$initial_stab*-1)
hist(dataset_zooplankton_pulse$initial_stab_log)
shapiro.test(dataset_zooplankton_pulse$initial_stab_log)
# p-value = 0.9424


# checking normal distribution of final recovery
hist(dataset_zooplankton_pulse$final_stab)
shapiro.test(dataset_zooplankton_pulse$final_stab)
# p-value = 5.371e-05
dataset_zooplankton_pulse$final_stab_log <- log(dataset_zooplankton_pulse$final_stab*-1)
hist(dataset_zooplankton_pulse$final_stab_log)
shapiro.test(dataset_zooplankton_pulse$final_stab_log)


# checking normal distribution of resilience
hist(dataset_zooplankton_pulse$rate_change_ort)
shapiro.test(dataset_zooplankton_pulse$rate_change_ort)
# p-value = 0.7825


# checking normal distribution of AUC
hist(dataset_zooplankton_pulse$total_impact)
shapiro.test(dataset_zooplankton_pulse$total_impact)
# p-value = 0.004421
dataset_zooplankton_pulse$total_impact_log <- log(dataset_zooplankton_pulse$total_impact)
hist(dataset_zooplankton_pulse$total_impact_log)
shapiro.test(dataset_zooplankton_pulse$total_impact_log)
# p-value = 0.09962



#---------------------------------------------------------------------------------------------------------#
#### 1. resistance for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resistance 
# explanatory = Temp, biomass, evenness, ens, phyto biovolume, chl a, phyto biovolume 

resistance <- lmer(initial_stab_log ~ mean_Chla_log + mean_Temp_log + mean_J + mean_ENS_D + mean_biomass_log + mean_DOC_sqrt + (1|Lake), 
                       data=dataset_zooplankton_pulse)

summary(resistance)
# Chl a
# Temperature
# Evenness
# ENS
# biomass
# DOC


r.squaredGLMM(resistance)
#    R2m       R2c
# [1,] 0.2174836 0.4095002

#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = recovery 
# explanatory = Temp, biomass, evenness, ens, phyto biovolume, chl a, phyto biovolume 

recovery <- lmer(final_stab_log ~ mean_Chla_log + mean_Temp_log + mean_DOC_sqrt + phyto_biovolume_log + mean_TP_log + (1|Lake), 
                   data=dataset_zooplankton_pulse)

summary(recovery)
# Chl a t-value = -1.463
# Temperature t-value = -1.463 
# TP t-value = 2.255 


r.squaredGLMM(recovery)
#   R2m       R2c
# [1,] 0.3293807 0.3293807

#---------------------------------------------------------------------------------------------------------#
#### 3. resilience for zooplankton ####
#---------------------------------------------------------------------------------------------------------#

resilience <- lmer(rate_change_ort ~ mean_Chla_log + mean_Temp_log + mean_DOC_sqrt + mean_biomass_log + mean_TP_log + (1|Lake), 
                 data=dataset_zooplankton_pulse)

summary(resilience)
# Chl a t-value = 1.861
# DOC t-value =  2.132 
# TP t-value = -2.050

r.squaredGLMM(resilience)
#     R2m       R2c
# [1,] 0.1373039 0.1373039

#---------------------------------------------------------------------------------------------------------#
#### 4. AUC for zooplankton ####
#---------------------------------------------------------------------------------------------------------#

AUC <- lmer(total_impact_log ~ mean_Chla_log + mean_Temp_log + mean_DOC_sqrt + mean_biomass_log + mean_TP_log + (1|Lake), 
                   data=dataset_zooplankton_pulse)

summary(AUC)
# Biomass t-value = -3.503 
# TP t-value = 1.501

r.squaredGLMM(AUC)
#         R2m       R2c
# [1,] 0.5512366 0.6200007

#---------------------------------------------------------------------------------------------------------#
#### 5. Biomass for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = mean zoopl biomass
# explanatory = Chl a, phyto biomass, Temp, DOC, TP 

biomass <- lmer(mean_biomass_log ~ mean_Chla_log + phyto_biovolume_log + mean_Temp_log + mean_DOC_sqrt + mean_TP_log + (1|Lake), 
            data=dataset_zooplankton_pulse)

summary(biomass)
# Chl a 
# Temperature
# DOC
# TP

r.squaredGLMM(biomass)
#  R2m       R2c
# [1,] 0.4009 0.4372035

#---------------------------------------------------------------------------------------------------------#
#### 6. Evenness for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = Chl a, phyto biomass, Temp, DOC, TP 

evenness <- lmer(mean_J ~ mean_Chla_log + phyto_biovolume_log + mean_Temp_log + mean_DOC_sqrt + mean_TP_log + (1|Lake), 
                data=dataset_zooplankton_pulse)

summary(evenness)
# Chl a 
# Temp
# TP 

r.squaredGLMM(evenness)
# R2m       R2c
# [1,] 0.3654577 0.7285885

#---------------------------------------------------------------------------------------------------------#
#### 7. ENS for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = Chl a, phyto biomass, Temp, DOC, TP 

ENS <- lmer(mean_ENS_D ~ mean_Chla_log + mean_Temp_log + mean_DOC_sqrt + mean_TP_log + (1|Lake), 
                 data=dataset_zooplankton_pulse)


summary(ENS)
# Chl a 
# Temp
# TP

r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.4649958 0.6706771

#---------------------------------------------------------------------------------------------------------#
