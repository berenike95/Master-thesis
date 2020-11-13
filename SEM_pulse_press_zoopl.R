#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse& press disturbance for zooplankton ####
#---------------------------------------------------------------------------------------------------------#


# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/master_dataset.csv")

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for pulse disturbance only 
zoopl_pulse_press <- dataset_zoopl[which(dataset_zoopl$Treatment=='FS'), ]

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for phytoplankton data only 
dataset_phyto <- dataset_final[which(dataset_final$community=='phyto'), ]

# filtering data for pulse disturbance only 
phyto_pulse_press <- dataset_phyto[which(dataset_phyto$Treatment=='FS'), ]

# renaming columns for phyto biovolume and chl a
names(phyto_pulse_press)[names(phyto_pulse_press)=="mean_biomass"] <- "phyto_biovolume"

phyto_pulse_press <- select(phyto_pulse_press, Lake, Experiment, Treatment, Enclosure, phyto_biovolume)

zoopl_pulse_press <- merge(x=phyto_pulse_press, y=zoopl_pulse_press, by= c("Lake", "Experiment", 
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
hist(zoopl_pulse_press$mean_TN)
shapiro.test(zoopl_pulse_press$mean_TN)
#   p-value = 2.56e-05
zoopl_pulse_press$mean_TN_100 <- zoopl_pulse_press$mean_TN*100


# checking normal distribution of TP
hist(zoopl_pulse_press$mean_TP)
shapiro.test(zoopl_pulse_press$mean_TP)
# p-value = 0.03724
zoopl_pulse_press$mean_TP_log <- log(zoopl_pulse_press$mean_TP)
hist(zoopl_pulse_press$mean_TP_log)
shapiro.test(zoopl_pulse_press$mean_TP_log)
# p-value = 0.3883


# checking normal distribution of DOC
hist(zoopl_pulse_press$mean_DOC)
shapiro.test(zoopl_pulse_press$mean_DOC)
#  p-value = 0.001361
zoopl_pulse_press$mean_DOC_sqrt <- sqrt(zoopl_pulse_press$mean_DOC)
hist(zoopl_pulse_press$mean_DOC_sqrt)
shapiro.test(zoopl_pulse_press$mean_DOC_sqrt)
#  p-value = 0.001937


# checking normal distribution of PAR
hist(zoopl_pulse_press$mean_PAR)
shapiro.test(zoopl_pulse_press$mean_PAR)
# p-value = 1.844e-06
zoopl_pulse_press$mean_PAR_log <- log(zoopl_pulse_press$mean_PAR)
hist(zoopl_pulse_press$mean_PAR_log)
shapiro.test(zoopl_pulse_press$mean_PAR_log)
# p-value = 0.01372


# checking normal distribution of Temp
hist(zoopl_pulse_press$mean_Temp)
shapiro.test(zoopl_pulse_press$mean_Temp)
# p-value = 3.616e-06
zoopl_pulse_press$mean_Temp_10 <- zoopl_pulse_press$mean_Temp/10


# checking normal distribution of TN/TP ratio 
hist(zoopl_pulse_press$mean_TN_TP_ratio)
shapiro.test(zoopl_pulse_press$mean_TN_TP_ratio)
# p-value = 0.002027
zoopl_pulse_press$mean_TN_TP_ratio_log <- log(zoopl_pulse_press$mean_TN_TP_ratio * 100)
hist(zoopl_pulse_press$mean_TN_TP_ratio_log)
shapiro.test(zoopl_pulse_press$mean_TN_TP_ratio_log)
# p-value = 0.003649


# checking normal distribution of DN/TP ratio 
hist(zoopl_pulse_press$mean_DN_TP_ratio)
shapiro.test(zoopl_pulse_press$mean_DN_TP_ratio)
# p-value = 0.0003171
zoopl_pulse_press$mean_DN_TP_ratio_log <- log(zoopl_pulse_press$mean_DN_TP_ratio * 100)
hist(zoopl_pulse_press$mean_DN_TP_ratio_log)
shapiro.test(zoopl_pulse_press$mean_DN_TP_ratio_log)
#  p-value = 0.01289

#---------------------------------------------------------------------------------------------------------#
# Biomass, Evenness, ENS, Richness, Phytoplankton biovolume, Chl a 


# checking normal distribution of biomass
hist(zoopl_pulse_press$mean_biomass)
shapiro.test(zoopl_pulse_press$mean_biomass)
# p-value = 3.378e-06
zoopl_pulse_press$mean_biomass_sqrt <- sqrt(zoopl_pulse_press$mean_biomass)
hist(zoopl_pulse_press$mean_biomass_sqrt)
shapiro.test(zoopl_pulse_press$mean_biomass_sqrt)
# p-value = 0.0282

# checking normal distribution of evenness
hist(zoopl_pulse_press$mean_J)
shapiro.test(zoopl_pulse_press$mean_J)
# p-value = 0.04245


# checking normal distribution of ENS
hist(zoopl_pulse_press$mean_ENS_D)
shapiro.test(zoopl_pulse_press$mean_ENS_D)
# p-value = 0.8125


# checking normal distribution of richness
hist(zoopl_pulse_press$mean_s)
shapiro.test(zoopl_pulse_press$mean_s)
# p-value = 0.1654


# checking normal distribution of phytoplankton biovolume
hist(zoopl_pulse_press$phyto_biovolume)
shapiro.test(zoopl_pulse_press$phyto_biovolume)
# p-value = 1.432e-06
zoopl_pulse_press$phyto_biovolume_log <- log(zoopl_pulse_press$phyto_biovolume)
hist(zoopl_pulse_press$phyto_biovolume_log)
shapiro.test(zoopl_pulse_press$phyto_biovolume_log)
# p-value = 0.1644


# checking normal distribution of Chl a 
hist(zoopl_pulse_press$mean_Chla)
shapiro.test(zoopl_pulse_press$mean_Chla)
# p-value = 1.133e-08
zoopl_pulse_press$mean_Chla_log <- log(zoopl_pulse_press$mean_Chla)
hist(zoopl_pulse_press$mean_Chla_log)
shapiro.test(zoopl_pulse_press$mean_Chla_log)
# p-value = 0.07454


#---------------------------------------------------------------------------------------------------------#
# Resistance, final reovery, resilience, area under the curve


# checking normal distribution of resistance
hist(zoopl_pulse_press$initial_stab)
shapiro.test(zoopl_pulse_press$initial_stab)
# p-value = 0.004312
zoopl_pulse_press$initial_stab_log <- log(zoopl_pulse_press$initial_stab*-1)
hist(zoopl_pulse_press$initial_stab_log)
shapiro.test(zoopl_pulse_press$initial_stab_log)
#  p-value = 0.6004


# checking normal distribution of final recovery
hist(zoopl_pulse_press$final_stab)
shapiro.test(zoopl_pulse_press$final_stab)
# p-value = 2.133e-07
zoopl_pulse_press$final_stab_log <- log(zoopl_pulse_press$final_stab*-1)
hist(zoopl_pulse_press$final_stab_log)
shapiro.test(zoopl_pulse_press$final_stab_log)
# p-value = 0.004258


# checking normal distribution of resilience
hist(zoopl_pulse_press$rate_change_ort)
shapiro.test(zoopl_pulse_press$rate_change_ort)
# p-value = 1.444e-06


# checking normal distribution of AUC
hist(zoopl_pulse_press$total_impact)
shapiro.test(zoopl_pulse_press$total_impact)
# p-value = 8.193e-05
zoopl_pulse_press$total_impact_log <- log(zoopl_pulse_press$total_impact)
hist(zoopl_pulse_press$total_impact_log)
shapiro.test(zoopl_pulse_press$total_impact_log)
# p-value = 0.9235

#---------------------------------------------------------------------------------------------------------#
#### 1. resistance for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resistance 
# explanatory = Temp, biomass, evenness, ens, phyto biovolume, chl a, phyto biovolume, DOC

resistance <- lmer(initial_stab_log ~ mean_Chla_log  + mean_J + phyto_biovolume_log + 
                     mean_biomass_sqrt + mean_DOC_sqrt + (1|Lake), data=zoopl_pulse_press)

summary(resistance)
# biomass
# DOC 


r.squaredGLMM(resistance)
# R2m       R2c
# [1,] 0.4336315 0.4731877



#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = recovery 
# explanatory = Temp, biomass, evenness, ens, phyto biovolume, chl a, phyto biovolume, DOC, TP

recovery <- lmer(final_stab_log ~ mean_DOC_sqrt + phyto_biovolume_log + mean_Chla_log + mean_J + 
                   mean_Temp_10 + (1|Lake), data=zoopl_pulse_press)

summary(recovery)
# DOC
# biovolume 
# Chl a 

r.squaredGLMM(recovery)
# R2m       R2c
# [1,] 0.280111 0.2935477



#---------------------------------------------------------------------------------------------------------#
#### 3. AUC for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC 
# explanatory = Temp, biomass, evenness, ens, phyto biovolume, chl a, phyto biovolume, DOC, TP

AUC <- lmer(total_impact_log ~ mean_Chla_log + mean_Temp_10 + mean_DOC_sqrt + mean_s + 
              phyto_biovolume_log + mean_DOC_sqrt + mean_biomass_sqrt + mean_TP_log + (1|Lake), 
            data=zoopl_pulse_press)

summary(AUC)
# Chl a 
# Temp
# DOC
# richness
# biovolume
# TP

r.squaredGLMM(AUC)
# R2m       R2c
# [1,] 0.6970184 0.6970184

#---------------------------------------------------------------------------------------------------------#
#### 4. resilience for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience 
# explanatory = Temp, biomass, evenness, ens, phyto biovolume, chl a, phyto biovolume, DOC, TP


resilience <- lmer(rate_change_ort ~ mean_Chla_log + mean_Temp_10 + mean_ENS_D + phyto_biovolume_log + 
                     mean_TP_log + (1|Lake), data=zoopl_pulse_press)

summary(resilience)
# Chl a 
# Temp
# ENS 
# phyto biovolume
# TP 

r.squaredGLMM(resilience)
#  R2m       R2c
# [1,] 0.1349492 0.5510136


#---------------------------------------------------------------------------------------------------------#
#### 5. Biomass for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = mean zoopl biomass
# explanatory = Chl a, phyto biomass, Temp, DOC, TP 

biomass <- lmer(mean_biomass_sqrt ~ mean_Chla_log + phyto_biovolume_log + mean_Temp_10 + mean_DOC_sqrt + 
                  mean_TP_log + (1|Lake), data=zoopl_pulse_press)

summary(biomass)
# CHl a 
# phyto biovolume
# Temp
# DOC 
# TP

r.squaredGLMM(biomass)
# R2m       R2c
# [1,] 0.4417781 0.9905488


#---------------------------------------------------------------------------------------------------------#
#### 6. Evenness for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = Chl a, phyto biomass, Temp, DOC, TP 

evenness <- lmer(mean_J ~ mean_Chla_log + phyto_biovolume_log + mean_Temp_10 + mean_TP_log + (1|Lake), 
                 data=zoopl_pulse_press)

summary(evenness)
# Chl a 
# phyto biovolume
# Temp
# TP

r.squaredGLMM(evenness)
# R2m       R2c
# [1,] 0.1864838 0.5923856

#---------------------------------------------------------------------------------------------------------#
#### 7. ENS for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = Chl a, phyto biomass, Temp, DOC, TP 

ENS <- lmer(mean_ENS_D ~ mean_Chla_log + mean_Temp_10 + mean_DOC_sqrt + mean_TP_log + (1|Lake), 
            data=zoopl_pulse_press)


summary(ENS)
# Chl a 
# TP 

r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.325464 0.5520196

#---------------------------------------------------------------------------------------------------------#

