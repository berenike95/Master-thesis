#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse& press disturbance for zooplankton ####
#---------------------------------------------------------------------------------------------------------#

# load packages

library(lme4)

library(lmerTest)

library(MuMIn)

library(tidyverse)

library(dplyr)

library(faraway)

#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/master_dataset3.csv")

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
#### 0. normal distribution and scaling of variables ####
#---------------------------------------------------------------------------------------------------------#
# Variables:

# TN, TP, DOC, PAR, Temp


# checking normal distribution of TN
hist(zoopl_pulse_press$mean_TN)
shapiro.test(zoopl_pulse_press$mean_TN)
#   p-value = 2.56e-05

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

#---------------------------------------------------------------------------------------------------------#
# Biomass, body size, Evenness, ENS, Richness, Chl a 


# checking normal distribution of biomass
hist(zoopl_pulse_press$mean_biomass)
shapiro.test(zoopl_pulse_press$mean_biomass)
# p-value = 3.378e-06
zoopl_pulse_press$mean_biomass_sqrt <- sqrt(zoopl_pulse_press$mean_biomass)
hist(zoopl_pulse_press$mean_biomass_sqrt)
shapiro.test(zoopl_pulse_press$mean_biomass_sqrt)
# p-value = 0.0282

# checking normal distribution of body size
hist(zoopl_pulse_press$initial_zoop_body_size)
shapiro.test(zoopl_pulse_press$initial_zoop_body_size)
# p-value = 0.0009254
zoopl_pulse_press$initial_zoop_body_size_log <- log(zoopl_pulse_press$initial_zoop_body_size*10)
hist(zoopl_pulse_press$initial_zoop_body_size_log)
shapiro.test(zoopl_pulse_press$initial_zoop_body_size_log)

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
# explanatory = TP, TN, DOC, PAR, Temp, biomass, evenness, ens, richness, Chl a, body size

# testing significance of all variables individually 
resistance <- lmer(initial_stab_log ~ initial_zoop_body_size + (1|Lake), data=zoopl_pulse_press)

summary(resistance)
# Chl a p-value = 0.784 
# TP p-value = 0.706 
# TN p-value = 0.831 
# DOC p-value = 0.0369 !
# PAR p-value = 0.649 
# Temp p-value = 0.756 
# evenness p-value = 0.589
# ENS p-value = 0.420 
# richness p-value = 0.00627 !
# body size p-value = 0.00457 !

# ---> DOC, bivolume, richness

resistance <- lmer(initial_stab_log ~ mean_DOC + (1|Lake), data=zoopl_pulse_press)

resistance <- lmer(initial_stab_log ~ mean_s + initial_zoop_body_size + mean_TN + (1|Lake), data=zoopl_pulse_press)


summary(resistance)

r.squaredGLMM(resistance)
#  R2m       R2c
# [1,] 0.2426139 0.5349752

#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = recovery 
# explanatory = TP, TN, DOC, PAR, Temp, biomass, evenness, ens, richness, chl a, body size

# testing significance of all variables individually 
recovery <- lmer(final_stab_log ~ initial_zoop_body_size + (1|Lake), data=zoopl_pulse_press)

summary(recovery)
# Chl a p-value = 0.833
# TP p-value =  0.278 
# TN p-value = 0.197 
# DOC p-value = 0.000464 --> singular fit
# PAR p-value = 0.0869 
# Temp p-value = 0.945 
# biomass p-value = 0.717 
# evenness p-value = 0.429
# ENS p-value = 0.494
# richness p-value = 0.511 
# body size p-value = 0.0343 ! 

#---------------------------------------------------------------------------------------------------------#
#### 3. AUC for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC 
# explanatory = TP, TN, DOC, PAR, Temp, biomass, evenness, ens, richness, chl a, body size

# testing significance of all variables individually 
AUC <- lmer(total_impact_log ~ initial_zoop_body_size + (1|Lake), data=zoopl_pulse_press)

summary(AUC)
# Chl a p-value = 0.710574  
# TP p-value =  0.9954  
# TN p-value = 0.95808  
# DOC p-value = 0.01005 !
# PAR p-value = 0.797  
# Temp p-value = 0.1623 
# biomass p-value = 0.0957  
# evenness p-value = 0.052 
# ENS p-value = 0.104  
# richness p-value = 0.0203 !
# body size p-value =  0.751

# --> DOC, richness

AUC <- lmer(total_impact_log ~ mean_DOC + mean_s + (1|Lake), data=zoopl_pulse_press)

summary(AUC)

r.squaredGLMM(AUC)
# R2m       R2c
# [1,] 0.4912176 0.5062719

#---------------------------------------------------------------------------------------------------------#
#### 4. resilience for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience 
# explanatory = TP, TN, DOC, PAR, Temp, biomass, evenness, ens, richness, chl a, body size

# testing significance of all variables individually 
resilience <- lmer(rate_change_ort ~ initial_zoop_body_size + (1|Lake), data=zoopl_pulse_press)

summary(resilience)
# Chl a p-value = 0.519 
# TP p-value = 0.701 
# TN p-value = 0.920 
# DOC p-value = 0.559 
# PAR p-value = 0.751 
# Temp p-value = 0.712 
# biomass p-value = 0.3303 
# evenness p-value = 0.879 
# ENS p-value = 0.855 
# richness p-value =  0.304 
# body size p-value = -4.853 2.27e-05 !


#---------------------------------------------------------------------------------------------------------#
#### 5. Biomass for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = mean zoopl biomass
# explanatory = TP, TN, DOC, PAR, Temp, chl a, body size

# testing significance of all variables individually 
biomass <- lmer(mean_biomass_sqrt ~ initial_zoop_body_size + (1|Lake), data=zoopl_pulse_press)

summary(biomass)
# Chl a p-value = 0.0179 !
# TP p-value =  0.0635  
# TN p-value = 7.56e-09 !
# DOC p-value = 0.5693   
# PAR p-value = 0.00202 !
# Temp p-value = 0.5080 
# body size p-value = 0.0188 !

# --> Chl a, TN, PAR

biomass <- lmer(mean_biomass_sqrt ~ mean_TN + mean_PAR_log + (1|Lake), data=zoopl_pulse_press)

summary(biomass)

r.squaredGLMM(biomass)
# R2m      R2c
# [1,] 0.4802795 0.984169

#---------------------------------------------------------------------------------------------------------#
#### 6. Initial body size for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = initial body size
# explanatory = TP, TN, DOC, PAR, Temp,chl a

# testing significance of all variables individually 
size <- lmer(initial_zoop_body_size ~ mean_Chla_log + (1|Lake), data=zoopl_pulse_press)

summary(size)
# Chl a p-value = 0.0245

r.squaredGLMM(size)

#---------------------------------------------------------------------------------------------------------#
#### 7. Evenness for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = TP, TN, DOC, PAR, Temp, chl a, body size

# testing significance of all variables individually 
evenness <- lmer(mean_J ~ initial_zoop_body_size + (1|Lake), data=zoopl_pulse_press)

summary(evenness)
# Chl a p-value = 0.91601   
# TP p-value = 0.0555 
# TN p-value =  0.94567  
# DOC p-value = 0.20500 
# Temp p-value = 0.0957 
# body size p-value =  0.462 

#---------------------------------------------------------------------------------------------------------#
#### 8. ENS for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = TP, TN, DOC, PAR, Temp, chl a, body size

# testing significance of all variables individually 
ENS <- lmer(mean_ENS_D ~ initial_zoop_body_size + (1|Lake), data=zoopl_pulse_press)

summary(ENS)
# Chl a p-value = 0.217091 
# TP p-value =  0.14644  
# TN p-value = 0.8495  
# DOC p-value =  0.13404   
# PAR p-value = 0.348  
# Temp p-value = 0.9135 
# body size p-value =  0.647

#---------------------------------------------------------------------------------------------------------#
#### 9. Richness for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = richness
# explanatory = TP, TN, DOC, PAR, Temp, chl a, body size

richness <- lmer(mean_s ~ mean_PAR_log + mean_TN +(1|Lake), data=zoopl_pulse_press)
summary(richness)
# Chl a  0.001966 !
# TP 0.600
# TN 0.00292 !
# DOC 0.31323 
# Temp 0.79860 
# PAR 0.00639 !

#---------------------------------------------------------------------------------------------------------#
#### 10. SEM####
#---------------------------------------------------------------------------------------------------------#
## final individual models 
resistance <- lmer(initial_stab_log ~ mean_s + initial_zoop_body_size + mean_TN + (1|Lake), data=zoopl_pulse_press)
biomass <- lmer(mean_biomass_sqrt ~ mean_TN + (1|Lake), data=zoopl_pulse_press)
richness <- lmer(mean_s ~ mean_TN + mean_DOC_sqrt + (1|Lake), data=zoopl_pulse_press)
size <- lmer(initial_zoop_body_size ~ mean_Chla_log + (1|Lake), data=zoopl_pulse_press)
AUC <- lmer(total_impact_log ~ mean_s + mean_DOC_sqrt + mean_Chla_log + (1|Lake), data=zoopl_pulse_press)


zoopl_pulse_press_complete <- na.omit(zoopl_pulse_press)

modelList <- psem(
 resistance,
  richness,
 biomass,
 size,
  AUC,
  mean_s %~~% mean_biomass_sqrt,
 total_impact_log %~~% initial_stab_log,
 mean_biomass_sqrt %~~% initial_zoop_body_size,
  data=zoopl_pulse_press_complete
)

results <- summary(modelList)
# excluding recovery, resilience, DOC and AUC because of singular fit 

# Akaike’s information criterion
results$IC$AIC
# 70.45

results$dTable

# Fisher’s C 
results$Cstat
#      Fisher.C df P.Value
# 1   20.108 24   0.691

results$coefficients

rsquared(modelList)





