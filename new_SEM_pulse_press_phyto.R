#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse& press disturbance for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# load packages

library(lme4)

library(lmerTest)

library(MuMIn)

library(tidyverse)

library(dplyr)

library(faraway)

library(piecewiseSEM)
#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/new_master_dataset.csv")

# filtering dataset for zooplankton data only 
dataset_phyto <- dataset_final[which(dataset_final$community=='phyto'), ]

# filtering data for pulse disturbance only 
phyto_pulse_press <- dataset_phyto[which(dataset_phyto$Treatment=='FS'), ]

phyto_pulse_press = subset(phyto_pulse_press, select = -c(14:15))

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for press disturbance only 
dataset_zoopl_pulse <- dataset_zoopl[which(dataset_zoopl$Treatment=='FS'), ]

# filtering for biomass 
dataset_zoopl_pulse <- select(dataset_zoopl_pulse, Lake, Experiment, Treatment, Enclosure, mean_zoop_biomass)

# merging both datasets 
phyto_pulse_press <- merge(x=phyto_pulse_press, y=dataset_zoopl_pulse, by= c("Lake", "Experiment", 
                                                                             "Treatment", "Enclosure"))
#---------------------------------------------------------------------------------------------------------#
#### 0. normal distribution and scaling of variables ####
#---------------------------------------------------------------------------------------------------------#
# Variables:

# TN, TP, DOC, PAR, Temp

# checking normal distribution of TN
hist(phyto_pulse_press$mean_TN)
shapiro.test(phyto_pulse_press$mean_TN)
# p-value = 2.56e-05

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

#---------------------------------------------------------------------------------------------------------#
# Chl a, Evenness, ENS, Richness, zooplankton biomass

# checking normal distribution of Chl a  
hist(phyto_pulse_press$mean_Chla)
shapiro.test(phyto_pulse_press$mean_Chla)
# p-value = 1.133e-08
phyto_pulse_press$mean_Chla_log <- log(phyto_pulse_press$mean_Chla)
hist(phyto_pulse_press$mean_Chla_log)
shapiro.test(phyto_pulse_press$mean_Chla_log)
# p-value = 0.07454

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

# checking normal distribution of zooplankton biomass
hist(phyto_pulse_press$mean_zoop_biomass)
shapiro.test(phyto_pulse_press$mean_zoop_biomass)
# p-value = 3.378e-06
phyto_pulse_press$mean_zoop_biomass_sqrt <- sqrt(phyto_pulse_press$mean_zoop_biomass)
hist(phyto_pulse_press$mean_zoop_biomass_sqrt)
shapiro.test(phyto_pulse_press$mean_zoop_biomass_sqrt)
# p-value = 0.0282

#---------------------------------------------------------------------------------------------------------#
# Resistance, final recovery, resilience

# checking normal distribution of  resistance 
hist(phyto_pulse_press$initial_stab)
shapiro.test(phyto_pulse_press$initial_stab)
# p-value = 0.04944

# checking normal distribution of recovery 
hist(phyto_pulse_press$final_stab)
shapiro.test(phyto_pulse_press$final_stab)
# p-value = 0.06005

# checking normal distribution of resilience 
hist(phyto_pulse_press$rate_change_ort)
shapiro.test(phyto_pulse_press$rate_change_ort)
# p-value = 0.4692

#---------------------------------------------------------------------------------------------------------#
#### 1. resistance for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resistance 
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, evenness, richness, ENS, zooplankton biomass

# testing significance of all variables individually 
resistance <- lmer(initial_stab ~ mean_PAR_log + (1|Lake), data=phyto_pulse_press)

summary(resistance)
# Chl a p-value = 0.5428
# TP p-value = 0.607
# TN p-value = 0.865
# DOC p-value = 0.567
# PAR p-value = 0.030 !
# Temp p-value = 0.2344  
# evenness p-value = 0.1023
# richness p-value = 0.717
# ENS p-value = 0.2222 
# zoop biomass p-value = 0.5346 

r.squaredGLMM(resistance)
# R2m       R2c
# [1,] 0.1410903 0.5230575

#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = recovery 
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, evenness, richness, ENS, zooplankton biomass

# testing significance of all variables individually 
recovery <- lmer(final_stab ~ mean_zoop_biomass_sqrt + (1|Lake), data=phyto_pulse_press)

summary(recovery)
# Chl a p-value = 0.698
# TP p-vale = 0.382
# TN p-value = 0.621
# DOC p-value = 0.1919 
# PAR p-value = 0.228 
# Temp p-value = 0.000885 !
# evenness p-value = 0.0228 !
# richness p-value = 0.504
# ENS p-value = 0.0984 !
# zoop biomass p-value = 0.0899 !

recovery <- lmer(final_stab ~ mean_zoop_biomass_sqrt + (1|Lake), data=phyto_pulse_press)

summary(recovery)


r.squaredGLMM(resistance)
# R2m       R2c
# [1,] 0.1410903 0.5230575

#---------------------------------------------------------------------------------------------------------#
#### 3. resilience for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience 
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, evenness, richness, ENS, zooplankton biomass

# testing significance of all variables individually 
resilience <- lmer(rate_change_ort ~ mean_zoop_biomass_sqrt + (1|Lake), data=phyto_pulse_press)

summary(resilience)
# Chl a p-value = 0.426
# TP p-value = 0.880
# TN p-value = 0.653
# DOC p-value = 0.865
# PAR p-value = 0.228
# Temp p-value = 0.0521 !
# evenness p-value = 0.0107 !
# richness p-value = 0.543
# ENS p-value = 0.204
# zoop biomass p-value = 0.528

resilience <- lmer(rate_change_ort ~ mean_Temp + mean_J + (1|Lake), data=phyto_pulse_press)

summary(resilience)

r.squaredGLMM(resilience)
# R2m       R2c
# [1,] 0.1978856 0.2504836

#---------------------------------------------------------------------------------------------------------#
#### 4. Chl a for phytoplankton  ####
#---------------------------------------------------------------------------------------------------------#
# response = Chl as proxy for phytoplankton biomass 
# explanatory = TP, TN, DOC, PAR, Temp, zooplankton biomass

# testing significance of all variables individually 

Chla <- lmer(mean_Chla_log ~ mean_TP + (1|Lake), data=phyto_pulse_press)
summary(Chla)
# TP p-value = 0.00249 !
# TN p-value = 3.25e-05 !
# DOC p-value = 2.43e-06  !
# PAR p-value = 4.87e-06 !
# Temp p-value = 0.421 
# zoopl biomass p-value = 0.00243 !

Chla <- lmer(mean_Chla_log ~ mean_TP_log + mean_DOC + mean_PAR + (1|Lake), data=phyto_pulse_press)
summary(Chla)

r.squaredGLMM(Chla)
# R2m       R2c
# [1,] 0.2179853 0.9863594

#---------------------------------------------------------------------------------------------------------#
#### 5. Evenness for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, zooplankton biomass

# testing significance of all variables individually 
evenness <- lmer(mean_J ~ mean_Chla_log + (1|Lake), data=phyto_pulse_press)

summary(evenness)
# Chl a p-value = 0.0321  !
# TP p-value =  0.78658    
# TN p-value = 0.674568  
# DOC p-value = 0.3530 
# PAR p-value = 0.0489 !
# zoopl biomass p-value = 0.245

# --> Chl a, PAR
evenness <- lmer(mean_J ~ mean_Chla_log + mean_PAR_log + (1|Lake), data=phyto_pulse_press)
# PAR not significant anymore 

summary(evenness)

r.squaredGLMM(evenness)
# R2m       R2c
# [1,] 0.2736337 0.3283547


#---------------------------------------------------------------------------------------------------------#
#### 6. ENS for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, zooplankton biomass

# testing significance of all variables individually 
ENS <- lmer(mean_ENS_D_log ~ mean_zoop_biomass_sqrt + (1|Lake), data=phyto_pulse_press)

summary(ENS)
# Chl a p-value =  0.004853 !
# TP p-value = 0.6583 
# TN p-value = 0.469606
# DOC p-value = 0.000149  !
# PAR p-value =  0.198 
# Temp p-value = 0.34347  
# zoopl biomass p-value = 0.0892 !

# --> DOC, zoop biomass

r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.03086841 0.7624222


#---------------------------------------------------------------------------------------------------------#
#### 7. Richness for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = richness
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, zooplankton biomass

# testing significance of all variables individually 
richness <- lmer(mean_s ~ mean_zoop_biomass_sqrt + (1|Lake), data=phyto_pulse_press)

summary(richness)
# Chl a p-value = 0.0598 !
# TP p-value = 0.0503 !
# TN p-value = 0.0646 !
# DOC p-value = 0.15559 
# PAR p-value = 0.89359 
# Temp p-value = 0.32968 
# zoop biomass p-value = 0.94091 


richness <- lmer(mean_s ~ mean_TP_log + (1|Lake), data=phyto_pulse_press)

summary(richness)

r.squaredGLMM(richness)
# R2m       R2c
# [1,] 0.08564191 0.9039477


#---------------------------------------------------------------------------------------------------------#
#### 8. SEM  ####
#---------------------------------------------------------------------------------------------------------#
## final individual models 
resistance <- lmer(initial_stab ~ mean_PAR_log + mean_Temp + (1|Lake), data=phyto_pulse_press)
recovery <- lmer(final_stab ~ mean_Temp + mean_TP_log + mean_Chla_log + mean_ENS_D_log + (1|Lake), data=phyto_pulse_press)
Chla <- lmer(mean_Chla_log ~ mean_Temp + mean_TP_log + mean_PAR_log + (1|Lake), data=phyto_pulse_press)
# evenness <- lmer(mean_J ~ mean_Chla_log + mean_PAR_log + (1|Lake), data=phyto_pulse_press)
ENS <- lmer(mean_ENS_D_log ~ mean_zoop_biomass_sqrt + mean_PAR_log + (1|Lake), data=phyto_pulse_press)

# omit NA observations
phyto_pulse_press_complete <- na.omit(phyto_pulse_press)

modelList <- psem(
  resistance,
  recovery,
  Chla,
  ENS,
  mean_ENS_D_log %~~% mean_Chla_log,
  data=phyto_pulse_press_complete
)

results <- summary(modelList)
# excluding evenness because of singular fit

rsquared(modelList)

# Akaike’s information criterion
results$IC$AIC
# 66.861

results$dTable

# Fisher’s C 
results$Cstat
#       Fisher.C df P.Value
# 1   20.861 20   0.405

results$coefficients






