#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse disturbance & phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# load packages

library(lmerTest)

library(MuMIn)

library(tidyverse)

library(dplyr)

library(faraway)

library(piecewiseSEM)

#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/new_master_dataset.csv")

# filtering dataset for phytoplankton data only 
dataset_phyto <- dataset_final[which(dataset_final$community=='phyto'), ]

# filtering data for pulse disturbance only 
dataset_phyto_pulse <- dataset_phyto[which(dataset_phyto$Treatment=='F'), ]

dataset_phyto_pulse <- dataset_phyto_pulse[-c(14:15)] 

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for press disturbance only 
dataset_zoopl_pulse <- dataset_zoopl[which(dataset_zoopl$Treatment=='F'), ]

# filtering for zoop biomass 
dataset_zoopl_pulse <- select(dataset_zoopl_pulse, Lake, Experiment, Treatment, Enclosure, mean_zoop_biomass)

# merging both datasets 
dataset_phyto_pulse <- merge(x=dataset_phyto_pulse, y=dataset_zoopl_pulse, by= c("Lake", "Experiment", 
                                                                                 "Treatment", "Enclosure"))

#---------------------------------------------------------------------------------------------------------#
#### 0. normal distribution and scaling of variables ####
#---------------------------------------------------------------------------------------------------------#
# Variables:

# TN, TP, DOC, PAR, Temp

# checking normal distribution of TN
hist(dataset_phyto_pulse$mean_TN)
shapiro.test(dataset_phyto_pulse$mean_TN)
# p-value = 1.406e-05
# not possible to transform (2 peaks)

# checking normal distribution of TP
hist(dataset_phyto_pulse$mean_TP)
shapiro.test(dataset_phyto_pulse$mean_TP)
# p-value = 0.003156
dataset_phyto_pulse$mean_TP_log <- log(dataset_phyto_pulse$mean_TP)
hist(dataset_phyto_pulse$mean_TP_log)
shapiro.test(dataset_phyto_pulse$mean_TP_log)
# p-value = 0.1248

# checking normal distribution of DOC
hist(dataset_phyto_pulse$mean_DOC)
shapiro.test(dataset_phyto_pulse$mean_DOC)
# p-value = 0.002989
dataset_phyto_pulse$mean_DOC_sqrt <- sqrt(dataset_phyto_pulse$mean_DOC)
hist(dataset_phyto_pulse$mean_DOC_sqrt)
shapiro.test(dataset_phyto_pulse$mean_DOC_sqrt)
# p-value = 0.004453

# checking normal distribution of PAR
hist(dataset_phyto_pulse$mean_PAR)
shapiro.test(dataset_phyto_pulse$mean_PAR)
# p-value = 5.475e-07
dataset_phyto_pulse$mean_PAR_log <- log(dataset_phyto_pulse$mean_PAR)
hist(dataset_phyto_pulse$mean_PAR_log)
shapiro.test(dataset_phyto_pulse$mean_PAR_log)
# p-value = 0.1951

# checking normal distribution of Temperature 
hist(dataset_phyto_pulse$mean_Temp)
shapiro.test(dataset_phyto_pulse$mean_Temp)
# p-value = 3.626e-06
# not possible to transform
#---------------------------------------------------------------------------------------------------------#
# Chl a, Evenness, ENS, Richness, Zooplankton biomass

# checking normal distribution of Chl a
hist(dataset_phyto_pulse$mean_Chla)
shapiro.test(dataset_phyto_pulse$mean_Chla)
# p-value = 8.299e-09
dataset_phyto_pulse$mean_Chla_log <- log(dataset_phyto_pulse$mean_Chla)
hist(dataset_phyto_pulse$mean_Chla_log)
shapiro.test(dataset_phyto_pulse$mean_Chla_log)
# p-value = 0.2532

# checking normal distribution of evenness
hist(dataset_phyto_pulse$mean_J)
shapiro.test(dataset_phyto_pulse$mean_J)
# p-value = 0.1123

# checking normal distribution of ENS
hist(dataset_phyto_pulse$mean_ENS_D)
shapiro.test(dataset_phyto_pulse$mean_ENS_D)
# p-value = 0.04182

# checking normal distribution of richness
hist(dataset_phyto_pulse$mean_s)
shapiro.test(dataset_phyto_pulse$mean_s)
# p-value = 0.02638
dataset_phyto_pulse$mean_s_sqrt <- sqrt(dataset_phyto_pulse$mean_s)
hist(dataset_phyto_pulse$mean_s_sqrt)
shapiro.test(dataset_phyto_pulse$mean_s_sqrt)
# p-value = 0.02162

# checking normal distribution of mean zooplankton biomass
hist(dataset_phyto_pulse$mean_zoop_biomass)
shapiro.test(dataset_phyto_pulse$mean_zoop_biomass)
# p-value = 7.328e-06
dataset_phyto_pulse$mean_zoop_biomass_log <- log(dataset_phyto_pulse$mean_zoop_biomass)
hist(dataset_phyto_pulse$mean_zoop_biomass_log)
shapiro.test(dataset_phyto_pulse$mean_zoop_biomass_log)
#  p-value = 0.2326

#---------------------------------------------------------------------------------------------------------#
# Resistance, recovery, resilience

# checking normal distribution of resistance
hist(dataset_phyto_pulse$initial_stab)
shapiro.test(dataset_phyto_pulse$initial_stab)
# p-value = p-value = 0.0004821

# checking normal distribution of final recovery
hist(dataset_phyto_pulse$final_stab)
shapiro.test(dataset_phyto_pulse$final_stab)
# p-value = 0.01628

# checking normal distribution of final resilience
hist(dataset_phyto_pulse$rate_change_ort)
shapiro.test(dataset_phyto_pulse$rate_change_ort)
# p-value = 0.03346

#---------------------------------------------------------------------------------------------------------#
#### 1. resistance for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resistance 
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, evenness, richness, ENS, zooplankton biomass

# testing significance of all variables individually 
resistance <- lmer(initial_stab ~ mean_zoop_biomass_log + (1|Lake), data=dataset_phyto_pulse)
summary(resistance)
# Chl a p-value = 0.264
# TP p-value = 0.731 
# TN p-value = 0.640 
# DOC p-value = 0.279 
# PAR p-value = 0.0788 !
# Temp p-value = 0.0666  !
# evenness p-value = 0.0394 !
# richness p-value = 0.648 
# ENS p-value = 0.386 
# zoop biomass p-value = 0.0529 !


resistance <- lmer(initial_stab ~ mean_PAR_log + mean_J + (1|Lake), data=dataset_phyto_pulse)
summary(resistance)


r.squaredGLMM(resistance)
# R2m      R2c
# [1,] 0.3177943 0.431953

#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = final recovery 
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, evenness, richness, ENS, zooplankton biomass

# testing significance of all variables individually 
recovery <- lmer(final_stab ~ mean_zoop_biomass_log + (1|Lake), data=dataset_phyto_pulse)
summary(recovery)
# Chl a p-value =  0.0894 !
# TP p-value = 0.186 
# TN p-value = 0.283 
# DOC p-value = 0.622 
# PAR p-value = 0.02156 !
# Temp p-value = 3.96e-05 !
# evenness p-value = 0.508 
# richness p-value = 0.231 
# ENS p-value =  0.871 
# zoop biomass p-value = 0.00157 !


recovery <- lmer(final_stab ~ mean_Chla_log + mean_zoop_biomass_log + (1|Lake), data=dataset_phyto_pulse)
summary(recovery)

r.squaredGLMM(recovery)
# R2m       R2c
# [1,] 0.3808407 0.4176573


#---------------------------------------------------------------------------------------------------------#
#### 3. Resilience for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, evenness, richness, ENS, zooplankton biomass

# testing significance of all variables individually 
resilience <- lmer(rate_change_ort ~ mean_zoop_biomass_log + (1|Lake), data=dataset_phyto_pulse)
summary(resilience)
# Chl a p-value = 0.9286 -> singular fit
# TP p-value =  0.341 -> singular fit
# TN p-value = 0.202 -> singular fit
# DOC p-value =  0.215 -> singular fit
# PAR p-value = 0.677 -> singular fit
# Temp p-value = 0.856 -> singular fit

# -> no significant p-value! 

#---------------------------------------------------------------------------------------------------------#
#### 4. Biomass for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = Chl a 
# explanatory = TP, TN, Temp, PAR, DOC, mean zoopl biomass

Chla <- lmer(mean_Chla_log ~ mean_zoop_biomass_log + (1|Lake), data=dataset_phyto_pulse)
summary(Chla)
# TP p-value = 0.00284 !
# TN p-value = 0.00439 !
# Temp p-value= 0.313
# PAR p-value = 0.00283 !
# DOC p-value = 4.24e-05 !
# zoopl biomass p-value = 0.151 

Chla <- lmer(mean_Chla_log ~ mean_TP_log + mean_PAR_log + mean_DOC_sqrt + (1|Lake), data=dataset_phyto_pulse)
summary(Chla)
r.squaredGLMM(Chla)
# R2m       R2c
# [1,] 0.2850456 0.9722623

#---------------------------------------------------------------------------------------------------------#
#### 5. Evenness for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = Chl a, TP, TN, Temp, PAR, DOC, mean zoopl biomass

# testing significance of all variables individually 
evenness <- lmer(mean_J ~ mean_Chla + (1|Lake), data=dataset_phyto_pulse)
summary(evenness)
# Chl a p-value = 0.0295
# TP p-value = 0.70544 
# TN p-value =  0.672728
# Temp p-value = 0.0319  !
# PAR p-value = 0.0517 
# DOC p-value = 0.28594 
# zoopl biomass p-value = 0.0168 !

# --> Temp

# setting up the model fit 
evenness <- lmer(mean_J ~ mean_Temp + mean_zoopl_biomass_log + (1|Lake), data=dataset_phyto_pulse)
summary(evenness)

r.squaredGLMM(evenness)
#   R2m       R2c
# [1,] 0.2859262 0.5106154


#---------------------------------------------------------------------------------------------------------#
#### 6. ENS for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = Chl a, TP, TN, Temp, PAR, DOC, mean zoopl biomass

# testing significance of all variables individually 
ENS <- lmer(mean_ENS_D ~ mean_Chla + (1|Lake), data=dataset_phyto_pulse)
summary(ENS)
# Chl a p-value =  0.00558 !
# TP p-value = 0.07516
# TN p-value = 0.07326
# Temp p-value = 0.9815
# PAR p-value = 0.0134 !
# DOC p-value = 0.000107 !
# zoopl biomass p-value =  0.209784 

# --> Chl a, PAR, DOC

# setting up the model fit 
ENS <- lmer(mean_ENS_D ~  mean_PAR_log + mean_TN + (1|Lake), data=dataset_phyto_pulse)

summary(ENS)

r.squaredGLMM(ENS)
#  R2m       R2c
# [1,] 0.2617936 0.8055128


#---------------------------------------------------------------------------------------------------------#
#### 7. Richness for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = richness
# explanatory = Chl a, TP, TN, Temp, PAR, DOC, mean zoopl biomass

# testing significance of all variables individually 
richness <- lmer(mean_s_sqrt ~ mean_zoop_biomass_log + (1|Lake), data=dataset_phyto_pulse)
summary(richness)
# Chl a p-value = 0.010685 !
# TP p-value = 0.5867 
# TN p-value = 0.850122
# DOC p-value = 0.176782 
# PAR p-value = 0.949 
# Temp p-value = 0.467 
# zoop biomass p-value = 0.852 

richness <- lmer(mean_s_sqrt ~ mean_Chla_log + (1|Lake), data=dataset_phyto_pulse)
summary(richness)

r.squaredGLMM(richness)
#  R2m       R2c
# [1,] 0.07292423 0.8476864

#---------------------------------------------------------------------------------------------------------#
#### 8. SEM  ####
#---------------------------------------------------------------------------------------------------------#

## final individual models 
resistance <- lmer(initial_stab ~ mean_zoop_biomass_log + (1|Lake), data=dataset_phyto_pulse)
# recovery <- lmer(final_stab ~ mean_Chla_log + mean_zoop_biomass_log + (1|Lake), data=dataset_phyto_pulse)
Chla <- lmer(mean_Chla_log ~ mean_TP_log + mean_PAR_log + mean_DOC_sqrt + (1|Lake), data=dataset_phyto_pulse)
# evenness <- lmer(mean_J ~ mean_PAR_log + mean_Temp + mean_zoop_biomass_log + (1|Lake), data=dataset_phyto_pulse)
ENS <- lmer(mean_ENS_D ~  mean_PAR_log + mean_TN + mean_zoop_biomass_log + mean_DOC_sqrt + (1|Lake), data=dataset_phyto_pulse)
# richness <- lmer(mean_s_sqrt ~ mean_Chla_log + (1|Lake), data=dataset_phyto_pulse)


# SEM with piecewiseSEM version 2.0
modelList <- psem(
  resistance,
  Chla,
  ENS,
  dataset_phyto_pulse
)

results <- summary(modelList)
# excluding recovery and evenness because of singular fit 

results$IC$AIC
# 54.067

results$dTable

results$Cstat
# Fisher.C df P.Value
# 1   20.067 20   0.454

results$coefficients




