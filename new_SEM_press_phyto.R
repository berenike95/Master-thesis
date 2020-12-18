#---------------------------------------------------------------------------------------------------------#
#### SEM for press disturbance & phytoplankton####
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

# filtering data for press disturbance only 
dataset_phyto_press <- dataset_phyto[which(dataset_phyto$Treatment=='S'), ]

dataset_phyto_press = subset(dataset_phyto_press, select = -c(14:15))

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for press disturbance only 
dataset_zoopl_press <- dataset_zoopl[which(dataset_zoopl$Treatment=='S'), ]

# filtering for body size and biomass 
dataset_zoopl_press <- select(dataset_zoopl_press, Lake, Experiment, Treatment, Enclosure, mean_zoop_biomass)

# merging both datasets 
dataset_phyto_press <- merge(x=dataset_phyto_press, y=dataset_zoopl_press, by= c("Lake", "Experiment", 
                                                                                 "Treatment", "Enclosure"))
#---------------------------------------------------------------------------------------------------------#
#### 0. normal distribution and scaling of variables ####
#---------------------------------------------------------------------------------------------------------#
# Variables:

# TN, TP, DOC, PAR, Temp

# checking normal distribution of TP
hist(dataset_phyto_press$mean_TP)
shapiro.test(dataset_phyto_press$mean_TP)
# p-value = 0.005709

# checking normal distribution of TN  
hist(dataset_phyto_press$mean_TN)
shapiro.test(dataset_phyto_press$mean_TN)
# p-value = 5.19e-05

# checking normal distribution of DOC
hist(dataset_phyto_press$mean_DOC)
shapiro.test(dataset_phyto_press$mean_DOC)
# p-value = 0.0008227
dataset_phyto_press$mean_DOC_sqrt <- sqrt(dataset_phyto_press$mean_DOC)
hist(dataset_phyto_press$mean_DOC_sqrt)
shapiro.test(dataset_phyto_press$mean_DOC_sqrt)
#  p-value = 0.001198

# checking normal distribution of PAR
hist(dataset_phyto_press$mean_PAR)
shapiro.test(dataset_phyto_press$mean_PAR)
# p-value = 1.545e-06
dataset_phyto_press$mean_PAR_log <- log(dataset_phyto_press$mean_PAR)
hist(dataset_phyto_press$mean_PAR_log)
shapiro.test(dataset_phyto_press$mean_PAR_log)
# p-value = 0.00313

# checking normal distribution of Temp
hist(dataset_phyto_press$mean_Temp)
shapiro.test(dataset_phyto_press$mean_Temp)
# p-value = 3.777e-06

#---------------------------------------------------------------------------------------------------------#
# Chl a, Evenness, ENS, Richness, Zooplankton biomass

# checking normal distribution of Chl a 
hist(dataset_phyto_press$mean_Chla)
shapiro.test(dataset_phyto_press$mean_Chla)
# p-value = 1.67e-08
dataset_phyto_press$mean_Chla_log <- log(dataset_phyto_press$mean_Chla)
hist(dataset_phyto_press$mean_Chla_log)
shapiro.test(dataset_phyto_press$mean_Chla_log)
# p-value = 0.3784

# checking normal distribution of evenness
hist(dataset_phyto_press$mean_J)
shapiro.test(dataset_phyto_press$mean_J)
# p-value = 0.005483

# checking normal distribution of ENS
hist(dataset_phyto_press$mean_ENS_D)
shapiro.test(dataset_phyto_press$mean_ENS_D)
# p-value = 0.009258
dataset_phyto_press$mean_ENS_D_sqrt <- sqrt(dataset_phyto_press$mean_ENS_D)
hist(dataset_phyto_press$mean_ENS_D_sqrt)
shapiro.test(dataset_phyto_press$mean_ENS_D_sqrt)
# p-value = 0.05193

# checking normal distribution of richness
hist(dataset_phyto_press$mean_s)
shapiro.test(dataset_phyto_press$mean_s)
# p-value = 0.005884

# checking normal distribution of zooplankton biomass
hist(dataset_phyto_press$mean_zoop_biomass)
shapiro.test(dataset_phyto_press$mean_zoop_biomass)
# p-value = 0.0007597
dataset_phyto_press$mean_zoop_biomass_log <- log(dataset_phyto_press$mean_zoop_biomass)
hist(dataset_phyto_press$mean_zoop_biomass_log)
shapiro.test(dataset_phyto_press$mean_zoop_biomass_log)
# p-value = 0.04232

#---------------------------------------------------------------------------------------------------------#
# Final recovery, resilience

# checking normal distribution of final recovery
hist(dataset_phyto_press$final_stab)
shapiro.test(dataset_phyto_press$final_stab)
#  p-value = 0.1352

# checking normal distribution of resilience
hist(dataset_phyto_press$rate_change_oti)
shapiro.test(dataset_phyto_press$rate_change_oti)
# p-value = 2.28e-06

#---------------------------------------------------------------------------------------------------------#
#### 1. final recovery for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# final recovery = response
# TP, TN, DOC, Temp, PAR, Chl a, evenness, ENS, richness, zoopl biomass = explanatory

# testing significance of all variables individually 
final_recovery <- lmer(final_stab ~ mean_zoop_biomass_log + (1|Lake), data=dataset_phyto_press)

summary(final_recovery)
# TP p-value = 0.456 
# TN p-value =  0.610 
# DOC p-value =  0.741 
# Temp p-value = 0.272
# PAR p-value = 0.000856 !
# Chl a p-value = 0.00795  !
# evenness p-value = 0.00286 !
# ENS p-value = 0.167
# richness p-value = 0.3239  
# zoop biomass p-value =  0.181 

final_recovery <- lmer(final_stab ~ mean_J + (1|Lake), data=dataset_phyto_press)

summary(final_recovery)

r.squaredGLMM(final_recovery)
# R2m       R2c
# [1,] 0.08305317 0.7945533

#---------------------------------------------------------------------------------------------------------#
#### 2. resilience for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience
# TP, TN, DOC, Temp, PAR, Chl a, evenness, ENS, richness, zoopl biomass = explanatory

# testing significance of all variables individually 
resilience <- lmer(rate_change_oti ~ mean_zoop_biomass_log + (1|Lake),data=dataset_phyto_press)

summary(resilience)
# TP p-value = 0.2107 
# TN p-value = 0.814
# DOC p-value = 0.394
# Temp p-value = 0.002941 !
# PAR p-value = 0.06650 ! 
# Chl a p-value = 0.0207 ! but singular fit
# evenness p-value = 0.629 
# ENS p-value = 0.1147 
# richness p-value = 0.2447 
# zoopl biomass p-value = 0.434

# --> Temp, PAR 

resilience <- lmer(rate_change_oti ~ mean_PAR_log + (1|Lake),data=dataset_phyto_press)

summary(resilience)


r.squaredGLMM(resilience)
#           R2m       R2c
# [1,] 0.1174635 0.2824506

#---------------------------------------------------------------------------------------------------------#
#### 3. Biomass for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = Chl a as biomass proxy
# TP, TN, DOC, Temp, PAR, zoopl biomass = explanatory

Chla <- lmer(mean_Chla_log ~ mean_PAR_log + (1|Lake), data=dataset_phyto_press)
summary(Chla)
# TP p-value = 4.34e-06 !
# TN p-value = 0.000265 !
# DOC p-value = 0.000407 !
# Temp p-value =  0.555 
# PAR p-value = 2.56e-09 !
# zoopl biomass = 0.0595 !

Chla <- lmer(mean_Chla_log ~ mean_DOC_sqrt + mean_zoop_biomass_log + (1|Lake), data=dataset_phyto_press)
summary(Chla)

r.squaredGLMM(Chla)
# R2m       R2c
# [1,] 0.3611991 0.9904947

#---------------------------------------------------------------------------------------------------------#
#### 4. Evenness for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# TP, TN, DOC, Temp, PAR, Chl a, zoopl biomass = explanatory

# testing significance of all variables individually
evenness <- lmer(mean_J ~ mean_TP + (1|Lake), data=dataset_phyto_press)

summary(evenness)
# TP p-value = 0.0116  ! but singular fit
# TN p-value = 0.223537 
# TN/TP p-value = 0.426 
# DN/TP p-value =  0.117  
# DOC p-value = 0.985  
# Temp p-value = 0.118  
# PAR p-value = 0.0697 !
# Chl a p-value = 0.00141 ! but singular fit
# zoopl biomass p-value = 0.91  

evenness <- lmer(mean_J ~ mean_PAR_log + (1|Lake), data=dataset_phyto_press)
summary(evenness)
r.squaredGLMM(evenness)
# R2m       R2c
# [1,] 0.4114403 0.9527127

#---------------------------------------------------------------------------------------------------------#
#### 5. ENS for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# TP, TN, DOC, Temp, PAR, Chl a, zoopl biomass = explanatory

# testing significance of all variables individually
ENS <- lmer(mean_ENS_D ~ mean_TP + (1|Lake), data=dataset_phyto_press)

summary(ENS)
# TP p-value = 0.187 
# TN p-value = 0.878 
# DOC p-value = 1.06e-08 !
# Temp p-value = 0.7458  
# PAR p-value = 0.011191 !
# Chl a p-value = 0.00149 !
# zoopl biomass p-value = 0.106060  

ENS <- lmer(mean_ENS_D ~  mean_DOC_sqrt + mean_PAR_log + mean_Chla_log + (1|Lake), data=dataset_phyto_press)

summary(ENS)

r.squaredGLMM(ENS)
#     R2m       R2c
# [1,] 0.382522 0.9930729

#---------------------------------------------------------------------------------------------------------#
#### 6. Richness for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = richness
# TP, TN, DOC, Temp, PAR, Chl a, zoopl biomass = explanatory

# testing significance of all variables individually
richness <- lmer(mean_s ~ mean_zoop_biomass_log + (1|Lake), data=dataset_phyto_press)

summary(richness)
# TP p-value = 0.0246 !
# TN p-value = 0.0567 !
# DOC p-value = 0.440 
# Temp p-value =  0.5178   
# PAR p-value = 0.24004 
# Chl a p-value = 0.03494 !
# zoop biomass p-value =  0.42634 

richness <- lmer(mean_s ~ mean_TP + (1|Lake), data=dataset_phyto_press)

summary(richness)

r.squaredGLMM(richness)
# R2m       R2c
# [1,] 0.1723985 0.8482334

#---------------------------------------------------------------------------------------------------------#
#### 7. SEM ####
#---------------------------------------------------------------------------------------------------------##  

## final individual models 
final_recovery <- lmer(final_stab ~ mean_J + (1|Lake), data=dataset_phyto_press)
evenness <- lmer(mean_J ~ mean_PAR_log + mean_TP + (1|Lake), data=dataset_phyto_press)
resilience <- lmer(rate_change_oti ~ mean_PAR_log + (1|Lake),data=dataset_phyto_press)
Chla <- lmer(mean_Chla_log ~ mean_PAR_log + mean_TP + (1|Lake), data=dataset_phyto_press)
# ENS <- lmer(mean_ENS_D ~  mean_DOC_sqrt + mean_PAR_log + mean_Chla_log + (1|Lake), data=dataset_phyto_press)
richness <- lmer(mean_s ~ mean_TP +  (1|Lake), data=dataset_phyto_press)


dataset_phyto_press_complete <- na.omit(dataset_phyto_press)

modelList <- psem(
  final_recovery,
  resilience,
  evenness,
  Chla,
  richness,
  mean_Chla_log %~~% mean_J,
  dataset_phyto_press_complete
)


results <- summary(modelList)
# excluding ENS because of singular fit

rsquared(modelList)

results$IC$AIC
# 66.689

results$dTable

results$Cstat
#  Fisher.C df P.Value
# 1   22.689 24   0.538
results$coefficients








