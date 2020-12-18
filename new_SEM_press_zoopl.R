#---------------------------------------------------------------------------------------------------------#
#### SEM for press disturbance & zooplankton ####
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
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for press disturbance only 
dataset_zoopl_press <- dataset_zoopl[which(dataset_zoopl$Treatment=='S'), ]

#---------------------------------------------------------------------------------------------------------#
#### 0. normal distribution and scaling of variables ####
#---------------------------------------------------------------------------------------------------------#
# Variables:

# TN, TP, DOC, PAR, Temp, Chl a 

# checking normal distribution of TP
hist(dataset_zoopl_press$mean_TP)
shapiro.test(dataset_zoopl_press$mean_TP)
#  p-value = 0.005709
dataset_zoopl_press$mean_TP_sqrt <- sqrt(dataset_zoopl_press$mean_TP)
hist(dataset_zoopl_press$mean_TP_sqrt)
shapiro.test(dataset_zoopl_press$mean_TP_sqrt)
# p-value = 0.08518

# checking normal distribution of TN  
hist(dataset_zoopl_press$mean_TN)
shapiro.test(dataset_zoopl_press$mean_TN)
# p-value = 5.19e-05

# checking normal distribution of DOC
hist(dataset_zoopl_press$mean_DOC)
shapiro.test(dataset_zoopl_press$mean_DOC)
# p-value = 0.0008227


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


# checking normal distribution of Chl a 
hist(dataset_zoopl_press$mean_Chla)
shapiro.test(dataset_zoopl_press$mean_Chla)
# p-value = 1.67e-08
dataset_zoopl_press$mean_Chla_log <- log(dataset_zoopl_press$mean_Chla)
hist(dataset_zoopl_press$mean_Chla_log)
shapiro.test(dataset_zoopl_press$mean_Chla_log)
# p-value = 0.3784

#---------------------------------------------------------------------------------------------------------#
# Biomass, Evenness, ENS, Richness

# checking normal distribution of biomass
hist(dataset_zoopl_press$mean_zoop_biomass)
shapiro.test(dataset_zoopl_press$mean_zoop_biomass)
# p-value = 0.0007597
dataset_zoopl_press$mean_zoop_biomass_log <- log(dataset_zoopl_press$mean_zoop_biomass)
hist(dataset_zoopl_press$mean_zoop_biomass_log)
shapiro.test(dataset_zoopl_press$mean_zoop_biomass_log)
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
#---------------------------------------------------------------------------------------------------------#
# final recovery, resilience

# checking normal distribution of final recovery
hist(dataset_zoopl_press$final_stab)
shapiro.test(dataset_zoopl_press$final_stab)
# p-value = 0.3266

# checking normal distribution of resilience
hist(dataset_zoopl_press$rate_change_oti)
shapiro.test(dataset_zoopl_press$rate_change_oti)
# p-value = 0.000104

#---------------------------------------------------------------------------------------------------------#
#### 1. final recovery for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = recovery 
# explanatory = TP, TN, DOC, PAR, Temp, biomass, evenness, ens, richness, chl a

# testing significance of all variables individually 
recovery <- lmer(final_stab ~ mean_s + (1|Lake), data=dataset_zoopl_press)

summary(recovery)
# TP p-value = 0.112 -> singular fit
# TN p-value = 0.0202 ! -> singular fit
# DOC p-value = 0.118 -> singular fit
# PAR p-value = 0.1038  
# Temp p-value =  0.236
# Chl a p-value = 0.378 
# evenness p-value =  0.336
# biomass p-value = 0.788
# ENS p-value = 0.626
# richness p-value =  0.964

#---------------------------------------------------------------------------------------------------------#
#### 2. resilience for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience 
# explanatory = TP, TN, DOC, PAR, Temp, biomass, evenness, ens, richness, chl a

# testing significance of all variables individually 
resilience <- lmer(rate_change_oti ~ mean_J + (1|Lake), data=dataset_zoopl_press)
summary(resilience)
# TP p-value = 0.330
# TN p-value = 0.954
# DOC p-value =  0.200
# PAR p-value = 0.0227 ! -> singular fit 
# Temp p-value = 0.628
# biomass p-value = 0.08992 !
# evenness p-value = 0.03911 !-> singular fit 
# richness p-value =  0.02463 ! -> singular fit
# ENS p-value = 0.077204 ! -> singular fit
# Chl a p-value = 0.807 
# body size p-value = 0.048025 !

resilience <- lmer(rate_change_oti ~ mean_zoop_biomass_log + (1|Lake), data=dataset_zoopl_press)
summary(resilience)

r.squaredGLMM(resilience)

#---------------------------------------------------------------------------------------------------------#
#### 3. Biomass for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = mean zoopl biomass
# explanatory = TP, TN, DOC, PAR, Temp, chl a

# testing significance of all variables individually 
biomass <- lmer(mean_zoop_biomass_log ~ mean_Chla_log + (1|Lake), data=dataset_zoopl_press)

summary(biomass)
# TP p-value = 0.069  
# TN p-value =  0.0532  
# DOC p-value = 0.57  
# PAR p-value = 0.0318 !
# Temp p-value = 0.00961 !
# Chl a p-value = 0.00781 !

# --> PAR, Temp, Chl a 

# excluding mean_PAR_log
biomass <- lmer(mean_zoop_biomass_log ~ mean_Temp + mean_Chla_log + (1|Lake), data=dataset_zoopl_press)

summary(biomass)

r.squaredGLMM(biomass)
# R2m       R2c
# [1,] 0.4258296 0.5741361


#---------------------------------------------------------------------------------------------------------#
#### 3. Evenness for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = TP, TN, DOC, PAR, Temp,chl a

# testing significance of all variables individually 
evenness <- lmer(mean_J ~ mean_TP_sqrt + (1|Lake), data=dataset_zoopl_press)

summary(evenness)
# TP p-value = 0.114 
# TN p-value = 0.342270 
# DOC p-value = 0.043304 !
# PAR p-value =  0.255   
# Temp p-value = 0.07669  
# Chl a p-value = 0.632821 

evenness <- lmer(mean_J ~ mean_DOC + mean_Temp + (1|Lake), data=dataset_zoopl_press)
summary(evenness)
r.squaredGLMM(evenness)
# R2m       R2c
# [1,] 0.3818884 0.5133085

#---------------------------------------------------------------------------------------------------------#
#### 4. ENS for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = TP, TN, DOC, PAR, Temp, chl a

# testing significance of all variables individually 
ENS <- lmer(mean_ENS_D ~ mean_Chla_log + (1|Lake), data=dataset_zoopl_press)

summary(ENS)
# TP p-value = 0.6377 
# TN p-value = 0.55486  
# DOC p-value = 0.05769  
# PAR p-value = 0.582156  
# Temp p-value = 0.0571 
# Chl a p-value = 0.181389 


ENS <- lmer(mean_ENS_D ~ mean_Temp + mean_DOC + (1|Lake), data=dataset_zoopl_press)

summary(ENS)

r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.4509424 0.5801275

#---------------------------------------------------------------------------------------------------------#
#### 5. Richness for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = richness
# explanatory = TP, TN, DOC, PAR, Temp, chl a

# testing significance of all variables individually 
richness <- lmer(mean_s ~ mean_Chla_log + (1|Lake), data=dataset_zoopl_press)

summary(richness)
# TP p-value = 0.2130 
# TN p-value = 0.1066 
# DOC p-value = 0.88761 
# PAR p-value = 0.41  
# Temp p-value = 0.1070 
# Chl a p-value = 0.00526 !

r.squaredGLMM(richness)
# R2m      R2c
# [1,] 0.3626503 0.728659

#---------------------------------------------------------------------------------------------------------#
#### 6. SEM  ####
#---------------------------------------------------------------------------------------------------------#

## final individual models 
# recovery <- lmer(final_stab ~ mean_s + (1|Lake), data=dataset_zoopl_press)
biomass <- lmer(mean_zoop_biomass_log ~ mean_Temp + mean_Chla_log + (1|Lake), data=dataset_zoopl_press)
resilience <- lmer(rate_change_oti ~ mean_zoop_biomass_log + (1|Lake), data=dataset_zoopl_press)
# evenness <- lmer(mean_J ~ mean_DOC + mean_Temp + (1|Lake), data=dataset_zoopl_press)
ENS <- lmer(mean_ENS_D ~ mean_Temp + mean_DOC + (1|Lake), data=dataset_zoopl_press)
richness <- lmer(mean_s ~ mean_Chla_log + (1|Lake), data=dataset_zoopl_press)

# omit NA observations
dataset_zoopl_press_complete <- na.omit(dataset_zoopl_press)

modelList <- psem(
  biomass,
  ENS,
  richness,
mean_s %~~% mean_zoop_biomass_log,
mean_s %~~% mean_ENS_D,
  dataset_zoopl_press_complete
)

results <- summary(modelList)
# excluding recovery, resilience and evenness because of singular fit

rsquared(modelList)

# Akaike’s information criterion
results$IC$AIC
# 39.369

results$dTable

# Fisher’s C 
results$Cstat
#     Fisher.C df P.Value
# 1   11.369 10    0.33

results$coefficients


