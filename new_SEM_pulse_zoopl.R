#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse disturbance & zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# load packages

library(lmerTest)

library(MuMIn)

library(tidyverse)

library(dplyr)

library(piecewiseSEM)

library(faraway)

#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/new_master_dataset.csv")

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for pulse disturbance only 
dataset_zooplankton_pulse <- dataset_zoopl[which(dataset_zoopl$Treatment=='F'), ]

#---------------------------------------------------------------------------------------------------------#
#### 0. normal distribution and scaling of variables ####
#---------------------------------------------------------------------------------------------------------#
# Variables: TN, TP, DOC, PAR, Temp 

# checking normal distribution of TN
hist(dataset_zooplankton_pulse$mean_TN)
shapiro.test(dataset_zooplankton_pulse$mean_TN)
#  p-value = 1.406e-05

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
# p-value = 3.626e-06

#---------------------------------------------------------------------------------------------------------#
# Biomass, body size, Evenness, ENS, Richness, Chl a 

# checking normal distribution of biomass
hist(dataset_zooplankton_pulse$mean_zoop_biomass)
shapiro.test(dataset_zooplankton_pulse$mean_zoop_biomass)
# p-value = 7.328e-06
dataset_zooplankton_pulse$mean_zoop_biomass_log <- log(dataset_zooplankton_pulse$mean_zoop_biomass)
hist(dataset_zooplankton_pulse$mean_zoop_biomass_log)
shapiro.test(dataset_zooplankton_pulse$mean_zoop_biomass_log)
# p-value = 0.2326

# checking normal distribution of body size
hist(dataset_zooplankton_pulse$initial_zoop_body_size)
shapiro.test(dataset_zooplankton_pulse$initial_zoop_body_size)
# p-value = 1.455e-05
dataset_zooplankton_pulse$initial_zoop_body_size_log <- log(dataset_zooplankton_pulse$initial_zoop_body_size)
hist(dataset_zooplankton_pulse$initial_zoop_body_size_log)
shapiro.test(dataset_zooplankton_pulse$initial_zoop_body_size_log)
#  p-value = 0.002341

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

# checking normal distribution of Chl a 
hist(dataset_zooplankton_pulse$mean_Chla)
shapiro.test(dataset_zooplankton_pulse$mean_Chla)
# p-value = 8.299e-09
dataset_zooplankton_pulse$mean_Chla_log <- log(dataset_zooplankton_pulse$mean_Chla)
hist(dataset_zooplankton_pulse$mean_Chla_log)
shapiro.test(dataset_zooplankton_pulse$mean_Chla_log)
# p-value = 0.2532

#---------------------------------------------------------------------------------------------------------#
# Resistance, recovery, resilience

# checking normal distribution of resistance
hist(dataset_zooplankton_pulse$initial_stab)
shapiro.test(dataset_zooplankton_pulse$initial_stab)
# p-value = 4.078e-05

# checking normal distribution of final recovery
hist(dataset_zooplankton_pulse$final_stab)
shapiro.test(dataset_zooplankton_pulse$final_stab)
# p-value = 0.02508

# checking normal distribution of final recovery
hist(dataset_zooplankton_pulse$rate_change_ort)
shapiro.test(dataset_zooplankton_pulse$rate_change_ort)
# p-value = 0.8246

#---------------------------------------------------------------------------------------------------------#
#### 1. resistance for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resistance 
# explanatory = TP, TN, DOC, PAR, Temp, biomass, body size, evenness, ens, richness, chl a

# testing significance of all variables individually 
resistance <- lmer(initial_stab ~ initial_zoop_body_size_log + (1|Lake), data=dataset_zooplankton_pulse)

summary(resistance)
# TP p-value = 0.158
# TN p-value = 0.280
# DOC p-value =  0.0289 !
# PAR p-value = 0.768 
# Temp p-value = 0.574 
# biomass p-value = 0.023038 !
# body size p-value = 0.22783 
# evenness p-value = 0.000392 !
# ENS p-value = 0.012358  !
# richness p-value = 0.008368 !
# Chl a p-value = 0.4984  

test <- lm(initial_stab ~ mean_DOC + mean_zoop_biomass_log + mean_J + mean_s,data=dataset_zooplankton_pulse)

vif(test)
# excluding mean_ENS

resistance <- lmer(initial_stab ~ mean_DOC + mean_zoop_biomass_log + (1|Lake), data=dataset_zooplankton_pulse)
summary(resistance)
# maybe evenness, ENS or richness? 

r.squaredGLMM(resistance)
# R2m       R2c
# [1,] 0.3770805 0.4540666

#---------------------------------------------------------------------------------------------------------#
#### 2. recovery for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = recovery 
# explanatory = TP, TN, DOC, PAR, Temp, biomass, body size , evenness, ens, richness, chl a

# testing significance of all variables individually 
recovery <- lmer(final_stab ~ mean_Chla_log + (1|Lake), data=dataset_zooplankton_pulse)

summary(recovery)
# TP p-value = 0.0152 !
# TN p-value = 0.0317 !
# DOC p-value = 0.0314 !
# PAR p-value = 0.793 
# Temp p-value = 0.000937 !
# biomass p-value = 0.2007  
# body size p-value = 0.3936  
# evenness p-value = 0.268  
# ENS p-value = 0.1330  
# richness p-value =  0.566 
# Chl a p-value = 0.590 

recovery <- lmer(final_stab ~ mean_TP_log + mean_Temp + (1|Lake), data=dataset_zooplankton_pulse)
summary(recovery)

r.squaredGLMM(recovery)
#  R2m       R2c
# [1,] 0.3735539 0.5639275


#---------------------------------------------------------------------------------------------------------#
#### 3. resilience for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience
# explanatory = TP, TN, DOC, PAR, Temp, biomass, evenness, ens, richness, chl a, body size

# testing significance of all variables individually 
resilience <- lmer(rate_change_ort ~ mean_Temp + (1|Lake), data=dataset_zooplankton_pulse)

summary(resilience)
# TP p-value = 0.498
# TN p-value = 0.609
# DOC p-value = 0.788
# PAR p-value = 0.388 
# Temp p-value = 0.00452 !
# biomass p-value = 0.676
# evenness p-value =  0.480
# richness p-value = 0.2211 
# ENS p-value =  0.694
# body size p-value = 0.854

r.squaredGLMM(resilience)
# R2m       R2c
# [1,] 0.2013108 0.3236604

#---------------------------------------------------------------------------------------------------------#
#### 4. Biomass for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = mean zoopl biomass
# explanatory = TP, TN, DOC, PAR, Temp, chl a, body size

# testing significance of all variables individually 
biomass <- lmer(mean_zoop_biomass_log ~ initial_zoop_body_size_log + (1|Lake), data=dataset_zooplankton_pulse)
summary(biomass)
# TP p-value = 0.173 
# TN p-value = 0.0712 !
# DOC p-value = 0.58048   
# PAR p-value = 0.0371 !
# Temp p-value = 0.00506 !
# Chl a p-value = 0.131076  
# body size p-value = 0.0841 !


biomass <- lmer(mean_zoop_biomass_log ~ mean_TN + mean_Temp + (1|Lake), data=dataset_zooplankton_pulse)
summary(biomass)

r.squaredGLMM(biomass)
# R2m       R2c
# [1,] 0.4169407 0.8901657


#---------------------------------------------------------------------------------------------------------#
#### 5. Evenness for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = TP, TN, DOC, PAR, Temp, chl a, body size

# testing significance of all variables individually 
evenness <- lmer(mean_J ~ mean_TN + (1|Lake), data=dataset_zooplankton_pulse)

summary(evenness)
# TP p-value = 0.069240 !
# TN p-value = 0.5618  
# DOC p-value = 0.04962 !
# PAR p-value = 0.0623 
# Temp p-value = 0.00127 !
# Chl a p-value = 0.616283 
# body size p-value =  0.0562 !

evenness <- lmer(mean_J ~ mean_TP_log + mean_DOC + mean_Temp + (1|Lake), data=dataset_zooplankton_pulse)
summary(evenness)

r.squaredGLMM(evenness)
# R2m       R2c
# [1,] 0.1757529 0.8037823


#---------------------------------------------------------------------------------------------------------#
#### 6. Richness for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = richness
# explanatory = TP, TN, DOC, PAR, Temp, chl a, body size

# testing significance of all variables individually 
richness <- lmer(mean_s ~ initial_zoop_body_size_log + (1|Lake), data=dataset_zooplankton_pulse)

summary(richness)
# TP p-value = 0.198 
# TN p-value = 0.0391 !
# DOC p-value = 0.154630  
# PAR p-value =  0.00301 !
# Temp p-value =  0.59  
# Chl a p-value =  0.002321 !
# body size p-value = 0.200797 

richness <- lmer(mean_s ~ mean_PAR_log + mean_Chla_log + (1|Lake), data=dataset_zooplankton_pulse)
summary(richness)

r.squaredGLMM(richness)
# R2m       R2c
# [1,] 0.204301 0.8690829

#---------------------------------------------------------------------------------------------------------#
#### 7. ENS for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = TP, TN, DOC, PAR, Temp, chl a, body size

# testing significance of all variables individually 
ENS <- lmer(mean_ENS_D ~ mean_TP_log + (1|Lake), data=dataset_zooplankton_pulse)

summary(ENS)
# TP p-value =  0.10445 
# TN p-value = 0.40502 
# DOC p-value =  0.02700 !
# PAR p-value = 0.00887 !
# Temp p-value =  0.161 
# Chl a p-value = 0.445973
# body size p-value =  0.405 


ENS <- lmer(mean_ENS_D ~ mean_DOC + mean_PAR_log + (1|Lake), data=dataset_zooplankton_pulse)

summary(ENS)

r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.29634 0.6494586


#---------------------------------------------------------------------------------------------------------#
#### 8. SEM ####
#---------------------------------------------------------------------------------------------------------### 

## final individual models 
resistance <- lmer(initial_stab ~ mean_J +  (1|Lake), data=dataset_zooplankton_pulse)
recovery <- lmer(final_stab ~ mean_Temp + (1|Lake), data=dataset_zooplankton_pulse)
resilience <- lmer(rate_change_ort ~ mean_Temp + mean_J + (1|Lake), data=dataset_zooplankton_pulse)
biomass <- lmer(mean_zoop_biomass_log ~ mean_TN + mean_Temp + (1|Lake), data=dataset_zooplankton_pulse)
evenness <- lmer(mean_J ~ mean_Temp + mean_PAR_log + mean_TP_log + (1|Lake), data=dataset_zooplankton_pulse)
richness <- lmer(mean_s ~ mean_PAR_log + mean_Chla_log + (1|Lake), data=dataset_zooplankton_pulse)
# ENS <- lmer(mean_ENS_D ~ mean_DOC + mean_PAR_log + (1|Lake), data=dataset_zooplankton_pulse)


modelList <- psem(
  resistance,
  recovery,
  resilience,
  biomass,
  evenness,
  richness,
  mean_J %~~% mean_zoop_biomass_log,
  mean_s  %~~% mean_J,
  mean_zoop_biomass_log  %~~% mean_s,
  initial_stab %~~% mean_s,
  rate_change_ort %~~% final_stab,
  initial_stab %~~% rate_change_ort,
  dataset_zooplankton_pulse
)

# excluding recovery and ENS due to singular fit

results <- summary(modelList)

results$IC$AIC
# 118.87

results$dTable

# Fisher’s C 
results$Cstat
#   Fisher.C df P.Value
# 1    60.87 56   0.305

results$coefficients

rsquared(modelList)






