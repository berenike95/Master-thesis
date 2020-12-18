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
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/new_master_dataset.csv")

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for pulse disturbance only 
zoopl_pulse_press <- dataset_zoopl[which(dataset_zoopl$Treatment=='FS'), ]


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
hist(zoopl_pulse_press$mean_zoop_biomass)
shapiro.test(zoopl_pulse_press$mean_zoop_biomass)
# p-value = 3.378e-06
zoopl_pulse_press$mean_zoop_biomass_sqrt <- sqrt(zoopl_pulse_press$mean_zoop_biomass)
hist(zoopl_pulse_press$mean_zoop_biomass_sqrt)
shapiro.test(zoopl_pulse_press$mean_zoop_biomass_sqrt)
# p-value = 0.0282

# checking normal distribution of body size
hist(zoopl_pulse_press$initial_zoop_body_size)
shapiro.test(zoopl_pulse_press$initial_zoop_body_size)
# p-value = 0.0009254

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
# Resistance, final recovery

# checking normal distribution of resistance
hist(zoopl_pulse_press$initial_stab)
shapiro.test(zoopl_pulse_press$initial_stab)
# p-value = 0.01436

# checking normal distribution of final recovery
hist(zoopl_pulse_press$final_stab)
shapiro.test(zoopl_pulse_press$final_stab)
# p-value = 1.25e-05

# checking normal distribution of resilience
hist(zoopl_pulse_press$rate_change_ort)
shapiro.test(zoopl_pulse_press$rate_change_ort)
# p-value = 2.61e-07

#---------------------------------------------------------------------------------------------------------#
#### 1. resistance for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resistance 
# explanatory = TP, TN, DOC, PAR, Temp, biomass, evenness, ens, richness, Chl a, body size

# testing significance of all variables individually 
resistance <- lmer(initial_stab ~ mean_Chla_log + (1|Lake), data=zoopl_pulse_press)

summary(resistance)
# TP p-value = 0.558
# TN p-value = 0.996
# DOC p-value = 0.078 !
# PAR p-value = 0.647
# Temp p-value = 0.571
# biomass p-value = 0.00604 !
# body size p-value = 0.000985 !
# evenness p-value = 0.32308 
# richness p-value = 0.00399 !
# ENS p-value = 0.29964  
# Chl a p-value = 0.7322 

resistance <- lmer(initial_stab ~ mean_s + mean_zoop_biomass_sqrt + (1|Lake), data=zoopl_pulse_press)

summary(resistance)

r.squaredGLMM(resistance)
#  R2m       R2c
# [1,] 0.3517262 0.6428198

#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = recovery 
# explanatory = TP, TN, DOC, PAR, Temp, biomass, evenness, ens, richness, chl a, body size

# testing significance of all variables individually 
recovery <- lmer(final_stab ~ mean_Chla_log + (1|Lake), data=zoopl_pulse_press)

summary(recovery)
# TP p-value = 0.0431 ! -> singular fit
# TN p-value = 0.260
# DOC p-value = 0.00945 ! -> singular fit
# PAR p-value = 0.1557 
# Temp p-value = 0.659
# biomass p-value = 0.5390 
# body size p-value =  0.000848 !
# evenness p-value =  0.03582 !
# richness p-value = 0.3268 
# ENS p-value  0.03173 !
# Chl a p-value = 0.5354 

recovery <- lmer(final_stab ~ initial_zoop_body_size + mean_J + (1|Lake), data=zoopl_pulse_press)

summary(recovery)


r.squaredGLMM(resistance)
# R2m       R2c
# [1,] 0.3517262 0.6428198

#---------------------------------------------------------------------------------------------------------#
#### 3. resilience for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience 
# explanatory = TP, TN, DOC, PAR, Temp, biomass, evenness, ens, richness, chl a, body size

# testing significance of all variables individually 
resilience <- lmer(rate_change_ort ~ mean_s + (1|Lake), data=zoopl_pulse_press)

summary(resilience)
# Chl a p-value = 0.4632 
# TP p-value = 0.886
# TN p-value = 0.950
# DOC p-value = 0.652
# PAR p-value = 0.988
# Temp p-value = 0.772
# biomass p-value = 0.3549 -> singular fit 
# evenness p-value = 0.634
# ENS p-value = 0.511
# richness p-value =  0.424
# body size p-value = -4.853 2.27e-05 ! -> singular fit 

#---------------------------------------------------------------------------------------------------------#
#### 4. Biomass for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = mean zoopl biomass
# explanatory = TP, TN, DOC, PAR, Temp, chl a, body size

# testing significance of all variables individually 
biomass <- lmer(mean_zoop_biomass_sqrt ~ initial_zoop_body_size + (1|Lake), data=zoopl_pulse_press)

summary(biomass)
# Chl a p-value = 0.0179 !
# TP p-value =  0.0635  
# TN p-value = 7.56e-09 !
# DOC p-value = 0.5693   
# PAR p-value = 0.00202 !
# Temp p-value = 0.5080 
# body size p-value = 0.0188 !

# --> Chl a, TN, PAR

biomass <- lmer(mean_zoop_biomass_sqrt ~ mean_TN + mean_PAR_log + (1|Lake), data=zoopl_pulse_press)

summary(biomass)

r.squaredGLMM(biomass)
# R2m      R2c
# [1,] 0.4802795 0.984169

#---------------------------------------------------------------------------------------------------------#
#### 5. Initial body size for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = initial body size
# explanatory = TP, TN, DOC, PAR, Temp, chl a

# testing significance of all variables individually 
size <- lmer(initial_zoop_body_size ~ mean_Chla_log + (1|Lake), data=zoopl_pulse_press)

summary(size)
# Chl a p-value = 0.0245 ! -> singular fit 

r.squaredGLMM(size)

#---------------------------------------------------------------------------------------------------------#
#### 6. Evenness for zooplankton ####
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
#### 7. ENS for zooplankton ####
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
#### 8. Richness for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = richness
# explanatory = TP, TN, DOC, PAR, Temp, chl a, body size

richness <- lmer(mean_s ~ mean_PAR_log + (1|Lake), data=zoopl_pulse_press)
summary(richness)
# Chl a  0.001966 !
# TP 0.600
# TN 0.00292 !
# DOC 0.31323 
# Temp 0.79860 
# PAR 0.00639 !

richness <- lmer(mean_s ~ mean_PAR_log + mean_TN +(1|Lake), data=zoopl_pulse_press)
summary(richness)

r.squaredGLMM(richness)
# R2m       R2c
# [1,] 0.3024803 0.8915669

#---------------------------------------------------------------------------------------------------------#
#### 9. SEM####
#---------------------------------------------------------------------------------------------------------#
## final individual models 
resistance <- lmer(initial_stab ~ mean_s + mean_zoop_biomass_sqrt + (1|Lake), data=zoopl_pulse_press)
# recovery <- lmer(final_stab ~ mean_J + (1|Lake), data=zoopl_pulse_press)
biomass <- lmer(mean_zoop_biomass_sqrt ~ mean_TN + mean_PAR_log + (1|Lake), data=zoopl_pulse_press)
# size <- lmer(initial_zoop_body_size ~ mean_Chla_log + (1|Lake), data=zoopl_pulse_press)
richness <- lmer(mean_s ~ mean_PAR_log + mean_TN +(1|Lake), data=zoopl_pulse_press)


zoopl_pulse_press_complete <- na.omit(zoopl_pulse_press)

modelList <- psem(
  resistance,
  biomass,
  richness,
  mean_zoop_biomass_sqrt %~~% mean_s,
  data=zoopl_pulse_press_complete
)

results <- summary(modelList)
# excluding recovery and body size because of singular fit

# Akaike’s information criterion
results$IC$AIC
# 37.011

results$dTable

# Fisher’s C 
results$Cstat
#      Fisher.C df P.Value
#1    7.011  4   0.135

results$coefficients

rsquared(modelList)


