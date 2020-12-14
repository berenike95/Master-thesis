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
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/master_dataset3.csv")

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
#---------------------------------------------------------------------------------------------------------#
# final recovery, resilience, area under the curve


# checking normal distribution of final recovery
hist(dataset_zoopl_press$final_stab)
shapiro.test(dataset_zoopl_press$final_stab)
# p-value = 0.0008583
dataset_zoopl_press$final_stab_sqrt <- sqrt(dataset_zoopl_press$final_stab*-1)
hist(dataset_zoopl_press$final_stab_sqrt)
shapiro.test(dataset_zoopl_press$final_stab_sqrt)
# p-value = 0.4505
                                            
# checking normal distribution of resilience
hist(dataset_zoopl_press$rate_change_oti)
shapiro.test(dataset_zoopl_press$rate_change_oti)
# p-value = 0.000104
dataset_zoopl_press$rate_change_oti_sqrt <- sqrt(dataset_zoopl_press$rate_change_oti*-1)
hist(dataset_zoopl_press$rate_change_oti_sqrt)
shapiro.test(dataset_zoopl_press$rate_change_oti_sqrt)
# p-value = 0.5277

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
# explanatory = TP, TN, DOC, PAR, Temp, biomass, evenness, ens, richness, chl a, body size

# testing significance of all variables individually 
recovery <- lmer(final_stab_sqrt ~ initial_zoop_body_size + (1|Lake), data=dataset_zoopl_press)

summary(recovery)
# TP p-value = 0.24609 -> singular fit
# TN p-value =  0.3836 -> singular fit
# DOC p-value =  0.16892  -> singular fit
# PAR p-value = 0.142  -> singular fit
# Temp p-value =  0.745 
# biomass p-value = 0.562 
# evenness p-value = 0.267 -> singular fit
# ENS p-value = 0.418 -> singular fit
# richness p-value =  0.405 
# Chl a p-value = 0.7216  
# body size p-value = 0.50869  

?isSingular

#---------------------------------------------------------------------------------------------------------#
#### 2. resilience for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience 
# explanatory = TP, TN, DOC, PAR, Temp, biomass, evenness, ens, richness, chl a, body size

# testing significance of all variables individually 
resilience <- lmer(rate_change_oti_sqrt ~ mean_Chla_log + (1|Lake), data=dataset_zoopl_press)
summary(resilience)
# TP p-value = 0.3516 
# TN p-value = 0.7756 
# DOC p-value = 0.104559 -> singular fit
# PAR p-value = 0.0207 -> singular fit
# Temp p-value = 0.818 
# biomass p-value = 0.20366  
# evenness p-value = 0.087994 -> singular fit
# ENS p-value = 0.177 -> singular fit
# richness p-value = 0.0866 -> singular fit
# Chl a p-value =  0.871  

# --> nothing significant (also without random effect)

#---------------------------------------------------------------------------------------------------------#
#### 3. AUC for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC 
# explanatory = TP, TN, DOC, PAR, Temp, biomass, evenness, ens, richness, chl a, body size

# testing significance of all variables individually 
AUC <- lmer(total_impact_log ~ mean_Chla_log + (1|Lake), data=dataset_zoopl_press)
summary(AUC)
# Chl a p-value = 0.612   -> singular fit
# TP p-value =  0.639 -> singular fit
# TN p-value = 0.342  -> singular fit
# DOC p-value = 0.896  -> singular fit
# PAR p-value = 0.591  -> singular fit
# Temp p-value = 0.5617  -> singular fit
# biomass p-value = 0.00953 ! -> singular fit
# evenness p-value =  0.533 
# richness p-value = 0.119    
# ENS p-value =  0.83   

# --> all singular fit

#---------------------------------------------------------------------------------------------------------#
#### 4. Biomass for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = mean zoopl biomass
# explanatory = TP, TN, DOC, PAR, Temp, chl a

# testing significance of all variables individually 
biomass <- lmer(mean_biomass_log ~ mean_Chla_log + (1|Lake), data=dataset_zoopl_press)

summary(biomass)
# TP p-value = 0.069  
# TN p-value =  0.0532  
# DOC p-value = 0.57  
# PAR p-value = 0.0318 !
# Temp p-value = 0.00961 !
# Chl a p-value = 0.00781 !

# --> PAR, Temp, Chl a 

# excluding mean_PAR_log
biomass <- lmer(mean_biomass_log ~ mean_Temp + mean_Chla_log + (1|Lake), data=dataset_zoopl_press)

summary(biomass)

r.squaredGLMM(biomass)
# R2m       R2c
# [1,] 0.4258296 0.5741361

#---------------------------------------------------------------------------------------------------------#
#### 5. Evenness for zooplankton ####
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
#### 6. ENS for zooplankton ####
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
#### 8. SEM  ####
#---------------------------------------------------------------------------------------------------------#

## final individual models 
biomass <- lmer(mean_biomass_log ~ mean_Temp + mean_Chla_log + (1|Lake), data=dataset_zoopl_press)
evenness <- lmer(mean_J ~ mean_DOC + mean_Temp + (1|Lake), data=dataset_zoopl_press)
ENS <- lmer(mean_ENS_D ~ mean_Temp + mean_DOC + (1|Lake), data=dataset_zoopl_press)

# omit NA observations
dataset_zoopl_press_complete <- na.omit(dataset_zoopl_press)

modelList <- psem(
  biomass,
  ENS,
  dataset_zoopl_press_complete
)

results <- summary(modelList)
# excluding evenness because of singular fit 

rsquared(modelList)

# Akaike’s information criterion
results$IC$AIC
# 26.137

results$dTable

# Fisher’s C 
results$Cstat
#     Fisher.C df P.Value
# 1    6.137  6   0.408

results$coefficients


