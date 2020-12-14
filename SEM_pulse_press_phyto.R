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
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/master_dataset3.csv")

# filtering dataset for zooplankton data only 
dataset_phyto <- dataset_final[which(dataset_final$community=='phyto'), ]

# filtering data for pulse disturbance only 
phyto_pulse_press <- dataset_phyto[which(dataset_phyto$Treatment=='FS'), ]

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for press disturbance only 
dataset_zoopl_pulse <- dataset_zoopl[which(dataset_zoopl$Treatment=='FS'), ]

# filtering for body size and biomass 
dataset_zoopl_pulse <- select(dataset_zoopl_pulse, Lake, Experiment, Treatment, Enclosure, 
                              initial_zoop_body_size, mean_biomass)

# renaming columns
names(dataset_zoopl_pulse)[names(dataset_zoopl_pulse)=="mean_biomass"] <- "mean_zoopl_biomass"
names(dataset_zoopl_pulse)[names(dataset_zoopl_pulse)=="initial_zoop_body_size"] <- "zoopl_body_size"

# merging both datasets 
phyto_pulse_press <- merge(x=phyto_pulse_press, y=dataset_zoopl_pulse, by= c("Lake", "Experiment", 
                                                                                 "Treatment", "Enclosure"))

phyto_pulse_press = subset(phyto_pulse_press, select = -16)

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
hist(phyto_pulse_press$mean_zoopl_biomass)
shapiro.test(phyto_pulse_press$mean_zoopl_biomass)
# p-value = 3.378e-06
phyto_pulse_press$mean_zoopl_biomass_sqrt <- sqrt(phyto_pulse_press$mean_zoopl_biomass)
hist(phyto_pulse_press$mean_zoopl_biomass_sqrt)
shapiro.test(phyto_pulse_press$mean_zoopl_biomass_sqrt)
# p-value = 0.0282

#---------------------------------------------------------------------------------------------------------#
# Resistance, final reovery, resilience, area under the curve

# checking normal distribution of zooplankton resistance 
hist(phyto_pulse_press$initial_stab)
shapiro.test(phyto_pulse_press$initial_stab)
# p-value = 0.3974

# checking normal distribution of zooplankton resistance 
hist(phyto_pulse_press$final_stab)
shapiro.test(phyto_pulse_press$final_stab)
# p-value = 0.001492
phyto_pulse_press$final_stab_sqrt <- sqrt(phyto_pulse_press$final_stab*-1)
hist(phyto_pulse_press$final_stab_sqrt)
shapiro.test(phyto_pulse_press$final_stab_sqrt)
# p-value = 0.06107

# checking normal distribution of resilience 
hist(phyto_pulse_press$rate_change_ort)
shapiro.test(phyto_pulse_press$rate_change_ort)
# p-value = 0.1132

# checking normal distribution of AUC 
hist(phyto_pulse_press$total_impact)
shapiro.test(phyto_pulse_press$total_impact)
# p-value = 0.04629

#---------------------------------------------------------------------------------------------------------#
#### 1. resistance for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resistance 
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, evenness, richness, ENS, zooplankton biomass

# testing significance of all variables individually 
resistance <- lmer(initial_stab ~ mean_ENS_D_log + (1|Lake), data=phyto_pulse_press)

summary(resistance)
# Chl a p-value = 0.1725
# TP p-value = 0.932 
# TN p-value = 0.3424  
# DOC p-value = 0.774 
# PAR p-value = 0.00207 !
# Temp p-value =  0.613 
# evennes p-value = 0.5423  
# richness p-value = 0.7495 
# ENS p-value = 0.6690 
# zoopl biomass p-value = 0.12120  

resistance <- lmer(initial_stab ~ mean_PAR + (1|Lake), data=phyto_pulse_press)

summary(resistance)

r.squaredGLMM(resistance)
#  R2m      R2c
# [1,] 0.1780378 0.647813

#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = recovery 
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, evenness, richness, ENS, zooplankton biomass

# testing significance of all variables individually 
recovery <- lmer(final_stab ~ mean_J + (1|Lake), data=phyto_pulse_press)

summary(recovery)
# Chl a p-value = 0.2743
# TP p-value = 0.693 
# TN p-value = 0.1872  
# DOC p-value = 0.927 
# PAR p-value = 0.7471   
# Temp p-value = 0.726 
# evenness p-value = 0.0964  
# richness p-value =  0.9180 
# ENS p-value = 0.2745  
# zoopl biomass p-value = 0.9916   

# no significant variable

#---------------------------------------------------------------------------------------------------------#
#### 3. AUC for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC 
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, evenness, richness, ENS, zooplankton biomass

# testing significance of all variables individually 
AUC <- lmer(total_impact ~ mean_PAR + (1|Lake), data=phyto_pulse_press)

summary(AUC)
# Chl a p-value = 0.08800  
# TP p-value = 0.0957 
# TN p-value = 1.31e-05 --> singular fit
# DOC p-value = 0.775 
# PAR p-value = 0.02833 !
# Temp p-value = 0.06655 
# evenness p-value =  0.1392   
# richness p-value = 0.5779  
# ENS p-value = 0.31411  
# zoopl biomass p-value = 0.532902  

AUC <- lmer(total_impact ~ mean_PAR + (1|Lake), data=phyto_pulse_press)

summary(AUC)

r.squaredGLMM(AUC)
#  R2m       R2c
# [1,] 0.1059402 0.5480618

#---------------------------------------------------------------------------------------------------------#
#### 4. resilience for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience 
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, evenness, richness, ENS, zooplankton biomass

# testing significance of all variables individually 
resilience <- lmer(rate_change_ort ~ mean_TP + (1|Lake), data=phyto_pulse_press)

summary(resilience)
# Chl a p-value = 0.715 
# TP p-value = 0.730 
# TN p-value = 0.799 
# DOC p-value = 0.617 
# PAR p-value = 0.369 
# Temp p-value =  0.904 
# evenness p-value =  0.20 
# richness p-value = 0.573 
# ENS p-value =  0.554 
# zoopl biomass p-value = 0.381 

#---------------------------------------------------------------------------------------------------------#
#### 5. Chl a for phytoplankton  ####
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

#---------------------------------------------------------------------------------------------------------#
#### 6. Evenness for phytoplankton ####
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
#### 7. ENS for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = Chl a, TP, TN, DOC, PAR, Temp, zooplankton biomass

# testing significance of all variables individually 
ENS <- lmer(mean_ENS_D_log ~ mean_TP + (1|Lake), data=phyto_pulse_press)

summary(ENS)
# Chl a p-value =  0.004853 !
# TP p-value = 0.6583 
# TN p-value = 0.469606
# DOC p-value = 9.74e-05  !
# PAR p-value =  0.041393 !
# Temp p-value = 0.0892  
# zoopl biomass p-value = 0.38481  

# --> Chl a, DN/TP ratio, DOC, PAR 

test <- lm(mean_ENS_D_log ~ mean_Chla_log + mean_DOC + mean_PAR, data=phyto_pulse_press)
vif(test)
#  mean_Chla_log        mean_DN_TP_ratio_100        mean_DOC_sqrt         mean_PAR_100 
#  1.279871             2.465052                    2.277862             2.457407

ENS <- lmer(mean_ENS_D_log ~ mean_DOC + mean_TP_log + (1|Lake), data=phyto_pulse_press)
summary(ENS)

r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.4172463 0.9793816

#---------------------------------------------------------------------------------------------------------#
#### 8. SEM  ####
#---------------------------------------------------------------------------------------------------------#
## final individual models 
resistance <- lmer(initial_stab ~ mean_PAR_log + (1|Lake), data=phyto_pulse_press)
AUC <- lmer(total_impact ~ mean_PAR_log + (1|Lake), data=phyto_pulse_press)
ENS <- lmer(mean_ENS_D_log ~ mean_zoopl_biomass_sqrt + mean_PAR_log + mean_DOC + (1|Lake), data=phyto_pulse_press)
Chla <- lmer(mean_Chla_log ~ mean_TP_log + mean_DOC + mean_PAR_log + (1|Lake), data=phyto_pulse_press)
recovery <- lmer(final_stab ~ mean_J + (1|Lake), data=phyto_pulse_press)
AUC <- lmer(total_impact ~ mean_PAR + (1|Lake), data=phyto_pulse_press)
evenness <- lmer(mean_J ~  mean_zoopl_biomass_sqrt + mean_Chla_log + (1|Lake), data=phyto_pulse_press)

# omit NA observations
phyto_pulse_press_complete <- na.omit(phyto_pulse_press)

modelList <- psem(
  resistance,
  ENS,
  Chla,
  recovery,
  evenness,
  mean_ENS_D_log %~~% mean_Chla_log,
  mean_ENS_D_log %~~% mean_J,
  data=phyto_pulse_press_complete
)

results <- summary(modelList)

rsquared(modelList)

# Akaike’s information criterion
results$IC$AIC
# 72.721

results$dTable

# Fisher’s C 
results$Cstat
#       Fisher.C df P.Value
# 1   22.721 36   0.958

results$coefficients


