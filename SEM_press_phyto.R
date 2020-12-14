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
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/master_dataset3.csv")

# filtering dataset for phytoplankton data only 
dataset_phyto <- dataset_final[which(dataset_final$community=='phyto'), ]

# filtering data for press disturbance only 
dataset_phyto_press <- dataset_phyto[which(dataset_phyto$Treatment=='S'), ]

dataset_phyto_press = subset(dataset_phyto_press, select = -16)

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for press disturbance only 
dataset_zoopl_press <- dataset_zoopl[which(dataset_zoopl$Treatment=='S'), ]

# filtering for body size and biomass 
dataset_zoopl_press <- select(dataset_zoopl_press, Lake, Experiment, Treatment, Enclosure, 
                              initial_zoop_body_size, mean_biomass)

# renaming columns
names(dataset_zoopl_press)[names(dataset_zoopl_press)=="mean_biomass"] <- "zoopl_biomass"
names(dataset_zoopl_press)[names(dataset_zoopl_press)=="initial_zoop_body_size"] <- "zoopl_body_size"

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
hist(dataset_phyto_press$zoopl_biomass)
shapiro.test(dataset_phyto_press$zoopl_biomass)
# p-value = 0.0007597
dataset_phyto_press$zoopl_biomass_log <- log(dataset_phyto_press$zoopl_biomass)
hist(dataset_phyto_press$zoopl_biomass_log)
shapiro.test(dataset_phyto_press$zoopl_biomass_log)
# p-value = 0.04232

#---------------------------------------------------------------------------------------------------------#
# Final reovery, resilience, area under the curve

# checking normal distribution of resilience
hist(dataset_phyto_press$rate_change_oti)
shapiro.test(dataset_phyto_press$rate_change_oti)
# p-value = 2.28e-06
dataset_phyto_press$rate_change_oti_trans <- sqrt(dataset_phyto_press$rate_change_oti*-1)
hist(dataset_phyto_press$rate_change_oti_trans)
shapiro.test(dataset_phyto_press$rate_change_oti_trans)
# p-value = 0.04547

# checking normal distribution of AUC
hist(dataset_phyto_press$total_impact)
shapiro.test(dataset_phyto_press$total_impact)
# p-value = 3.368e-06
dataset_phyto_press$total_impact_log <- log(dataset_phyto_press$total_impact)
hist(dataset_phyto_press$total_impact_log)
shapiro.test(dataset_phyto_press$total_impact_log)
# p-value = 0.5938

# checking normal distribution of final recovery
hist(dataset_phyto_press$final_stab)
shapiro.test(dataset_phyto_press$final_stab)
# p-value = 0.000172
dataset_phyto_press$final_stab_log <- log(dataset_phyto_press$final_stab*-1)
hist(dataset_phyto_press$final_stab_log)
shapiro.test(dataset_phyto_press$final_stab_log)
# p-value = 0.07287

#---------------------------------------------------------------------------------------------------------#
#### 1. final recovery for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# final recovery = response
# TP, TN, DOC, Temp, PAR, Chl a, evenness, ENS, richness, zoopl biomass = explanatory

# testing significance of all variables individually 
final_recovery <- lmer(final_stab_log ~ mean_Temp + (1|Lake), data=dataset_phyto_press)

summary(final_recovery)
# TP p-value = 0.17809
# TN p-value = 0.20662
# DOC p-value = 0.491
# Temp p-value = 0.0472 !
# PAR p-value = 0.108327  
# Chl a p-value = 0.00849 ! but single fit
# evenness p-value = 0.0823
# ENS p-value = 0.2184
# richness p-value = 0.111462 
# zoopl biomass p-value = 0.4605

# --> Temp

# running linear mixed-effects model for final recovery
final_recovery <- lmer(final_stab_log ~ mean_Temp + (1|Lake), data=dataset_phyto_press)

# excluding  mean_TN because of singular fit 

summary(final_recovery)

# explained variance
r.squaredGLMM(final_recovery)
#            R2m       R2c
# [1,] 0.4721046 0.6096454

#---------------------------------------------------------------------------------------------------------#
#### 2. resilience for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience
# TP, TN, DOC, Temp, PAR, Chl a, evenness, ENS, richness, zoopl biomass = explanatory

# testing significance of all variables individually 
resilience <- lmer(rate_change_oti_trans ~ mean_TP + (1|Lake),data=dataset_phyto_press)

summary(resilience)
# TP p-value = 0.20865 
# TN p-value = 0.7484
# DOC p-value = 0.416
# Temp p-value = 0.002751 !
# PAR p-value = 0.19992  
# Chl a p-value = 0.0275 ! but singular fit
# evenness p-value = 0.4011 
# ENS p-value = 0.0842
# richness p-value =  0.20590  
# zoopl biomass p-value = 0.860

# --> Temp

resilience <- lmer(rate_change_oti_trans ~ mean_Temp + mean_Chla_log + (1|Lake),data=dataset_phyto_press)

summary(resilience)


r.squaredGLMM(resilience)
#           R2m       R2c
# [1,] 0.2071197 0.2368784

#---------------------------------------------------------------------------------------------------------#
#### 3. AUC for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC
# TP, TN, DOC, Temp, PAR, Chl a, evenness, ENS, richness, zoopl biomass = explanatory

# testing significance of all variables individually
AUC <- lmer(total_impact_log ~ mean_TP +(1|Lake), data=dataset_phyto_press)

summary(AUC)
# TP p-value = 0.148816
# TN p-value = 0.6761
# DOC p-value = 0.786
# Temp p-value = 0.012 !
# Chl a p-value = 0.0145 !
# evenness p-value = 0.119
# ENS p-value = 0.0289 !
# richness p-value =  0.535  
# zoopl biomass p-value = 0.03672 !

# --> Temp, Chl a , ENS, zoopl biomass 

test <- lm(total_impact_log ~  mean_Temp + mean_Chla_log + mean_ENS_D + zoopl_biomass_log, data=dataset_phyto_press)
vif(test)
#  mean_Temp     mean_Chla_log        mean_ENS_D zoopl_biomass_log 
# 1.699506          9.021920          4.704285          3.760333 

# excluding Chl a 
AUC <- lmer(total_impact_log ~  mean_Temp + mean_ENS_D + (1|Lake), data=dataset_phyto_press)

summary(AUC)

r.squaredGLMM(AUC)
# R2m       R2c
# [1,] 0.3569754 0.5928481

#---------------------------------------------------------------------------------------------------------#
#### 4. Biovolume for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = Chl a as biomass proxy
# TP, TN, DOC, Temp, PAR, zoopl biomass = explanatory

Chla <- lmer(mean_Chla_log ~ mean_TP + (1|Lake), data=dataset_phyto_press)
summary(Chla)
# TP p-value = 4.34e-06 !
# TN p-value = 0.000265 !
# DOC p-value = 0.000407 !
# Temp p-value = 0.555 
# PAR p-value = 2.56e-09 !
# zoopl biomass = 0.0595 !

Chla <- lmer(mean_Chla_log ~ mean_TP + mean_PAR_log + (1|Lake), data=dataset_phyto_press)

summary(Chla)

#---------------------------------------------------------------------------------------------------------#
#### 5. Evenness for phytoplankton ####
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
# PAR p-value = 0.0697 
# Chl a p-value = 0.00141 ! but singular fit
# zoopl biomass p-value = 0.91  


#---------------------------------------------------------------------------------------------------------#
#### 6. ENS for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# TP, TN, OC, Temp, PAR, Chl a, zoopl biomass = explanatory

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

# --> DOC, PAR, Chl a 

ENS <- lmer(mean_ENS_D ~  mean_DOC_sqrt + mean_PAR_log + mean_Chla_log + (1|Lake), data=dataset_phyto_press)

summary(ENS)

r.squaredGLMM(ENS)
#     R2m       R2c
# [1,] 0.382522 0.9930729

#---------------------------------------------------------------------------------------------------------#
#### 7. SEM ####
#---------------------------------------------------------------------------------------------------------##  

## final individual models 
AUC <- lmer(total_impact_log ~  mean_ENS_D + mean_Chla_log + mean_PAR_log + (1|Lake), data=dataset_phyto_press)
ENS <- lmer(mean_ENS_D ~  mean_TP + mean_DOC_sqrt + mean_PAR_log + (1|Lake), data=dataset_phyto_press)
Chla <- lmer(mean_Chla_log ~ mean_TP + mean_PAR_log + mean_DOC_sqrt + (1|Lake), data=dataset_phyto_press)

dataset_phyto_press_complete <- na.omit(dataset_phyto_press)

# SEM
modelList <- psem(
  AUC,
  ENS,  
  Chla,
  dataset_phyto_press_complete
  )


results <- summary(modelList)
# excluding recovery, Temperature and resilience because of singular fit 

rsquared(modelList)

results$IC$AIC
# 41.082

results$dTable

results$Cstat
#  Fisher.C df P.Value
# 1    5.082  6   0.533

results$coefficients

